open Transport;

let bind = (f, opt) => Result.bind(opt, f);

module ByteParser = {
  let readUInt8 = bytes => {
    let result = Bytes.get_uint8(bytes, 0);
    let bytes = Bytes.sub(bytes, 1, Bytes.length(bytes) - 1);
    Ok((result, bytes));
  };

  let readUInt32: bytes => result((int, bytes), string) =
    bytes => {
      Bytes.get_int32_be(bytes, 0)
      |> Int32.unsigned_to_int
      |> Option.to_result(~none="Invalid conversion of int32 to int")
      |> Result.map((v: int) => {
           let bytes = Bytes.sub(bytes, 4, Bytes.length(bytes) - 4);
           (v, bytes);
         });
    };

  let readShortString = bytes => {
    let len = Bytes.length(bytes);
    let strLength = Bytes.get_uint8(bytes, 0);
    let str = Bytes.sub(bytes, 1, strLength) |> Bytes.to_string;
    let bytes = Bytes.sub(bytes, 1 + strLength, len - 1 - strLength);
    Ok((str, bytes));
  };

  let readLongString: bytes => result((string, bytes), string) =
    bytes => {
      let len = Bytes.length(bytes);
      let maybeStrLength =
        Bytes.get_int32_be(bytes, 0) |> Int32.unsigned_to_int;
      maybeStrLength
      |> Option.to_result(~none="Invalid conversion of int32 to int")
      |> Result.map(strLen => {
           let str = Bytes.sub(bytes, 4, strLen) |> Bytes.to_string;
           let bytes = Bytes.sub(bytes, 4 + strLen, len - 4 - strLen);
           (str, bytes);
         });
    };

  let readJSONArgs = bytes => {
    bytes
    |> readUInt8
    |> bind(((rpcId, buffer)) =>
         buffer
         |> readShortString
         |> Result.map(((method, buffer)) => (rpcId, method, buffer))
       )
    |> bind(((rpcId, method, buffer)) =>
         buffer
         |> readLongString
         |> Result.map(((args, buffer)) => (rpcId, method, args))
       )
    |> bind(((rpcId, method, args)) =>
         try({
           let json = args |> Yojson.Safe.from_string;
           Ok((rpcId, method, json));
         }) {
         | exn => Error(Printexc.to_string(exn))
         }
       );
  };
};

module Message = {
  [@deriving (show, yojson({strict: false}))]
  type ok =
    | Empty
    | Json(Yojson.Safe.t)
    | Bytes(bytes);

  [@deriving (show, yojson({strict: false}))]
  type error =
    | Empty
    | Message(string);

  [@deriving (show, yojson({strict: false}))]
  type t =
    | Connected
    | Initialized
    | Ready
    | Terminate
    | RequestJSONArgs({
        requestId: int,
        rpcId: int,
        method: string,
        args: Yojson.Safe.t,
        usesCancellationToken: bool,
      })
    // TODO
    | RequestMixedArgs
    | Acknowledged({requestId: int})
    | Cancel({requestId: int})
    | TellOk(ok)
    | Error(error)
    | Unknown(bytes)
    | Closing
    | Disconnected;

  // Needs to be in sync with rpcProtocol.t
  let requestJsonArgs = 1;
  let requestJsonArgsWithCancellation = 2;
  let requestMixedArgs = 3;
  let requestMixedArgsWithCancellation = 4;
  let acknowledged = 5;
  let cancel = 6;
  let replyOkEmpty = 7;
  let replyOkBuffer = 8;
  let replyOkJSON = 9;
  let replyErrError = 10;
  let replyErrEmpty = 11;

  let terminate = (~id) => {
    let bytes = Bytes.create(1);
    Bytes.set_uint8(bytes, 0, 3);
    Packet.create(~id, ~bytes, ~packetType=Regular);
  };

  let ofPacket = (packet: Packet.t) => {
    let {body, _}: Packet.t = packet;

    let len = Bytes.length(body);
    prerr_endline("ofPacket - length: " ++ string_of_int(len));

    if (len == 0) {
      Ok(Unknown(body));
    } else if (len == 1) {
      let byte = Bytes.get_uint8(body, 0);
      (
        switch (byte) {
        | 1 => Initialized
        | 2 => Ready
        | 3 => Terminate
        | _ => Unknown(body)
        }
      )
      |> Result.ok;
    } else {
      body
      |> ByteParser.readUInt8
      |> bind(((messageType, buffer)) => {
           buffer
           |> ByteParser.readUInt32
           |> Result.map(((reqId, buffer)) => (messageType, reqId, buffer))
         })
      |> bind(((messageType, requestId, buffer)) => {
           prerr_endline(
             "Got a message of type: " ++ string_of_int(messageType),
           );
           if (messageType == requestJsonArgs
               || messageType == requestJsonArgsWithCancellation) {
             let usesCancellationToken =
               messageType == requestJsonArgsWithCancellation;
             buffer
             |> ByteParser.readJSONArgs
             |> Result.map(((rpcId, method, args)) =>
                  RequestJSONArgs({
                    requestId,
                    rpcId,
                    method,
                    args,
                    usesCancellationToken,
                  })
                );
           } else {
             Error("Unknown message - type: " ++ string_of_int(messageType));
           };
         });
    };
  };
};

type t = {transport: ref(option(Transport.t))};

let start =
    (~namedPipe: string, ~initData: InitData.t, ~dispatch: Message.t => unit) => {
  let transport = ref(None);

  let send = packet =>
    switch (transport^) {
    | Some(t) => Transport.send(~packet, t)
    | None => ()
    };

  let onPacket = (packet: Transport.Packet.t) => {
    Message.(
      if (packet.header.packetType == Packet.Regular) {
        let message = Message.ofPacket(packet);

        message
        |> Result.iter(msg => {
             switch (msg) {
             | Initialized =>
               //send(Message.terminate(~id=2));
               prerr_endline("INITIALIZED!");
             | Ready =>
               let bytes =
                 initData
                 |> InitData.to_yojson
                 |> Yojson.Safe.to_string
                 |> Bytes.of_string;

               let packet =
                 Packet.create(~bytes, ~packetType=Packet.Regular, ~id=1);

               send(packet);
               prerr_endline("READY!");
             | _ => ()
             };

             dispatch(msg);
           });

        message |> Result.iter_error(msg => dispatch(Error(Message(msg))));
      }
    );
  };

  let transportHandler = msg =>
    switch (msg) {
    | Transport.Error(msg) => dispatch(Message.Error(Message(msg)))
    | Transport.Connected => dispatch(Message.Connected)
    | Transport.Disconnected => dispatch(Message.Disconnected)
    | Transport.Closing => dispatch(Message.Closing)
    | Transport.Received(packet) => onPacket(packet)
    };

  let resTransport = Transport.start(~namedPipe, ~dispatch=transportHandler);

  resTransport |> Result.iter(t => transport := Some(t));

  resTransport |> Result.map(_ => {transport: transport});
};

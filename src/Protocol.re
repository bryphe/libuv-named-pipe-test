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
    | Initialized
    | Ready
    | Terminate
    | RequestJSONArgs({
        requestId: int,
        rpcId: int,
        method: string,
        args: string,
        usesCancellationToken: bool,
      })
    // TODO
    | RequestMixedArgs
    | Acknowledged({requestId: int})
    | Cancel({requestId: int})
    | Ok(ok)
    | Error(error)
    | Unknown(bytes);

  let ofPacket = (packet: Packet.t) => {
    let {body, _}: Packet.t = packet;

    if (Bytes.length(body) == 1) {
      let byte = Bytes.get_uint8(body, 0);
      switch (byte) {
      | 1 => Initialized
      | 2 => Ready
      | 3 => Terminate
      | _ => Unknown(body)
      };
    } else {
      Unknown(body);
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

  let onPacket = packet => {
    let message = Message.ofPacket(packet);

    switch (message) {
    | Initialized => prerr_endline("INITIALIZED!")
    | Ready =>
      let bytes =
        initData
        |> InitData.to_yojson
        |> Yojson.Safe.to_string
        |> Bytes.of_string;

      let packet =
        Packet.create(~bytes, ~packetType=Packet.Type.Regular, ~id=1);

      send(packet);
      prerr_endline("READY!");
    | _ => ()
    };

    dispatch(message);
  };

  let transportHandler = msg =>
    switch (msg) {
    | Transport.Error(msg) => dispatch(Message.Error(Message(msg)))
    | Transport.Connected => ()
    | Transport.Disconnected => ()
    | Transport.Closing => ()
    | Transport.Received(packet) => onPacket(packet)
    };

  let resTransport = Transport.start(~namedPipe, ~dispatch=transportHandler);

  resTransport |> Result.iter(t => transport := Some(t));

  resTransport |> Result.map(_ => {transport: transport});
};

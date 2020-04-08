module Message: {
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
        args: Yojson.Safe.t,
        usesCancellationToken: bool,
      })
    // TODO
    | RequestMixedArgs
    | Acknowledged({requestId: int})
    | Cancel({requestId: int})
    | TellOk(ok)
    | Error(error)
    | Unknown(bytes);

  let ofPacket: Transport.Packet.t => result(t, string);
};

type t;

let start:
  (~namedPipe: string, ~initData: InitData.t, ~dispatch: Message.t => unit) =>
  result(t, string);

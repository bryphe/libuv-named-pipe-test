type msg =
  | Received(Protocol.Packet.t)
  | Error(string)
  | Disconnected
  | Closing;

type t;

let start: (~namedPipe: string, ~dispatch: msg => unit) => result(t, string);

let close: t => unit;

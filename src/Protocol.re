module Constants = {
  let headerByteLength = 13;
};

module Packet = {
  module Type = {
    type t =
      | None
      | Regular
      | Control
      | Ack
      | KeepAlive
      | Disconnect;

    let of_int =
      fun
      | 0 => Ok(None)
      | 1 => Ok(Regular)
      | 2 => Ok(Control)
      | 3 => Ok(Ack)
      | 4 => Ok(KeepAlive)
      | 5 => Ok(Disconnect)
      | v => Error(Printf.sprintf("Unknown packet type: %d", v));
  };

  // The header for a packet is defined as follows by the VSCode extension host:
  // Byte 0 - uint8 - message type
  // Byte 1 - uint32 (big endian) - id
  // Byte 5 - uint32 (big endian) - ack
  // Byte 9 - unit32 (big endian) - data length
  module Header = {
    type t = {
      packetType: Type.t,
      id: int32,
      ack: int32,
      length: int32,
    };

    let of_bytes = bytes =>
      if (Bytes.length(bytes) != Constants.headerByteLength) {
        Error(
          Printf.sprintf("Incorrect header size: %d", Bytes.length(bytes)),
        );
      } else {
        let packetTypeUint = Bytes.get_uint8(bytes, 0);
        let id = Bytes.get_int32_be(bytes, 1);
        let ack = Bytes.get_int32_be(bytes, 5);
        let length = Bytes.get_int32_be(bytes, 9);

        packetTypeUint
        |> Type.of_int
        |> Result.map(packetType => {packetType, id, ack, length});
      };
  };

  type t = {
    header: Header.t,
    body: Bytes.t,
  };
};

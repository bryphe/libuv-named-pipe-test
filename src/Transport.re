type msg =
  | Connected
  | Received(Packet.t)
  | Error(string)
  | Disconnected
  | Closing;

type t = Luv.Pipe.t;

let start = (~namedPipe: string, ~dispatch: msg => unit) => {
  let handleError = (msg, err) => {
    let msg = Printf.sprintf("%s: %s\n", msg, Luv.Error.strerror(err));
    dispatch(Error(msg));
  };

  let read = clientPipe => {
    let parser = ref(Packet.Parser.initial);

    let readBuffer = (buffer: Luv.Buffer.t) => {
      let bytes = Luv.Buffer.to_bytes(buffer);

      let (newParser, packets) =
        try(Packet.Parser.parse(bytes, parser^)) {
        // TODO: Proper exception
        | exn =>
          dispatch(Error(Printexc.to_string(exn)));
          (Packet.Parser.initial, []);
        };

      parser := newParser;
      packets |> List.iter(packet => dispatch(Received(packet)));
    };

    let handleClosed = () => {
      dispatch(Disconnected);
      Luv.Handle.close(clientPipe, ignore);
    };

    Luv.Stream.read_start(
      clientPipe,
      fun
      | Error(`EOF) => handleClosed()
      | Error(msg) => handleError("read_start", msg)
      | Ok(buffer) => readBuffer(buffer),
    );
  };

  // Listen for an incoming connection...
  let listen = serverPipe => {
    Luv.Stream.listen(
      serverPipe,
      listenResult => {
        // Create a pipe for the client
        let clientPipeResult =
          listenResult |> (r => Stdlib.Result.bind(r, _ => Luv.Pipe.init()));

        switch (clientPipeResult) {
        | Ok(pipe) =>
          dispatch(Connected);
          read(pipe);
        | Error(err) => handleError("listen", err)
        };
      },
    );
  };

  let serverPipeResult =
    Luv.Pipe.init() |> Result.map_error(Luv.Error.strerror);

  serverPipeResult |> Result.iter(listen);

  serverPipeResult;
};

let close = server => Luv.Handle.close(server, ignore);

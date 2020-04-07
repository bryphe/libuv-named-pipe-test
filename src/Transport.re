type msg =
  | Received(Protocol.Packet.t)
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
    Luv.Stream.read_start(
      clientPipe,
      fun
      | Error(`EOF) => {
          dispatch(Disconnected);
          Luv.Handle.close(clientPipe, ignore);
        }
      | Error(msg) => handleError("read_start", msg)
      | Ok(buffer) => prerr_endline("Got buffer!"),
    );
  };

  let listen = serverPipe => {
    Luv.Stream.listen(
      serverPipe,
      listenResult => {
        // Create a pipe for the client
        let clientPipeResult =
          listenResult |> (r => Stdlib.Result.bind(r, _ => Luv.Pipe.init()));

        switch (clientPipeResult) {
        | Ok(pipe) => read(pipe)
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

type msg =
  | Connected
  | Received(Packet.t)
  | Error(string)
  | Disconnected
  | Closing;

type t = {
  server: Luv.Pipe.t,
  maybeClient: ref(option(Luv.Pipe.t)),
};

let start = (~namedPipe: string, ~dispatch: msg => unit) => {

  let maybeClient = ref(None);

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

    maybeClient := Some(clientPipe);
    let handleClosed = () => {
      maybeClient := None;
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
    prerr_endline ("Listening...")
    Luv.Pipe.bind(serverPipe, namedPipe) |> ignore;
    Luv.Stream.listen(
      serverPipe,
      listenResult => {
        prerr_endline("Got listen result");
        // Create a pipe for the client
        let clientPipeResult =
          listenResult |> (r => {
          prerr_endline("Trying to create client pipe...");
          Stdlib.Result.bind(r, _ => Luv.Pipe.init());
          })
          |> r => {
            Stdlib.Result.bind(r, pipe => {
            Luv.Stream.accept(~server=serverPipe, ~client=pipe)
            |> Result.map(_ => pipe);
            })
          };

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
  
  serverPipeResult |> Result.map(server => {
    server,
    maybeClient
  });
};

let send = (~packet, {maybeClient, }) => switch(maybeClient^) {
| None => prerr_endline ("No client, yet!");
| Some(c) =>
   let bytes = Packet.toBytes(packet);
   let byteLen= bytes |> Bytes.length;
   prerr_endline (Printf.sprintf("Sending %d bytes...", byteLen));
   let buffer = Luv.Buffer.from_bytes(bytes);
   // TODO: FIX PENDING BYTES
   Luv.Stream.write(c, [buffer], (err, count) => {
    prerr_endline (Printf.sprintf("Wrote %d bytes...", count)); 
   });
};

let close = ({server, _}) => Luv.Handle.close(server, ignore);

print_endline("Hello, world!");

let server = Luv.Pipe.init() |> Result.get_ok;

Luv.Pipe.bind(server, "/tmp/test-pipe22") |> ignore;

Luv.Stream.listen(
  server,
  fun
  | Error(e) => Printf.eprintf("Listen error: %s\n", Luv.Error.strerror(e))
  | Ok () => {
      print_endline("Starting server...");
      let client = Luv.Pipe.init() |> Result.get_ok;

      switch (Luv.Stream.accept(~server, ~client)) {
      | Error(_) =>
        prerr_endline("Closing!");
        Luv.Handle.close(client, ignore);
      | Ok () =>
        prerr_endline("Got client!");
        Luv.Stream.read_start(
          client,
          fun
          | Error(`EOF) => Luv.Handle.close(client, ignore)
          | Error(msg) =>
            Printf.eprintf("Read error: %s\n", Luv.Error.strerror(msg))
          | Ok(buffer) => {
              //prerr_endline ("Got buffer");
              Printf.printf(
                "Got buffer: %s\n",
                Luv.Buffer.to_string(buffer),
              );
              flush_all();
            },
          //failwith("oh no");
          //print_endline("yo");
        );
        prerr_endline("After read_start");
      };
    },
);

Luv.Loop.run() |> ignore;
print_endline("Done!");

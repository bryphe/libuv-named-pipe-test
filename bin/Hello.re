print_endline("Hello, world!");

open Exthost;
open Exthost.Transport;

let runningServer: ref(option(Exthost.Transport.t)) = ref(None);
let hasSent = ref(false);

let dispatch = 
  fun
  | Connected => prerr_endline ("Connected")
  | Received(packet) => {
    prerr_endline ((Unix.gettimeofday() |> string_of_float) ++ " Packet: \n" ++ Packet.toString(packet))

  let initData = {|
  {
    "version": "9.9.9",
    "parentPid": 1,
    "environment": {},
    "resolvedExtensions": [],
    "hostExtensions": [],
    "extensions": [],
    "telemetryInfo": {},
    "logLevel": 0,
    "logsLocation": { "scheme": "file", "authority": null, "path": null, "path": "/tmp/loggy" },
    "logFile": { "scheme": "file", "authority": null, "path": null, "path": "/tmp/log-file" },
    "autoStart": true,
    "remote": { "isRemote": false }
  }
  |}
  
  let bytes = initData |> Bytes.of_string;
  let packet = Packet.create(~bytes, ~packetType=Packet.Type.Regular, ~id=1);
  if (!hasSent^) {
    switch (runningServer^) {
    | None => ()
    | Some(server) => Exthost.Transport.send(~packet, server);
    };
    hasSent := true;
  };
  }
  | Error(msg) => prerr_endline ("ERROR: " ++ msg)
  | Disconnected => prerr_endline ("Disconnected")
  | Closing => prerr_endline ("Closing");

let serverResult = Exthost.Transport.start(
  ~namedPipe="/tmp/test-pipe122.sock",
  ~dispatch,
)
|> Result.iter((server) => runningServer := Some(server));

Luv.Loop.run() |> ignore;

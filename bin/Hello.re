print_endline("Hello, world!");

open Exthost;

Printexc.record_backtrace(true);

let extensions = InitData.Extension.[{
  identifier: "oni-dev-extension",
  extensionLocation: Uri.fromPath("/Users/bryphe/reason-vscode-exthost/test_collateral/extensions/oni-activation-events-tests"),
  version: "9.9.9",
  name: "oni-dev-extension",
  main: Some("./extension.js"),
  engines:"vscode",
  activationEvents: ["*"],
  extensionDependencies: [],
  extensionKind: "ui",
  enableProposedApi: true,
}];

/*let runningServer: ref(option(Exthost.Transport.t)) = ref(None);
  let hasSent = ref(false);

  let dispatch =
    fun
    | Connected => prerr_endline ("Connected")
    | Received(packet) => {
      prerr_endline ((Unix.gettimeofday() |> string_of_float) ++ " Packet: \n" ++ Packet.toString(packet))

    let initData = InitData.create(
      ~version="9.9.9",
      ~parentPid=1,
      ~logsLocation=Uri.fromPath("/tmp/loggy"),
      ~logFile=Uri.fromPath("/tmp/log-file"),
      []
    )
    |> InitData.to_yojson
    |> Yojson.Safe.to_string;

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
    ~namedPipe="/tmp/test-pipe128.sock",
    ~dispatch,
  )
  |> Result.iter((server) => runningServer := Some(server));*/

let initData =
  InitData.create(
    ~version="9.9.9",
    ~parentPid=1,
    ~logsLocation=Uri.fromPath("/tmp/loggy"),
    ~logFile=Uri.fromPath("/tmp/log-file"),
    extensions,
  );
//  |> InitData.to_yojson
//  |> Yojson.Safe.to_string;

let dispatch = msg => {
  prerr_endline("Got message: " ++ Protocol.Message.show(msg));
};

let protocol =
  Protocol.start(~namedPipe="/tmp/test-pipe151.sock", ~initData, ~dispatch);

Luv.Loop.run() |> ignore;

const net = require("net");

var server = net.createServer(stream => {
	stream.on("data", (c) => console.log(`data: ${c.length}|${c}|`));
	stream.on("end", () => server.close());
});

server.listen("/tmp/test-pipe5");

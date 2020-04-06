const net = require("net");

/*var server = net.createServer(stream => {
	stream.on("data", (c) => console.log(`data: ${c.length}|${c}|`));
	stream.on("end", () => server.close());
});

server.listen("/tmp/test4.sock");*/

const client = net.connect("/tmp/test-pipe21", () => {
	console.log("Hey!");
	setInterval(() => {
	client.write("hi\n");
	client.write("hello again2\n");
	}, 1000);
});

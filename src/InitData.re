[@deriving (show, yojson({strict: false}))]
type extensionInfo = unit;

module Environment = {
	[@deriving (show, yojson({strict: false}))]
	type t = {
		isExtensionDevelopmentDebug: bool,
		appName: string,
		// TODO
		/*
		appRoot: option(Uri.t),
		appLanguage: string,
		appUriScheme: string,
		appSettingsHome: option(Uri.t),
		globalStorageHome: Uri.t,
		userHome: Uri.t,
		webviewResourceRoot: string,
		webviewCspSource: string,
		useHostProxy: boolean,
		*/
	};

	let default = {
		isExtensionDevelopmentDebug: false,
		appName: "reason-vscode-exthost",
	}
};

[@deriving (show, yojson({strict: false}))]
module Remote = {
	[@deriving (show, yojson({strict: false}))]
	type t = {
		isRemote: bool,
		// TODO:
		// authority: string,
	};

	let default = { isRemote: false };
};

[@deriving (show, yojson({strict: false}))]
type t = {
	version: string,
	parentPid: int,
	extensions: list(extensionInfo),
	resolvedExtensions: list(extensionInfo),
	hostExtensions: list(extensionInfo),
	environment: Environment.t,
	logLevel: int,
	logsLocation: Uri.t,
	logFile: Uri.t,
	autoStart: bool,
	remote: Remote.t,
};

let create = (
	~version,
	~parentPid,
	~logsLocation,
	~logFile,
	~environment=Environment.default,
	~resolvedExtensions=[],
	~hostExtensions=[],
	~logLevel=0,
	~autoStart=true,
	~remote=Remote.default,
	extensions,
) => {
	version,
	parentPid,
	logLevel,
	extensions,
	resolvedExtensions,
	hostExtensions,
	environment,
	logsLocation,
	logFile,
	autoStart,
	remote,
}

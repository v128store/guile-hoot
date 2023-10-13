var waitFor;
if (typeof drainJobQueue !== 'undefined') {
    waitFor = function waitFor(p) { drainJobQueue(); return p; };
} else {
    // JSC and V8 will drain promises before exiting and don't require a
    // specific waiter.
    waitFor = function waitFor(p) { return p; };
}

var args;
if (typeof scriptArgs !== 'undefined') {
    args = scriptArgs;
} else if (typeof arguments !== 'undefined') {
    args = arguments;
} else {
    // No script arguments available
    args = [];
}

var log = print;
var logErr = print;
if (typeof printErr !== 'undefined') {
    logErr = printErr;
}

var _exit;
if (typeof quit !== 'undefined') {
    _exit = quit.bind(this);
} else if (typeof testRunner !== 'undefined') {
    _exit = testRunner.quit.bind(testRunner);
}

// V8 treats multiple arguments as files, unless -- is given, but
// SpiderMonkey doesn't treat -- specially.  This is a hack to allow
// for -- on SpiderMonkey.
if (args[0] == '--') {
    args.shift();
}

if (args.length < 2) {
    logErr('usage: test-call.js SRCDIR PROC.WASM ARG.WASM...');
    _exit(1);
}

async function runTest(call) {
    try {
        let [procFile, ...argFiles] = call;
        let [proc] = await Scheme.load_main(procFile);
        let argPromises =
            argFiles.map(file => proc.reflector.load_extension(file));
        let args = [];
        for (let p of argPromises) {
            let [arg] = await p;
            args.push(arg);
        }
        for (let result of proc.call(...args))
            log(repr(result));
    } catch (e) {
        log(`error: ${e} (${e.stack})`);
        _exit(1);
    }
}

var srcdir = args[0];
os.chdir(srcdir);
load('js-runtime/reflect.js');
args.shift();
waitFor(runTest(args));

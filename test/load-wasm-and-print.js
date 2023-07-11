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

if (args.length != 2) {
    logErr('usage: load-wasm-and-print.js SRCDIR FOO.WASM');
    _exit(1);
}

async function runTest(wasmFile) {
    try {
        for (let obj of await Scheme.load_main(wasmFile))
            log(repr(obj));
    } catch (e) {
        log(`error: ${e}`);
        _exit(1);
    }
}

load(`${args[0]}/js-runtime/reflect.js`);
waitFor(runTest(args[1]));

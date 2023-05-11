var waitFor;
if (typeof drainJobQueue !== 'undefined') {
    waitFor = function waitFor(p) { drainJobQueue(); return p; };
} else if (typeof testRunner !== 'undefined') {
    waitFor = function waitFor(p) {
        testRunner.waitUntilDone();
        return p.then(val=> { testRunner.notifyDone(); return val; },
                      err=> { throw err });
    };
} else {
    // JSC will drain promises before exiting and doesn't require a
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

if (args.length < 1) {
    logErr('usage: test-call.js PROC.WASM ARG.WASM...');
    _exit(1);
}

async function runTest(call) {
    try {
        let vals = []
        for (let file of call) {
            let [val] = await test_load(file);
            vals.push(val);
        }
        let [proc, ...args] = vals;
        for (let result of proc.call(...args))
            log(repr(result));
    } catch (e) {
        log(`error: ${e} (${e.stack})`);
        _exit(1);
    }
}

load('reflect.js');
waitFor(runTest(args));

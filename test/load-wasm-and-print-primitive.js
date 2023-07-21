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
  logErr('usage: load-wasm-and-print-primitive.js FOO.WASM FUNC [ARGS ...]');
  _exit(1);
}

async function instantiateStreaming(path, imports) {
  if (typeof fetch !== 'undefined')
    return WebAssembly.instantiateStreaming(fetch(path), imports);
  let bytes;
  if (typeof read !== 'undefined') {
    bytes = read(path, 'binary');
  } else if (typeof readFile !== 'undefined') {
    bytes = readFile(path);
  } else {
    let fs = require('fs');
    bytes = fs.readFileSync(path);
  }

  return WebAssembly.instantiate(bytes, imports);
}

async function compileAndRun(wasmFile, funcName, args) {
  const imports = {};
  const { module, instance } = await instantiateStreaming(wasmFile, imports);
  const f = instance.exports[funcName];
  return f.apply(null, args);
}

async function runTest(wasmFile, funcName, ...args) {
  const parsedArgs = args.map(JSON.parse);
  try {
    log(await compileAndRun(wasmFile, funcName, parsedArgs));
  } catch (e) {
    log(`error: ${e}`);
    _exit(1);
  }
}

waitFor(runTest.apply(null, args));

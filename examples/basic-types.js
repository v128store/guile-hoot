load('test-lib.js');

let scm = load_wasm('basic-types.wasm');

let result = scm.call_named("_init");
print(result);

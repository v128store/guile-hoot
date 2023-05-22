var args;
if (typeof scriptArgs !== 'undefined') {
    args = scriptArgs;
} else if (typeof arguments !== 'undefined') {
    args = arguments;
} else {
    // No script arguments available
    args = [];
}

srcdir = args[0] || '.';
builddir = args[1] || '.';
load(`${srcdir}/examples/test-lib.js`);

let scm = load_wasm(`${builddir}/examples/basic-types.wasm`);

let result = scm.call_named("_init");
print(result);

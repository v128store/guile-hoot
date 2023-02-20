let readBinaryFile = (()=> {
    if (typeof read !== 'undefined')
        return f => read(f, 'binary');
    if (typeof readFile !== 'undefined')
        return f => readFile(f);
    let fs = require('fs');
    return f => fs.readFileSync(f);
})();

function call_scheme(exports, func_name, ...args) {
    for (let [idx, arg] of args.entries())
        exports.set_arg(idx, arg);
    let nvals = exports[func_name](args.length);
    switch (nvals) {
        case 0:
            return;
        case 1:
            return exports.get_arg(0);
        default: {
            let values = [];
            for (let idx = 0; idx < nvals; idx++)
                values.push(exports.get_arg(idx));
            return values;
        }
    }
}

function scm_to_js(scm) {
    if (typeof(scm) == 'number') {
        if ((scm|0) === scm) {
            // If low tag is 1, it's a fixnum.
            if (scm & 1)
                return scm >> 1;
            // Otherwise it's an oddball.
            throw new Error(`unimplemented oddball: ${scm}`);
        } else {
            throw new Error(`got a float; weird: ${scm}`);
        }
    } else if (typeof(scm) == 'string') {
        // Stringref.
        return scm;
    } else {
        return scm;
    }
}

function main(file, expected) {
    let bytes = readBinaryFile(file);
    let mod = new WebAssembly.Module(bytes);
    let imports = {}
    let instance = new WebAssembly.Instance(mod, imports);
    let result = scm_to_js(call_scheme(instance.exports, "_init"));
    print(`expected: ${expected}; got: ${result}`);
    if (expected !== result.toString())
        throw new Error(`not equal`);
}

if (arguments.length != 2)
    throw new Error(`usage: d8 test.js -- test.wasm expected-result`);
    
main(...arguments)

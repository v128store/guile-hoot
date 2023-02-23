let readBinaryFile = (()=> {
    if (typeof read !== 'undefined')
        return f => read(f, 'binary');
    if (typeof readFile !== 'undefined')
        return f => readFile(f);
    let fs = require('fs');
    return f => fs.readFileSync(f);
})();

class Char {
    constructor(codepoint) {
        this.codepoint = codepoint;
    }
    toString() {
        return `#\\x${this.codepoint.toString(16)}`;
    }
}
class Eof { toString() { return "#eof"; } }
class Nil { toString() { return "#nil"; } }
class Null { toString() { return "()"; } }
class Unspecified { toString() { return "#<unspecified>"; } }

class HeapObject {
    constructor(obj) { this.obj = obj; }
}
class MutablePair extends HeapObject { toString() { return "#<mutable-pair>"; } }
class Pair extends HeapObject { toString() { return "#<pair>"; } }
class MutableBytevector extends HeapObject { toString() { return "#<mutable-bytevector>"; } }
class Bytevector extends HeapObject { toString() { return "#<bytevector>"; } }
class Struct extends HeapObject { toString() { return "#<struct>"; } }
class Procedure extends HeapObject { toString() { return "#<procedure>"; } }

class SCM {
    #rt = {
        bignum_is_i64(n) {
            return -0x8000_0000_0000_0000n <= n && n <= 0x7FFF_FFFF_FFFF_FFFFn;
        },
        bignum_is_u64(n) {
            return 0n <= n && n <= 0xFFFF_FFFF_FFFF_FFFFn;
        },
        // This truncates; see https://tc39.es/ecma262/#sec-tobigint64.
        bignum_get_i64(n) { return n; }
    };

    constructor(mod) {
        let imports = { rt: this.#rt };
        this.instance = new WebAssembly.Instance(mod, imports);
    }

    #to_scm(js) {
        let api = this.instance.exports;
        if (typeof(js) == 'number') {
            return api.scm_from_f64(js);
        } else if (typeof(js) == 'bigint') {
            return api.scm_from_integer(js);
        } else if (typeof(js) == 'boolean') {
            return js ? api.scm_true() : api.scm_false();
        } else if (typeof(js) == 'string') {
            return api.scm_from_string(js);
        } else if (typeof(js) == 'object') {
            if (js instanceof Eof) return api.scm_eof();
            if (js instanceof Nil) return api.scm_nil();
            if (js instanceof Null) return api.scm_null();
            if (js instanceof Unspecified) return api.scm_unspecified();
            if (js instanceof Char) return api.scm_from_char(js.codepoint);
            if (js instanceof HeapObject) return js.obj;
            throw new Error(`unhandled; ${typeof(js)}`);
        } else {
            throw new Error(`unexpected; ${typeof(js)}`);
        }
    }

    #to_js(scm) {
        let api = this.instance.exports;
        let descr = api.describe(scm);
        let handlers = {
            fixnum: () => BigInt(api.fixnum_value(scm)),
            bignum: () => api.bignum_value(scm),
            flonum: () => api.flonum_value(scm),
            char: () => new Char(api.char_value(scm)),
            true: () => true,
            false: () => false,
            eof: () => new Eof,
            nil: () => new Nil,
            null: () => new Null,
            unspecified: () => new Unspecified,
            string: () => api.string_value(scm),
            pair: () => new MutablePair(scm),
            'immutable-pair': () => new Pair(scm),
            bytevector: () => new MutableBytevector(scm),
            'immutable-bytevector': () => new Bytevector(scm),
            struct: () => new Struct(scm),
            procedure: () => new Procedure(scm),
        };
        print(descr);
        let handler = handlers[descr];
        return handler ? handler() : scm;
    }

    call_named(func_name, ...args) {
        let api = this.instance.exports;
        for (let [idx, arg] of args.entries())
            api.set_arg(idx, this.#to_scm(arg));
        let f = api[func_name];
        if (!f) throw new Error(`no such function: ${func_name}`);
        let nvals = f(args.length);
        let values = [];
        for (let idx = 0; idx < nvals; idx++)
            values.push(this.#to_js(api.get_arg(idx)));
        return values;
    }
}

function check_true(actual, what) {
    print(`checking ${what} is true`);
    if (!actual)
        throw new Error(`unexpected ${what}: ${actual}`);
}

function check_same(expected, actual, what) {
    print(`checking expected ${what}: ${expected}`);
    if (expected !== actual)
        throw new Error(`unexpected ${what}: ${actual}`);
}

function load_wasm(file) {
    let bytes = readBinaryFile(file);
    let mod = new WebAssembly.Module(bytes);
    return new SCM(mod);
}

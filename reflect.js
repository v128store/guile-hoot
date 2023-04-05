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

class Complex {
    constructor(real, imag) {
        this.real = real;
        this.imag = imag;
    }
    toString() {
        return `${this.real}+${this.imag}i`;
    }
}
class Fraction {
    constructor(num, denom) {
        this.num = num;
        this.denom = denom;
    }
    toString() {
        return `${this.num}/${this.denom}`;
    }
}

class HeapObject {
    constructor(obj) { this.obj = obj; }
}
class Pair extends HeapObject { toString() { return "#<pair>"; } }
class MutablePair extends HeapObject { toString() { return "#<mutable-pair>"; } }
class Vector extends HeapObject { toString() { return "#<vector>"; } }
class MutableVector extends HeapObject { toString() { return "#<mutable-vector>"; } }
class Bytevector extends HeapObject { toString() { return "#<bytevector>"; } }
class MutableBytevector extends HeapObject { toString() { return "#<mutable-bytevector>"; } }
class Bitvector extends HeapObject { toString() { return "#<bitvector>"; } }
class MutableBitvector extends HeapObject { toString() { return "#<mutable-bitvector>"; } }
class MutableString extends HeapObject { toString() { return "#<mutable-string>"; } }
class Procedure extends HeapObject { toString() { return "#<procedure>"; } }
class Sym extends HeapObject { toString() { return "#<symbol>"; } }
class Keyword extends HeapObject { toString() { return "#<keyword>"; } }
class Variable extends HeapObject { toString() { return "#<variable>"; } }
class AtomicBox extends HeapObject { toString() { return "#<atomic-box>"; } }
class HashTable extends HeapObject { toString() { return "#<hash-table>"; } }
class WeakTable extends HeapObject { toString() { return "#<weak-table>"; } }
class Fluid extends HeapObject { toString() { return "#<fluid>"; } }
class DynamicState extends HeapObject { toString() { return "#<dynamic-state>"; } }
class Syntax extends HeapObject { toString() { return "#<syntax>"; } }
class Port extends HeapObject { toString() { return "#<port>"; } }
class Struct extends HeapObject { toString() { return "#<struct>"; } }

function load_reflector_mod(abi) {
    if (typeof fetch !== 'undefined')
        return WebAssembly.instantiateStreaming(fetch("./reflect.wasm"),
                                                imports);
    let bytes = (() => {
        if (typeof read !== 'undefined')
            return f => read(f, 'binary');
        if (typeof readFile !== 'undefined')
            return f => readFile(f);
        let fs = require('fs');
        return f => fs.readFileSync(f);
    })();
    return WebAssembly.instantiate(bytes, imports);
}

class SchemeReflector {
    constructor(target, reflector_mod) {
        this.target = target;
        this.reflector_mod = reflector_mod;
    }

    static async reflect(target) {
        let abi = {}
        for (let [k, v] of target.exports.entries()) {
            if (k.startsWith("$"))
                abi[k] = v;
        }
        return new SchemeReflector(target,
                                   await load_reflector_mod({ abi }));
    }

    #to_scm(js) {
        let api = this.reflector_mod.exports;
        if (typeof(js) == 'number') {
            return api.scm_from_f64(js);
        } else if (typeof(js) == 'bigint') {
            if (BigInt(api.scm_most_negative_fixnum()) <= js
                && js <= BigInt(api.scm_most_positive_fixnum()))
                return api.scm_from_fixnum(Number(js));
            return api.scm_from_bignum(js);
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
            if (js instanceof Fraction)
                return api.scm_from_fraction(this.#to_scm(js.num),
                                             this.#to_scm(js.denom));
            if (js instanceof Complex)
                return api.scm_from_complex(js.real, js.imag);
            throw new Error(`unhandled; ${typeof(js)}`);
        } else {
            throw new Error(`unexpected; ${typeof(js)}`);
        }
    }

    #to_js(scm) {
        let api = this.reflector_mod.exports;
        let descr = api.describe(scm);
        let handlers = {
            fixnum: () => BigInt(api.fixnum_value(scm)),
            char: () => new Char(api.char_value(scm)),
            true: () => true,
            false: () => false,
            eof: () => new Eof,
            nil: () => new Nil,
            null: () => new Null,
            unspecified: () => new Unspecified,
            flonum: () => api.flonum_value(scm),
            bignum: () => api.bignum_value(scm),
            complex: () => new Complex(api.complex_real(scm),
                                       api.complex_imag(scm)),
            fraction: () => new Fraction(this.#to_js(api.fraction_num(scm)),
                                         this.#to_js(api.fraction_denom(scm))),
            pair: () => new Pair(scm),
            'mutable-pair': () => new MutablePair(scm),
            vector: () => new Vector(scm),
            'mutable-vector': () => new MutableVector(scm),
            bytevector: () => new Bytevector(scm),
            'mutable-bytevector': () => new MutableBytevector(scm),
            bitvector: () => new Bitvector(scm),
            'mutable-bitvector': () => new MutableBitvector(scm),
            string: () => api.string_value(scm),
            'mutable-string': () => new MutableString(scm),
            procedure: () => new Procedure(scm),
            symbol: () => new Sym(scm),
            keyword: () => new Keyword(scm),
            variable: () => new Variable(scm),
            'atomic-box': () => new AtomicBox(scm),
            'hash-table': () => new HashTable(scm),
            'weak-table': () => new WeakTable(scm),
            fluid: () => new Fluid(scm),
            'dynamic-state': () => new DynamicState(scm),
            syntax: () => new Syntax(scm),
            port: () => new Port(scm),
            struct: () => new Struct(scm),
        };
        let handler = handlers[descr];
        return handler ? handler() : scm;
    }

    call(func, ...args) {
        let api = this.reflector_mod.exports;
        let argv = api.make_vector(args.length, api.scm_false());
        for (let [idx, arg] of args.entries())
            api.vector_set(argv, idx, this.#to_scm(arg));
        argv = api.call(func, argv);
        let results = [];
        for (let idx = 0; idx < api.vector_length(argv); idx++)
            results.push(this.#to_js(api.vector_ref(argv, idx)))
        return results;
    }
}

function reflect(instance) {
    return SchemeReflector.reflect(instance);
}

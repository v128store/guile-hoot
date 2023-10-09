let readBinaryFile = (()=> {
    if (typeof read !== 'undefined')
        return f => read(f, 'binary');
    if (typeof readFile !== 'undefined')
        return f => readFile(f);
    let fs = require('fs');
    return f => fs.readFileSync(f);
})();

let Wtf8 = (() => {
    let bytes = readBinaryFile(`${builddir}/js-runtime/wtf8.wasm`);
    return new WebAssembly.Instance(new WebAssembly.Module(bytes));
})();
function wtf8_to_string(wtf8) {
    let { as_iter, iter_next } = Wtf8.exports;
    let codepoints = [];
    let iter = as_iter(wtf8);
    for (let cp = iter_next(iter); cp != -1; cp = iter_next(iter))
        codepoints.push(cp);
    return String.fromCodePoint(...codepoints);
}
function string_to_wtf8(str) {
    let { make_builder, builder_push_codepoint, finish_builder } = Wtf8.exports;
    let builder = make_builder()
    for (let cp of str)
        builder_push_codepoint(builder, cp.codePointAt(0));
    return finish_builder(builder);
}

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

class SCM {
    #argv = [];

    constructor(mod) {
        let argv = this.#argv;
        let rt = {
            prepare_return_values(n) { argv.length = n; },
            set_return_value(i, x) { argv[i] = x; },
            get_argument(i) { return argv[i]; },

            bignum_from_i64(n) { return n; },
            bignum_from_u64(n) { return n < 0n ? 0xffff_ffff_ffff_ffffn + (n + 1n) : n; },
            bignum_is_i64(n) {
                return -0x8000_0000_0000_0000n <= n && n <= 0x7FFF_FFFF_FFFF_FFFFn;
            },
            bignum_is_u64(n) {
                return 0n <= n && n <= 0xFFFF_FFFF_FFFF_FFFFn;
            },
            // This truncates; see https://tc39.es/ecma262/#sec-tobigint64.
            bignum_get_i64(n) { return n; },

            wtf8_to_string,
            string_to_wtf8,

            make_weak_map() { return new WeakMap; },
            weak_map_get(map, k) { return map.get(k); },
            weak_map_set(map, k, v) { return map.set(k, v); },
            weak_map_delete(map, k) { return map.delete(k); }
        };
        this.instance = new WebAssembly.Instance(mod, { rt });
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
        let api = this.instance.exports;
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

    call_named(func_name, ...args) {
        let api = this.instance.exports;
        this.#argv.length = args.length;
        for (let [idx, arg] of args.entries())
            this.#argv[idx] = this.#to_scm(arg);
        let f = api[func_name];
        if (!f) throw new Error(`no such function: ${func_name}`);
        f(args.length);
        let values = [];
        for (let idx = 0; idx < this.#argv.length; idx++)
            values.push(this.#to_js(this.#argv[idx]));
        this.#argv.length = 0;
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

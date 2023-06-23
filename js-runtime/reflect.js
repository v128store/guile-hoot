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
    constructor(reflector, obj) {
        this.reflector = reflector;
        this.obj = obj;
    }
    repr() { return this.toString(); } // Default implementation.
}

class Pair extends HeapObject {
    toString() { return "#<pair>"; }
    repr() {
        let car_repr = repr(this.reflector.car(this));
        let cdr_repr = repr(this.reflector.cdr(this));
        if (cdr_repr == '()')
            return `(${car_repr})`;
        if (cdr_repr.charAt(0) == '(')
            return `(${car_repr} ${cdr_repr.substring(1)}`;
        return `(${car_repr} . ${cdr_repr})`;
    }
}
class MutablePair extends Pair { toString() { return "#<mutable-pair>"; } }

class Vector extends HeapObject {
    toString() { return "#<vector>"; }
    repr() {
        let len = this.reflector.vector_length(this);
        let out = '#(';
        for (let i = 0; i < len; i++) {
            if (i) out += ' ';
            out += repr(this.reflector.vector_ref(this, i));
        }
        out += ')';
        return out;
    }
}
class MutableVector extends Vector {
    toString() { return "#<mutable-vector>"; }
}

class Bytevector extends HeapObject {
    toString() { return "#<bytevector>"; }
    repr() {
        let len = this.reflector.bytevector_length(this);
        let out = '#vu8(';
        for (let i = 0; i < len; i++) {
            if (i) out += ' ';
            out += repr(this.reflector.bytevector_ref(this, i));
        }
        out += ')';
        return out;
    }
}
class MutableBytevector extends Bytevector {
    toString() { return "#<mutable-bytevector>"; }
}

class Bitvector extends HeapObject {
    toString() { return "#<bitvector>"; }
    repr() {
        let len = this.reflector.bitvector_length(this);
        let out = '#*';
        for (let i = 0; i < len; i++) {
            out += this.reflector.bitvector_ref(this, i) ? '1' : '0';
        }
        return out;
    }
}
class MutableBitvector extends Bitvector {
    toString() { return "#<mutable-bitvector>"; }
}

class MutableString extends HeapObject {
    toString() { return "#<mutable-string>"; }
    repr() { return this.reflector.string_value(this); }
}

class Procedure extends HeapObject {
    toString() { return "#<procedure>"; }
    call(...arg) {
        return this.reflector.call(this, ...arg);
    }
}

class Sym extends HeapObject {
    toString() { return "#<symbol>"; }
    repr() { return this.reflector.symbol_name(this); }
}

class Keyword extends HeapObject {
    toString() { return "#<keyword>"; }
    repr() { return `#:${this.reflector.keyword_name(this)}`; }
}

class Variable extends HeapObject { toString() { return "#<variable>"; } }
class AtomicBox extends HeapObject { toString() { return "#<atomic-box>"; } }
class HashTable extends HeapObject { toString() { return "#<hash-table>"; } }
class WeakTable extends HeapObject { toString() { return "#<weak-table>"; } }
class Fluid extends HeapObject { toString() { return "#<fluid>"; } }
class DynamicState extends HeapObject { toString() { return "#<dynamic-state>"; } }
class Syntax extends HeapObject { toString() { return "#<syntax>"; } }
class Port extends HeapObject { toString() { return "#<port>"; } }
class Struct extends HeapObject { toString() { return "#<struct>"; } }

function instantiate_streaming(path, imports) {
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

class Scheme {
    #instance;
    #abi;
    constructor(instance, abi) {
        this.#instance = instance;
        this.#abi = abi;
    }

    static async reflect(abi) {
        let { module, instance } =
            await instantiate_streaming('js-runtime/reflect.wasm', { abi });
        return new Scheme(instance, abi);
    }

    #init_module(mod) {
        let proc = new Procedure(this, mod.get_export('$load').value)
        return proc.call();
    }
    static async load_main(path, abi) {
        let mod = await SchemeModule.fetch_and_instantiate(path, abi);
        let reflect = await mod.reflect();
        return reflect.#init_module(mod);
    }
    async load_extension(path) {
        let mod = await SchemeModule.fetch_and_instantiate(path, this.#abi);
        return this.#init_module(mod);
    }

    #to_scm(js) {
        let api = this.#instance.exports;
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
        let api = this.#instance.exports;
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
            pair: () => new Pair(this, scm),
            'mutable-pair': () => new MutablePair(this, scm),
            vector: () => new Vector(this, scm),
            'mutable-vector': () => new MutableVector(this, scm),
            bytevector: () => new Bytevector(this, scm),
            'mutable-bytevector': () => new MutableBytevector(this, scm),
            bitvector: () => new Bitvector(this, scm),
            'mutable-bitvector': () => new MutableBitvector(this, scm),
            string: () => api.string_value(scm),
            'mutable-string': () => new MutableString(this, scm),
            procedure: () => new Procedure(this, scm),
            symbol: () => new Sym(this, scm),
            keyword: () => new Keyword(this, scm),
            variable: () => new Variable(this, scm),
            'atomic-box': () => new AtomicBox(this, scm),
            'hash-table': () => new HashTable(this, scm),
            'weak-table': () => new WeakTable(this, scm),
            fluid: () => new Fluid(this, scm),
            'dynamic-state': () => new DynamicState(this, scm),
            syntax: () => new Syntax(this, scm),
            port: () => new Port(this, scm),
            struct: () => new Struct(this, scm),
        };
        let handler = handlers[descr];
        return handler ? handler() : scm;
    }

    call(func, ...args) {
        let api = this.#instance.exports;
        let argv = api.make_vector(args.length + 1, api.scm_false());
        func = this.#to_scm(func);
        api.vector_set(argv, 0, func);
        for (let [idx, arg] of args.entries())
            api.vector_set(argv, idx + 1, this.#to_scm(arg));
        argv = api.call(func, argv);
        let results = [];
        for (let idx = 0; idx < api.vector_length(argv); idx++)
            results.push(this.#to_js(api.vector_ref(argv, idx)))
        return results;
    }

    car(x) { return this.#to_js(this.#instance.exports.car(x.obj)); }
    cdr(x) { return this.#to_js(this.#instance.exports.cdr(x.obj)); }

    vector_length(x) { return this.#instance.exports.vector_length(x.obj); }
    vector_ref(x, i) {
        return this.#to_js(this.#instance.exports.vector_ref(x.obj, i));
    }

    bytevector_length(x) {
        return this.#instance.exports.bytevector_length(x.obj);
    }
    bytevector_ref(x, i) {
        return this.#instance.exports.bytevector_ref(x.obj, i);
    }

    bitvector_length(x) {
        return this.#instance.exports.bitvector_length(x.obj);
    }
    bitvector_ref(x, i) {
        return this.#instance.exports.bitvector_ref(x.obj, i) == 1;
    }

    string_value(x) { return this.#instance.exports.string_value(x.obj); }
    symbol_name(x) { return this.#instance.exports.symbol_name(x.obj); }
    keyword_name(x) { return this.#instance.exports.keyword_name(x.obj); }
}

class SchemeTrapError extends Error {
    constructor(tag, data) { super(); this.tag = tag; this.data = data; }
    toString() { return `SchemeTrap(${this.tag}, ${this.data})`; }
}

class SchemeModule {
    #instance;
    static #rt = {
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

        make_weak_map() { return new WeakMap; },
        weak_map_get(map, k) { return map.get(k); },
        weak_map_set(map, k, v) { return map.set(k, v); },
        weak_map_delete(map, k) { return map.delete(k); },

        die(tag, data) { throw new SchemeTrapError(tag, data); }
    };

    constructor(instance) {
        this.#instance = instance;
    }
    static async fetch_and_instantiate(path, imported_abi) {
        let imports = { rt: SchemeModule.#rt, abi: imported_abi }
        let { module, instance } = await instantiate_streaming(path, imports);
        return new SchemeModule(instance);
    }
    all_exports() { return this.#instance.exports; }
    exported_abi() {
        let abi = {}
        for (let [k, v] of Object.entries(this.all_exports())) {
            if (k.startsWith("$"))
                abi[k] = v;
        }
        return abi;
    }
    exports() {
        let ret = {}
        for (let [k, v] of Object.entries(this.all_exports())) {
            if (!k.startsWith("$"))
                ret[k] = v;
        }
        return ret;
    }
    get_export(name) {
        if (name in this.all_exports())
            return this.all_exports()[name];
        throw new Error(`unknown export: ${name}`)
    }
    async reflect() {
        return await Scheme.reflect(this.exported_abi());
    }
}

function repr(obj) {
    if (obj instanceof HeapObject)
        return obj.repr();
    return obj + '';
}

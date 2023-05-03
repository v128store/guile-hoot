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

class Vector extends HeapObject { toString() { return "#<vector>"; } }
class MutableVector extends Vector { toString() { return "#<mutable-vector>"; } }
class Bytevector extends HeapObject { toString() { return "#<bytevector>"; } }
class MutableBytevector extends Bytevector { toString() { return "#<mutable-bytevector>"; } }
class Bitvector extends HeapObject { toString() { return "#<bitvector>"; } }
class MutableBitvector extends Bitvector { toString() { return "#<mutable-bitvector>"; } }
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

class SchemeReflector {
    #instance;
    constructor(instance) {
        this.#instance = instance;
    }

    static async reflect(abi) {
        let { module, instance } =
            await instantiate_streaming('./reflect.wasm', { abi });
        return new SchemeReflector(instance);
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
        weak_map_delete(map, k) { return map.delete(k); }
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
        return await SchemeReflector.reflect(this.exported_abi());
    }
}

function repr(obj) {
    if (obj instanceof HeapObject)
        return obj.repr();
    return obj + '';
}

async function test_load(path) {
    let mod = await SchemeModule.fetch_and_instantiate(path);
    let reflect = await mod.reflect();
    return reflect.call(mod.get_export('$load').value)
}

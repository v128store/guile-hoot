# Guile on WebAssembly: ABI

[TOC]

## Data representation

### `SCM` unitype

Scheme is an untyped language; any value can be stored in the common
`SCM` unitype.  Which concrete kind of value a given `SCM` holds can be
determined by inspecting the value.  (Of course, Guile’s compiler can
unbox values and sometimes will be able to work in e.g. raw `f64`
values.  But we need a story for the general case).

Since we are targetting the [GC
MVP](https://github.com/WebAssembly/gc/blob/master/proposals/gc/MVP.md),
we can represent `SCM` as `(ref eq)`.  There is no need to allow
nullability in this type.

### Immediates

All immediates (fixnums, chars, bools, etc) are encoded as `(ref i31)`
values.  The 2<sup>31</sup> possible values are partitioned by tagging.
The partition is the similar to [what native Guile
does](http://git.savannah.gnu.org/cgit/guile.git/tree/module/system/base/types/internal.scm?h=wip-tailify#n101),
except that the bottom 0 bit is left off and we eliminate some
intermediate 0 bits.  The fixnum range is therefore the same as in
native Guile for a 32-bit target: \[-2<sup>29</sup>, 2<sup>29</sup>-1\].

Note that there is a risk here, that [`i31ref` gets punted to
post-MVP](https://github.com/WebAssembly/gc/issues/320), but this
appears to be unlikely.

### Heap objects

In native Guile, there are immediate values and heap objects.  Immediate
values have their contents directly in the bits of the `SCM`.  Heap
objects are `SCM` values that are pointers to the heap.  The type of the
object is encoded in the first word of memory pointed to by the `SCM`
value.

We are mostly going to use the same strategy in Guile-on-Wasm: there
will be a `(struct $tagged (field $tag i32))` abstract type with a few
dozen subtypes.  We use the same heap-tagging strategy as Guile to allow
run-time type checks by looking at the low bits of `$tag`.  We can even
just use a similar numering to [the existing heap
tags](http://git.savannah.gnu.org/cgit/guile.git/tree/module/system/base/types/internal.scm?h=wip-tailify#n124)
from native Guile.

(This strategy is good but suboptimal.  Consider that on browser
WebAssembly implementations, every GC-managed object's first word is a
pointer to its "hidden class".  Really we want to have each distinct
`SCM` subtype have its own hidden class, and then type checks are a
pointer comparison of the first word against known hidden classes.
Support for this idiom is a [planned post-MVP
feature](https://github.com/WebAssembly/gc/issues/275), though, so we
can't rely on it yet.  Also it poses interesting modularity concerns:
all compiled modules would have to import a common module defining
types, in order to be able to pass values between each other.  Anyway,
sticking with explicit tags seems to be a good MVP measure for the
Guile-on-WebAssembly effort.)

We do make a few deviations from what native Guile does, because of
platform differences and because we don't have existing C API consumers
that have assumptions about Guile's ABI.  So though we are inspired by
Guile, the type definitions described below are the canonical
documentation.

## Calling convention

In order to support lightweight concurrency via delimited continuations
and variable argument counts, Guile-on-WebAssembly uses the standard
WebAssembly function call mechanism in a strange way.

On the side of continuations, the basic idea is that the compiler from
Guile to WebAssembly will transform the program so that all calls are
tail calls.  Non-tail calls within a function are transformed so that
the caller pushs the live values flowing to the return point onto an
explicit stack, pushes the return continuation then tail-calls the
callee.  Returning from a function is transforms so that the callee pops
the return continuation from the stack and tail-calls it.  For full
details, see [`tailify.scm` from the `wip-tailify` Guile
branch](http://git.savannah.gnu.org/cgit/guile.git/tree/module/language/cps/tailify.scm?h=wip-tailify#n19).

The advantage of this approach is that with an explicit stack
representation, we can capture and reinstate delimited continuations,
and write debuggers in terms of continuations whose structure can be
inspected by Guile instead of the host WebAssembly system.  The
transformation is minimal, so that e.g. inner loops without calls are
still as fast as direct-style compilation.

(This calling convention may be obviated by either the [typed
continuations or the fiber-based stack switching
proposal](https://github.com/WebAssembly/stack-switching), but we don't
see either of these features reaching consensus and shipping before 2025
or so.)

Additionally, Guile functions can accept a variable number of arguments,
whereas WebAssembly functions have a fixed type.  In the general case we
pass arguments via a global argument-passing array, and only pass the
number of arguments as a function parameter.  Since all calls are tail
calls, this convention applies to returning values as well.

### Dynamic stack

The dynamic stack associates stack frames with `dynamic-wind` winders,
prompts, individual fluid bindings, and dynamic states (whole sets of
fluid bindings).  Not yet specified.  Really upstream Guile should be
using continuation marks here, but we don't do this yet.

### Dynamic state

See below for more on fluids and threads, but basically Guile should
keep a cache of the current values of some dynamically-scoped variables.
Not yet specified.

### Return stack

The return stack is for stack-allocated continuations: basically those
that correspond to the return points of non-tail calls in the source
program.  There will be separate stacks for `SCM` values, raw `i64` and
`f64` values, and `(ref func)` return continuations, but the concrete
representation remains to be specified.

## Type definitions

### Immediate data types

An immediate is a fixnum, a char, or an oddball.

All immediate values are encoded as a `(ref i31)`.

The `i32` payload is extracted from the `(ref i31)` via sign extension,
using `i31.get_s`.

If the low bit of the payload is `#b0`, the value is a fixnum, and its
signed integer value is in the high bits of payload.

If the low 2 bits of the payload are `#b11`, the value is a char, and
its codepoint is in the high bits of the payload.  The sign bit will
always be unset, because codepoints only go up to 2<sup>21</sup>.

Otherwise, the possible payloads are:
  - `#b000001`: `#f`
  - `#b000101`: `nil`
  - `#b001101`: `'()` (null)
  - `#b010001`: `#t`
  - `#b100001`: the unspecified value
  - `#b101001`: EOF

Some common oddball tests:
  - `null?`: check for null or nil; `(= (logand payload #b110111) #b001001)`
  - `false?`: check for false or nil; `(= (logand payload #b111011) #b000001)`
  - `elisp-false?`: check for false or nil or null; `(= (logand payload #b110011) #b000001)`

### Utility data types

```wat
(type $raw-immutable-bitvector (array i32))
(type $raw-immutable-bytevector (array i8))

(type $raw-bitvector (array mut i32))
(type $raw-bytevector (array mut i8))
(type $raw-scmvector (array mut (ref eq)))
```

### Continuation types

The functions residualized by the tailify transform take a variable
number of arguments from a global array.  The number of values to take
is passed to the function as an argument.  Since we tailify everything,
all calls are tail calls, so there are no return values.

```wat
(type $kvarargs (func (param $nargs i32) (result)))
```

### Oddball heap types

#### Pairs

```wat
(type $pair
  (struct (field $car mut (ref eq)) (field $cdr mut (ref eq))))
```

It may make sense to have an immutable pair type:

```wat
(type $immutable-pair
  (struct (field $car (ref eq)) (field $cdr (ref eq))))
```

If I understand the WebAssembly type system correctly, we can specify
the `car` accessor in terms of `$immutable-pair`, and have it work for
`$pair` also.  `set-car!` would require the mutable `$pair`.

#### Structs

```wat
(rec
  (type $struct (struct (field $vtable (ref $vtable))))
  (type $vtable
    (sub $struct
      (struct
        (field $layout (ref eq))
        (field $flags i32)
        (field $finalize (ref null eq))
        (field $print (ref null eq))
        (field $name (ref eq))
        (field $size i32)
        (field $unboxed-fields (ref $raw-bitvector))))))
```

We may want to instead increase the set of fields in `$vtable` to be
like [record type
descriptors](http://git.savannah.gnu.org/cgit/guile.git/tree/module/ice-9/boot-9.scm?h=wip-tailify#n937).

#### Bytevectors

We can just use `$raw-bytevector` and `$raw-immutable-bytevector`: no
other tags.

### Tagged heap types

All tagged heap types are subtypes of `$tagged`.

```wat
(type $tagged (struct (field $tag i32)))
```

Each of the following concrete subtypes corresponds to a heap object
that native Guile represents using a specific heap tag (value of the low
bits of the `$tag` field of `$tagged`).  We represent this below by
`$tag`/*`n`* `=` *`tag`*, where *`n`* indicates the number of low bits
to check, and *`tag`* is the value of those bits for the given data
type.

Note that unless `mut` is specified on a field's type, that field is
immutable.

#### Strings

```wat
(type $string ; $tag/7 = #b0010101
  (sub $tagged
    (struct (field $str (ref string)))))
```

Gosh.  It would be nice to just have `(ref string)` be the string
representation, but [`stringref` is not a subtype of
`eqref`](https://github.com/WebAssembly/stringref/issues/20).  Therefore
we have to wrap strings with a tagged struct.

We will need to have a mutable/immutable flag.  In the initial
Guile-to-Wasm compiler, all strings will be immutable.

#### Symbols and keywords

```wat
(type $symbol ; $tag/7 = #b0000101
  (sub $tagged
    (struct (field $hash i32)
            (field $name (ref string)))))

(type $keyword ; $tag/7 = #b0110101
  (sub $tagged
    (struct (field $name (ref $symbol)))))
```

How to compute the symbol's hash is not yet determined.

#### Variables and atomic boxes

```wat
; $variable: $tag/7 = #b000111
; $atomic-box: $tag/7 = #b0110111
(type $box
  (sub $tagged (struct (field mut $val (ref eq)))))
```

These two types have the same representation so they end up using the
same WebAssembly type.  WebAssembly does support multiple threads, but
there is no multi-thread support for GC objects, so for the time being
atomic boxes don't need to use atomic operations.

#### Vectors

```wat
(type $vector ; $tag/7 = #b0001101
  (sub $tagged (struct (field $vals (ref $raw-scmvector)))))
; immutable-vector: $tag/8 = #b10001101
; mutable-vector: $tag/8 = #b00001101
```

You can get the length of the vector using `array.length` on the `$vals`
field.  We could later switch to use an immutable `(array (ref eq))` for
immutable vectors but that would be an optimization.

#### Heap numbers

```wat
; heap-number: $tag/7 = #b0010111
(type $bignum ; $tag/12 = #b000100010111
  (sub $tagged (struct (field $val (ref extern))))) ; BigInt
(type $flonum ; $tag/12 = #b001000010111
  (sub $tagged (struct (field $val f64))))
(type $complex ; $tag/12 = #b001100010111
  (sub $tagged (struct (field $real f64) (field $imag f64))))
(type $fraction ; $tag/12 = #b010000010111
  (sub $tagged (struct (field $num (ref eq)) (field $denom (ref eq)))))
```

All of these numbers are heap-tagged.  We could use just a `(struct
f64)` for flonums in the future, perhaps; the WebAssembly type system
attaches its own tag, but perhaps this is something to think of when we
get RTTs.

Also note that e.g. `inexact-heap-number?` can be a bit test over the
tags.

#### Hash tables and weak tables

Hash tables are annoying because we don't have a `hashq` facility in
WebAssembly, so we need to call out to use JavaScript `Map` objects.  If
this is a dealbreaker, the alternative would be to include a hash code
in all objects.

```wat
(type $hash-table ; $tag/7 = #b0011101
  (sub $tagged (struct (field $map (ref extern)))))
```

Unfortunately this strategy doesn't generalize to `equal?` hash tables.
But, these can be built in Scheme, so no big ABI concerns there.

For weak tables, we have the same, but with `$tag = #b1010111`, and the
JS ref is to a `WeakMap` instead of a `Map`.

#### Dynamic state

```wat
(type $fluid ; $tag/7 = #b0100101
  (sub $tagged (struct (field $hash i32) (field $init (ref eq)))))
(type $dynamic-state $tag/7 = #b0101101; flags in tag
  (sub $tagged
    (struct
      (field $values (ref extern))
```

In native Guile, a fluid is essentially a key and a dynamic state is a
weak hash table mapping all fluids to their values.  At run-time there
is a per-thread cache for faster access to fluid values.  There will
have to be some run-time support routines for fluids.

Note that the representation of a dynamic state above is therefore just
a reference to a JavaScript, specifically to a `WeakMap`, but the shape
of "tagged struct holding a `(ref extern)`" is the same as for
`$hash-table`, only with a different tag.

#### Syntax and macros

```wat
(type $syntax ; $tag/7 = #b0111101
  (sub $tagged
    (struct
      (field $expr (ref eq))
      (field $wrap (ref eq))
      (field $module (ref eq))
      (field $source (ref eq)))))
```

I dearly hope that we can avoid having `psyntax` compiled to WebAssembly
for any module that doesn't include `eval`.  Still, `read-syntax` can
produce syntax objects, which is a nice way of associating source info
with objects; it's a type in native Guile, so I guess it makes sense to
hollow out a space for it for Guile-on-WebAssembly.

Note there is also a `scm_tc16_macro` for syntax transformers in native
Guile, that we will also need to implement at some point.

#### Procedures

```wat
; Proper support for closures is a post-MVP feature.
(type $proc ; $tag/7 = #b1000101
  (sub $tagged (struct (field $func (ref $kvarargs)))
(type $closure
  (sub $proc (struct (field $free-vars (ref $raw-scmvector)))))
```

A procedure is just another name for a function.  A `$proc` is a tagged
function.  Some functions close over a set of free variables; for them,
we use the subtype `$closure`.  Probably the free-vars array should be
immutable.

#### Multi-dimensional arrays

In this first version, we'll punt on these.  We should check with Daniel
Lloda about the state of his rewrite of arrays in Scheme.

#### Bitvectors

We can't just use `$raw-bitvector`, as we need a length also which is
generally smaller than the storage space in the raw `i32` array.

```wat
(type $bitvector ; $tag/7 = #b1011111
  (sub $tagged (struct (field $len i32) (field $bits (ref $raw-bitvector)))))
```

#### Ports

We take inspiration from how native Guile represents ports.  However for
both WASI and Web environments, we can assume that I/O routines are all
capable of returning a promise instead of blocking, so we don't need
explicit support for e.g. read or write wait FDs; instead we can assume
that we use the pure-Scheme [suspendable port
implementation](http://git.savannah.gnu.org/cgit/guile.git/tree/module/ice-9/suspendable-ports.scm?h=wip-tailify),
so delimited continuation suspend and resume will just work.

We can also simplify and assume UTF-8 encoding for textual I/O on ports.

```wat
(type $port-type
  (struct
    (field $name (ref eq))
    ; in guile these are (port, bv, start, count) -> size_t
    (field $read (ref $closure)) ; could have a more refined type
    (field $write (ref $closure))
    (field $seek (ref $closure)) ; (port, offset, whence) -> offset
    (field $close (ref $closure)) ; (port) -> ()
    (field $get-natural-buffer-sizes (ref $closure)) ; port -> (rdsz, wrsz)
    (field $random-access? (ref $closure)) ; port -> bool
    (field $input-waiting (ref $closure)) ; port -> bool
    (field $truncate (ref $closure)) ; (port, length) -> ()
    (field $flags i32)
    ; Guile also has GOOPS classes here.
    ))
(type $port ; $tag/7 = #b1111101
  (sub $tagged
    (struct
      (field $pt (ref $port-type))
      (field $stream mut (ref eq))
      (field $file_name mut (ref eq))
      (field $position (ref $pair))
      (field $read_buf mut (ref eq)) ; A 5-vector
      (field $write_buf mut (ref eq)) ; A 5-vector
      (field $write_buf_aux mut (ref eq)) ; A 5-vector
      (field $read_buffering mut i32)
      (field $refcount mut i32)
      (field $rw_random mut i8)
      (field $properties mut (ref eq)))))
```

The meanings of these various fields are as in native Guile; you have to
go spelunking a bit to find this information as the port representation
isn't public API/ABI.  Also, there is quite a bit of run-time work
needed here.

In native Guile there is also a "port-with-print-state" data type;
unclear if we will need this eventually.  Probably not.

## Not-yet-supported types

Weak vectors are not yet supported.

Regular expressions: not yet supported.  Would be a JS callout.

Random states.

Charsets.

First-class threads, mutexes, and condition variables.

The representation of first-class delimited and undelimited
continuations is currently unspecified.  (It will be a slice of all
stacks, though.)

## Open questions

How to deal with lack of `hashq`: just punt and do what we can with JS,
or include a hash code in all objects?

Should we take the opportunity to have `cons` make immutable pairs,
perhaps adding `mcons` also?

Need to verify that the pair type above is actually disjoint from
structs, to WebAssembly's type system; otherwise we'd need a type code.

Should we be annotating struct types with `final`?  The MVP document
mentions it but binaryen does not seem to support it.

Should the type tag be separated into an `i8` or two, perhaps with a
user `i16` ?  Or just an `i8` and subtypes manage what to native Guile
are the "upper" tag bits on their own, perhaps with an `i16` or `i32`?

## JS API

Wasm GC values (and thus Guile-on-Wasm values) are opaque to JavaScript;
they can pass through JS by reference but if JS is to do something with
them on its own, there need to be explicit conversions to and from JS,
for example to unpack the integer in a fixnum.  There will be a side
Wasm library to do this.

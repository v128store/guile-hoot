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

In WebAssembly, garbage-collected objects can also also be associated
with a type identifier which WebAssembly programs can introspect over,
for example to branch to a label if a value has a given type.  We will
use these built-in capabilities when implementing the various Scheme
data types.

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
may need to pass arguments via a global argument-passing array.  The
first few arguments can be passed as parameters though.  The number of
arguments is also passed as a function parameter.  Since all calls are
tail calls, this convention applies to returning values as well.

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
  - `#b000001`: 1: `#f`
  - `#b000101`: 5: `nil`
  - `#b001101`: 13: `'()` (null)
  - `#b010001`: 17: `#t`
  - `#b100001`: 33: the unspecified value
  - `#b101001`: 41: EOF
  - `#b111001`: 57: the undefined value (used internally)

Some common oddball tests:
  - `null?`: check for null or nil; `(= (logand payload #b110111) #b000101)`
  - `false?`: check for false or nil; `(= (logand payload #b111011) #b000001)`
  - `elisp-false?`: check for false or nil or null; `(= (logand payload #b110011) #b000001)`

### Utility data types

```wat
(type $raw-immutable-bitvector (array i32))
(type $raw-immutable-bytevector (array i8))

(type $raw-bitvector (array (mut i32)))
(type $raw-bytevector (array (mut i8)))
(type $raw-scmvector (array (mut (ref eq))))
```

### Continuation types

The functions residualized by the tailify transform take a variable
number of arguments.  The number of values to take is passed to the
function as an argument.  The first three arguments are passed in
parameters; any addition arguments get passed through a global array.
If a function has fewer than three parameters, you can pass any value as
the argument, for example `(i31.new (i32.const 0))`.

Since we tailify everything, all calls are tail calls, so there are no
return values.

```wat
(type $kvarargs (func (param $nargs i32)
                      (param $arg0 (ref eq))
                      (param $arg1 (ref eq))
                      (param $arg2 (ref eq))
                      (result)))
```

### Heap types

All Guile objects which are not represented as `(ref i31)` are "heap
objects".  These objects are all subtypes of `$heap-object`:

```wat
(type $void-struct (struct))
(type $heap-object
  (sub $void-struct
    (struct
      (field $hash (mut i32)))))
```

There are subtypes for each concrete kind of object that Guile uses
(approximately 20 types).

As a WebAssembly type system detail, note that usually two types that
have the same shape will be deemed equivalent and thus
indistinguishable.  It is the case that there are Guile objects that
have the same shape.  However we use the "hybrid nominal typing"
facility to declare our types as being distinct, even when some of them
have the same shape.  By wrapping all our types in a `rec` block, we
ensure that dynamic type checks can be made with a simple `ref.test`.

For symbols and keywords, the `$hash` field is eagerly computed based on
the string-hash of the underlying string.  For other data types, the
hash is computed lazily.  A hash of 0 indicates an uninitialized hash.
Bit 0 is always set if the hash is initialized.  Otherwise for immediate
values, there is a simple bit-mixing hash function.

#### References to host facilities

```wat
(type $extern-ref
  (sub $heap-object
    (struct
      (field $hash (mut i32))
      (field $val (ref extern)))))
```

Sometimes we want to refer to a reference-typed value from the host, so
one data type is an `$extern-ref`.

#### Heap numbers

```wat
(type $heap-number
  (sub $heap-object
    (struct
      (field $hash (mut i32)))))
```

There is a supertype for heap numbers, in case we need to quickly check
that a non-fixnum is indeed a number.  Then there are the concrete heap
number types.

```wat
(type $bignum
  (sub $heap-number
    (struct
      (field $hash (mut i32))
      (field $val (ref extern)))))
(type $flonum
  (sub $heap-number
    (struct
      (field $hash (mut i32))
      (field $val f64))))
(type $complex
  (sub $heap-number
    (struct
      (field $hash (mut i32))
      (field $real f64)
      (field $imag f64))))
(type $fraction
  (sub $heap-number
    (struct
      (field $hash (mut i32))
      (field $num (ref eq))
      (field $denom (ref eq)))))
```

#### Pairs

```wat
(type $pair
  (sub $heap-object
    (struct
      (field $hash (mut i32))
      (field $car (mut (ref eq)))
      (field $cdr (mut (ref eq))))))
```

There is also a `$mutable-pair` subtype of `$pair`, with the same
fields.  `car` requires that the object be a `$pair`, but `set-car!`
requires the subset of `$pair` values which are also `$mutable-pair`.

#### Vectors

```wat
(type $vector
  (sub $heap-object
    (struct
      (field $hash (mut i32))
      (field $vals (ref $raw-scmvector)))))
```

There is also a `$mutable-vector` subtype of `$vector` with the same
fields.

You can get the length of the vector using `array.length` on the `$vals`
field.

#### Bytevectors

```wat
(type $bytevector
  (sub $heap-object
    (struct
      (field $hash (mut i32))
      (field $vals (ref $raw-bytevector)))))
```

There is also a `$mutable-bytevector` subtype of `$bytevector` with the same
fields.

You can get the length of the bytevector using `array.length` on the
`$vals` field.

#### Bitvectors

We need an explicit length which is generally smaller than the storage
space in the raw `i32` array.

```wat
(type $bitvector
  (sub $heap-object
    (struct
      (field $hash (mut i32))
      (field $len i32)
      (field $bits (ref $raw-bitvector)))))
```

There is also a `$mutable-bitvector` subtype of `$bitvector` with the
same fields.

#### Strings

```wat
(type $string
  (sub $heap-object
    (struct
      (field $hash (mut i32))
      (field $str (mut (ref string))))))
```

It would be nice to just have `(ref string)` be the string
representation, but [`stringref` is not a subtype of
`eqref`](https://github.com/WebAssembly/stringref/issues/20).  Therefore
we have to wrap strings with a tagged struct.  But, this also gives us
the possibility to have a hashq field, and to possibly mutate the string
(by replacing its contents).

There is also a `$mutable-string` subtype of `$string` with the same
fields.

#### Procedures

```wat
(type $proc
  (sub $heap-object
    (struct
      (field $hash (mut i32))
      (field $func (ref $kvarargs)))))
```

A procedure is just another name for a function.  A `$proc` is a tagged
function.

Some functions close over a set of free variables; for them,
there are subtypes of `$proc`:

```
(type $closure1
  (sub $proc
    (struct
      (field $hash (mut i32))
      (field $func (ref $kvarargs))
      (field $free0 (ref eq)))))
(type $closure2
  (sub $proc
    (struct
      (field $hash (mut i32))
      (field $func (ref $kvarargs))
      (field $free0 (ref eq))
      (field $free1 (ref eq)))))
;; ...
```

The set of closure types will depend on what is needed by the code being
compiled, and are not part of the `rec` block of distinct types.

Also note that in the future, WebAssembly will support funcrefs which
are themselves closures; in that case we can avoid the separate
`$closureN` types, storing the data in the funcref directly.

#### Symbols and keywords

```wat
(type $symbol
  (sub $heap-object
    (struct
      (field $hash (mut i32))
      (field $name (ref string)))))

(type $keyword
  (sub $heap-object
    (struct
      (field $hash (mut i32))
      (field $name (ref $symbol)))))
```

How to compute the symbol's hash is not yet determined.

#### Variables and atomic boxes

```wat
(type $variable
  (sub $heap-object
    (struct
      (field $hash (mut i32))
      (field $val (mut (ref eq))))))
(type $atomic-box
  (sub $heap-object
    (struct
      (field $hash (mut i32))
      (field $val (mut (ref eq))))))
```

WebAssembly does support multiple threads, but there is no multi-thread
support for GC objects, so for the time being atomic boxes don't need to
use atomic operations.

#### Hash tables

We use a simple buckets-and-chains hash table, implemented in Scheme.

```wat
(type $hash-table
  (sub $heap-object
    (struct
      (field $hash (mut i32))
      (field $size (mut (ref i31)))
      (field $buckets (ref $vector)))))
```

#### Weak tables

```wat
(type $weak-table
  (sub $heap-object
    (struct
      (field $hash (mut i32))
      (field $val (ref extern)))))
```

The external value held by the weak table is a host-supplied weak map.

#### Dynamic state

```wat
(type $fluid
  (sub $heap-object
    (struct
      (field $hash (mut i32))
      (field $init (ref eq)))))
(type $dynamic-state
  (sub $heap-object
    (struct
      (field $hash (mut i32))
      (field $val (ref extern)))))
```

In native Guile, a fluid is essentially a key and a dynamic state is a
weak hash table mapping all fluids to their values.  At run-time there
is a per-thread cache for faster access to fluid values.  There will
have to be some run-time support routines for fluids.

#### Syntax and macros

```wat
(type $syntax
  (sub $heap-object
    (struct
      (field $hash (mut i32))
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

#### Multi-dimensional arrays

In this first version, we'll punt on these.  We should check with Daniel
Lloda about the state of his rewrite of arrays in Scheme.

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
    (field $name (ref string))
    ;; in guile these are (port, bv, start, count) -> size_t
    (field $read (ref null $proc)) ;; could have a more refined type
    (field $write (ref null $proc))
    (field $seek (ref null $proc)) ;; (port, offset, whence) -> offset
    (field $close (ref null $proc)) ;; (port) -> ()
    (field $get-natural-buffer-sizes (ref null $proc)) ;; port -> (rdsz, wrsz)
    (field $random-access? (ref null $proc)) ;; port -> bool
    (field $input-waiting (ref null $proc)) ;; port -> bool
    (field $truncate (ref null $proc)) ;; (port, length) -> ()
    ;; Guile also has GOOPS classes here.
    ))
(type $port
  (sub $heap-object
    (struct
      (field $hash (mut i32))
      (field $pt (ref $port-type))
      (field $stream (mut (ref eq)))
      (field $file_name (mut (ref eq)))
      (field $position (ref $pair))
      (field $read_buf (mut (ref eq))) ;; A 5-vector
      (field $write_buf (mut (ref eq))) ;; A 5-vector
      (field $write_buf_aux (mut (ref eq))) ;; A 5-vector
      (field $read_buffering (mut i32))
      (field $refcount (mut i32))
      (field $rw_random (mut i8))
      (field $properties (mut (ref eq))))))
```

The meanings of these various fields are as in native Guile; you have to
go spelunking a bit to find this information as the port representation
isn't public API/ABI.  Also, there is quite a bit of run-time work
needed here.

In native Guile there is also a "port-with-print-state" data type;
unclear if we will need this eventually.  Probably not.

#### Structs

```wat
(type $struct
  (sub $heap-object
    (struct
      (field $hash (mut i32))
      (field $vtable (mut (ref null $vtable))))))
(type $vtable
  (sub $struct
    (struct
      (field $hash (mut i32))
      (field $vtable (mut (ref null $vtable)))
      (field $field0 (mut (ref eq)))
      (field $field1 (mut (ref eq)))
      (field $field2 (mut (ref eq)))
      (field $field3 (mut (ref eq))))))
```

Guile's structs are the underlying facility that implements records and
object-orientation.  They have the oddity that their vtable is also a
struct with at least 4 fields.

The `$struct` and `$vtable` definitions are inside the `rec` block.
Specific struct types as needed by the program will be residualized as
needed.  If the struct has more than 4 fields, it may store them in a
heap vector.

```
(type $struct1
  (sub $struct
    (struct
      (field $hash (mut i32))
      (field $vtable (mut (ref null $struct4)))
      (field $field0 (mut (ref eq))))))
(type $struct2
  (sub $struct
    (struct
      (field $hash (mut i32))
      (field $vtable (mut (ref null $struct4)))
      (field $field0 (mut (ref eq)))
      (field $field1 (mut (ref eq))))))
(type $struct3
  (sub $struct
    (struct
      (field $hash (mut i32))
      (field $vtable (mut (ref null $struct4)))
      (field $field0 (mut (ref eq)))
      (field $field1 (mut (ref eq)))
      (field $field2 (mut (ref eq))))))
(type $struct4
  (sub $struct
    (struct
      (field $hash (mut i32))
      (field $vtable (mut (ref null $struct4)))
      (field $field0 (mut (ref eq)))
      (field $field1 (mut (ref eq)))
      (field $field2 (mut (ref eq)))
      (field $field3 (mut (ref eq))))))
(type $structN
  (sub $struct
    (struct
      (field $hash (mut i32))
      (field $vtable (mut (ref null $struct4)))
      (field $field0 (mut (ref eq)))
      (field $field1 (mut (ref eq)))
      (field $field2 (mut (ref eq)))
      (field $field3 (mut (ref eq)))
      (field $tail (ref $raw-scmvector)))))
```

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

Should we be annotating struct types with `final`?  The MVP document
mentions it but binaryen does not seem to support it.

## JS API

Wasm GC values (and thus Guile-on-Wasm values) are opaque to JavaScript;
they can pass through JS by reference but if JS is to do something with
them on its own, there need to be explicit conversions to and from JS,
for example to unpack the integer in a fixnum.  There will be a side
Wasm library to do this.

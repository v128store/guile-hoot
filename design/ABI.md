# Guile on WebAssembly: ABI

## Data types

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
values.  The 2<sup>31</sup possible values are partitioned by tagging.
The partition is the same as [what native Guile
does](http://git.savannah.gnu.org/cgit/guile.git/tree/module/system/base/types/internal.scm?h=wip-tailify#n101),
except that the bottom 0 bit is left off.  The fixnum range is therefore
the same as in native Guile for a 32-bit target: `[-2<sup>29</sup>,
2<sup>29</sup>-1]`.

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
just use [the existing heap
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

We do make a few deviations from what native Guile does, for example to
use `(ref string)` instead of having Guile-like representations of
strings.

```wat
; TODO add immutable variants
(type $raw-bitvector (array mut i32))
(type $raw-bytevector (array mut i8))
(type $raw-scmvector (array mut (ref eq)))

; A continuation that takes a variable number of values receives its
; arguments from a global array.  The number of values to take is passed
; to the function.  Since we tailify everything, all calls are tail calls,
; so there are no return values.
(type $kvarargs (func (param $nargs i32) (result)))

; TODO: immutable pairs?
(type $pair (struct (field $car mut (ref eq)) (field $cdr mut (ref eq))))

(rec
  (type $struct (struct (field $vtable (ref $vtable))))
  (type $vtable
    (sub $struct
      (struct
        (field $layout (ref eq))
        (field $flags i32)
        (field $finalize (ref null func))
        (field $print (ref null func))
        (field $name (ref eq))
        (field $size i32)
        (field $unboxed-fields (ref $raw-bitvector))))))

; No need for type $string; just use (ref string).  Strings not mutable!
; No stringbufs either.

(type $tagged (struct (field $tag i32)))

(type $symbol ; $tag/7 = #b0000101
  (sub $tagged (struct (field $hash i32) (field $name (ref string)))))

(type $variable ; $tag/7 = #b000111
  (sub $tagged (struct (field mut $val (ref eq)))))

(type $vector ; $tag/7 = #b0001101
  (sub $tagged (struct (field $vals (ref $raw-scmvector)))))
; immutable-vector: $tag/8 = #b10001101
; mutable-vector: $tag/8 = #b00001101

; No weak vectors for the time being.

; heap-number: $tag/7 = #b0010111
(type $bignum ; $tag/12 = #b000100010111
  (sub $tagged (struct (field $val (ref extern))))) ; BigInt
(type $flonum ; $tag/12 = #b001000010111
  (sub $tagged (struct (field $val f64))))
(type $complex ; $tag/12 = #b001100010111
  (sub $tagged (struct (field $real f64) (field $imag f64))))
(type $fraction ; $tag/12 = #b010000010111
  (sub $tagged (struct (field $num (ref eq)) (field $denom (ref eq)))))

; TODO: Think about equal? tables.
(type $hash-table ; $tag/7 = #b0011101
  (sub $tagged (struct (field $map (ref extern)))))

; No pointer type.

(type $fluid ; $tag/7 = #b0100101
  (sub $tagged (struct (field $hash i32) (field $init (ref eq)))))
(type $dynamic-state $tag/7 = #b0101101; flags in tag
  (sub $tagged
    (struct
      (field $values (ref extern))
      (field $cache (ref $raw-scmvector)))))

; No frame type for now?

(type $keyword ; $tag/7 = #b0110101
  (sub $tagged (struct (field $name (ref $symbol)))))

(type $atomic-box ; $tag/7 = #b0110111
  (sub $tagged (struct (field $val mut (ref eq)))))

(type $syntax ; $tag/7 = #b0111101
  (sub $tagged
    (struct
      (field $expr (ref eq))
      (field $wrap (ref eq))
      (field $module (ref eq))
      (field $source (ref eq)))))

; Proper support for closures is a post-MVP feature.
(type $proc ; $tag/7 = #b1000101
  (sub $tagged (struct (field $func (ref $kvarargs)))
(type $closure
  (sub $proc (struct (field $free-vars (ref $raw-scmvector)))))

; vm-continuation is an internal object kind that is never seen
; by scheme, no need to define it now

; use $raw-bytevector for bytevectors

; no scheme-visible weak sets

; use a record type for weak tables?  in any case it’s a JS WeakMap.
; perhaps we need an “external” js value wrapper type visible to scheme

; no arrays in this first version; check with lloda for state of rewrite

(type $bitvector ; $tag/7 = #b1011111
  (sub $tagged (struct (field $len i32) (field $bits (ref $raw-bitvector)))))

; TODO: think about promises / async / suspension; look at wasi
; TODO: tighten types, recursive types
(type $port-type
  (struct
    (field $name (ref eq))
    ; in guile these are (port, bv, start, count) -> size_t
    (field $read (ref $closure)) ; could have a more refined type
    (field $write (ref $closure))
    (field $get-read-promise (ref $closure)) ; ?
    (field $get-write-promise (ref $closure)) ; ?
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
      (field $stream (ref eq))
      (field $file_name (ref null string))
      (field $position (ref $pair))
      (field $read_buf (ref eq)) ; A 5-vector
      (field $write_buf (ref eq)) ; A 5-vector
      (field $write_buf_aux (ref eq)) ; A 5-vector
      (field $read_buffering i32)
      (field $refcount i32)
      (field $rw_random i8)
      (field $properties (ref eq)))))

; TODO: useful smob types in guile to replace
scm_tc16_locale_smob_type
bytevector_output_port_procedure
tc16_srcprops
finalized_smob_tc16
tc16_guardian
scm_tc16_regex
random state
scm_tc16_promise
scm_tc16_dir
tc16_continuation
scm_tc16_macro
scm_tc16_port_with_ps
scm_tc16_charset
scm_tc16_charset_cursor
scm_tc16_thread
scm_tc16_mutex
scm_tc16_condvar
```

### Open questions

hashq

threading model

immutable pairs?

immutable strings

verify that pairs are disjoint from structs

attempt to base more types on guile structs?

## JS API

Wasm GC values (and thus Guile-on-Wasm values) are opaque to JavaScript;
they can pass through JS by reference but if JS is to do something with
them on its own, there need to be explicit conversions to and from JS,
for example to unpack the integer in a fixnum.  There will be a side
Wasm library to do this.

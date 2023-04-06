# Project log

An attempt at keeping a record of where we were when, and speculation on
what the log might contain soon

## The past

### 2023-02-15

Project kickoff.  Beginnings of the [compilation strategy](./ABI.md)
document, and the first WebAssembly files built by hand, to make sure
that the compilation strategy will work out.

Lots of confusion around nominal types.  For context, Scheme is quite
monomorphic in flavor (e.g. `length` only works on lists) but it has to
contend with dynamically-typed objects.  You don't need to do so much
polymorphic dispatch but you do need to be able to quickly dynamically
check that an object is of the expected type.  For WebAssembly as hosted
by a JS implementation, each GC object starts with an internal
"map"/"shape"/"structure"/"hidden class" word (the nomenclature depends
on the engine but it's all the same); the fastest checks will simply
check than an object's first word has a given value.

However the abstraction that exposes cheap map word checks to
WebAssembly (nominal types / run-time types) was [removed from the GC
MVP](https://github.com/WebAssembly/gc/issues/275).  You can still do
dynamic checks, but they are structural, and some objects have the same
shape.  So in these first couple weeks there was significant confusion
on whether structural checks would be sufficient, or whether you would
need a tag word.

### 2023-02-27

First, some work on making the
[basic-types](../examples/basic-types.wat) example represent all
fundamental types that there are in Guile.

Hash tables threw a bit of a spanner in the works.  The initial idea was
to punt to the embedder; for a JS host, you'd use
[`Map`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Map).
But how would you implement identity-based hashing in Wasmtime?
Assuming a GC that moves objects, you can't simply use object address as
the key.  It's asking too much of the host to implement some strange
side table.

So, as the JVM does, we now explicitly include space for a hash code in
all objects, even pairs (which before we were trying to keep lean, as in
Guile).  On the positive side this hash word can contain a tag, so
dynamic checks are cheaper than before.  Relatedly, because [`string`
is not a subtype of
`eq`](https://github.com/WebAssembly/stringref/issues/20), we needed to
wrap strings anyway; this gives us a way to have mutable strings if we
end up needing to have that.

All in all, the arc is towards using a little more memory, but not
taking short-cuts to compromise on semantics, relative to what native
Guile does.  In the end it will be fine.

Some additional run-time work in the basic-types example to show how to
implement a symbol table, a keyword table, string hashing, struct
vtables, and so on.

### 2023-03-03

Now that the hand-written basic-types example supports all types that
Guile does, we start on making WebAssembly from Scheme.  First, a port
of [wassemble](https://github.com/wingo/wassemble) [to
Scheme](../module/wasm/assemble.scm).  As we go, we update wassemble for
more recent additions to the wasm standard.  There are actually a couple
parts of the assembler: one to take s-expression input that mostly
corresponds to the WebAssembly [textual
grammar](https://webassembly.github.io/spec/core/text/index.html), and
parses to Guile records; and one that take a Guile WebAssembly record
and assembles it to bytes.

Having gotten that to work, next up was [a
disassembler](../module/wasm/parse.scm).  This one was built to handle
all of the WebAssembly that V8 can handle, including experimental
extensions.  For GC, this corresponds to the [Milestone 6
MVP](https://docs.google.com/document/d/1DklC3qVuOdLHSXB5UXghM_syCh-4cMinQ50ICiXnK3Q/edit#)
document.  Representing all WebAssembly features required some
expansions to the [../module/wasm/types.scm](Guile WebAssembly data type
definitions), and corresponding updates to the assembler.

Finally, after getting the parser working and the assembler back up to
speed on (almost) all the features that the parser can parse, tied
things together with some [tests](./test/test-wasm-assembler.scm).

### 2023-03-28

Gave a talk about our efforts at [BOB
2023](https://wingolog.org/archives/2023/03/20/a-world-to-win-webassembly-for-the-rest-of-us).
Seems to have been well-received.

On the implementation side, started looking at the
[tailify](http://git.savannah.gnu.org/cgit/guile.git/tree/module/language/cps/tailify.scm?h=wip-tailify)
pass.  Right now if you [compile](./compile.scm) the trivial Scheme
program `42`, you get this:

```scheme
L0:
  v0 := self
  L1(...)
L1:
  receive()
  v1 := const 42                              ; val 
  v2 := restore1[ptr]()                       ; ret 
  tail calli v2(v1)
```

Here we see two blocks.  (Really it's just one, but it renders as two
because the `$kentry` has two successors: the `$kclause` and the
`$ktail`).  `L0` is the entry, which binds `self` as the closure and
then continues to `L1` which parses the args, expecting 0 additional
args.  Then we define the return value (`42`), then pop a `ptr` from the
stack and call it indirectly (`tail calli`).

The thing is, `ptr` is a really lazy way to describe the type of the
continuation.  Can we assume that it's a return continuation or are
there other kinds of pointers that we might see?  Turns out, yes, for
now at least we can assume it's a return continuation; the other uses of
`ptr`-typed locals in native Guile are for interior pointers for strings
and pointers to the struct-unboxed-field bitvector, neither of which we
will have for the wasm target.

But, we still have some work to do on the compiler front-end, to avoid
eager "instruction explosion" of CPS primitives in `(language tree-il
compile-cps)` to primitives that correspond to the native Guile VM
rather than what we will use in wasm.

### 2023-04-06

Aaaaaaahhhhh, finally:

```
guild compile-wasm -o 42.wasm 42.scm
wrote `42.wasm`
```

We also have a new [reflect.wat](../js-runtime/reflect.wat) /
[reflect.js](../js-runtime/reflect.js) run-time library to allow these
WebAssembly files to be loaded in web browsers or other places where you
have a JavaScript implementation with a capable WebAssembly
implementation.

## The near future

Some good starter tasks for new contributors:

 - The "wat" component of the assembler isn't yet updated for reference
   types / GC.  Probably it should be.

 - We should have a wasm->wat serializer.  Probably a good way to test
   that the wat->wasm parser for reftypes is working: take
   `basic-test.wasm`, parse to records via `parse-wasm`, then take it to
   wat and then back to wasm and binary.
   
 - It would be nice to add a "symbolizer" pass for parsed wasm files.
   That way you can write some of the standard library in wat, use
   binaryen to compile, optimize, and validate it, and when you want to
   pull in parts of the standard library you can just parse the compiled
   wasm via `compute-stdlib` in `(hoot compile)`.  To do this though,
   intramodule references need to be symbolic (by-name) rather than by
   index, to allow the module to be picked apart and combined with the
   generated wasm.

 - The [WebAssembly tool
   conventions](https://github.com/WebAssembly/tool-conventions/blob/main/Linking.md)
   define a way to serialize names to object files.  Probably we should
   do this, for debuggability.

 - Currently the subset of the Scheme language that is supported is
   basically nothing -- just a minimal subset of the constants.  We need
   to expand this.  See [(hoot compile)](../module/hoot/compile.scm),
   anywhere it says "unimplemented", and add tests to
   [`test-constants.scm`](../test/test-constants.scm).

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

### 2023-05-05

Some progress, in that the compiler now supports all kinds of constant
literals, and we are starting to implement the different primcalls.
Then, non-tail function calls; once that is working we'll be close to
having a working Scheme.

### 2023-05-15

Some interesting progress: the test suite and JS host harness has been
enhanced so as to compile multiple compilation units and have them
interoperate.  To do this we need to ensure they share the same run-time
(stack pointers, etc).  Probably there are some more guard-rails to make
here but for now it's an interesting development.

We now compile value returns, tail calls, conditionals, and a few
minimal primcalls (add, add/immediate, sub, sub/immediate, mul).  We can
do non-tail calls too.  Recursive fac and fib run (though don't handle
overflow to bignum yet); speed appears to currently be about 15x slower
than Guile native.  Something to work on over time.

### 2023-05-22

Conversations from Hoot sync meeting:

- Land Robin's MRs on self-assembling
- Move stdlib from hoot compile to hoot stdlib
- Add function to WAT parsing module to parse an expression,
  unfolding at same time
- Use that function as part of compute stdlib
- Need to be able to raise our abstraction for defining runtime functions!

In medium term wanna define more functions in scheme.  Want to compile
expressions with lexically available environment, but that's kind of a
separate thing

Might be in medium term that we could define functions in scheme that have
their implementation in webassembly

Add special logic to compiler to emit WASM code instead of in other ways
to compile a function (kinda like inline-asm)

Gotta make it nicer to write WASM!  Especially all those damn make-type-use
things.  If you're parsing WAT, you get around a lot of those

In that file maybe have more emacs indentation rules so the code
doesn't look terrible.  Should put in local variables or `.dir-locals.el`
or etc.

Prioritization for this week:

- Land self-assembly MRs

- Move stdlib from hoot compile to hoot stdlib
- Compute stdlib return standard wasm module but parse from WAT
  instead of more imperative mode.  Put it in the WAT DSL instead of
  in scheme.
- Use WAT more.  Less explicit assembly records, more WAT parsing

### 2023-06-22

Moved the unify-returns pass into Guile (`wip-tailify` branch), and made
it so that Guile can select Hoot-specific backend passes to its CPS.

But the big change is that Guile no longer eagerly explodes
e.g. `vector-length` into generic low-level `word-ref/immediate`
instructions when converting from Tree-IL to CPS; we keep the various
type checks and bailouts as part of explicit control flow, but CPS
conversion residualizes e.g. `vector-ref` etc.  This will let Hoot
allocate objects with Wasm/GC typed objects.

### 2023-07-04

Finished adding support for vectors, pairs, closures, variables/boxes,
structs, and started on bytevectors.  Next up will be the dynamic
environment.

## The near future

### Cast optimizations

In WebAssembly, `vector-length` needs to work on a value of type
`(struct $vector)`.  But generally what is flowing around is the `(ref
eq)` unitype, so we have to introduce explicit `ref.cast` operations.
Still, type checks can help us automatically recover this information:
because `vector-length` is dominated by a `vector?` check, we should be
able to use `br_on_cast` or similar.

Right now we're just going to insert casts right before `vector-ref` et
al, as we emit the `vector-ref`.  But really we should instead do like
this:
 1. Add a pass to explicitly insert `$vector` casts right before each
    vector accessor (or pairs, structs, etc).
 2. That pass also inserts casts on the true branch of each `vector?`
    primcall.
 3. Run CSE to eliminate casts at the access points and instead use the
    `$vector` cast after the type check.  This is already a win because
    probably there's more than one access.
 4. When emitting wasm, detect a `br_on_cast` if a branch is followed by
    a cast.  Not sure how this integrates with the "beyond relooper"
    design though!

### Other potential starter tasks

Some good starter tasks for new contributors:

 - The "wat" component of the assembler isn't yet updated for reference
   types / GC.  It would be nice to fix that so that we can replace our
   use of binaryen to use our own assembler instead.

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
   quite minimal.  We need to expand this.  See [(hoot
   compile)](../module/hoot/compile.scm), anywhere it says
   "unimplemented", and add tests to
   [`test-constants.scm`](../test/test-constants.scm) or some other file
   there.

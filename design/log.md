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

## The near future

Some good starter tasks for new contributors:

 - The "wat" component of the assembler isn't yet updated for reference
   types / GC.  Probably it should be.

 - We should have a wasm->wat serializer.  Probably a good way to test
   that the wat->wasm parser for reftypes is working: take
   `basic-test.wasm`, parse to records via `parse-wasm`, then take it to
   wat and then back to wasm and binary.
   
 - The assembler should create "declarative" element segment for any
   function mentioned by a `ref.func`.  The engine uses this to know how
   to generate code for these functions.  See for example how there is
   no declarative element segment in basic-types.wat, but the wasm file
   is generated with one.

The broader picture is that we have two sides of a bridge and now just
have to build the bridge itself.  On one side, we have
[tailified](https://lists.gnu.org/archive/html/guile-devel/2021-06/msg00005.html)
CPS (see also
[tailify.scm](http://git.savannah.gnu.org/cgit/guile.git/tree/module/language/cps/tailify.scm?h=wip-tailify)).

On the other side, we have the calling convention of wasm/gc.  Happily
we now also have tail calls, so no need for trampolines.  So next up is
creating a compiler to go from minimal Scheme snippets to wasm, through
tailified CPS.

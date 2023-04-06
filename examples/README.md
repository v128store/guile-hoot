# This directory is going away

In this directory we were testing manual compilations of Scheme to wasm.
Now that we have the compiler working, this is no longer necessary and
these files will bitrot.  But, until we have all data types implemented
in the compiler, the files in this directory are still useful.

## Running checks

You will need recent V8 and Binaryen.  Annoyingly and for weird reasons
binaryen doesn't support the standard GC syntax yet, though its binaries
are good.  For binaryen:

```
git clone https://github.com/wingo/binaryen
cd binaryen
git checkout parse-sub
git submodule init
git submodule update
cmake . -DCMAKE_BUILD_TYPE=Debug
make
```

For V8, there is thing annoying thing that you need to have
`depot_tools` installed; see https://v8.dev/docs/source-code.  Once you
have that see https://v8.dev/docs/build to build.  You will end up with
a `d8` binary in `out/x64.release` (if you are on an x86-64 platform).

To run these tests, run `make check`:

```
$ make check
~/src/binaryen/bin/wasm-as --enable-gc --enable-strings --enable-tail-call --enable-reference-types -o trivial-1.wasm trivial-1.wat
~/src/v8/out/x64.release/d8 --experimental-wasm-gc --experimental-wasm-stringref --experimental-wasm-return-call test.js -- trivial-1.wasm 1
expected: 1; got: 1
```

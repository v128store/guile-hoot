# Guile Hoot

![Hoot logo](./hoot.png)

Hoot is the codename for the [Guile->WebAssembly
project](https://spritely.institute/news/guile-on-web-assembly-project-underway.html)
launched by the [Spritely Institute](https://spritely.institute/).  In
addition to the compiler, Hoot contains a full WebAssembly toolchain
with a WAT parser, an assembler, a disassembler, an interpreter, etc.

For a fuller picture of project status, including known limitations,
see the ["Status" section of our
documentation.](https://spritely.institute/files/docs/guile-hoot/latest/Status.html).

## Project goals and timeframe

Hoot aims to be an ahead-of-time compiler for all of [R7RS-small
Scheme](https://small.r7rs.org/) to WebAssembly (aka WASM).  Hoot uses
several WASM extensions such as tail calls and garbage collection.
The good news is that these extensions are already available in
nightly releases of major browsers and will soon be making their way
into stable browser releases everywhere!

After completing R7RS-small support, we will move on to supporting all
of Guile.  We are keeping this end-goal in mind as we build the early
deliverable.

Resulting code should all run on stock Guile.  We may need to upstream
some patches to Guile, and will do so as it seems appropriate.

## The shape of things

In the end we expect to be able to compile Scheme programs to single
WebAssembly files.  To deploy on web browsers there is an associated
JavaScript module.  Some non-web targets are hosted by JavaScript
implementations (e.g. node); those are similar to web browsers.
Otherwise on WASI hosts we expect to have a WASI-specific support
module eventually.

The minimal compiled module size is some tens of kilobytes,
uncompressed.  The auxiliary WebAssembly module to do impedance
matching with JavaScript is about four kilobytes uncompressed, and the
generic JS library is about 500 lines of unminified JS.  As we
implement more of Scheme, we hope to preserve this "small programs
compile to small files" property, rather than having every compiled
program include the whole of Guile's standard library.

## But... why the name "Hoot"?

We thought this project deserved a cute project name and mascot, and
everyone at the time agreed an owl was nice, and Christine
Lemmer-Webber had recently just drawn up this owl pixel art, and
so it became the mascot.
The name naturally flowed from there.

## Project updates

See the [log file](design/log.md).

## Installing Hoot's stable releases

Note that at the time of writing, Hoot requires a development version
of Guile.  This may not be the case at your time of reading!

Below are system-specific instructions for installing Hoot.

### On Guix

Hoot is already available in [Guix](https://guix.gnu.org/):

```
guix shell --pure guile-next guile-hoot
```

### On Mac OS (homebrew)

Hoot is
[available in Mac OS thanks to to Alex Conchillo Flaqué](https://emacs.ch/@aconchillo/111257400576804393)
(whose instructions we are repeating here)!

Add the Guile Homebrew tap if you haven't already:

```
brew tap aconchillo/guile
```

If Guile is already installed with Homebrew, unlink it since we need a newer version:

```
brew unlink guile
```

Now, just install Hoot:

```
brew install guile-hoot
```

This will also install `guile-next`, a bleeding edge version of Guile,
so it might take a while if there's no bottle available.

## Building from source

### Easy path: Use Guix

This is by far the easiest path because Guix does all the hard work
for you.

First, clone the repository:

```
git clone https://gitlab.com/spritely/guile-hoot
cd guile-hoot
guix shell
./bootstrap.sh && ./configure && make
```

The `guix shell` step will take a while to build because we're using a
custom version of Guile and a bleeding edge version of V8.
If everything worked okay you can now run `make check`:

```
make check
```

Did everything pass?  Cool!  That means Hoot works on your machine!

### Advanced path: Build dependencies on your own

Maybe you want to understand better what Hoot is actually doing, or
maybe you want to hack on the version of Guile used for Hoot, or etc!
This section is for you.

First, you need to build Guile from the `main` branch.

Then you can clone and build this repo:

```
git clone https://gitlab.com/spritely/guile-hoot
cd guile-hoot
./bootstrap.sh && ./configure && make
```

To run the test suite against a production WASM host, you will need a
recent version of V8.  Building V8 is annoying.  You need to have
`depot_tools` installed; see https://v8.dev/docs/source-code.  Once
you have that see https://v8.dev/docs/build to build.  You will end up
with a `d8` binary in `out/x64.release` (if you are on an x86-64
platform).

If all that works you should be able to `make check`:

```
make check
```

If you want to skip the V8 stuff, you can run the test suite against
our own WASM interpreter instead:

```
make check WASM_HOST=hoot
```

## Try it out

Hoot is a self-contained system, so the easiest way to try it is from
the Guile REPL:

```
./pre-inst-env guile
```

From the Guile prompt, enter the following to evaluate the program
`42` in Hoot's built-in WASM interpreter:

```
scheme@(guile-user)> ,use (hoot reflect)
scheme@(guile-user)> (compile-value 42)
$5 = 42
```

More interestingly, Scheme procedures that live within the WASM guest
module can be called from Scheme as if they were host procedures:

```
scheme@(guile-user)> (define hello (compile-value '(lambda (x) (list "hello" x))))
scheme@(guile-user)> hello
$6 = #<hoot #<procedure>>
scheme@(guile-user)> (hello "world")
$7 = #<hoot ("hello" "world")>
```

Hoot also introduces the `guild compile-wasm` subcommand which can be
used to compile a Scheme file to WASM via the CLI or a build script:

```
echo 42 > 42.scm
./pre-inst-env guild compile-wasm -o 42.wasm 42.scm
```

To actually load `42.wasm` you could use the Hoot VM as mentioned
above or use a production WebAssembly implementation such as a web
browser.  By the end of 2023 all common web browsers will support the
WASM capabilities Hoot is using, but currently Chrome Dev and Firefox
Nightly are the browsers to use.

The generated WebAssembly doesn't depend on a web browser/JavaScript,
but it does take some capabilities from the host system, notably the
bignum implementation and weak maps.  For web browsers, these
facilities are provided by [`reflect.js`](./js-runtime/reflect.js).
To help in adapting between JavaScript and the ABI of compiled Scheme
code, there is an auxiliary WebAssembly module `reflect.wasm` that
needs to be compiled from [`reflect.wat`](./js-runtime/reflect.wat),
as well as string helper called `wtf8.wasm`.

See the manual for a more in-depth tutorial and full API
documentation!

## Maintenance

### GitLab CI

Here's how to build a Docker image for use in GitLab CI.  Guix
produces the actual image, but skopeo is required to upload it to the
GitLab container registry.

Get `skopeo`:

```
guix shell skopeo
```

If this is your first time using the GitLab registry, you need to
login.  This requires setting up a [GitLab personal access
token](https://gitlab.com/-/profile/personal_access_tokens) with
`read_api` and `write_registry` permissions.  Once you have a token,
run:

```
skopeo login registry.gitlab.com
```

Build and upload the image:

```
./upload-ci-image
```

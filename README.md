# Hoot

![Hoot logo](./hoot.png)

This repo is for updates to "Hoot", the codename for the
[Guile->WebAssembly
project](https://spritely.institute/news/guile-on-web-assembly-project-underway.html)
launched by the [Spritely Institute](https://spritely.institute/).  As
work on Hoot progresses, updates will be posted here!

## Project goals and time-frame

Hoot aims to be an ahead-of-time compiler for all of [R7RS-small
Scheme](https://small.r7rs.org/) to WebAssembly.  We are targetting the
WebAssembly extensions that appear ready to ship in Q4 2023, notably
including garbage collection and tail calls, and hope to reach our goal
in July 2023.

Hoot is being developed here and in [Spritely's Guile development
branches](https://gitlab.com/spritely/guile).  This repository is for
tracking history and design decisions as the project progresses, and for
experimentations.

After completing R7RS support, we will move on to a full Guile port,
including delimited continuations and so on.  We are keeping this
end-goal in mind as we build the early deliverable.

Resulting code should all run on stock Guile.  We may need to upstream
some patches to Guile, and will do so as it seems appropriate.

## But... why the name "Hoot"?

We thought this project deserved a cute project name and mascot, and
everyone at the time agreed an owl was nice, and Christine
Lemmer-Webber had recently just drawn up this owl pixel art, and
so it became the mascot.
The name naturally flowed from there.

## Updates

See the [log file](design/log.md).

## Try it out

### Setting up your dev environment, guix edition

This is by far the easiest path because Guix does all the hard work
for you.

First, clone the repository:

```
$ git clone https://gitlab.com/spritely/guile-hoot
$ cd guile-hoot
$ guix shell
$ ./bootstrap.sh && ./configure && make
```

The `guix shell` step will take a while to build because we're using a
custom version of Guile and a bleeding edge version of V8.
If everything worked okay you can now run `make check`:

```
$ make check
```

Did everything pass?  Cool!  That means Hoot works on your machine!


## Try it out (manual edition)

Maybe you want to understand better what Hoot is actually doing, or
maybe you want to hack on the version of Guile used for Hoot, or etc!
This section is for you.

You need Guile from the `wip-tailify` branch.  Then you check out this
repo:

```
$ git clone https://gitlab.com/spritely/guile-hoot
$ cd guile-hoot
$ echo 42 > 42.scm
$ GUILE_LOAD_PATH=`pwd`/module guild compile-wasm -o 42.wasm 42.scm
wrote `42.wasm`
```

You're done!  With the start of things, that is :)  To actually load the
`42.wasm` you need a capable WebAssembly implementation.  By the end of
2023 all common web browsers will have these capabilities, but currently
we are on the bleeding edge and using development browsers.

The generated WebAssembly doesn't depend on web browsers, but it does
take some capabilities from the host system, notably the bignum
implementation and weak maps.  For web browsers, these facilities are
provided by [`reflect.js`](./js-runtime/reflect.js).  To help in
adapting between JavaScript and the ABI of compiled Scheme code, there
is an auxiliary WebAssembly module `reflect.wasm` that needs to be
compiled from [`reflect.wat`](./js-runtime/reflect.wat).

In `guile-hoot` type `make`:

```
$ make
```

Now, to load these files in V8, again you need a really recent V8.
Probably anything since mid-March 2023 will do.  Building V8 is
annoying.  You need to have `depot_tools` installed; see
https://v8.dev/docs/source-code.  Once you have that see
https://v8.dev/docs/build to build.  You will end up with a `d8` binary
in `out/x64.release` (if you are on an x86-64 platform).

If all that works you should be able to `make check`:

```
$ make check
============================================================================
Testsuite summary for guile-hoot 0.1.0
============================================================================
# TOTAL: 3
# PASS:  3
# SKIP:  0
# XFAIL: 0
# FAIL:  0
# XPASS: 0
# ERROR: 0
============================================================================
```

## The shape of things

In the end we expect to be able to compile Scheme programs to single
WebAssembly files.  To deploy on web browsers there will be an
associated JavaScript module.  Some  non-web targets are hosted by
JavaScript implementations (e.g. node.js); those are similar to web
browsers.  Otherwise on WASI hosts we expect to have a WASI-specific
support module.

The minimal compiled module size is a little less than a kilobyte of
wasm, uncompressed.  The auxiliary WebAssembly module to do impedance
matching with JavaScript is another three kilobytes uncompressed, and
the generic JS library is another 250 lines of unminified JS.  As we
implement more of Scheme, we hope to preserve this "small programs
compile to small files" property, rather than having every compiled
program include the whole of Guile's standard library.

## GitLab CI

Here's how to build a Docker image for use in GitLab CI.  Guix
produces the actual image, but Docker is required to upload it to the
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

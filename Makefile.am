V8_FEATURES=--experimental-wasm-stringref

SUBDIRS = module
WASMOBJECTS = $(WATS:%.wat=%.wasm)
CLEANFILES = $(WASMOBJECTS)
nobase_noinst_DATA = $(WASMOBJECTS)

WATS = 						\
  examples/basic-types.wat			\
  js-runtime/reflect.wat			\
  js-runtime/wtf8.wat

SUFFIXES = .wat .wasm
# The mkdir is necessary for 'make distcheck' since it's building in a
# different directory than the project root.
.wat.wasm:
	$(AM_V_GEN)$(MKDIR_P) $(dir $@) && \
	  $(top_builddir)/pre-inst-env $(GUILD) assemble-wasm -o "$@" "$<"

$(top_builddir)/js-runtime/reflect.wasm: js-runtime/reflect.wat module/hoot/stdlib.scm
	$(AM_V_GEN)$(MKDIR_P) $(dir $@) && \
	  $(top_builddir)/pre-inst-env $(GUILD) assemble-wasm \
	    --stdlib="((@ (hoot stdlib) compute-stdlib) #t)" -o "$@" "$<"

TEST_EXTENSIONS = .js .scm
SCM_LOG_COMPILER = $(top_builddir)/pre-inst-env $(GUILE)
AM_SCM_LOG_FLAGS = --no-auto-compile -L $(top_srcdir)
JS_LOG_COMPILER = $(top_builddir)/js-test
AM_JS_LOG_FLAGS = $(V8_FEATURES)
CLEANFILES += $(SCM_TESTS:test/%.scm=%.log)

SCM_TESTS = 					\
  test/test-assignment.scm			\
  test/test-atomics.scm				\
  test/test-bitwise.scm				\
  test/test-bytevectors.scm			\
  test/test-char-prelude.scm			\
  test/test-constants.scm			\
  test/test-exceptions.scm			\
  test/test-flonums.scm				\
  test/test-complex.scm				\
  test/test-fluids.scm				\
  test/test-fractions.scm			\
  test/test-hash-tables.scm			\
  test/test-inline-wasm.scm			\
  test/test-ints.scm				\
  test/test-keywords.scm			\
  test/test-lower.scm				\
  test/test-lower-globals.scm			\
  test/test-lower-stringrefs.scm		\
  test/test-number-to-string.scm		\
  test/test-string-to-number.scm		\
  test/test-numeric.scm                         \
  test/test-pairs.scm				\
  test/test-ports.scm				\
  test/test-procedures.scm			\
  test/test-promises.scm			\
  test/test-prompts.scm				\
  test/test-read.scm				\
  test/test-records.scm				\
  test/test-strings.scm				\
  test/test-symbols.scm				\
  test/test-vectors.scm				\
  test/test-vm.scm				\
  test/test-wasm-assembler.scm			\
  test/test-disassemble.scm			\
  test/test-write.scm				\
  test/test-ffi.scm				\
  #

JS_TESTS = 					\
  examples/basic-types.js

TESTS =						\
  $(JS_TESTS)					\
  $(SCM_TESTS)

info_TEXINFOS = doc/hoot.texi
doc_hoot_TEXINFOS = doc/hoot.texi 		\
                    doc/apache-2.0.texi

dvi: # Don't build dvi docs
html-local:
	$(GUILE) --no-auto-compile doc/build-html.scm

jsruntimedir = $(pkgdatadir)/js-runtime
dist_jsruntime_DATA = 				\
  js-runtime/reflect.js				\
  js-runtime/reflect.wasm			\
  js-runtime/wtf8.wasm

EXTRA_DIST = 					\
  $(WATS)					\
  $(TESTS)					\
  bin/eval.scm					\
  bin/call.scm					\
  bin/compile.scm				\
  bootstrap.sh					\
  guix.scm					\
  COPYING					\
  examples/hof.gwat				\
  examples/README.md				\
  examples/test-lib.js				\
  hoot.png					\
  README.md					\
  test/basic-types.wasm				\
  test/load-wasm-and-print.js			\
  test/load-wasm-and-print-primitive.js		\
  test/test-call.js				\
  test/utils.scm				\
  doc/build-html.scm				\
  doc/hoot.css

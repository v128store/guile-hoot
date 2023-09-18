V8_FEATURES=--experimental-wasm-gc --experimental-wasm-stringref --experimental-wasm-return-call

SUBDIRS = module
WASMOBJECTS = $(WATS:%.wat=%.wasm)
CLEANFILES = $(WASMOBJECTS)
nobase_noinst_DATA = $(WASMOBJECTS)

WATS = 						\
  examples/basic-types.wat			\
  js-runtime/reflect.wat

SUFFIXES = .wat .wasm
# The mkdir is necessary for 'make distcheck' since it's building in a
# different directory than the project root.
.wat.wasm:
	$(AM_V_GEN)$(MKDIR_P) $(dir $@) && \
	  $(top_builddir)/pre-inst-env $(GUILD) assemble-wasm -o "$@" "$<"

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
  test/test-constants.scm			\
  test/test-exceptions.scm			\
  test/test-flonums.scm				\
  test/test-fluids.scm				\
  test/test-fractions.scm			\
  test/test-ints.scm				\
  test/test-number-to-string.scm		\
  test/test-pairs.scm				\
  test/test-ports.scm				\
  test/test-procedures.scm			\
  test/test-prompts.scm				\
  test/test-strings.scm				\
  test/test-vectors.scm				\
  test/test-vm.scm				\
  test/test-wasm-assembler.scm			\
  #

JS_TESTS = 					\
  examples/basic-types.js

TESTS =						\
  $(JS_TESTS)					\
  $(SCM_TESTS)

EXTRA_DIST = 					\
  $(WATS)					\
  $(TESTS)					\
  bin/eval.scm					\
  bin/call.scm					\
  bin/compile.scm				\
  bootstrap.sh					\
  COPYING					\
  examples/hof.gwat				\
  examples/README.md				\
  examples/test-lib.js				\
  hoot.png					\
  js-runtime/reflect.js				\
  manifest.scm					\
  README.md					\
  test/basic-types.wasm				\
  test/load-wasm-and-print.js			\
  test/test-call.js

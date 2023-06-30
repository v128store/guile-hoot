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
	  $(top_builddir)/pre-inst-env guild assemble-wasm -o "$@" "$<"

TEST_EXTENSIONS = .js .scm
SCM_LOG_COMPILER = $(top_builddir)/pre-inst-env $(GUILE)
AM_SCM_LOG_FLAGS = --no-auto-compile
JS_LOG_COMPILER = $(top_builddir)/js-test
AM_JS_LOG_FLAGS = $(V8_FEATURES)
CLEANFILES += $(TESTS:test/%.scm=%.log)

TESTS =						\
  examples/basic-types.js			\
  test/test-wasm-assembler.scm			\
  test/test-constants.scm

EXTRA_DIST = 					\
  $(WATS)					\
  $(TESTS)					\
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

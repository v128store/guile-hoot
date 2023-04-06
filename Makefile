BINARYEN=~/src/binaryen/bin
BINARYEN_FEATURES=--enable-gc --enable-strings --enable-tail-call --enable-reference-types --enable-bulk-memory
WASM_AS=$(BINARYEN)/wasm-as $(BINARYEN_FEATURES)

V8=~/src/v8/out/x64.release
V8_FEATURES=--experimental-wasm-gc --experimental-wasm-stringref --experimental-wasm-return-call
D8=$(V8)/d8 $(V8_FEATURES)

OBJECTS=js-runtime/reflect.wasm examples/basic-types.wasm

V8_TESTS=examples/basic-types
V8_CHECKS=$(foreach test,$(V8_TESTS),$(test).check)

GUILE_TESTS=test/test-wasm-assembler
GUILE=guile
GUILE_CHECKS=$(foreach test,$(GUILE_TESTS),$(test).check)

CHECKS=$(GUILE_CHECKS) $(V8_CHECKS)

top_srcdir=$(shell pwd)

all: $(OBJECTS)
check: $(CHECKS)

$(OBJECTS): %.wasm: %.wat
	$(WASM_AS) -o $@ $<

$(GUILE_CHECKS): %.check: %.scm
	cd $(dir $<) && $(GUILE) -L $(top_srcdir)/module $(notdir $<)

$(V8_CHECKS): %.check: %.js %.wasm
	cd $(dir $<) && $(D8) $(notdir $<)

clean:
	rm -f $(OBJECTS)

.PHONY: all clean check $(CHECKS)

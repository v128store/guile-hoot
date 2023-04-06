BINARYEN=~/src/binaryen/bin
BINARYEN_FEATURES=--enable-gc --enable-strings --enable-tail-call --enable-reference-types --enable-bulk-memory
WASM_AS=$(BINARYEN)/wasm-as $(BINARYEN_FEATURES)

V8=~/src/v8/out/x64.release
V8_FEATURES=--experimental-wasm-gc --experimental-wasm-stringref --experimental-wasm-return-call
D8=$(V8)/d8 $(V8_FEATURES)

TESTS=examples/basic-types
OBJECTS=js-runtime/reflect.wasm examples/basic-types.wasm
CHECKS=$(foreach test,$(TESTS),$(test).check)

all: $(OBJECTS)
check: $(CHECKS)

$(OBJECTS): %.wasm: %.wat
	$(WASM_AS) -o $@ $<

$(CHECKS): %.check: %.js %.wasm
	cd $(dir $<) && $(D8) $(notdir $<)

clean:
	rm -f $(OBJECTS)

.PHONY: all clean check $(CHECKS)

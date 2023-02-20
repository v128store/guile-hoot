TESTS=trivial-1
OBJECTS=$(foreach test,$(TESTS),$(test).wasm)
CHECKS=$(foreach test,$(TESTS),$(test).check)

BINARYEN=~/src/binaryen/bin
BINARYEN_FEATURES=--enable-gc --enable-strings --enable-tail-call --enable-reference-types
WASM_AS=$(BINARYEN)/wasm-as $(BINARYEN_FEATURES)

V8=~/src/v8/out/x64.release
V8_FEATURES=--experimental-wasm-gc --experimental-wasm-stringref --experimental-wasm-return-call
D8=$(V8)/d8 $(V8_FEATURES)

all: $(OBJECTS)
check: $(CHECKS)

$(OBJECTS): %.wasm: %.wat
	$(WASM_AS) -o $@ $<

$(CHECKS): %.check: %.wasm %.expected
	$(D8) test.js -- $< $(shell cat $*.expected)

clean:
	rm -f $(OBJECTS)

.PHONY: all clean check $(CHECKS)

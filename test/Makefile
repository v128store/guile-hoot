TESTS=test-wasm-assembler
GUILE=guile

CHECKS=$(foreach test,$(TESTS),$(test).check)

check: $(CHECKS)

$(CHECKS): %.check: %.scm
	$(GUILE) -L ../module ./$<

.PHONY: clean check $(CHECKS)

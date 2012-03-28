UNAME_S:=$(shell uname -s)
FLAVOR=Debug
V=$(UNAME_S)_$(FLAVOR)

JS_TESTS=$(wildcard *Test.js)
JS_OKS=$(patsubst %,$V/%.passed,$(JS_TESTS))

all: $(JS_OKS)

NODE_DIR = ../../node/$V

$V/%.js.passed: %.js
	@mkdir -p $(@D)
	$(NODE_DIR)/node $<
	@touch $@

clean:
	rm -rf $V

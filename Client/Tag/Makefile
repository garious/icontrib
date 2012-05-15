UNAME_S:=$(shell uname -s)
FLAVOR=Debug
V=$(UNAME_S)_$(FLAVOR)

JS_TESTS=$(wildcard *Test.js)
JS_OKS=$(patsubst %,$V/%.passed,$(JS_TESTS))

all: $(JS_OKS)

tree_%:
	$(MAKE) $(patsubst tree_%,%,$@)

NODE_DIR = ../../node/$(UNAME_S)

$V/%Test.js.passed: %Test.js %.js
	@mkdir -p $(@D)
	@echo Testing: $<
	@$(NODE_DIR)/node YoinkAdapter.js $<
	@touch $@

clean:
	rm -rf $V

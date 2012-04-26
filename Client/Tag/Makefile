UNAME_S:=$(shell uname -s)
FLAVOR=Debug
V=$(UNAME_S)_$(FLAVOR)

JS_TESTS=ObservableTest.js TagTest.js ShapesTest.js LayoutTest.js #$(wildcard *Test.js)
JS_OKS=$(patsubst %,$V/%.passed,$(JS_TESTS))

all: $(JS_OKS)

NODE_DIR = ../../node/$V

$V/%Test.js.passed: %Test.js %.js
	@mkdir -p $(@D)
	@echo Testing: $<
	@$(NODE_DIR)/node YoinkAdapter.js $<
	@touch $@

clean:
	rm -rf $V

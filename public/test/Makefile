UNAME_S:=$(shell uname -s)
FLAVOR=Debug
V=$(UNAME_S)_$(FLAVOR)

all: $V/httpTest.js.passed

NODE_DIR = ../../node/$V

$V/%.js.passed: %.js
	@mkdir -p $(@D)
	$(NODE_DIR)/node $<
	@touch $@

clean:
	rm -rf $V

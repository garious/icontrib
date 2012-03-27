UNAME:=$(shell uname)
FLAVOR = Debug
V = $(UNAME)_$(FLAVOR)

-include $(UNAME).min

GHC_FLAGS+=-Wall -Werror -threaded

RUN_TESTS := $(wildcard *Test.hs)
TESTS = $(patsubst %,$V/%.passed,$(RUN_TESTS))

RUN_INTEGRATION_TESTS=IntegrationTest.js

RUN_JS_TESTS := $(filter-out $(RUN_INTEGRATION_TESTS),$(wildcard *Test.js))
JS_TESTS = $(patsubst %,$V/%.passed,$(RUN_JS_TESTS))

INTEGRATION_TESTS = $(patsubst %,$V/%.passed,$(RUN_INTEGRATION_TESTS))

all: server private/static.ok client #$(INTEGRATION_TESTS)

server: $V/ship/import $V/ship/icontrib $V/ship/libcryptopp.dylib #$(JS_TESTS)

client:
	$(MAKE) -C public V=$V
	$(MAKE) -C yoink V=$V

serve: server private/static.ok client
	$V/ship/icontrib

private/static.ok: $V/ship/import $(wildcard private/static/*/*) $(wildcard Server/Data/*.hs)
	@$<
	@touch $@

# TODO: Replace this with a proper dependency scanner: "ghc -M"
$(foreach n,$(RUN_TESTS),$(eval $(patsubst %,$V/%.passed,$n): $n $(patsubst %Test.hs,%.hs,$n)))

clean:
	rm -rf $V
	rm -rf dist

dist:
	cabal configure
	cabal build

deps:
	cabal update
	cabal install --only-dependencies


NODE_DIR = node/$V

$V/IntegrationTest.js.passed: public/$V/ship/IContrib.js

$V/%.js.passed: %.js
	@mkdir -p $(@D)
	@echo Testing: $<
	@$(NODE_DIR)/node $<
	@touch $@

$V/ship/%:Server/$V/ship/%
	@mkdir -p $(@D)
	cp $< $@

Server/%:
	$(MAKE) -C Server V=$V $*

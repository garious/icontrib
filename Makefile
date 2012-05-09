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

all: server private/db/static.ok client $(INTEGRATION_TESTS)



.PHONY: server client
tree: server client

server:
	$(MAKE) -C Server V=$V

client:
	$(MAKE) -C Client V=$V
	$(MAKE) -C Yoink V=$V

tree_%:
	$(MAKE) -C Server V=$V $(patsubst tree_%,%,$@)
	$(MAKE) -C Client V=$V $@
	$(MAKE) -C Yoink V=$V $(patsubst tree_%,%,$@)
	$(MAKE) V=$V $(patsubst tree_%,%,$@)

Client/$V/ship/IContrib.js: client

Server/$V/ship/icontrib: server
Server/$V/ship/import: server

serve: Server/$V/ship/icontrib private/static.ok client  
	@$<

private/db/static.ok: Server/$V/ship/import $(wildcard private/static/*/*) $(wildcard Server/Data/*.hs)
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
	cabal install --only-dependencies Server/icontrib.cabal
	npm install zombie


NODE_DIR = node/$V

$V/IntegrationTest.js.passed: Client/$V/ship/IContrib.js Server/$V/ship/icontrib

$V/SiteTest.js.passed: private/db/static.ok

$V/%.js.passed: %.js
	@mkdir -p $(@D)
	@echo Testing: $<
	@$(NODE_DIR)/node $<
	@touch $@


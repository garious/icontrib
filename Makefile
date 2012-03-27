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

all: server private/static.ok client $(INTEGRATION_TESTS)

server: server_install $(JS_TESTS)

client:
	$(MAKE) -C public V=$V
	$(MAKE) -C yoink V=$V

serve: server private/static.ok client
	$V/ship/icontrib

private/static.ok: $V/ship/import $(wildcard private/static/*/*) $(wildcard Data/*.hs)
	$<
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

server_install:Server/$V/ship/import Server/$V/ship/icontrib Server/$V/ship/libcryptopp.dylib
	@mkdir -p $(@D)
	$(MAKE) -C Server V=$V
	@cp Server/$V/ship/import  $V/ship/import
	@cp Server/$V/ship/icontrib  $V/ship/icontrib
	@cp Server/$V/ship/libcryptopp.dylib $V/ship/libcryptopp.dylib

Server/$V/ship/import Server/$V/ship/icontrib Server/$V/ship/libcryptopp.dylib:
	$(MAKE) -C Server V=$V

.PHONY:Server/$V/ship/import Server/$V/ship/icontrib Server/$V/ship/libcryptopp.dylib



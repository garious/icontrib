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

server: $V/ship/icontrib $V/ship/import $(JS_TESTS)

client:
	$(MAKE) -C public V=$V
	$(MAKE) -C yoink V=$V

serve: $V/ship/icontrib
	$<

libcryptopp.dylib:/usr/local/Cellar/cryptopp/5.6.1/lib/libcryptopp.a
	g++  -fpic -nostartfiles -nostdlib -shared /usr/local/Cellar/cryptopp/5.6.1/lib/libcryptopp.a -o libcryptopp.dylib

/usr/local/Cellar/cryptopp/5.6.1/lib/libcryptopp.a:
	brew install cryptopp #for ssl



$V/ship/icontrib: Main.hs Site.hs JsWidget.hs $(TESTS) libcryptopp.dylib
	@mkdir -p $(@D)
	ghc $(GHC_FLAGS) -outputdir $V -o $@ --make $<

private/static.ok: $V/ship/import $(wildcard private/static/*/*) $(wildcard Data/*.hs)
	$<
	@touch $@

$V/ship/import: import.hs $(wildcard Data/*.hs)
	@mkdir -p $(@D)
	ghc $(GHC_FLAGS) -outputdir $V -o $@ --make $<

# TODO: Replace this with a proper dependency scanner: "ghc -M"
$(foreach n,$(RUN_TESTS),$(eval $(patsubst %,$V/%.passed,$n): $n $(patsubst %Test.hs,%.hs,$n)))

$V/%.hs.passed:
	@mkdir -p $(@D)
	@echo Testing: $<
	@runghc $(GHC_FLAGS) $<
	@touch $@

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


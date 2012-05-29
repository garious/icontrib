UNAME:=$(shell uname)
FLAVOR = Debug
V = $(UNAME)_$(FLAVOR)

-include $(UNAME).min

GHC_FLAGS+=-Wall -Werror -threaded

RUN_TESTS := $(wildcard *Test.hs)
TESTS = $(patsubst %,$V/%.passed,$(RUN_TESTS))

# TODO: Figure out why latest version of Zombie JS test framework chokes.
#RUN_INTEGRATION_TESTS=IntegrationTest.js SiteTest.js

RUN_JS_TESTS := $(filter-out $(RUN_INTEGRATION_TESTS),$(wildcard *Test.js))
JS_TESTS = $(patsubst %,$V/%.passed,$(RUN_JS_TESTS))

INTEGRATION_TESTS = $(patsubst %,$V/%.passed,$(RUN_INTEGRATION_TESTS))

all: server private/db/static.ok client $(INTEGRATION_TESTS)



.PHONY: server client
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

Client/$V/Ship/Index.js: client

Server/$V/ship/icontrib: server
Server/$V/ship/import: server

# TODO: Enable SSL 
#SERVER_FLAGS.Release+=--ssl
SERVER_FLAGS=--yoinkdir=Yoink/$V/Ship --tagdir=Tag --moddir=Client/$V/Ship --moddir=private/images $(SERVER_FLAGS.$(FLAVOR))

serve: Server/$V/ship/icontrib private/db/static.ok client  
	$< $(SERVER_FLAGS)

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


NODE_DIR = node/$(UNAME)

WEBAPP_DEPS.Release=Client/$V/Ship/Index.js
WEBAPP_DEPS=$(WEBAPP_DEPS.$(FLAVOR))

$V/IntegrationTest.js.passed: $(WEBAPP_DEPS) Server/$V/ship/icontrib

$V/SiteTest.js.passed: private/db/static.ok

$V/%.js.passed: %.js
	@mkdir -p $(@D)
	@echo Testing: $<
	$(NODE_DIR)/node $< Server/$V/ship/icontrib Client/$V/Ship
	@touch $@


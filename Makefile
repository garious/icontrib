UNAME:=$(shell uname)

-include $(UNAME).min

GHC_FLAGS+=-Wall -Werror -threaded

RUN_TESTS := $(wildcard *Test.hs)

UNAME_S := $(shell uname -s)

TARGET = $(UNAME_S)
FLAVOR = Debug

V = $(TARGET)_$(FLAVOR)

all: test $V/icontrib lint $V/import private/static.ok

serve: $V/icontrib private/static.ok
	$<

test: $(patsubst %,$V/%.passed,$(RUN_TESTS))

$V/icontrib: Main.hs Site.hs test libcryptopp.dylib
	@mkdir -p $(@D)
	ghc $(GHC_FLAGS) -outputdir $V -o $@ --make $<

private/static.ok: $V/import private/static/*/* Data/*.hs
	$V/import 
	@touch private/static.ok

$V/import: import.hs Data/*.hs
	@mkdir -p $(@D)
	ghc $(GHC_FLAGS) -outputdir $V -o $@ --make $<

# TODO: Replace this with a proper dependency scanner: "ghc -M"
$(foreach n,$(RUN_TESTS),$(eval $(patsubst %,$V/%.passed,$n): $n $(patsubst %Test.hs,%.hs,$n)))

$V/%.passed:
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
	npm install jslint

deps.Darwin:
	brew install jslint
	brew install node
	brew install npm

JS_WHITELIST:= \
    public/js/json2.js \

JS_FILES:=$(filter-out $(JS_WHITELIST),$(wildcard public/*.js) $(wildcard public/*/*.js))

#JSLINT_FILES:=public/yoink/yoink.js

lint: $(patsubst %,$V/%.ok,$(JS_FILES)) $(patsubst %,$V/%.lint,$(JSLINT_FILES))

$V/%.js.ok: %.js
	@echo Linting: $<
	@jsl -nologo -nofilelisting -nosummary -output-format "$*.js:__LINE__:__COL__: __ERROR__" -process $<
	@mkdir -p $(@D)
	@touch $@

$V/%.js.lint: %.js
	node_modules/.bin/jslint --predef=define --predef=require --predef=baseUrl --predef=YOINK $<
	@mkdir -p $(@D)
	@touch $@

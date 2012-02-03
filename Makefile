UNAME:=$(shell uname)

-include $(UNAME).min

GHC_FLAGS+=-Wall -Werror

RUN_TESTS := $(wildcard *Test.hs)

o = out

all: test $o/icontrib lint

serve: $o/icontrib
	$<

libcryptopp.dylib:/usr/local/Cellar/cryptopp/5.6.1/lib/libcryptopp.a
	echo "HEY!!! YOU ARE BUILDING A SHARED LIBRARY FOR SSL"
	     "this needs to point to system installation of the library on a live server"
	g++  -fpic -nostartfiles -nostdlib -shared /usr/local/Cellar/cryptopp/5.6.1/lib/libcryptopp.a -o libcryptopp.dylib

/usr/local/Cellar/cryptopp/5.6.1/lib/libcryptopp.a:
	echo "HEY!!! YOU ARE FETCHING A SHARED LIBRARY FOR SSL"
	     "this needs to point to system installation of the library on a live server"
	brew install cryptopp #for ssl

test: $(patsubst %,$o/%.passed,$(RUN_TESTS))

$o/icontrib: Main.hs Site.hs test libcryptopp.dylib
	@mkdir -p $(@D)
	ghc $(GHC_FLAGS) -outputdir $o -o $@ --make $<

# TODO: Replace this with a proper dependency scanner: "ghc -M"
$(foreach n,$(RUN_TESTS),$(eval $(patsubst %,$o/%.passed,$n): $n $(patsubst %Test.hs,%.hs,$n)))

$o/%.passed:
	@mkdir -p $(@D)
	@echo Testing: $<
	@runghc $(GHC_FLAGS) $<
	@touch $@

clean:
	rm -rf $o
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
    $(wildcard public/jquery/jquery*.js) \
    $(wildcard public/js/less-*.js) \
    public/js/waitScreen.js \
    public/widgets/waitScreen.js \
    public/js/json2.js \

JS_FILES:=$(filter-out $(JS_WHITELIST),$(wildcard public/*.js) $(wildcard public/*/*.js))

JSLINT_FILES:=public/yoink/yoink.js

lint: $(patsubst %,$o/%.ok,$(JS_FILES)) $(patsubst %,$o/%.lint,$(JSLINT_FILES))

$o/%.js.ok: %.js
	jsl -output-format "$*.js:__LINE__:__COL__: __ERROR__" -process $<
	@mkdir -p $(@D)
	@touch $@

$o/%.js.lint: %.js
	node_modules/.bin/jslint --predef=define --predef=require --predef=baseUrl --predef=YOINK $<
	@mkdir -p $(@D)
	@touch $@

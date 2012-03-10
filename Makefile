UNAME:=$(shell uname)
FLAVOR = Debug
V = $(UNAME)_$(FLAVOR)

-include $(UNAME).min

GHC_FLAGS+=-Wall -Werror -threaded

RUN_TESTS := $(wildcard *Test.hs)

all: test $V/icontrib client $V/import private/static.ok

client:
	$(MAKE) -C public V=$V

serve: $V/icontrib private/static.ok
	$<

libcryptopp.dylib:/usr/local/Cellar/cryptopp/5.6.1/lib/libcryptopp.a
	g++  -fpic -nostartfiles -nostdlib -shared /usr/local/Cellar/cryptopp/5.6.1/lib/libcryptopp.a -o libcryptopp.dylib

/usr/local/Cellar/cryptopp/5.6.1/lib/libcryptopp.a:
	brew install cryptopp #for ssl

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


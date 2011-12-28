UNAME:=$(shell uname)

-include $(UNAME).min

GHC_FLAGS+=-Wall -Werror

o = out

all: test $o/icontrib

serve: $o/icontrib
	$<

test: $(patsubst %,$o/%.passed,$(wildcard *Test.hs))


$o/icontrib: Server.hs Site.hs test
	@mkdir -p $(@D)
	ghc $(GHC_FLAGS) -outputdir $(@D) -o $@ --make $<

$o/%.passed: %
	@mkdir -p $(@D)
	@echo Testing: $<
	@runghc $(GHC_FLAGS) $<
	@touch $@

%Test.hs: %.hs

clean:
	rm -rf $o
	rm -rf dist

dist:
	cabal configure
	cabal build

deps:
	cabal update
	cabal install --only-dependencies

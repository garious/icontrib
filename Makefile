UNAME:=$(shell uname)

-include $(UNAME).min

GHC_FLAGS+=-Wall -Werror

o = out

all: test $o/icontrib

serve: $o/icontrib
	$<

test: $(patsubst %,$o/%.passed,$(wildcard *Test.hs))

$o/%Test: %Test.hs %.hs


$o/icontrib: Server.hs Site.hs test
	@mkdir -p $(@D)
	ghc $(GHC_FLAGS) -outputdir $(@D) -o $@ --make $<

$o/%.passed: %
	@mkdir -p $(@D)
	@echo Testing: $<
	@runghc -Wall -Werror $<
	@touch $@

clean:
	rm -rf $o

deps:
	cabal install dataenc
	cabal install json
	cabal install acid-state
	cabal install crypto
	cabal install cryptohash
	cabal install happstack-lite

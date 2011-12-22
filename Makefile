o = out

all: test $o/icontrib

test: $(patsubst %,$o/%.passed,$(wildcard *Test.hs))

$o/%Test: %Test.hs %.hs

$o/icontrib: Server.hs
	@mkdir -p $(@D)
	ghc -Wall -Werror -outputdir $(@D) -o $@ --make $<

$o/%.passed: %
	@mkdir -p $(@D)
	@echo Testing: $<
	@runghc -Wall -Werror $<
	@touch $@

clean:
	rm -rf $o

deps:
	cabal install acid-state
	cabal install crypto
	cabal install cryptohash
	cabal install happstack-lite

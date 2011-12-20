
out/%_q:%.hs
	@mkdir -p out
	ghc -hide-package mtl -Wall -Werror -O2 -optl"-Wl,-read_only_relocs,suppress" -outputdir out -main-is $*.test -o $@ --make $*.hs

out/%.hs.ok:out/%_q
	$^ && touch $@

test:$(addsuffix .ok,$(addprefix out/,$(wildcard *.hs)))

deps:
	cabal install acid-state
	cabal install crypto
	cabal install cryptohash
	cabal install happstack-lite

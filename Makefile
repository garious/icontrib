all:$(addsuffix .ok,$(addprefix out/,$(wildcard *.hs)))

out/%:%.hs
	@mkdir -p out
	ghc -hide-package mtl -outputdir out -main-is $*.test -o $@ --make $*.hs

out/%.hs.ok:out/%
	$^ && touch $@

deps:
	cabal install acid-state
	cabal install crypto
	cabal install cryptohash

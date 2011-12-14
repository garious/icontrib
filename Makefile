
out/%_q:%.hs
	@mkdir -p out
	ghc -hide-package mtl -Wall -Werror -O2 -outputdir out -main-is $*.test -o $@ --make $*.hs 2>&1 | grep -v "ld: warning: text reloc in"

out/%.hs.ok:out/%_q
	$^ && touch $@

test:$(addsuffix .ok,$(addprefix out/,$(wildcard *.hs)))

deps:
	cabal install acid-state
	cabal install crypto
	cabal install cryptohash

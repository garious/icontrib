
out/%Q:%Q.hs
	@mkdir -p out
	ghc -hide-package mtl -Wall -Werror -O2 -optl"-Wl,-read_only_relocs,suppress" -outputdir out -main-is $*Q.test -o $@ --make $^

out/%Q.hs.ok:out/%Q
	$^ && touch $@

test:$(addsuffix .ok,$(addprefix out/,$(wildcard *Q.hs)))

deps:
	cabal install acid-state
	cabal install crypto
	cabal install cryptohash
	cabal install happstack-lite

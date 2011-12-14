all:
	ghc -hide-package mtl Account.hs

deps:
	cabal install acid-state
	cabal install crypto
	cabal install cryptohash

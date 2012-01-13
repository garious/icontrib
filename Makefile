UNAME:=$(shell uname)

-include $(UNAME).min

GHC_FLAGS+=-Wall -Werror

RUN_TESTS := $(wildcard *Test.hs)

o = out

all: test $o/icontrib

serve: $o/icontrib
	$<



test: $(patsubst %,$o/%.passed,$(RUN_TESTS))

$o/icontrib: Server.hs Site.hs test
	@mkdir -p $(@D)
	ghc $(GHC_FLAGS) -outputdir $(@D) -o $@ --make $<

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

%.js.lint:%.js Makefile
	echo '(function() {' > $<.pre
	echo '});' > $<.post
	cat $<.pre $< $<.post > $<.lint

%.js.ok:%.js.lint
	jsl -output-format $*.js:__LINE__:__ERROR__ -process $<
	touch $@

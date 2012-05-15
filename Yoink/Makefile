UNAME:=$(shell uname)
FLAVOR = Debug
V = $(UNAME)_$(FLAVOR)

JSLINT = ../node/$(UNAME)/node ../node_modules/jslint/bin/jslint.js
JSL    = ../jsl/$(UNAME)/jsl

all: lint $V/ship/Yoink.js

clean:
	rm -rf $V

JS_FILES:=$(wildcard *.js)

lint: $(patsubst %,$V/%.ok,$(JS_FILES)) $(patsubst %,$V/%.lint,$(JS_FILES))

$V/%.js.ok: %.js
	@echo Linting with jsl: $<
	@$(JSL) -nologo -nofilelisting -nosummary -output-format "$*.js:__LINE__:__COL__: __ERROR__" -process $<
	@mkdir -p $(@D)
	@touch $@

$V/%.js.lint: %.js
	@echo Linting with JSLint: $<
	@$(JSLINT) $<
	@mkdir -p $(@D)
	@touch $@

$(UNAME)_Debug/ship/Yoink.js: Yoink.js lint
	@mkdir -p $(@D)
	cp $< $@

$(UNAME)_Release/ship/Yoink.js: Yoink.js lint
	@mkdir -p $(@D)
	java -jar ../Closure/Compiler.jar --js_output_file=$@ --js=$<


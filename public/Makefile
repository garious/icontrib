UNAME:=$(shell uname)
FLAVOR = Debug
V = $(UNAME)_$(FLAVOR)

all: lint

clean:
	rm -rf $V

deps:
	npm install jslint

deps.Darwin:
	brew install jslint
	brew install node
	brew install npm

JS_WHITELIST:= \
    js/json2.js \

JS_FILES:=$(filter-out $(JS_WHITELIST),$(wildcard *.js) $(wildcard */*.js))

JSLINT_FILES:=yoink/yoink.js

lint: $(patsubst %,$V/%.ok,$(JS_FILES)) $(patsubst %,$V/%.lint,$(JSLINT_FILES))

$V/%.js.ok: %.js
	@echo Linting: $<
	@jsl -nologo -nofilelisting -nosummary -output-format "$*.js:__LINE__:__COL__: __ERROR__" -process $<
	@mkdir -p $(@D)
	@touch $@

$V/%.js.lint: %.js
	node_modules/.bin/jslint --predef=define --predef=require --predef=baseUrl --predef=YOINK $<
	@mkdir -p $(@D)
	@touch $@

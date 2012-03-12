UNAME:=$(shell uname)
FLAVOR = Debug
V = $(UNAME)_$(FLAVOR)

LUA_DIR=../lua-5.2.0/$V/ship

all: lint $V/ship/IContrib.js

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

$V/ship/IContrib.js: JsCache.lua $(filter-out yoink/%.js,$(filter-out test/%.js,$(filter-out %Test.js,$(JS_FILES))))
	@mkdir -p $(@D)
	$(LUA_DIR)/lua JsCache.lua -o $@ $(filter-out $<,$^)


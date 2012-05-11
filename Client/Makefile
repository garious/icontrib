UNAME:=$(shell uname)
FLAVOR = Debug
V = $(UNAME)_$(FLAVOR)

JSL_DIR=../jsl/$V

all: lint $V/ship/IContrib.js

clean:
	rm -rf $V

tree_%:
	$(MAKE) -C Tag $@
	$(MAKE) -C Skin $@
	$(MAKE) $(patsubst tree_%,%,$@)

JS_WHITELIST:=

JS_FILES:=$(filter-out $(JS_WHITELIST),$(wildcard *.js) $(wildcard */*.js))

lint: $(patsubst %,$V/%.ok,$(JS_FILES))

$V/%.js.ok: %.js
	@echo Linting: $<
	@$(JSL_DIR)/jsl -nologo -nofilelisting -nosummary -output-format "$*.js:__LINE__:__COL__: __ERROR__" -process $<
	@mkdir -p $(@D)
	@touch $@

$V/ship/IContrib.js: JsCache.hs $(filter-out Tag/YoinkAdapter.js Tag/Assert.js,$(filter-out %Example.js,$(filter-out %Test.js,$(JS_FILES))))
	@mkdir -p $(@D)
	runghc JsCache.hs -o $@ $(filter-out $<,$^)


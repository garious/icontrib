UNAME_S:=$(shell uname -s)
FLAVOR=Debug
V=$(UNAME_S)_$(FLAVOR)
_@ = @
JS_TESTS=$(wildcard *_test.js)
JS_PASSED=$(patsubst %,$V/%.passed,$(JS_TESTS))

all: lint $(JS_PASSED)

tree_%:
	$(MAKE) $(patsubst tree_%,%,$@)


JS_FILES:=$(wildcard *.js)

lint: $(patsubst %,$V/%.ok,$(JS_FILES))

$V/%.js.ok: %.js
	@echo Linting: $<
	$(_@)jsl -nologo -nofilelisting -nosummary -output-format "$*.js:__LINE__:__COL__: __ERROR__" -process $<
	@mkdir -p $(@D)
	@touch $@


$V/%_test.js.passed: %_test.js %.js
	@mkdir -p $(@D)
	@echo Testing: $<
	$(_@)node ./yoink-adapter.js $<
	@touch $@

serve: server.go all
	go run $<

clean:
	rm -rf $V

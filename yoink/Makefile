UNAME_S:=$(shell uname -s)
FLAVOR=Debug
V=$(UNAME_S)_$(FLAVOR)

JSL_DIR=../jsl/$(UNAME_S)
YOINK_DIR = .

JS_TESTS=$(wildcard *_test.js)
JS_PASSED=$(patsubst %,$V/%.passed,$(JS_TESTS))

LINT_DEPS.Darwin=lint
LINT_DEPS=$(LINT_DEPS.$(UNAME_S))

all: $(LINT_DEPS) $(JS_PASSED)

tree_%:
	$(MAKE) $(patsubst tree_%,%,$@)


JS_FILES:=$(wildcard *.js)

lint: $(patsubst %,$V/%.ok,$(JS_FILES))

$V/%.js.ok: %.js
	@echo Linting: $<
	@$(JSL_DIR)/jsl -nologo -nofilelisting -nosummary -output-format "$*.js:__LINE__:__COL__: __ERROR__" -process $<
	@mkdir -p $(@D)
	@touch $@

$V/%_test.js.passed: %_test.js %.js
	@mkdir -p $(@D)
	@echo Testing: $<
	@node $(YOINK_DIR)/yoink-adapter.js $<
	@touch $@

serve: server.go all
	go run $<

clean:
	rm -rf $V

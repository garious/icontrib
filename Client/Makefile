UNAME:=$(shell uname)
FLAVOR = Debug
V = $(UNAME)_$(FLAVOR)

JSL_DIR=../jsl/$(UNAME)

LINT_DEPS.Darwin=lint
LINT_DEPS=$(LINT_DEPS.$(UNAME))

JS_LINT_FILES:=$(wildcard *.js)

ALL_JS_FILES:=$(JS_LINT_FILES) $(wildcard Skin/*.js)

JS_FILES.Release=$(filter-out %Example.js,$(filter-out %Test.js,$(ALL_JS_FILES)))
JS_FILES.Debug=$(ALL_JS_FILES)
JS_FILES=$(JS_FILES.$(FLAVOR))

FILES:=$(JS_FILES) $(wildcard *.png) favicon.ico $(wildcard *.html) $(wildcard Skin/*.gif) $(wildcard Skin/*.png)

all: $(LINT_DEPS)

clean:
	rm -rf $V

tree_%:
	$(MAKE) -C ../Tag $@
	$(MAKE) -C Skin $@
	$(MAKE) $(patsubst tree_%,%,$@)

# Add dependencies for shipped files
SHIP_FILES=$(addprefix $V/Ship/,$(FILES))
$(foreach X,$(FILES),$(eval $V/Ship/$X: $X))

# Add dependencies for lint files
JS_LINT_OKS=$(patsubst %,$V/%.ok/,$(JS_LINT_FILES))
$(foreach X,$(JS_LINT_FILES),$(eval $V/$X.ok: $X))

lint: $(JS_LINT_OKS)
$(JS_LINT_OKS):
	@echo Linting: $<
	@$(JSL_DIR)/jsl -nologo -nofilelisting -nosummary -output-format "$*.js:__LINE__:__COL__: __ERROR__" -process $<
	@mkdir -p $(@D)
	@touch $@

all: $(SHIP_FILES)
$(SHIP_FILES):
	@mkdir -p $(@D)
	cp $< $@



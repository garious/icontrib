UNAME:=$(shell uname)
FLAVOR = Debug
V = $(UNAME)_$(FLAVOR)

all: $V/Ship/Yoink.js

clean:
	rm -rf $V

JS_FILES:=$(wildcard *.js)

$V/Ship/%.js: %.js
	@mkdir -p $(@D)
	cp $< $@


target := $(shell uname)
flavor = Debug
o = $(target)_$(flavor)

all: $o/ship/yoink.js

clean:
	rm -rf $o

$o/ship/%.js: %.js
	@mkdir -p $(@D)
	cp $< $@


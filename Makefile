target := $(shell uname)
flavor = Debug
o = $(target)_$(flavor)

all: $o/ship/yoink/yoink.js

clean:
	rm -rf $o

serve:
	go run yoink.go

$o/ship/%.js: %.js
	@mkdir -p $(@D)
	cp $< $@


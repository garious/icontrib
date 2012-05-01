all: test compress

V=Release

test:
	$(MAKE) -C test

compress: $V/Yoink.js

$V/Yoink.js: Yoink.js
	@mkdir -p $(@D)
	yuicompressor $< -o $@

clean:
	rm -rf Release
	$(MAKE) -C test clean


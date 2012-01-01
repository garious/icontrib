all: test compress

test:
	$(MAKE) -C test

compress: yoink-min.js

yoink-min.js: yoink.js
	yuicompressor $< -o $@

clean:
	rm -rf yoink-min.js
	$(MAKE) -C test clean


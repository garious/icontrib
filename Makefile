o = tmp

all: test compress

test: $o/node.passed $o/spidermonkey.passed

compress: yoink-min.js

$o/spidermonkey.passed: test/goodScript.js yoink_spidermonkey.js yoink.js
	@mkdir -p $(@D)
	js -f yoink_spidermonkey.js $<
	@touch $@

$o/node.passed: test/goodScript.js yoink_node.js yoink.js
	@mkdir -p $(@D)
	node yoink_node.js $<
	@touch $@

yoink-min.js: yoink.js
	yuicompressor $< -o $@

clean:
	rm -rf $o
	rm -rf yoink-min.js



o = tmp

all: $o/node.passed $o/spidermonkey.passed $o/timers_node.passed #$o/timers_spidermonkey.passed

SRC = ..

$o/spidermonkey.passed: goodScript.js yoink_spidermonkey.js $(SRC)/yoink.js
	@mkdir -p $(@D)
	js -f yoink_spidermonkey.js $<
	@touch $@

$o/node.passed: goodScript.js yoink_node.js $(SRC)/yoink.js
	@mkdir -p $(@D)
	node yoink_node.js $<
	@touch $@

$o/timers_spidermonkey.passed: timers.js yoink_spidermonkey.js $(SRC)/yoink.js
	@mkdir -p $(@D)
	js -f yoink_spidermonkey.js $<
	@touch $@

$o/timers_node.passed: timers.js yoink_node.js $(SRC)/yoink.js
	@mkdir -p $(@D)
	node yoink_node.js $<
	@touch $@


clean:
	rm -rf $o


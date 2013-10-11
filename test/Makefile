V = tmp

all: $V/timers.js.passed

SRC = ..

$V/%.js.passed: %.js yoink-node.js $(SRC)/yoink.js
	@mkdir -p $(@D)
	node yoink-node.js $<
	@touch $@

clean:
	rm -rf $V


V = tmp

all: $V/Timers.js.passed

SRC = ..

$V/%.js.passed: %.js YoinkNode.js $(SRC)/Yoink.js
	@mkdir -p $(@D)
	node YoinkNode.js $<
	@touch $@

clean:
	rm -rf $V


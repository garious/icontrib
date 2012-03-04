V=out

all: $V/httpTest.js.passed

$V/%.js.passed: %.js
	@mkdir -p $(@D)
	node yoink-adapter-node.js $<
	@touch $@

clean:
	rm -rf $V

# This Makefile is generated by the made script:
# https://github.com/alx741/made
#
# Dependencies:
# 	tddlight (https://github.com/alx741/tdd_traffic-light)

TASK_RUNNER=cabal
BUILD_COMMAND=$(TASK_RUNNER) build
TEST_COMMAND=$(TASK_RUNNER) test

export TDDLIGHT_COMPORT=/dev/ttyUSB0

.PHONY: build test css

build: css
	tddlight y
	$(BUILD_COMMAND) && tddlight g || tddlight r
	tddlight y
	$(TASK_RUNNER) exec site rebuild && tddlight g || tddlight r

css:
	cd css && cabal runhaskell Gen.hs

test:
	tddlight y
	$(TEST_COMMAND) && tddlight g || tddlight r

watch:
	while true; do tddlight y; make css && tddlight g || tddlight r; inotifywait -qre close_write css/*.cassius; done

clean:
	$(TASK_RUNNER) exec site clean
	$(TASK_RUNNER) clean && tddlight c

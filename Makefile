all: setup build test lint

.PHONY: setup
setup:
	stack setup
	stack build \
	  --dependencies-only --test --no-run-tests
	stack install hlint weeder
	stack install cc-coverage

.PHONY: build
build:
	stack build \
	  --coverage \
	  --fast --pedantic --test --no-run-tests

.PHONY: test
test:
	stack build \
	  --coverage \
	  --fast --pedantic --test

.PHONY: lint
lint:
	hlint .
	weeder .

cc-coverage:
	[ -n "$(CC_TEST_REPORTER_ID)" ]
	curl -L https://codeclimate.com/downloads/test-reporter/test-reporter-latest-linux-amd64 > ./cc-test-reporter
	chmod +x cc-test-reporter
	tix2cc | ./cc-test-reporter upload-coverage --input -

.PHONY: clean
clean:
	stack clean

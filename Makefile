all: setup build test lint

.PHONY: setup
setup:
	stack setup $(STACK_ARGUMENTS)
	stack build $(STACK_ARGUMENTS) \
	  --dependencies-only --test --no-run-tests

.PHONY: setup.lint
setup.lint:
	stack install $(STACK_ARGUMENTS) \
	  --copy-compiler-tool \
	    hlint \
	    weeder

.PHONY: setup.coverage
setup.coverage:
	stack install $(STACK_ARGUMENTS) \
	  --copy-compiler-tool cc-coverage
	curl -L \
	  https://codeclimate.com/downloads/test-reporter/test-reporter-latest-linux-amd64 \
	  > ./cc-test-reporter
	  chmod +x cc-test-reporter

.PHONY: build
build:
	stack build $(STACK_ARGUMENTS) \
	  --coverage \
	  --fast --pedantic --test --no-run-tests

.PHONY: test
test:
	stack build $(STACK_ARGUMENTS) \
	  --coverage \
	  --fast --pedantic --test

.PHONY: lint
lint:
	stack exec $(STACK_ARGUMENTS) hlint .
	stack exec $(STACK_ARGUMENTS) weeder .

.PHONY: coverage.report
coverage.report:
	stack exec $(STACK_ARGUMENTS) tix2cc \
	  | ./cc-test-reporter upload-coverage --input -

.PHONY: clean
clean:
	stack $(STACK_ARGUMENTS) clean

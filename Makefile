all: setup build test lint

.PHONY: setup
setup:
	stack setup
	stack build --dependencies-only --test --no-run-tests

.PHONY: setup.lint
setup.lint:
	stack install --copy-compiler-tool \
	  hlint \
	  weeder

.PHONY: setup.coverage
setup.coverage:
	stack install --copy-compiler-tool cc-coverage
	curl -L \
	  https://codeclimate.com/downloads/test-reporter/test-reporter-latest-linux-amd64 \
	  > ./cc-test-reporter
	  chmod +x cc-test-reporter

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
	stack exec hlint .
	stack exec weeder .

.PHONY: clean
clean:
	stack clean

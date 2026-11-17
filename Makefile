SCHEME ?= scheme
NODE ?= node
NPM ?= npm
CAP ?=
OBJ := build/obj
LIBDIRS := src::$(OBJ)/src:tests/lib::$(OBJ)/tests/lib
RUN := $(SCHEME) --libdirs $(LIBDIRS) --script tests/run.ss
BUILD_TIMEOUT ?= 1800
TEST_TIMEOUT ?= 600
BENCH_TIMEOUT ?= 1800
TESTS := $(sort $(wildcard tests/*-test.sps))

DJOTJS ?= tests/djot.js
DJOTJS_BUILD := build/djot.js
# The version djot.js's package-lock.json pins.
TYPESCRIPT := 4.9.4
DATA := build/data
CORPUS := pandoc-manual tartan-wikipedia
CORPUS_URL := https://raw.githubusercontent.com/dcampbell24/djot-implementations/962ba43915ba7e8a06e9f6afc6f2f3bb95061a14/benchmark-files
BENCH_DATA := $(DJOTJS)/bench/readme.dj $(wildcard $(DATA)/*.dj)

.PHONY: build test test-data corpus bench clean $(TESTS:%=run/%)
.SECONDARY: $(CORPUS:%=$(DATA)/%.dj)

# Phony so that Chez, which tracks what each object was built against,
# decides what is stale.
build:
	$(CAP) timeout $(BUILD_TIMEOUT) $(SCHEME) -q --script build.ss

# One process per test file, so a crash or leaked global state stays inside
# its file. A test that starts its own Scheme reads SCHEME.
test: $(TESTS:%=run/%)

$(TESTS:%=run/%): run/%: build
	@SCHEME=$(SCHEME) $(CAP) timeout $(TEST_TIMEOUT) $(RUN) ./$(OBJ)/$(*:.sps=.so)

$(DATA)/%.dj: bench/data.sha256
	@mkdir -p $(DATA)
	curl -sSfL -o $@.part $(CORPUS_URL)/$*.dj
	@echo "$$(grep ' $*.dj$$' bench/data.sha256 | cut -d' ' -f1)  $@.part" | sha256sum -c --quiet
	@mv $@.part $@

# Only TypeScript is installed: the rest of djot.js's development packages
# are for its own tests and bundles. cli.ts needs Node's types, which
# TypeScript alone lacks, so the library is compiled from index.ts.
$(DJOTJS_BUILD)/lib/index.js: $(wildcard $(DJOTJS)/src/*.ts)
	rm -rf $(DJOTJS_BUILD)
	mkdir -p $(DJOTJS_BUILD)
	cp -R $(DJOTJS)/src $(DJOTJS_BUILD)
	$(NPM) install --prefix $(DJOTJS_BUILD) --no-save --no-package-lock --ignore-scripts typescript@$(TYPESCRIPT)
	cd $(DJOTJS_BUILD) && node_modules/.bin/tsc --target es2016 --lib es2020 --module commonjs \
	  --esModuleInterop --strict --isolatedModules --outDir lib src/index.ts

test-data: $(DJOTJS_BUILD)/lib/index.js
	$(NODE) tests/djot-ast.mjs $< $$(git -C $(DJOTJS) rev-parse HEAD) $(DJOTJS)/bench/readme.dj tests/data/readme

$(DATA)/%.ast $(DATA)/%.html: $(DATA)/%.dj tests/djot-ast.mjs $(DJOTJS_BUILD)/lib/index.js
	$(NODE) tests/djot-ast.mjs $(DJOTJS_BUILD)/lib/index.js $$(git -C $(DJOTJS) rev-parse HEAD) $< $(DATA)/$*

corpus: build $(CORPUS:%=$(DATA)/%.ast)
	$(CAP) timeout $(TEST_TIMEOUT) $(RUN) ./$(OBJ)/tests/corpus-test.so $(CORPUS:%=$(DATA)/%)
	$(CAP) timeout $(TEST_TIMEOUT) $(RUN) ./$(OBJ)/tests/positions-test.so $(CORPUS:%=$(DATA)/%.dj)

bench: build
	$(CAP) timeout $(BENCH_TIMEOUT) $(RUN) ./$(OBJ)/bench/bench.so $(BENCH_DATA)

clean:
	rm -rf build

SRCROOT = $(shell pwd)

PREFIX  = /usr/local

FIND    = find
INSTALL = install

HC      = ghc
HCFLAGS =

MAIN_IYQL = dist/bin/iyql
MAIN_SRC  = $(shell $(FIND) src/main/haskell/Yql -name \*.hs | grep -v '^\.')
TEST_IYQL = dist/bin/test_iyql
TEST_SRC  = $(shell $(FIND) src/test/haskell/Test/Yql -name \*.hs | grep -v '^\.')

.PHONY: default
default: compile

.PHONY: compile
compile: $(MAIN_IYQL)

.PHONY: install
install: compile
	$(INSTALL) -m 0755 $(MAIN_IYQL) $(PREFIX)/bin

.PHONY: test
test: $(TEST_IYQL)
	$(TEST_IYQL)

.PHONY: clean
clean:
	$(FIND) src/main/haskell -name \*.o -exec rm -f {} \;
	$(FIND) src/main/haskell -name \*.hi -exec rm -f {} \;
	$(FIND) src/test/haskell -name \*.o -exec rm -f {} \;
	$(FIND) src/test/haskell -name \*.hi -exec rm -f {} \;
	rm -f -r dist

dist:
	mkdir $(@)

dist/bin: dist
	mkdir $(@)

$(MAIN_IYQL): src/main/haskell/iyql.hs $(MAIN_SRC) dist/bin
	$(HC) -o $(@) -isrc/main/haskell --make $(HCFLAGS) $(<)

$(TEST_IYQL): src/test/haskell/test_iyql.hs $(MAIN_SRC) $(TEST_SRC) dist/bin
	$(HC) -o $(@) -isrc/test/haskell -isrc/main/haskell --make $(HCFLAGS) $(<)


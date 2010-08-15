SRCROOT = $(shell pwd)

PREFIX  = /usr/local

TEST    = test
RM      = rm -f
FIND    = find
INSTALL = install

HC      = ghc
HCFLAGS =

MAIN_IYQL = dist/bin/iyql
MAIN_SRC  = $(FIND) src/main/haskell/Yql -name \*.hs
TEST_IYQL = dist/bin/test_iyql
TEST_SRC  = $(FIND) src/test/haskell/Test/Yql -name \*.hs

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
	$(FIND) src/main/haskell -name \*.o -exec $(RM) {} \;
	$(FIND) src/main/haskell -name \*.hi -exec $(RM) {} \;
	$(FIND) src/test/haskell -name \*.o -exec $(RM) {} \;
	$(FIND) src/test/haskell -name \*.hi -exec $(RM) {} \;
	$(RM) -r dist

dist:
	mkdir $(@)

dist/bin: dist
	mkdir $(@)

$(MAIN_IYQL): src/main/haskell/iyql.hs $(MAINSRC) dist/bin
	$(HC) -o $(@) -isrc/main/haskell --make $(HCFLAGS) $(<)

$(TEST_IYQL): src/test/haskell/test_iyql.hs $(MAINSRC) $(TESTSRC) dist/bin
	$(HC) -o $(@) -isrc/test/haskell -isrc/main/haskell --make $(HCFLAGS) $(<)


SRCROOT = $(shell pwd)

PREFIX  = /usr/local

CABAL   = cabal
FIND    = find
INSTALL = install
HC      = ghc
HPC     = hpc

HCFLAGS =

MAIN_IYQL = dist/bin/iyql
MAIN_SRC  = $(foreach d,$(shell $(FIND) src/main/haskell/Yql -type d),$(wildcard $(d)/*.hs))
TEST_IYQL = dist/bin/test_iyql
TEST_SRC  = $(foreach d,$(shell $(FIND) src/test/haskell/Test/Yql -type d),$(wildcard $(d)/*.hs))

.PHONY: default
default: compile

.PHONY: dist
dist:
	$(CABAL) configure && $(CABAL) sdist

.PHONY: default
default: compile

.PHONY: compile
compile: $(MAIN_IYQL)

.PHONY: compile-hpc
compile-hpc: HCFLAGS += -fhpc
compile-hpc: $(MAIN_IYQL)

.PHONY: install
install: compile
	$(INSTALL) -m 0755 $(MAIN_IYQL) $(PREFIX)/bin

.PHONY: test
test: $(TEST_IYQL)
	$(TEST_IYQL)

.PHONY: test-hpc
test-hpc: compile-hpc $(TEST_IYQL)
	-@$(TEST_IYQL) >/dev/null
	$(HPC) markup --destdir=dist/hpc test_iyql.tix
	$(HPC) report test_iyql.tix

.PHONY: clean
clean:
	$(CABAL) clean
	$(FIND) src/main/haskell -name \*.o -exec rm -f {} \;
	$(FIND) src/main/haskell -name \*.hi -exec rm -f {} \;
	$(FIND) src/test/haskell -name \*.o -exec rm -f {} \;
	$(FIND) src/test/haskell -name \*.hi -exec rm -f {} \;
	rm -f -r dist
	rm -f -r *.tix
	rm -f -r .hpc

$(MAIN_IYQL): src/main/haskell/iyql.hs $(MAIN_SRC)
	@[ -d dist ] || mkdir dist
	@[ -d dist/bin ] || mkdir dist/bin
	$(HC) -o $(@) -isrc/main/haskell --make $(HCFLAGS) $(<)

$(TEST_IYQL): src/test/haskell/test_iyql.hs $(MAIN_SRC) $(TEST_SRC)
	@[ -d dist ] || mkdir dist
	@[ -d dist/bin ] || mkdir dist/bin
	$(HC) -o $(@) -isrc/test/haskell -isrc/main/haskell --make $(HCFLAGS) $(<)


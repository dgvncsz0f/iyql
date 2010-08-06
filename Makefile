SRCROOT=$(shell pwd)

MAINSRC=$(wildcard src/main/haskell/Yql/Core/*.hs \
                   src/main/haskell/Yql/Core/Functions/*.hs \
                   src/main/haskell/Yql/*.hs \
                   src/main/haskell/Yql/UI/*.hs)
MAINOBJ=$(addsuffix .o,$(basename $(MAINSRC)))
MAINPRG_SRC=src/main/haskell/iyql.hs
MAINPRG_OBJ=$(wildcard .o,$(basename $(MAINPRG_SRC)))
MAINPRG=$(basename $(MAINPRG_SRC))

TESTSRC=$(wildcard src/test/haskell/Test/*.hs \
                   src/test/haskell/Test/Yql/Core/*.hs \
                   src/test/haskell/Test/Yql/Core/Functions/*.hs \
                   src/test/haskell/Test/Yql/*.hs src/test/haskell/Test/Yql/UI/*.hs)
TESTOBJ=$(addsuffix .o,$(basename $(TESTSRC)))
TESTPRG_SRC=src/test/haskell/all_tests.hs
TESTPRG_OBJ=$(addsuffix .o,$(basename $(TESTPRG_SRC)))
TESTPRG=$(basename $(TESTPRG_SRC))

HC      = /usr/bin/ghc
INSTALL = /usr/bin/install

HCFLAGS =
PREFIX  = /usr/local

PREFIX=/usr/local


.PHONY: default
default: compile

.PHONY: compile
compile: $(MAINOBJ) $(MAINPRG)

.PHONY: install
install: $(MAINPRG)
	$(INSTALL) -m 0755 $(MAINPRG) $(PREFIX)/bin

.PHONY: test
test: compile $(TESTPRG)
	$(TESTPRG)

.PHONY: clean
clean:
	$(RM) $(shell find src/main/haskell -name "*.o")
	$(RM) $(shell find src/main/haskell -name "*.hi")
	$(RM) $(shell find src/test/haskell -name "*.o")
	$(RM) $(shell find src/test/haskell -name "*.hi")
	$(RM) $(TESTPRG) $(MAINPRG)

$(TESTPRG): $(TESTPRG_SRC) $(MAINSRC) $(TESTSRC)
$(MAINPRG): $(MAINPRG_SRC) $(MAINSRC)

.SUFFIXES: .o .hs
.hs.o:
	$(HC) -isrc/test/haskell -isrc/main/haskell --make $(HCFLAGS) $(<)

%: %.hs
	$(HC) -isrc/test/haskell -isrc/main/haskell --make $(HCFLAGS) $(<)

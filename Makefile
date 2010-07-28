SRCROOT=$(shell pwd)

MAINSRC=$(wildcard src/main/haskell/Yql/Core/*.hs src/main/haskell/Yql/*.hs)
MAINOBJ=$(addsuffix .o,$(basename $(MAINSRC)))
TESTSRC=$(wildcard src/test/haskell/Test/Yql/Core/*.hs src/test/haskell/Test/Yql/*.hs)
TESTOBJ=$(addsuffix .o,$(basename $(TESTSRC)))

TESTPRG_SRC=src/test/haskell/Main.hs
TESTPRG_OBJ=$(addsuffix .o,$(basename $(TESTPRG_SRC)))

TESTPRG=$(basename $(TESTPRG_SRC))

HC=/usr/bin/ghc
HCFLAGS=

.PHONY: default
default: compile

.PHONY: compile
compile: $(MAINOBJ)

.PHONY: test
test: compile $(TESTPRG)
	$(TESTPRG)

.PHONY: clean
clean:
	$(RM) $(addsuffix .o,$(basename $(MAINSRC)))
	$(RM) $(addsuffix .hi,$(basename $(MAINSRC)))
	$(RM) $(addsuffix .o,$(basename $(TESTSRC)))
	$(RM) $(addsuffix .hi,$(basename $(TESTSRC)))
	$(RM) $(addsuffix .o,$(basename $(TESTPRG_SRC)))
	$(RM) $(addsuffix .hi,$(basename $(TESTPRG_SRC)))

$(TESTOBJ): $(TESTSRC) $(MAINOBJ)
$(TESTPRG): $(TESTOBJ)
	$(HC) -isrc/test/haskell -isrc/main/haskell --make $(HCFLAGS) $(TESTPRG_SRC)

.SUFFIXES: .o .hs
.hs.o:
	$(HC) -isrc/test/haskell -isrc/main/haskell --make $(HCFLAGS) $(<)

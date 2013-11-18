MODULES  = PureSyntax PureInterpreter Main
TESTS    = TestPureCalculus

OBJECTS  = $(MODULES:=.o)  $(TESTS:=.o)
HIFILES  = $(MODULES:=.hi) $(TESTS:=.hi)
BINARIES = $(MODULES)      $(TESTS)

all: Main $(TESTS)

clean:
	rm -f $(OBJECTS) $(HIFILES) $(BINARIES)

Main: Main.hs
	ghc --make -o Main Main.hs

TestPureCalculus: TestPureCalculus.hs
	ghc --make -o TestPureCalculus TestPureCalculus.hs
	@./TestPureCalculus

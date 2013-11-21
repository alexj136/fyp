MODULES  = TypedSyntax TypeChecker TypedInterpreter Main
TESTS    = TestTypeChecker TestTypedInterpreter

OBJECTS  = $(MODULES:=.o)  $(TESTS:=.o)
HIFILES  = $(MODULES:=.hi) $(TESTS:=.hi)
BINARIES = $(MODULES)      $(TESTS)

all: Main $(TESTS)

clean:
	rm -f $(OBJECTS) $(HIFILES) $(BINARIES)

Main: Main.hs
	ghc --make -o Main Main.hs

TestTypeChecker: TestTypeChecker.hs
	ghc --make -o TestTypeChecker TestTypeChecker.hs
	@./TestTypeChecker

TestTypedInterpreter: TestTypedInterpreter.hs
	ghc --make -o TestTypedInterpreter TestTypedInterpreter.hs
	@./TestTypedInterpreter

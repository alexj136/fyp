MODULES  = TypedSyntax TypeChecker CoreSyntax CoreInterpreter Main
TESTS    = test/TestCoreInterpreter test/TestTypeChecker

OBJECTS  = $(MODULES:=.o)  $(TESTS:=.o)
HIFILES  = $(MODULES:=.hi) $(TESTS:=.hi)
BINARIES = $(MODULES)      $(TESTS)

all: Main $(TESTS)
	@echo BUILD SUCCESSFUL

clean:
	rm -f $(OBJECTS) $(HIFILES) $(BINARIES)

Main: Main.hs
	ghc --make -o Main Main.hs

test/TestCoreInterpreter: test/TestCoreInterpreter.hs
	ghc --make -o test/TestCoreInterpreter test/TestCoreInterpreter.hs
	@test/TestCoreInterpreter

test/TestTypeChecker: test/TestTypeChecker.hs
	ghc --make -o test/TestTypeChecker test/TestTypeChecker.hs
	@test/TestTypeChecker

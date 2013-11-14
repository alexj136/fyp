MODULES  = TypedSyntax CoreSyntax Interpreter Main
TESTS    = test/TestInterpreter

OBJECTS  = $(MODULES:=.o)  $(TESTS:=.o)
HIFILES  = $(MODULES:=.hi) $(TESTS:=.hi)
BINARIES = $(MODULES)      $(TESTS)

all: Main $(TESTS)
	@echo BUILD SUCCESSFUL

clean:
	rm -f $(OBJECTS) $(HIFILES) $(BINARIES)

Main: Main.hs
	ghc --make -o Main Main.hs

test/TestInterpreter: test/TestInterpreter.hs
	ghc --make -o test/TestInterpreter test/TestInterpreter.hs
	@test/TestInterpreter

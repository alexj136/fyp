MODULES  = AbstractSyntax Interpreter Main
TESTS    = test/TestInterpreter test/TestAbstractSyntax

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

test/TestAbstractSyntax: test/TestAbstractSyntax.hs
	ghc --make -o test/TestAbstractSyntax test/TestAbstractSyntax.hs
	@test/TestAbstractSyntax

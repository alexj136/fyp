COMPILE = ghc
SOURCES = AbstractSyntax.hs Interpreter.hs
TESTS = test/TestInterpreter.hs
OBJECTS = $(SOURCES:.hs=.o)
OUTPUTS = $(SOURCES:.hs=.hi) $(OBJECTS)

all: $(OBJECTS)
	@echo BUILD SUCCESSFUL

clean:
	rm -f $(OUTPUTS)

Main: $(OBJECTS) Main.hs
	ghc -o Main Interpreter.o AbstractSyntax.o

Interpreter.o: Interpreter.hs
	ghc -c Interpreter.hs

AbstractSyntax.o: AbstractSyntax.hs
	ghc -c AbstractSyntax.hs

test/TestInterpreter: test/TestInterpreter.hs Interpreter.o
	ghc -o test/TestInterpreter test/TestInterpreter.hs Interpreter.o

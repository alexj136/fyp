MAIN     = RunTests
MODULES  = Unifier Syntax Lexer Parser Interpreter
TESTS    = TestUnifier TestSyntax TestInterpreter

OBJECTS  = $(MAIN:=.o)  $(MODULES:=.o)  $(TESTS:=.o)
HIFILES  = $(MAIN:=.hi) $(MODULES:=.hi) $(TESTS:=.hi)
BINARIES = $(MAIN)      $(MODULES)      $(TESTS)

all: $(MAIN)

clean:
	@rm -f $(OBJECTS) $(HIFILES) $(BINARIES)
	@echo "CLEANED"

RunTests: RunTests.hs
	@ghc --make -o RunTests RunTests.hs
	@./RunTests

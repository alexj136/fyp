MAIN     = RunTests
MODULES  = TypeReconstructer PolymorphicSyntax
TESTS    = TestTypeReconstructer

OBJECTS  = $(MAIN:=.o)  $(MODULES:=.o)  $(TESTS:=.o)
HIFILES  = $(MAIN:=.hi) $(MODULES:=.hi) $(TESTS:=.hi)
BINARIES = $(MAIN)      $(MODULES)      $(TESTS)

all: $(MAIN)

clean:
	@rm -f $(OBJECTS) $(HIFILES) $(BINARIES)

RunTests: RunTests.hs
	@ghc --make -o RunTests RunTests.hs
	@./RunTests

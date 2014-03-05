# Joe Jevnik
# 2014.3.5
# A dumb makefile that just wraps the ghc --make and adds clean

all:
	ghc --make Compiler

clean:
	rm *.o *.hi Compiler/*.o Compiler/*.hi

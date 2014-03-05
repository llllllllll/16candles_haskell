# Joe Jevnik
# 2014.3.5
# A dumb makefile that just wraps the ghc --make and adds clean

all:
	ghc --make Compiler -main-is Compiler -o h16cc

clean:
	rm h16cc *.o *.hi Compiler/*.o Compiler/*.hi

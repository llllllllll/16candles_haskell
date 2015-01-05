all:
	ghc --make Assembler -o h16cc

clean:
	rm h16cc *.o *.hi Compiler/*.o Compiler/*.hi

h16cc
=====

An assembler for 16candles asm.

By Joe Jevnik


Building
--------

Simply run:

    $ make

to build the project. The make file is just a light wrapper over ghc because
it does not carry its own dependencies. Instead, it is equivelent to calling:

    $ ghc --make Assembler -main-is Assembler -o h16cc

where `ghc --make` will act like a makefile for us, but the first command is
faster to type than the second one. It also provides:

    $ make clean

which will just be equivelent to:

    $ rm h16cc *.o *.hi Assembler/*.o Assembler/*.hi

but again, it is faster to type make clean.

Usage
-----

    Usage:

        h16cc [OPTION] SOURCE

    Where SOURCE is the .16c source file you wish to assemble to the
    file:'a.out' The 16candles compiler accecpts the following additional
    arguments:

        -o OUTPUT-FILE   The file to name the output binary.
        -v --version     Print version information about the this assembler.
        -h --help        Prints this message.

When one runs `$ h16cc mySource.16c`, the source file `mySource.16c` will be
assembled into the binary, `a.out`.

If one wishes to change the name of the outputted binary file, one may use the
`-o` flag, just like `gcc`. This can be permuted, so the following two calls
are equivelent:

    $ h16cc -o myBin mySource.16c
    $ h16cc mySource.16c -o myBin

One may also use the `-h --help` or `-v --version` flags to get more information
about the usage or version of `h16cc` that you are running.

16candles
---------

16candles is a 16 bit 16 register virtual machine written in c.
This assemble will assemble the language defined in the 16candles project
into the bytecode that the machine accepts. For more information, see
[16candles](https://github.com/llllllllll/16c)

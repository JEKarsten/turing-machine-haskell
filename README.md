# Models of Computation: Turing Machines in Haskell

Author: James Karsten

Course: CS 131 (Programming Languages)

## How To Run
In order to run a machine in `examples/Examples.hs`, run the `tm.sh` file on the command line:
```
sh tm.sh [machine]
```
This will compile `tm.hs` and run it using the first argument, which should be the name of an existing machine. This prints the simulation to stdout.

In order to easily generate all trace and dot files, run the `test.sh` file on the command line:
```
sh test.sh
```
This will put all generated files into the `/testing` directory. The trace files can be compared with the pre-existing files in that directory, which are the original test cases.

## About
All Haskell files are located in the `/src` directory:
+ `TuringMachine.hs` contains all the code that constructs and simulates a Turing Machine
+ `CaesarCipher.hs` contains all the code that is used to construct and a Turing Machine that can encipher or decipher a string of text using the Caesar cipher
+ `TMToDot.hs` contains all the code that turns a Turing Machine into a .dot file
+ `Examples.hs` contains 12 example Turing Machine that are hard-coded in; the first 8 machines are based on the files in the `/examples` directory
+ `Testing.hs` contains the functions and filenames needed to generate the trace and dot files for the Turing Machines in `Examples.hs`
+ `tm.hs` allows the user to use a command line argument to print the simulation of a Turing Machine (from `Examples.hs`) to the terminal
+ `trace-file-generation.hs` imports the `writeTraceFiles` function from `Testing.hs` into `main` so that it can be compiled and run from the command line; this generates simple trace files
+ `trace-log-file-generation.hs` imports the `writeTraceLogFiles` function from `Testing.hs` into `main` so that it can be compiled and run from the command line; this generates trace files that also include statistics about the machine that was run
+ `dot-file-generation.hs` imports the `writeDotFiles` function from `Testing.hs` into `main` so that it can be compiled and run from the command line
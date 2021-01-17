# OccamToGoTranspiler
This is a bachelors project concerning building a transpiler from Occam to Go. The project consists of a parser and a code generator, which are run seperately. The implementation works for a subset of Occam, and the transpiler cannot yet be relyed on for translating all types of programs.

## Prerequisites
To build and run the project you need to have a working version of Haskell installed, as well as the Haskell package manager Cabal. You will also need the Haskell libraries *parsec*, *indents*, *random-strings*, *tasty* and *tasty-hunit*. 

## Building project
To build the project navigate to the `OccamToGoTranspiler/transpiler` folder and run the following code.
```
cabal update
cabal build
```

## Running code
The code can be run either using `cabal run` or through `ghci`. The main function of the project simply runs the test suite for the project. If wanting to use the transpiler functionality, navigate to the `OccamToGoTranspile/transpiler` folder and start `ghci`. The code is automatically loaded. Now to translate the example program `alt.occ` and save it in a file `alt.go` execute the following command:
```
> trans "../occam-programs/alt.occ" "alt"
```
This will produce a new file containing the Go code, which can be run using `go run alt.go` outside of `ghci`. Some redundent imports might me given in the file, and if the compiler complains these will have to be removed manually. 

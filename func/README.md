# The FunC Compiler


## Installation (macOS)
1. Install ocaml:
   `$ brew install ocaml`
2. Install opam:
   `$ brew install opam`
3. Set up opam environment:
   `$ opam init`
   `$ eval `opam env``
4. Install LLVM 7.0.1.
   `$ brew install llvm@7`
5. Install the ocaml llvm library.
   `$ opam install llvm.7.0.0`
6. Add llvm to path.
   `$ export PATH=$PATH:/usr/local/opt/llvm/bin`
7. Make sure you have permission to execute ./testall.sh
   `$ sudo chmod u+x ./testall.sh`


## Compillation
* Compile FunC:
  `$ ocamlbuild -use-ocamlfind func.native`

* Run tests:
  `$ ./testall.sh`

* Compile FunC and run tests:
  `$ make`

* Clean up the directory, remove build, output and log files:
  `$ make clean`

Hello world:
1. Compile FunC.
   `$ make`
2. Use FunC to compile the tests/helloworld.fc file.
   `$ ./func.native -c tests/test-helloworld.fc > helloworld.ll`
3. Run helloworld.ll with lli.
   `$ lli helloworld.ll`

## Files
built_ins.fc: built-in functions
utility.ml: utility functions
scanner.mll: token rules
parser.mly: context-free grammar
ast.ml: abstract-syntax tree
sast.ml: semantically checked abstract-syntax tree
semant.ml: semantic checking
codegen.ml: LLVM IR generation
func.ml: top-level
func.native: a symbolic link to the compiler in the build folder
_tags: enabling recursive traversal of subdirectories
Makefile: build automation
testall.sh: running all tests

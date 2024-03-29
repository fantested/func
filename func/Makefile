# "make test" Compiles everything and runs the regression tests

.PHONY : test
test : all testall.sh
	./testall.sh

# "make all" builds the executable as well as the "printbig" library designed
# to test linking external code

.PHONY : all
all : func.native printbig.o

# "make func.native" compiles the compiler
#
# The _tags file controls the operation of ocamlbuild, e.g., by including
# packages, enabling warnings
#
# See https://github.com/ocaml/ocamlbuild/blob/master/manual/manual.adoc

func.native :
	opam config exec -- \
	ocamlbuild -use-ocamlfind func.native

# "make clean" removes all generated files

.PHONY : clean
clean :
	ocamlbuild -clean
	rm -rf testall.log ocamlllvm *.diff

# Testing the "printbig" example

printbig : printbig.c
	cc -o printbig -DBUILD_TEST printbig.c

# Building the tarball

TESTS = \
  add1 anon1 arith1 arith2 arith3 array1 array2 array3 array4 array5 array6 \
  fib float1 float2 float3 for1 for2 fstcls1 func1 func2 func3 func4 func5 \
  func6 func7 func8 func9 gcd2 gcd global1 global2 global3 hello helloworld \
  higherorder1 higherorder2 if1 if2 if3 if4 if5 if6 local1 local2 map1 mod1 \
  ops1 ops2 pipe1 pipe2 pipe3 pipe4 pipe5 printbig reduce1 var1 var2 while1 \
  while2

FAILS = \
  anon1 array1 assign1 assign2 assign3 dead1 dead2 expr1 expr2 expr3 \
  float1 float2 for1 for2 for3 for4 for5 fstcls1 fstcls2 fstcls3 func1 func2 \
  func3 func4 func5 func6 func7 func8 func9 global1 global2 if1 if2 if3 map1 \
  map2 map3 mod1 nomain pipe1 pipe2 printbig printb print reduce1 reduce2 \
  reduce3 return1 return2 while1 while2

TESTFILES = $(TESTS:%=test-%.fc) $(TESTS:%=test-%.out) \
	    $(FAILS:%=fail-%.fc) $(FAILS:%=fail-%.err)

TARFILES = ast.ml sast.ml codegen.ml Makefile _tags func.ml parser.mly \
	README scanner.mll semant.ml testall.sh \
	printbig.c \
	Dockerfile \
	$(TESTFILES:%=tests/%) 

func.tar.gz : $(TARFILES)
	cd .. && tar czf func/func.tar.gz \
		$(TARFILES:%=func/%)

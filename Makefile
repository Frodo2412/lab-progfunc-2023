Compiler : Compiler.hs Syntax.hs Checker.hs Generator.hs
	ghc --make Compiler

clean :
	rm -f *.hi *.o Compiler

# tests : FORCE
#	mkdir -p localtests
#	rm -rf ./localtests/*
#	cp -r tests/*.fun localtests
#	for i in 1 2 3 4 5 6 7 8 9; \
         do runhaskell Compiler.hs localtests/ejemplo$$i; \
            runhaskell Compiler.hs -o localtests/ejemplo$$i; done
#	for i in 1 2 3 4; \
 #        do runhaskell Compiler.hs localtests/ejemplo$${i}err > \
  #          localtests/ejemplo$${i}err.err ; done


#We want the above tests: command to do waht it does and compare the output wiht the files with same name in ./tests
AMOUNT_OF_NORMAL_TESTS = 1
AMOUNT_OF_ERROR_TESTS = 1
#At each step make it clear what you are doing
tests: FORCE
	mkdir -p localtests
	rm -rf ./localtests/*
	cp -r tests/*.fun localtests
	for i in `seq 1 $(AMOUNT_OF_NORMAL_TESTS)`; \
		 do runhaskell Compiler.hs localtests/ejemplo$$i; \
			runhaskell Compiler.hs -o localtests/ejemplo$$i; done
	for i in `seq 1 $(AMOUNT_OF_ERROR_TESTS)`; \
		 do runhaskell Compiler.hs localtests/ejemplo$${i}err > \
			localtests/ejemplo$${i}err.err ; done
	for i in `seq 1 $(AMOUNT_OF_NORMAL_TESTS)`; \
		 do diff localtests/ejemplo$$i.c tests/ejemplo$$i.c; done
	for i in `seq 1 $(AMOUNT_OF_ERROR_TESTS)`; \
		 do diff localtests/ejemplo$${i}err.err tests/ejemplo$${i}err.err; done

FORCE: ;
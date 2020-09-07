FLAGS = -Wall -O3
LINKOPTS = -lssl -L. -L..

all: clean lemon libparsers.so parse-benchmark parse-term-test parse-decl-test \
     load-benchmark generate-mac xattr-benchmark conditions-benchmark

############################

lemon: lemon.c
	gcc $(FLAGS) lemon.c -o lemon

pcfs-gram.h: pcfs-gram.y
	./lemon pcfs-gram.y  || echo

pcfs-gram.o: pcfs-gram.h 
	g++ $(FLAGS) -c pcfs-gram.c

pcfs-lex.hpp: pcfs-gram.h pcfs-lex.lex
	flex -s -F pcfs-lex.lex

pcfs-lex.o: pcfs-lex.hpp
	g++ $(FLAGS) -c pcfs-lex.cpp 

pcfs-declarations.o: pcfs-declarations.hpp pcfs-declarations.cpp
	g++ $(FLAGS) -c pcfs-declarations.cpp

pcfs-file-parsers.o: pcfs-file-parsers.cpp pcfs-lex.hpp pcfs-parse-ds.hpp pcfs-gram.h
	g++ $(FLAGS) -c pcfs-file-parsers.cpp

loadProCap.o: loadProCap.cpp loadProCap.hpp
	g++ $(FLAGS) -c loadProCap.cpp

loadDeclarations.o: loadDeclarations.cpp loadDeclarations.hpp
	g++ $(FLAGS) -c loadDeclarations.cpp

checkProCapConditions.o: checkProCapConditions.cpp checkProCapConditions.hpp
	g++ $(FLAGS) -c checkProCapConditions.cpp

xattr-terms.o: xattr-terms.cpp xattr-terms.hpp
	g++ $(FLAGS) -c xattr-terms.cpp

libparsers.so: loadProCap.o loadDeclarations.o pcfs-lex.o pcfs-gram.o \
               pcfs-file-parsers.o pcfs-declarations.o checkProCapConditions.o \
               xattr-terms.o
	ar rvs libparsers.so loadProCap.o loadDeclarations.o pcfs-lex.o \
	       pcfs-gram.o pcfs-file-parsers.o pcfs-declarations.o checkProCapConditions.o \
               xattr-terms.o

parse-benchmark.o: parse-benchmark.cpp
	g++ $(FLAGS) -c parse-benchmark.cpp

parse-benchmark: parse-benchmark.o libparsers.so
	g++ $(FLAGS) $(LINKOPTS) -o parse-benchmark parse-benchmark.o -lparsers

load-benchmark.o: load-benchmark.cpp
	g++ $(FLAGS) -c load-benchmark.cpp

load-benchmark: load-benchmark.o libparsers.so
	g++ $(FLAGS) $(LINKOPTS) -o load-benchmark load-benchmark.o -lparsers

parse-term-test.o: parse-term-test.cpp
	g++ $(FLAGS) -c parse-term-test.cpp

parse-term-test: parse-term-test.o libparsers.so
	g++ $(FLAGS) $(LINKOPTS) -o parse-term-test parse-term-test.o -lparsers

parse-decl-test.o: parse-decl-test.cpp
	g++ $(FLAGS) -c parse-decl-test.cpp

parse-decl-test: parse-decl-test.o libparsers.so
	g++ $(FLAGS) $(LINKOPTS) -o parse-decl-test parse-decl-test.o -lparsers

generate-mac.o: ../crypto-common.hpp pcfs-file-parsers.hpp generate-mac.cpp
	g++ $(FLAGS) -c generate-mac.cpp

generate-mac: generate-mac.o libparsers.so
	g++ $(FLAGS) $(LINKOPTS) -o generate-mac generate-mac.o -lparsers

xattr-benchmark.o: xattr-benchmark.cpp
	g++ $(FLAGS) -c xattr-benchmark.cpp

xattr-benchmark: xattr-benchmark.o libparsers.so
	g++ $(FLAGS) $(LINKOPTS) -o xattr-benchmark xattr-benchmark.o -lparsers -lcommon

conditions-benchmark.o: conditions-benchmark.cpp
	g++ $(FLAGS) -c conditions-benchmark.cpp

conditions-benchmark: conditions-benchmark.o libparsers.so
	g++ $(FLAGS) $(LINKOPTS) -o conditions-benchmark conditions-benchmark.o -lparsers -lcommon
############################

clean:
	rm -f *.o
	rm -f pcfs-gram.c
	rm -f pcfs-gram.h
	rm -f pcfs-gram.out
	rm -f pcfs-lex.cpp
	rm -f pcfs-lex.hpp
	rm -f libparsers.so
	rm -f parse-benchmark
	rm -f parse-term-test
	rm -f parse-decl-test
	rm -f load-benchmark
	rm -f generate-mac
	rm -f xattr-benchmark
	rm -f conditions-benchmark

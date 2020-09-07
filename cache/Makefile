FLAGS = -Wall -O3
LINKOPTS = -L.. -L.

all: clean libdiskcache.so main.out cache-benchmark stat-benchmark

######################################

DiskCache.o: DiskCache.cpp  DiskCache.hpp diskcache-common.hpp BiQueue.hpp
	g++ -c $(FLAGS) DiskCache.cpp

trivial-parser.o: trivial-parser.cpp trivial-parser.hpp diskcache-common.hpp DiskCache.hpp
	          g++ -c $(FLAGS) trivial-parser.cpp

libdiskcache.so: trivial-parser.o DiskCache.o
	ar rvs libdiskcache.so DiskCache.o trivial-parser.o

######################################

main.o: main.cpp DiskCacheTrivial.hpp DiskCache.hpp
	g++ -c $(FLAGS) main.cpp

main.out: main.o trivial-parser.o DiskCache.o
	g++ $(FLAGS) $(LINKOPTS) main.o  -o main.out -lcommon -ldiskcache

cache-benchmark.o: cache-benchmark.cpp
	g++ -c $(FLAGS) cache-benchmark.cpp

cache-benchmark: cache-benchmark.o
	g++ $(FLAGS)  $(LINKOPTS) cache-benchmark.o  -o cache-benchmark -lcommon -ldiskcache

stat-benchmark.o: stat-benchmark.cpp
	g++ -c $(FLAGS) stat-benchmark.cpp

stat-benchmark: stat-benchmark.o
	g++ $(FLAGS)  $(LINKOPTS) stat-benchmark.o -o stat-benchmark -lcommon -ldiskcache

######################################

clean: 
	rm -f *.o
	rm -f libdiskcache.so
	rm -f main.out
	rm -f cache-benchmark
	rm -f stat-benchmark


BASEFLAGS = -Wall -D_FILE_OFFSET_BITS=64 -DFUSE_USE_VERSION=26 -O3

EXTRAFLAGS = -DNO_BACKGROUND

# description of extra flags

# (R) means a recommended flag
# (NR) means a not recommended flag
# (PD) means a flag that will depend on the use case scenario

# -DNO_BACKGROUND             ## (NR) add this flag to run fuse in the foreground instead of background

# -DNO_LOG                    ## (NR) add this to flags to prevent logging

FLAGS = $(BASEFLAGS) $(EXTRAFLAGS)

LINKOPTS = -L. -Lparse-check -Lcache

libs: clean libcommon.so libruntime.so

all: clean libcommon.so libruntime.so pcfs-main pcfs-make-pcs

############################

paths-common.o: paths-common.cpp paths-common.hpp
	g++ -c $(FLAGS) paths-common.cpp

fsutils.o: fsutils.cpp fsutils.hpp
	g++ -c $(FLAGS) fsutils.cpp

libcommon.so: paths-common.o fsutils.o
	ar rvs libcommon.so paths-common.o fsutils.o

pcfs-runtime.o: pcfs-runtime.cpp pcfs-runtime.hpp
	g++ -c $(FLAGS) pcfs-runtime.cpp

checkPerm.o: checkPerm.cpp checkPerm.hpp
	g++ -c $(FLAGS) checkPerm.cpp

pcfs-handlers.o: pcfs-handlers.cpp pcfs-handlers.hpp
	g++ -c $(FLAGS) pcfs-handlers.cpp

pcfs-procaps.o: pcfs-procaps.cpp pcfs-procaps.hpp
	g++ -c $(FLAGS) pcfs-procaps.cpp

libruntime.so: pcfs-runtime.o checkPerm.o pcfs-handlers.o pcfs-procaps.o
	ar rvs libruntime.so pcfs-runtime.o checkPerm.o pcfs-handlers.o pcfs-procaps.o

pcfs-main.o: pcfs-main.cpp
	g++ -c $(FLAGS) pcfs-main.cpp

pcfs-main: pcfs-main.o libruntime.so libcommon.so
	g++ -o pcfs-main $(FLAGS) $(LINKOPTS) pcfs-main.o -lruntime -ldiskcache -lparsers -lcommon -lfuse -lrt -lssl 

############################

clean:
	rm -f *.o
	rm -f libcommon.so
	rm -f libruntime.so


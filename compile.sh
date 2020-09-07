#!/bin/bash

make clean;
make libcommon.so;

cd parse-check;
make clean;
make libparsers.so;
cd ..;

cd cache;
make clean;
make libdiskcache.so;
cd ..;

make libruntime.so;
make pcfs-main;

cd prove-verify;
make;
cd ..;


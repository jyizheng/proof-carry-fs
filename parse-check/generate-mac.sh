#!/bin/bash

# Produce a mac on a file. Takes three arguments: $1 -- name of file
# to produce MAC on. $2 -- name of a file that contains the key to
# sign. Exactly the first 40 characters of this file are read, and
# treated as hex digits that code a 160 bit symmetric key. Output is
# written to file $3.

cat "$2" | ./generate-mac "$1" - | cat "$1" - > "$3"



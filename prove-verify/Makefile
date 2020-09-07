MLTON_OPTS = -verbose 1 -codegen native

all: clean pcfs-search pcfs-verify pcfs-cert pcfs-qprocap

xattr-c-wrap.h: xattr-c-wrap.sml
	mlton $(MLTON_OPTS) -default-ann 'allowFFI true' -export-header xattr-c-wrap.h -stop tc xattr-c-wrap.sml

xattr-c-wrap.o: xattr-c-wrap.c xattr-c-wrap.h
	gcc -c -O xattr-c-wrap.c

crypto-c-wrap.h: crypto-c-wrap.sml
	mlton $(MLTON_OPTS) -default-ann 'allowFFI true' -export-header crypto-c-wrap.h -stop tc crypto-c-wrap.sml

crypto-c-wrap.o: crypto-c-wrap.cpp crypto-c-wrap.h ../crypto-common.hpp
	g++ -c -O3 crypto-c-wrap.cpp

bl-lex.lex.sml: bl-lex.lex
	mllex bl-lex.lex

bl-grm.grm.sig: bl-grm.grm
	mlyacc bl-grm.grm

bl-grm.grm.sml: bl-grm.grm
	mlyacc bl-grm.grm

xml-lex.lex.sml: xml-lex.lex
	mllex xml-lex.lex

xml-grm.grm.sml: xml-grm.grm
	mlyacc xml-grm.grm

xml-grm.grm.sig: xml-grm.grm
	mlyacc xml-grm.grm

pcfs-search: bl-lex.lex.sml bl-grm.grm.sig bl-grm.grm.sml xml-lex.lex.sml xml-grm.grm.sml \
	     xml-grm.grm.sig xattr-c-wrap.o crypto-c-wrap.o pcfs-search.sml
	mlton -default-ann 'allowFFI true' $(MLTON_OPTS) -link-opt '-lstdc++ -lcrypto -lssl' \
	      -output pcfs-search pcfs-search.mlb xattr-c-wrap.o crypto-c-wrap.o

pcfs-verify: bl-lex.lex.sml bl-grm.grm.sig bl-grm.grm.sml xml-lex.lex.sml xml-grm.grm.sml \
	     xml-grm.grm.sig xattr-c-wrap.o crypto-c-wrap.o pcfs-verify.sml
	mlton -default-ann 'allowFFI true' $(MLTON_OPTS) -link-opt '-lstdc++ -lcrypto -lssl' \
	      -output pcfs-verify pcfs-verify.mlb xattr-c-wrap.o crypto-c-wrap.o

pcfs-cert: bl-lex.lex.sml bl-grm.grm.sig bl-grm.grm.sml xml-lex.lex.sml xml-grm.grm.sml \
	     xml-grm.grm.sig xattr-c-wrap.o crypto-c-wrap.o pcfs-cert.sml
	mlton -default-ann 'allowFFI true' $(MLTON_OPTS) -link-opt '-lstdc++ -lcrypto -lssl' \
	      -output pcfs-cert pcfs-cert.mlb xattr-c-wrap.o crypto-c-wrap.o

pcfs-qprocap: bl-lex.lex.sml bl-grm.grm.sig bl-grm.grm.sml xattr-c-wrap.o crypto-c-wrap.o \
	      pcfs-qprocap.sml
	mlton -default-ann 'allowFFI true' $(MLTON_OPTS) -link-opt '-lstdc++ -lcrypto -lssl' \
	      -output pcfs-qprocap pcfs-qprocap.mlb xattr-c-wrap.o crypto-c-wrap.o

genmac: genmac.sml crypto-c-wrap.o crypto-c-wrap.sml
	mlton -default-ann 'allowFFI true' $(MLTON_OPTS) -link-opt '-lstdc++ -lcrypto -lssl' \
	      -output genmac genmac.mlb crypto-c-wrap.o

clean:
	rm -f *.lex.sml
	rm -f *.grm.sig
	rm -f *.grm.sml
	rm -f *.grm.desc
	rm -f xattr-c-wrap.o
	rm -f xattr-c-wrap.h
	rm -f crypto-c-wrap.o
	rm -f crypto-c-wrap.h
	rm -f bl-grm.grm.desc
	rm -f xml-grm.grm.desc
	rm -f mlmon.out
	rm -f cinterface.o


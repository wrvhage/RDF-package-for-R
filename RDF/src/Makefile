#R = R
R = R64
CC = gcc
OPTS = -c -fPIC
LOADER = gcc

#C_INCLUDE_PATH = /Developer/SDKs/MacOSX10.5.sdk/usr/include
C_INCLUDE_PATH = /Developer/SDKs/MacOSX10.7.sdk/usr/include
RAPTOR_INCLUDE_PATH = /usr/local/include/raptor2
#R_INCLUDE_PATH = /Library/Frameworks/R.framework/Versions/2.12/Resources/include
R_INCLUDE_PATH = /Library/Frameworks/R.framework/Headers/

RDF: RDF.so
	$(R) -f ../R/RDF.R

RDF.so: RDF.o
	$(R) CMD SHLIB -o RDF.so -lraptor2 RDF.o

.c.o: ; $(CC) $(OPTS) -I $(R_INCLUDE_PATH) -I $(C_INCLUDE_PATH) -I $(RAPTOR_INCLUDE_PATH) -c $<

clean:
	rm *.o *.so rtest.* filetest.* httptest.*
pretty:
	indent -nbad -bap -nbc -br -brs -c33 -cd33 -ncdb -ce -ci4 -cli0 -d0 -di1 -nfc1 -i4 -ip0 -l80 -lp -npcs -npsl -sob RDF.c


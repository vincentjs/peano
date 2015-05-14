FC = /usr/bin/gfortran
DIR = ./debug
CFLAGS = -shared -O2
#WFLAGS = -Wall
WFLAGS = -Wall -Wextra -Wconversion -pedantic
DFLAGS = -g -fbacktrace -fbounds-check -ffpe-trap=zero,overflow,underflow
LDFLAGS = 
SOURCES = $(patsubst %.f90, %.o, $(wildcard *.f90))
#OBJECTS = $(patsubst %.f90, %.o, ${SOURCES})
LIBRARY = peano.so

all: $(LIBRARY)

$(LIBRARY): $(SOURCES)
	@$(FC) $(CFLAGS) $(WFLAGS) $(DFLAGS) $(SOURCES) -o $(LIBRARY) -fPIC
	@mv *.mod *.o *.so ./debug
	@cp ./debug/*.mod ./debug/*.so ./debug/*.o ../tests/lib

%.o: %.f90
	@$(FC) $(CFLAGS) $(WFLAGS) $(DFLAGS) -o $@ $< -fPIC

clean:
	@rm -f ./debug/*.mod ./debug/*.o

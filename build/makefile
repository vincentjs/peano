FC = /usr/bin/gfortran
DIR = ./debug
CFLAGS = -shared -O2
WFLAGS = -Wall
#WFLAGS = -Wall -Wextra -Wconversion -pedantic
DFLAGS = -g -fbacktrace -fbounds-check -ffpe-trap=zero,overflow,underflow
LDFLAGS = 
SOURCES = $(wildcard *.f90)
OBJECTS = $(patsubst %.f90, %.o, ${SOURCES})
LIBRARY = peano.so

all: $(LIBRARY)

$(LIBRARY):
	@$(FC) $(CFLAGS) $(WFLAGS) $(DFLAGS) $(SOURCES) -o $(LIBRARY) -fPIC
	@mv *.mod ./debug
clean:
	@rm -f *.mod

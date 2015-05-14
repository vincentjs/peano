FC = /usr/bin/gfortran
DIR = ./debug
LIB = ./lib
CFLAGS = -shared -O2
WFLAGS = -Wall
#WFLAGS = -Wall -Wextra -Wconversion -pedantic
DFLAGS = -g -fbacktrace -fbounds-check -ffpe-trap=zero,overflow,underflow
LDFLAGS = 
SOURCES = $(patsubst %.f90, %.o, $(wildcard *.f90))
EXECUTABLE = peano_tester.exe

all: $(EXECUTABLE)

$(EXECUTABLE): $(SOURCES)
	@$(FC) $(LDFLAGS) -o $@ $^

%.o: %.f90
	@$(FC) -L $(CFLAGS) $(WFLAGS) $(DFLAGS) -lpeano -o $@ $< 

clean:
	rm -f *.o *.mod

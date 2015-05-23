FC = /usr/bin/gfortran
CFLAGS = 
WFLAGS = -Wall -Wextra -Wconversion -pedantic
DFLAGS = -g -fbacktrace -fbounds-check -ffpe-trap=zero,overflow,underflow
LDLIBS = peano.a
SOURCES = $(wildcard *.f90)
OBJECTS = $(SOURCES:.f90=.o)
OUTPUT = peano_tester

all: $(OUTPUT)

$(OUTPUT): $(OBJECTS)
	@$(FC) -o $@ $^ $(LDLIBS)

%.o: %.f90
	@$(FC) $(CFLAGS) $(WFLAGS) $(DFLAGS) -c  $< 

clean:
	@rm -f *.o *.mod

install: $(OUTPUT) clean

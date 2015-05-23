FC := gfortran
OBJDIR := ../include/
BINDIR := ../bin/
CFLAGS := -c
WFLAGS := -Wall -Wextra -Wconversion -pedantic
DFLAGS := -g -fbacktrace -fbounds-check -ffpe-trap=zero,overflow,underflow
LDFLAGS := -I$(OBJDIR)
LDLIBS := $(BINDIR)peano.a
SOURCES := $(wildcard *.f90)
OBJECTS := $(SOURCES:.f90=.o)
OUTPUT := peano_tester

all: $(OUTPUT)

$(OUTPUT): $(OBJECTS)
	$(FC) -o $@ $^ ../include/statistics.o $(LDLIBS)

$(OBJECTS): %.o: %.f90
	$(FC) $(LDFLAGS) $(CFLAGS) $(WFLAGS) $(DFLAGS) $< 

clean:
	@rm -f *.o *.mod

install: $(OUTPUT) clean

FC = /usr/bin/gfortran
DIR = ./debug
LIB = ./lib
CFLAGS = -shared -O2
WFLAGS = -Wall
#WFLAGS = -Wall -Wextra -Wconversion -pedantic
DFLAGS = -g -fbacktrace -fbounds-check -ffpe-trap=zero,overflow,underflow
LDFLAGS = -L${LIB}/peano.so
SOURCES = $(patsubst %.f90, %.o, $(wildcard *.f90))
EXECUTABLE = peano_tester.exe

all: $(EXECUTABLE)

$(EXECUTABLE): $(SOURCES)
	@$(FC) -o $@ $^ ./lib/cov.o
	@mv *.o $(EXECUTABLE) ./debug

%.o: %.f90
	@$(FC) $(CFLAGS) $(WFLAGS) $(DFLAGS) -o $@ $< $(LDFLAGS) -fPIC

clean:
	@rm -f ./debug/*

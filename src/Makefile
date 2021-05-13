#!/bin/sh

FC = gfortran
LIBS = -llapack -lblas

fdpoisson: InputHandler.o OutputHandler.o OneDimSolver.o TwoDimSolver.o fdpoisson.o
	$(FC) InputHandler.o OutputHandler.o OneDimSolver.o TwoDimSolver.o fdpoisson.o -o fdpoisson $(LIBS)

InputHandler.o: InputHandler.F90
	$(FC) -c InputHandler.F90 -o InputHandler.o $(LIBS) 

OutputHandler.o: OutputHandler.F90
	$(FC) -c OutputHandler.F90 -o OutputHandler.o $(LIBS)

OneDimSolver.o: OneDimSolver.F90
	$(FC) -c OneDimSolver.F90 -o OneDimSolver.o $(LIBS)

TwoDimSolver.o: TwoDimSolver.F90
	$(FC) -c TwoDimSolver.F90 -o TwoDimSolver.o $(LIBS)

fdpoisson.o: fdpoisson.F90
	$(FC) -c fdpoisson.F90 -o fdpoisson.o $(LIBS)

clean:
	rm *.o *.mod fdpoisson

install:
	if [ ! -d ../bin ]; then mkdir ../bin; fi
	cp fdpoisson ../bin/;

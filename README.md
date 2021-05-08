# FDPoisson - Finite Difference code to solve Poisson Equation

## Features
Currently the code is limited to 1D and Dirichlet Boundary Conditions. It can obtain the potential and electric field for a polynomial charge density (the degree of the polynomial and the coefficients have to be specified in the input file). We are working on expanding this to 2D.

## Installation

To build this code, a fortran compiler is needed, in addition to LAPACK and BLAS libraries. To build this package, do the following
<ol>
  <li> Download the code <br>
<code>
git clone https://github.com/vishnu2709/FDPoisson.git
</code>
   <li> Go to the source folder
     <code>
       cd FDPoisson/src
     </code>
   <li> Build the package using make, and copy the executable to the bin folder <br>
    <code>
      make </code> <br> <code>
      make install
     </code> <br>
     Note that gfortran will be uesd by default, if you want to use an alternate fortran compiler, modify the FC variable in the Makefile to the desired compiler (eg mpif90)
</ol>
   

## Usage Instructions

To use FDPoisson, an input file (like the one provided in the sample folder) will be needed to specify the parameters of the calculation, including the dimension, bounds, number of grid points, potential at the boundaries and charge density

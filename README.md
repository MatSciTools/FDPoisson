# FDPoisson - Finite Difference code to solve Poisson Equation



## Features
The code can obtain the 1D/2D potential and electric field for polynomial charge densities (the degree of the polynomial and the coefficients have to be specified in the input file). We are currently working on 3D expansion.

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
     Note that gfortran will be used by default, if you want to use an alternate fortran compiler, modify the FC variable in the Makefile to the desired compiler (eg mpif90)
</ol>
   

## Usage Instructions

Detailed instructions on using FDPoisson are given in the usage_guide.pdf document in the docs/ folder. Input/output files for certain testcases can be found in the sample/ folder. Additionally, the utils/ folder contains python scripts that may be used to make graphs from the obtained potential/field. The utils folder also contains FD_derivation.pdf, which contains the mathematical derivation for the finite difference coefficient matrices.

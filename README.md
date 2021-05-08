# FDPoisson - Finite Difference code to solve Poisson Equation

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
   
## Features

## Usage Instructions

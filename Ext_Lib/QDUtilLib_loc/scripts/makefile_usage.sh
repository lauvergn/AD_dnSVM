#!/bin/bash
lib=QDUtillib
libShort=QD
app=`ls APP/*.f90`
base_app=$(basename "$app" .f90)

test=`ls TESTS/*.f90`
base_test=$(basename "$test" .f90)

echo "
1) The "$lib" makfile has the following targets:

* help : 
  The help usage of the makefile.
  Execute the the bash script: scripts/makefile_usage.sh (the present script)

* doc :
  Make the Ford documantation.
  From the library directory, the documentation location is: doc/ford_site/index.html

* lib :
  Create the library (lib*.a) for a given set of options

* ut : 
  Run some tests and analyses the number of errors
  The test executable is: "$base_test.x"

* app :
  Compilation of main fortran file: "$app"
  The executable is: "$base_app.x"

* clean :
  Remove the object files (with defined options), the executables, some log files (without the ones in the TESTS directory)

* cleanall :
  For all options, remove the object directories, the libraries, executables, all log files
  Then perform a cleanall for external libraries (if any)

* cleanlocextlib :
  Perform a cleanall and then remove the external library directories (if any)

* dep :
  Create the Fortran file list (of the SRC directory) and the makefile dependencies.

* all :
  Perform a lib app ut
  Therefore, it creates the library, the main and test executables.


2) The compiler options are (the first values is the default):

- FC=gfortran or ifx or ifort ...
- OPT=1 or 0: compilation with optimization or without optimization
- OMP=1 or 0: with or without openmp
- LAPACK=1 or 0: with or without blas and lapack libraries
- INT=4 or 8: change the integer kind default compilation option
- RKIND=real64 or real32 or real128: change the real kind
- WITHRK16=1 or 0: enables to turn on (off) the quadruple precision (real128). There is no default, hence the value is defined automatically

3) Examples:

make lib
  It creates a library with the folowing name: lib"$libshort"_gfortran_opt1_omp1_lapack1_int4_real64.a
  The object and mod files are in OBJ/obj_gfortran_opt1_omp1_lapack1_int4_real64

make FC=gfortran OPT=1 OMP=0 RKIND=real128
  It is equivalent to: make all FC=gfortran OPT=1 OMP=0 RKIND=real128

  It creates a library with the folowing name: lib"$libshort"_gfortran_opt1_omp0_lapack1_int4_real128.a
  The object and mod files are in OBJ/obj_gfortran_opt1_omp0_lapack1_int4_real128
  Run the tests
" | more
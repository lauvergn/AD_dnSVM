# QDUtilLib

List of Fortran modules which contain utilities for codes.
All modules contain a testing unit.

It has been tested with:

- gfortran (11, 12, 13, 14, 15 on macos, 12 on linux)
- ifx/ifort (2023 on linux)
- nagor (7.1 on linux)

## 1) Installation and testing

### a) with a makefile:

For more details, run:

```bash
make help
```

To build the library, **libQD_gfortran_opt1_lapack1_omp1_real64.a**, with the default options (OPT=1, OMP=1, LAPACK=1, INT=4, RKIND=real64)
```bash
make lib
```
In general, the makefile creates a library with the folowing name: **libQD_XXX_optW_lapackX_ompY_intZ_realR.a**
with:
- XXX: the compiler name (like gfortran)
- W, X, Y: the value O or 1 
- Z:  the value 4 or 8
- R: the value of real kind (32, 64, 128)

For instance, the default library is: **libQD_gfortran_opt1_lapack1_omp1_int4_real64.a**
The module file (.mod) are in the **OBJ/obj__XXX_optW_lapackX_ompY_intZ_realR** directory.

The compiler options are (the first values are the default):

- FC=gfortran or ifx or ifort ...
- OPT=1 or 0: compilation with optimization or without optimization
- OMP=1 or 0: with or without openmp
- LAPACK=1 or 0: with or without blas and lapack libraries
- INT=4 or 8: change the integer kind default compilation option
- RKIND=real64 or real32 or real128: change the real kind
- WITHRK16=1 or 0: enables to turn on (off) the quadruple precision (real128). There is no default, hence the value is defined automatically

Exemple: 

```bash
make FC=gfortran OPT=1 OMP=0 RKIND=real128
```

Two options to clean:

```bash
make clean
```

Remove some files, but keep the libraries, **libQD_XXX_optW_lapackX_ompY_intZ_realA.a**

```bash
make cleanall
```

Remove all files (executable, library, .mod, .o, build, documentation)

To test the module (it is long), from the TESTS directory, run

```bash
./run_tests.sh
```

The tests are running with gfortran and with combinations of several options:

- OPT=1 or 0: compilation with optimization or without optimization
- OMP=1 or 0: with or without openmp
- LAPACK=1 or 0: with or without blas and lapack libraries
- INT=4 or 8: change the integer kind default compilation option
- RKIND=real64 or real32 or real128: change the real kind

In the TESTS directory, the file, **ALL_Tests.log**, contains a summary of all the tests and other test log files are in the TESTS/output directory.

Alternatively, you run a test with the makefile:

```bash
make ut FC=gfortran OPT=1 OMP=0 RKIND=real128
```

### b) with fpm:

To build the library, **libQDUtilLib.a**, with the Lapack (LAPACK=1)
```bash
fpm build
```
The library is in **build/gfortran_xxxx/QDUtilLib** directory.
To remove Lapack library, the **fpm.toml** file must be edited.

Two options to clean:
```bash
fpm clean
```

Remove the build directory.


To test the module:

```bash
fpm test | grep TESTING | grep Number
```

To run an example:

```bash
fpm run AppQDLib 
```

### c) Documentation installation

The documentation is build with ford and you have two ways to make it:

With the makefile:
```bash
make doc
```
or directly with ford:
```bash
ford doc/ford-front-matter.md
```

The documentation is available as: "doc/ford_site/index.html"

## 2) How to use it

### 2a) With fpm

If you want to use it with **fpm**, you have to add a dependency in your own the **fpm.toml** file:

```
[dependencies]
QDUtilLib = { git = "https://github.com/lauvergn/QDUtilLib" }
```

### 2b) With makefile

If the QDUtil library is build with make, you have to:
- compile your code (such xxx.f90) with the QDUtil module files (.mod files). Those files are in the **OBJ/obj_XXX_optW_lapackX_ompY_intZ** directory.
For instance, with gfortran, OpenMP, lapack and integer kind=4:

```bash
gfortan -c xxx.f90 -fopenmp -IOBJ/libQD_gfortran_opt0_omp1_lapack1_int4
```

- link with **libQD_XXX_optW_lapackX_ompY_intZ_realR.a** library

```bash
gfortran ...    libQD_gfortran_opt0_omp1_int4_real64.a -llapack -lblas
```

The library and the module directory with old names (without **_realR**) are linked to the new library and the new module directory.

## 3) List of modules

### NumParameters

This module contains:

- **kind** definitions from the intrinsinc ISO_FORTRAN_ENV module
  - Rkind: default real kind (normaly real64)
  - Rk4, Rk8, Rk16 for real32, real64, real128
  - Ikind: default integer kind (usualy int32)
  - ILkind: default long integer kind (usualy int64)
  - Ik4, Ik8 for int32, int64
- Some real (kind=Rkind) numbers : ZERO to TWELVE, HALF, HUNDRED,ONETENTH ..., PI
- Some complexe (kind=Rkind) numbers : EYE (i), CZERO, CONE, CHALF
- out_unit and in_unit: the standard output and input units from the intrinsinc ISO_FORTRAN_ENV module
- print_level: level of printing:     0 minimal, 1 default, 2 large, -1 nothing
To change print_level (**prtlev** is an integer):

```Fortran
CALL set_print_level(prtlev=0)
```

To print information about the library

```Fortran
CALL version_QDUtil(Print_Version=.TRUE.)
```

### String

This module contains functions and subroutines to manipulate character string (character(len=)):

- Functions to change to upper to lowercase or the reverse: **TO_lowercase**, **TO_uppercase**
- Functions to convert numbers (integer, real, complex, Frac, logical) in string: **TO_string**
  All kinds defined in NumParameters module and logical type are possible
  For real and comlex convertions, an optional format (Rformat) can be given.
  It can work with table of dimension 1.
- Function to read a line from a file define with its unit: **Read_line**
- Function to check is a string is empty: **string_IS_empty**
- Concatenation (//) between string and integer, real, complex, and logical (and the reverse)
- Examples:
```Fortran
character (len=:), allocatable :: str

str = TO_string(1)        ! => "1"
str = TO_string(1.0)      ! => "1."
str = TO_string(EYE)      ! => "(0.,1.)"
str = TO_string(.FALSE.)  ! => "F"
str = TO_string([1,3,-1]) ! => "1 3 -1"

str = 'coucou' // 1.      ! => "coucou1."

str = TO_lowercase("AbC") ! => "abc"
str = TO_uppercase("aBc") ! => "ABC"
```

### Frac

This module contains functions and subroutines to manipulate fractions.
All functions are **elemental** (except **TO_string(frac)**). Therefore, one can use those functions with table of fractions.

- A type: **Frac_t**, which contains the numerator and denominator
  When the fraction is negative, the numerator has the minus sign.
  When the denominator is **0**, the fraction is simplified as 1/0.
- The operators (+, -, *, /, **, ==, >, <, <=, >=, /=) and the affectation (=) are orverloaded.
- When Frac_t is used for initialization and after numerical operations, the fraction is always reduced  (with the Greatest common divisor).
- One can test if a fraction is an integer with: frac_IS_integer(Frac)
- A fraction can be exported to:
  - to string: **TO_string(frac)**
  - to real (with the default kind=Rkind): **TO_real(frac)**
- The affectation can be done as follows:
  - Frac2 = Frac_t(3,-6) ! stored as -1/2
  - Frac2 = Frac1
  - Frac2 = Int1
  - Frac2 = '1/4' ! using a string of characters
  - where Frac1, Frac2 are fraction, Int1 is an integer
- Examples:
```Fortran
  TYPE(Frac_t) :: Frac1, Frac2
  TYPE(Frac_t), allocatable :: tab_Frac(:)

  Frac1 = '1/-2' ! use the conversion from string to Frac_t
  write(*,*) 'Frac1: ',TO_String(Frac1) ! it gives "Frac1: -1/2"
  Frac2 = -2*Frac1 ! here the result is one and it is simplified
  write(*,*) 'Frac2: ',TO_String(Frac2) ! it give "Frac2: 1"
  Frac2 = Frac1**3
  write(*,*) 'Frac2: ',TO_String(Frac2) ! it give "Frac2: -1/8"
  tab_Frac = Frac_t(1,[2,3,4])
  write(*,*) 'tab_Frac: ',(TO_String(tab_Frac(i)) // ' ',i=1,size(tab_Frac)) ! it give "tab_Frac: 1/2 1/3 1/4 "
```

### RW_MatVec

This module contains public subroutines to read and write real and complex (kind=Rkind) vectors and matrices:

- Subroutines to read a matrix from a file: **Read_Mat** or **Read_Vec**
  All real kinds, all complexe kinds and one integer kind from NumParameters module are possible
  **Read_Mat(Mat,nio,nbcol,err)** or **Read_Vec(Vec,nio,nbcol,err)**
  - Mat or Vec: the matrice or the Vector to be readed. Mat or Vec have to be allocated
  - nio: the file unit
  - nbcol: the number of columns to be readed per block. If the number of columns of Mat or Vec is larger than nbcol, then several blocks are readed
  - err: integer to catch error (err=0 => no error)
- Subroutines to write a matrix to a file or string: **Write_Mat** or **Write_Vec**
  All real and complexe kinds from NumParameters module are possible. For complex number, it is not possible to write into a string.
  **Write_Mat(Mat,nio,nbcol,Rformat,info,iprint)** or **Write_Mat(Mat,string,nbcol,Rformat,info,iprint)** or **Write_Vec(Vec,nio,nbcol,Rformat,info,iprint)** or **Write_Vec(Vec,string,nbcol,Rformat,info,iprint)**
  - Mat or Vec: the matrice or the Vector to be printed. Mat or Vec have to be allocated
  - nio: the file unit
  - string: a character string with len long enough. It is better an allocatable character length [character (len=:), allocatable :: string]
  - nbcol: the number of columns to be printed per block. If the number of columns of Mat or Vec is larger than nbcol, then several blocks are printed
  - Rformat: (optional) format for real and complex. If Rformat is not present, then the default formats (RMatIO_format for real and CMatIO_format for complex) are used.
  - iprint: (optional) the Mat or Vec are printed if iprint is not present or if iprint=0
- The module contains the two default formats which are public:
  - RMatIO_format: (f18.10)
  - CMatIO_format: '(',f15.7,',',f15.7,')'

### Matrix

This module contains public functions and subroutines to perform some lienar algebra operations with the default real kind (kind=Rkind). It can use some LAPACK subroutines:

- Function to compute the inverse of a matrix:      **inv_OF_Mat_TO**
- Function to solve a linear system of equation:    **LinearSys_Solve**
- Function to compute the determinant of a matrix:  **Det_OF**
- Function to initialyze a real identity matrix:    **Identity_Mat**
- Gram-Schmidt Orthonormaization (real or complex): **Ortho_GramSchmidt**

### Diagonalization

This module contains public subroutines to digonalize a matrix with the default real kind (kind=Rkind). It can use some LAPACK subroutines:

- Subroutine to diagnalize a real matrix: **diagonalization**

### Quadrature

This module contains public subroutines to generate Gauss quadratures for some orthonormal polynomials (Hermite or HO (Harmonic Oscillator), Legendre) or other basis sets (sine, BoxAB, Fourier).

- The quadrature (name, grid points (x), weights (w)) is defined in a derived type:

```Fortran
  TYPE Quadrature_t
    real (kind=Rkind), allocatable :: x(:,:) ! x(ndim,nq): position
    real (kind=Rkind), allocatable :: w(:)   ! w(ndim,nq): weight
    character (len=:), allocatable :: name
    ...
  END TYPE Quadrature_t
```

- Subroutine to initialize the quadrature: 

```Fortran
CALL Init_Quadrature_QDUtil(Quadrature,nq,name,A,B,xc,scale,isym_grid,err)
```

- **Quadrature**: Variable (intent(inout)) which contains the quadrature (Quadrature_t)
- **nq**: number of grid points (intent(in)).
- **name**: type of the quadrature (intent(in)). 

```text
Possibilities: 
"sine"      => particle-in-a-box in ]0,PI[ (sin(k.x))
"BoxAB"     => particle-in-a-box in ]A,B[
"Fourier"   => Fourier in [-PI,PI]
"FourierAB" => Fourier in [A,B]
"HermiteH"  => Hermite polynomials ]-inf,+inf[, with xc=0. and scale=1.
"HO"        => HO basis set ]-inf,+inf[ (the gaussian part is included in the weights)
"LegendreP" => Legendre polynomials [-1,1] (or "Pl0").
```

- **A** and **B**: Numerical domain [A,B] (intent(in) and optional) for "BoxAB" or "FourierAB" basis sets (B>A).
- **xc** and **scale**: are, respectively, the center and the scaling factor (scale > 0.) of HO basis set (intent(in) and optional).
- **isym_grid**: Integer (0, -1, 1) wich enables to control the Fourier or FourierAB grid (intent(in) and optional).

```text
isym_grid=0  => the first grid point is in A+dx/2 (default)
isym_grid=-1 => the first grid point is in A
isym_grid=-1 => the first grid point is in A+dx (last point in B)
```

- **err**: Integer to check the error (intent(inout) and optional). err=0 => no error.

Examples for an HO grid (with 10 points):

```Fortran
CALL Init_Quadrature(Quadrature,nq=10,name="HO",xc=1._real64,scale=0.5_real64,err=err_grid)
```

Or for High Precision (HP) calculation

```Fortran
CALL Init_Quadrature_HP(Quadrature,nq=10,name="HO",xc=1._real64,scale=0.5_real64,err=err_grid)
```

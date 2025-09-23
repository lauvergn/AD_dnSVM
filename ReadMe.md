# AD_dnSVM

**AD_dnSVM** is a Fortran Automatic Differentiation library using forward mode for scalars, vectors and matrices.
Its features are:

- no limit in terms of the number of independent variables (this number is defined at runtime)
- up to third derivatives

It is similar to Auto_Deriv fortran module:
  <http://www.autodiff.org/?module=Tools&tool=AUTO_DERIV>

date: 23/09/2025

  Copyright 2021 David Lauvergnat [1]

*Originally, it has been developed for Quantum Model Lib (QML):* <https://github.com/lauvergn/QuantumModelLib>


## 1) Utilisation with scalars

This library enables to get the derivatives (up to third order) of a function of several variables (type(dnS_t)).
See the APP/Example_dnS.f90 file.

### 1a) Variable initializations

Variables need to be initialized. For instance the variables, X, Y, Z

```Fortran
  USE ADdnSVM_m
  IMPLICIT NONE
  !....

  type(dnS_t) :: X,Y,Z
  type(dnS_t) :: f

  X = Variable( Val=HALF, nVar=3, iVar=1, nderiv=1 )
  Y = Variable( Val=ONE,  nVar=3, iVar=2, nderiv=1 )
  Z = Variable( Val=TWO,  nVar=3, iVar=3, nderiv=1 )
```

X, Y and Z are, respectively, the first, the second and the third variables (iVar) among 3 (nVar).
Their derivatives are defined up to the 1st order (nderiv=1).
The variable values are set with Val.

Remark: for Y (the second variable), its value is ONE, the three 1st derivatives are [ZERO,ONE,ZERO] and all 2d derivatives are null.

### 1b) Variable operations
Operations with X, Y and Z:
Remarks: X, Y, Z, f, df, gradf are of dnS_t type and gradf is a table.

```Fortran
    f = ONE + cos(X)**2 + sin(Y*Z)
```

```Text
 f value   : 1. + cos(0.5)**2 + sin(2.) = 2.679448580
 f gradient: [-2. * sin(0.5)*cos(0.5), 2. * cos(2.),   cos(2.)] =
             [-0.8414709848,          -0.8322936731, -0.4161468365]
```

```Fortran
CALL Write_dnS(f,info='f=1.0 + cos(X)**2 + sin(Y*Z), value: 2.67945')
```

gives:

```Text
 f=1.0 + cos(X)**2 + sin(Y*Z), value: 2.67945
 0   derivative            +0.268E+01
 1st derivative  1         -0.841E+00
 1st derivative  2         -0.832E+00
 1st derivative  3         -0.416E+00
 ```

 One can define a new function from the first derivative with respect to a variable (defined by the ider index) of a function:

```Fortran
 df = deriv(f,ider=1)
```

Here, df constains df/dX. 
WARNING: the nderiv of df is reduced by one with respect to nderiv of f.

The gradient is also possible, the result is in a table of dnS. It has the same limitation with nderiv (reduction of nderiv)

```Fortran
 gradf = grad(f)
```

### 1c) Vector initialization and Jacobian matrix

Instead of several variable initializations, one can initialize a vector:

```Fortran
  USE ADdnSVM_m
  IMPLICIT NONE
  !....

  type(dnS_t), allocatable :: VecOld(:),VecNew(:)

  r  = TWO
  th = Pi/3
  VecOld = Variable([r,th], nderiv=2 )                ! VecOld(1) : r, VecOld(2) : th
  VecNew = VecOld(1)*[cos(VecOld(2)),sin(VecOld(2))]  ! [r*cos(th), r*sin(th)]

  JacNewOld = get_Jacobian( VecNew )                  ! JacNewOld(inew,iold)=[ dQinew/dQiold ]
```

The vector intialization has optional arguments:

- `nVar`: the total number of the independent variables. It must be larger or equal to the vector size.
- `iVar(:)`: a table with the indices of the independent variables. It size must be equal to the vector size.

```Fortran
  VecOld = Variable([r,th], nVar=3, iVar=[1,2], nderiv=2 )
```

### 1d) Special functions/subroutines

- Subroutine `set_dnS(S,d0,d1,d2,d3)`to set the corresponding derivatives of `S` (`dnS_t`). `d0` is real (scalar), `d1` is a vector of real, `d2` is a matrix of real and `d3` has a rank 3. `d0`, `d1`, `d2` and `d3` are optional. `nderiv` is defined accordingly to the presence of  `d0`, `d1`, `d2` or `d3`.

- Functions `get_d0`, `get_d1`, `get_d2` and `get_d3` to get, respectively, the corresponding derivatives, `dnS%d0`, `dnS%d1(:)`, `dnS%d2(:,:)` and `dnS%d3(:,:,:)`.
- Function `get_Flatten` to get all `dnS` derivatives (`dnS%d0`, `dnS%d1(:)` ...) in a single real vector.

## 2) Installation

### a) With fpm:

```bash
fpm build
```


### b) with a makefile:

```bash
make all
```

It will make the library, the executable tests and example.
You can change the compiler, Lapack flag, the OpenMP flag, the compiler optimization flag, the default integer, the real kind either in the makefile or when calling make:

```bash
make all FC=ifort OMP=0 OPT=0 LAPACK=0 INT=4 RKIND=real64
  # FC=ifort to change the compiller to ifort
  # OMP=0/1 to turn off/on the OpenMP fortran flag.
  # OPT=0/1 to turn off/on the fortran optimization.
  # LAPACK=0/1 to turn off/on the lapack use
  # INT=4/8 to change the default integer
  # RKIND=real64 or real32 or real128: change the real kind
```


The library, **libAD_dnSVM_XXX_oppY_ompZ_lapackW_intV_realA.a** is created in the main directory and the **libAD_dnSVM.a** library is linked to it.
Remarks : 
- XXX is the compiller (gfortran, ifort ...)
- Y is 0 or 1 (opt0 / opt1: compiler optimization)
- Z is 0 or 1 (omp0 / omp1: whitout/with OpenMP)
- W is 0 or 1 (lapack0 / lapack1: whitout/with lapack)
- V is 4 or 8 (int4 / int8)
- A is 32 or 64 or 128 (real32, real64, real128)


If needed, the .mod files are in the **OBJ/obj_XXX_oppY_ompZ_lapackW_intV_realA** directory.

Remark: the external library(ies) are stored in the Ext_Lib directory. However, it can be modified the makefile variable ExtLibDIR. For instance, to store the external library directory in ../../LIB, use:

```bash
make lib OPT=0 ExtLibDIR=../../LIB
```

It has been tested with:

- gfortran (11, 12, 13, 14, 15 on macos, 12 on linux)
- ifx/ifort (2023 on linux)
- nagor (7.1 on linux)

## 3) run the tests

With **fpm**

```bash
    fpm test dnPoly
    fpm test dnS
    fpm test dnVec
    fpm test dnMat
```

or in the Tests directory, run the scripts

```bash
    ./run_test_dnS
    ./run_test_dnPoly
```

or in the Tests directory (with several combinations of OMP, OPT ...), run the script

```bash
    ./run_tests.sh
```

## 4) run the example

With **fpm**

```bash
    fpm run Exa_dnS
```

or in the main directory

```bash
    ./Exa_dnS.x
```


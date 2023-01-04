# AD_dnSVM

**AD_dnSVM** is a Fortran Automatic Differentiation library using forward mode for scalars.
Its features are:

- no limit in terms of the number of independent variables (this number is defined at runtime)
- up to third derivatives

It is similar to Auto_Deriv fortran module:
  <http://www.autodiff.org/?module=Tools&tool=AUTO_DERIV>

date: 17/12/2022

  Copyright 2021 David Lauvergnat [1]

*Originally, it has been developed for Quantum Model Lib (QML):* <https://github.com/lauvergn/QuantumModelLib>

## 1) Utilisation

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

Remark: for Y, it value is ONE, the three 1st derivatives are [ZERO,ONE,ZERO] and all 2d derivatives are null.

Operations with X, Y and Z:

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

### 1b) Vector !nitialization and Jacobian matrix

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
You can change the compiler, the OpenMP flag and the compiler optimization falg either in the makefile or when calling make:

```bash
make all FC=ifort OMP=0 OPT=0
  # FC=ifort to change the compiller to ifort
  # OMP=0/1 to turn off/on the OpenMP fortran flag.
  #OPT=0/1 to turn off/on the fortran optimization.
```

The library, **libAD_dnSVM_XXX_oppy_ompz.a** is created in the main directory and the **libAD_dnSVM.a** library is linked to it.
Remarks : XXX is the compiller (gfortran, ifort ...), y is 0 or 1 (opt0 / opt1: compiler optimization) and z is 0 or 1 (omp0 / omp1: whitout/with OpenMP)
The .mod files are need. They are in the **OBJ/obj_XXX_oppy_ompz** directory.

## 3) run the tests

With **fpm**

```bash
    fpm test dnPoly
    fpm test dnS
```

or in the Tests directory, run the scripts

```bash
    ./run_test_dnS
    ./run_test_dnPoly
```

or in the Tests directory, run the script

```bash
    ./run_tests
```

## 4) run the example

With **fpm**

```bash
    fpm run Exa_dnS
````

or in the main directory

```bash
    ./Exa_dnS.x
```

=========================================
=========================================
 AD_dnSVM* is a Fortran Automatic Differentiation tool using forward mode for scalars (S), Vectors (V) and Matrices (M).
 Its features are:
 - no limit in terms of the number of independent variables (this number is defined at runtime)
 - up to third derivatives

It is similar to Auto_Deriv fortran module: http://www.autodiff.org/?module=Tools&tool=AUTO_DERIV

  date: 26/12/2021

    Copyright 2021 David Lauvergnat [1]

[1]: Institut de Chimie Physique, UMR 8000, CNRS-Université Paris-Saclay, France

* Originally, it has been developed for Quantum Model Lib (QML) :
https://github.com/lauvergn/QuantumModelLib


1) Utilisation:

This library enables to get the derivatives (up to third order) of a
function of several variables (type(dnS_t)).
See the APP/Example_dnS.f90 file.

Variables need to be initialized, for instance the variables, X, Y, Z:

  type(dnS_t) :: X,Y,Z
  type(dnS_t) :: f

  X = Variable( Val=HALF, nVar=3, iVar=1, nderiv=1 )
  Y = Variable( Val=ONE,  nVar=3, iVar=2, nderiv=1 )
  Z = Variable( Val=TWO,  nVar=3, iVar=3, nderiv=1 )

X, Y and Z are, respectively, the first, the second and the third variables (iVar)
among 3 (nVar).
Their derivatives are defined up to the 1st order (nderiv=1).
The variable values are set with Val.

Remark: for Y, it value is ONE, the three 1st derivatives are [ZERO,ONE,ZERO]
and all 2d derivatives are null.


Operations with X, Y and Z:

f = ONE + cos(X)**2 + sin(Y*Z)

 f value   : 1. + cos(0.5)**2 + sin(2.) = 2.679448580
 f gradient: [-2. * sin(0.5)*cos(0.5), 2. * cos(2.),   cos(2.)] =
             [-0.8414709848,          -0.8322936731, -0.4161468365]

CALL Write_dnS(f,info='f=1.0 + cos(X)**2 + sin(Y*Z), value: 2.67945')

gives:

 f=1.0 + cos(X)**2 + sin(Y*Z), value: 2.67945
 0   derivative            +0.268E+01
 1st derivative  1         -0.841E+00
 1st derivative  2         -0.832E+00
 1st derivative  3         -0.416E+00

=========================================
 1) Installation
   with fpm:
    fpm build

   with a makefile:
       gfortran 11.2.0 (linux)
       ifort    ??
       nagfor   ??

    make all # it will make the library, the executable tests and example.

 2) library: libAD_dnSVM.a
 The .mod files are need. They are in the OBJ directory

 3) run the tests
    fpm test dnPoly
    fpm test dnS

or in the Tests directory, run the scripts:
    ./run_test_dnS
    ./run_test_dnPoly

or in the Tests directory, run the script
    ./run_tests

4) run the example
    fpm run Exa_dnS
  or in the main directory
    ./Exa_dnS.x

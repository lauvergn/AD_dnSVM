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
Most of the intrinsic functions (cos, sin, exp, ...) and operations (+, -, *, /, **) are possible to use with dnS_t.

Remarks: X, Y, Z, f, df, gradf are of dnS_t type and gradf is a table of dnS_t.

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

### 1d) Full list of functions and operations

- `=` : from another dnS_t, a real(kind=Rkind), an integer (default kind)
- `+`, `-`, `*`, `/` : from one dnS_t and another dnS_t, a real(kind=Rkind), an integer (default kind)
- `**` : from one dnS_t (left argument) and a real(kind=Rkind), an integer (default kind) (right argument)

- `>`, `>=`, `<`, `<=`, `==`, `/=`,  (`.GT.`, `.GE.`, `.LT.`, `.LE.`, `.EQ.`, `.NEQ.`) : between two dnS_t (it uses only the d0 components)

- `sqrt`, `exp`, `log`, `log10`, `abs`
- `sin`, `asin`, `cos`, `acos`, `tan`, `atan`
- `sinh`, `asinh`, `cosh`, `acosh`, `tanh`, `atanh`

- `mod`, `modulo` : only with the d0 components

- `atan2` : with two dnS_t arguments

- `dot_product` : from two vectors of dnS_t or on vector of dnS_t and another real vector
- `sum` : from a vector or a matrix of dnS_t
- `transpose` : from a matrix of dnS_t
- `matmul`: from two of dnS_t or on matrix of dnS_t and another real one
- `cross_product` : from two vectors (size=3) of dnS_t or on vector of dnS_t and another real vector

- `dnMonomial(x,i)`: polynomials: `x**i`
- `dnBox(x,i,[ReNorm])`: sine or particle-in-a-box between $]0,\pi[$. If the optional argument `ReNorm=.TRUE.` or if `ReNorm` is not present, the function is renormalized.
- `dnFourier(x,i,[ReNorm])`: Fourier series (order: `cos(0*x)`, `sin(1*x)`, `cos(1*x)`, `sin(2*x)`, `cos(2*x)` ...) between $]-\pi,\pi[$. If the optional argument `ReNorm=.TRUE.` or if `ReNorm` is not present, the function is renormalized.
- `dnFourier2(x,i,[ReNorm])`: Fourier series (order: ..., `sin(2*x)`, `sin(1*x)`, `cos(0*x)`, `cos(1*x)`, , `cos(2*x)` ...) between $]-\pi,\pi[$. If the optional argument `ReNorm=.TRUE.` or if `ReNorm` is not present, the function is renormalized.

- `dnLegendre0(x,i,[ReNorm])`: Legendre polynomials [-1,1]. If the optional argument `ReNorm=.TRUE.` or if `ReNorm` is not present, the function is renormalized.
- `dnLegendre(x,l,m,[ReNorm])`: Associated Legendre polynomials ]-1,1[ (`l >= 0` and `-l <= m <= l`). If the optional argument `ReNorm=.TRUE.` or if `ReNorm` is not present, the function is renormalized.

- `dnJocobi(x,n,alpha,beta,[ReNorm])`: Jacobi polynomials ]-1,1[ (`n`,`alpha`,`beta` are integers). If the optional argument `ReNorm=.TRUE.` or if `ReNorm` is not present, the function is renormalized.

- `dnHermite(x,l,[ReNorm])`: Physicist Hermite polynomials between $]-\infty,+\infty[$. If the optional argument `ReNorm=.TRUE.` or if `ReNorm` is not present, the function is renormalized.
- `dnExpHermite(x,l,[ReNorm])`: Hermite functions or Hermite-Gaussian functions between $]-\infty,+\infty[$. If the optional argument `ReNorm=.TRUE.` or if `ReNorm` is not present, the function is renormalized.

- `RSphericalHarmonics(th,phi,l,lm,ReNorm)`: Real Spherical Harmonics $Y_{l}^{lm}(th,phi)$. It uses the `dnFourier` order.
- `RSphericalHarmonics2(th,phi,l,lm,ReNorm)`: Real Spherical Harmonics $Y_{l}^{lm}(th,phi)$. It uses the `dnFourier2` order.

### 1e) Special functions/subroutines

- Subroutine `Write_dnS(S,nio,info,all_type,FOR_test,Rfmt,nderiv)` write `S` (`dnS_t`) in a file of unit `nio` and  Subroutine `Write_dnS(S,string,info,all_type,FOR_test,Rfmt,nderiv)` write `S` (`dnS_t`) in a string of characters (`string`). For both subroutines, `info` is an optional string which is printed. `all_type` or `FOR_test` are some optional flags when one is present and is .TRUE., all components of the `dnS_t` is printed without format (different writing). When those flags are absent or set to .FALSE., some derivatives are printed (all or up to `nderiv` if present) with the format `e12.3` or `Rfmt` if present.

- Subroutine `set_dnS(S,d0,d1,d2,d3)` to set the corresponding derivatives of `S` (`dnS_t`). `d0` is real (scalar), `d1` is a vector of real, `d2` is a matrix of real and `d3` has a rank 3. `d0`, `d1`, `d2` and `d3` are optional. `nderiv` is defined accordingly to the presence of  `d0`, `d1`, `d2` or `d3`.

- Subroutine `set_d0S(S,d0)` to set the component `d0`of of `S` (`dnS_t`) without changing `nderiv`.

- Functions `get_d0`, `get_d1`, `get_d2` and `get_d3` to get, respectively, the corresponding derivatives, `dnS%d0`, `dnS%d1(:)`, `dnS%d2(:,:)` and `dnS%d3(:,:,:)`.
- Subroutine `sub_get_dn`:  gets the `d0`, `d1`, `d2` or `d3` components  according to their presence.
- Function `get_Flatten(S,i_der)` to get all `S` (`dnS_t`) derivatives (`dnS%d0`, `dnS%d1(:)` ...) in a single real vector. If `ider` is present only the `ider`-derivatives (all components) are returned (it is equivalent the reshape of after a `get_d1`, `get_d2` or `get_d3`).

- Functions, `get_nderiv` and `get_nVar`, get, respectively, `nderiv` and `nVar`.
- Function, `TO_dnSReducedDer(S,list)` returns a `dnS_t` from `S` (of `dnS_t`) with a subset of derivatives defined by the `list` of integer.
- Function, `FROM_dnSReducedDer(nVar,list,S)` returns a `dnS_t` from `S` (of `dnS_t`). The derivative `i` of `S` are set in `list(i)` of the result. `nVar` is the number of variables (derivatives) of the results. It must be larger or equal than the size of `list`.

- Function `Deriv(S,ider)` returns `dS` (`dnS_t`) as the `ider`-derivative of `S` (`dnS_t`). `nderiv` is reduced by 1.
- Function `grad(S)` returns the gradient of of `S` (`dnS_t`) as a vector of `dnS_t`. `nderiv` is reduced by 1.
- Function `get_Jacobian(Vec)` returns the Jacobian matrix as a real matrix. `Vec`is a vector of `dnS_t`.

- Function `dnf_OF_dnS(f,S)` returns `fS` (`dnS_t`) as the function `f` (`dnS_t` with `nVar=1` i.e. 1D) of `S` (`dnS_t`).
- Function `dnf_OF_dnS(f,S(:))` returns `fS` (`dnS_t`) as the function `f` (`dnS_t` with `nVar>=1`) of `S(:)` (`dnS_t`) with `nVar` of `f` equal the size of `S(:)`.

## 2) Utilisation with vector
## 3) Utilisation with matrix

## 4) Installation

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

Remark: the external library(ies) are stored in the Ext_Lib directory. However, it can be modified with the makefile variable ExtLibDIR. For instance, to compile with the external library directory stored in ../../LIB, use:

```bash
make lib OPT=0 ExtLibDIR=../../LIB
```

It has been tested with:

- gfortran (11, 12, 13, 14, 15 on macos, 12 on linux)
- ifx/ifort (2023 on linux)
- nagor (7.1 on linux)

## 5) run the tests

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


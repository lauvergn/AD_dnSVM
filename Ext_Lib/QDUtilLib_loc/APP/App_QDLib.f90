PROGRAM App_QDLib
#ifndef __WITHRK16
#define __WITHRK16 1
#endif
  USE QDUtil_m
  IMPLICIT NONE

  integer                          :: i,n
  real(kind=Rkind),    allocatable :: RMat(:,:),REigVal(:),REigVec(:,:),RVec(:)
  real(kind=Rkind),    allocatable :: RVec2(:)

  TYPE(Frac_t)                     :: Frac1, Frac2
  TYPE(Frac_t),        allocatable :: tab_Frac(:)
  character (len=:),   allocatable :: str

  TYPE (Quadrature_t) :: xw
  integer             :: err_grid
  real(kind=Rkind)    :: time


  CALL version_QDUtil(Print_Version=.TRUE.)

  time = 100000._Rkind
  write(*,*) int(time),' seconds => d,h,m,s: ',conv_seconds(time)

 !====================================================================
  ! Tests string
  write(*,*) 'TO_string, integer: ',TO_string(-123)
  write(*,*) 'TO_string, integer: ',TO_string(+123)
  write(*,*) 'TO_string, integer: ',TO_string(HUGE(1))
  write(*,*) 'TO_string, integer: ',TO_string(-HUGE(1_Ik8))


  write(*,*) 'TO_string, integer: ',TO_string([0,1,2,3,4,5,6,7,8],max_col=5)
  write(*,*) 'TO_string, integer Ik8: ',TO_string([integer(kind=Ik8) :: 0,1,2,3,4,5,6,7,8],max_col=5)

  write(*,*) 'TO_string, real: ',TO_string([0.,1.,2.,3.,4.,5.,6.,7.,8.],max_col=5)
  write(*,*) 'TO_string, real Rk4: ',TO_string([real(kind=Rk4) :: 0,1,2,3,4,5,6,7,8],max_col=5)
  write(*,*) 'TO_string, real Rk8: ',TO_string([real(kind=Rk8) :: 0,1,2,3,4,5,6,7,8],max_col=5)
#if __WITHRK16 == 1
  write(*,*) 'TO_string, real Rk16: ',TO_string([real(kind=Rk16) :: 0,1,2,3,4,5,6,7,8],max_col=5)
#endif

  write(*,*) 'TO_string, complex Rk4: ',TO_string([complex(kind=Rk4) :: 0,1,2,3,4,5,6,7,8],max_col=5)
  write(*,*) 'TO_string, complex Rk8: ',TO_string([complex(kind=Rk8) :: 0,1,2,3,4,5,6,7,8],max_col=5)
#if __WITHRK16 == 1
  write(*,*) 'TO_string, complex Rk16: ',TO_string([complex(kind=Rk16) :: 0,1,2,3,4,5,6,7,8],max_col=5)
#endif
  !====================================================================
  ! Tests on fractions
  Frac1 = '1/-2' ! use the conversion from string to Frac_t
  write(*,*) 'Frac1: ',TO_String(Frac1) ! it give "Frac1: -1/2"
  Frac2 = -2*Frac1 ! here the result is one and it is simplified
  write(*,*) 'Frac2: ',TO_String(Frac2) ! it give "Frac2: 1"
  Frac2 = Frac1**3
  write(*,*) 'Frac2: ',TO_String(Frac2) ! it give "Frac2: -1/8"
  tab_Frac = Frac_t(1,[2,3,4])
  write(*,*) 'tab_Frac: ',(TO_String(tab_Frac(i)) // ' ',i=1,size(tab_Frac)) ! it give "tab_Frac: 1/2 1/3 1/4 "

  !====================================================================
  ! Tests for the matrix digonalization
  !
  ! define the matrices
  n = 3
  RMat =  reshape([ONE,HALF,ZERO,                             &
                   HALF,ONE,HALF,                             &
                   ZERO,HALF,ONE],shape=[n,n])

  allocate(REigVal(n))
  allocate(REigVec(n,n))


  CALL diagonalization(RMat,REigVal,REigVec)


  CALL Write_Mat(RMat,out_unit,5,info='RMat')
  write(out_unit,*)
  CALL Write_Mat(REigVec,out_unit,5,info='REigVec (in column)')
  write(out_unit,*)
  CALL Write_Vec(REigVal,out_unit,5,info='REigVal')
  write(out_unit,*)

  DO i=1,n
    write(out_unit,*) i,matmul(Rmat,REigVec(:,i))-REigVal(i)*REigVec(:,i)
  END DO


  !====================================================================
  ! Tests for identity matrix
  !
  ! define the matrices
  write(out_unit,*) 'Test identity matrix'
  n = 100
  RMat =  Identity_Mat(n)
  RVec  = [(real(i,kind=Rkind)/Pi,i=1,n)]
  RVec2 = matmul(Rmat,RVec)


  write(out_unit,*) 'RVec(1) and RVec(n)',RVec(1),RVec(n)
  write(out_unit,*) 'diff',maxval(abs((RVec-RVec2)))


  CALL Sort_Vec(RVec,sort_type=-1)
  write(out_unit,*)
  CALL Write_Vec(RVec,out_unit,5,info='RVec (decending order)')
  !====================================================================
  ! Test on quadrature:
  !
  CALL Init_Quadrature(xw,nq=10,name='HO',err=err_grid)
  CALL Write_Quadrature(xw)
  CALL Init_Quadrature_HP(xw,nq=10,name='HO',err=err_grid)
  CALL Write_Quadrature(xw)
  IF (err_grid == 0) THEN
    RMat = matmul(xw%d0bgw,xw%d0gb)
    CALL Write_Mat(RMat, nio=out_unit, nbcol=5, info='Overlap')
  END IF

END PROGRAM App_QDLib
!===============================================================================
!===============================================================================
!This file is part of QDUtil.
!
!===============================================================================
! MIT License
!
! Copyright (c) 2022 David Lauvergnat
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.
!===============================================================================
!===============================================================================
MODULE QDUtil_Matrix_m
  IMPLICIT NONE

  PRIVATE

  PUBLIC inv_OF_Mat_TO,inv_OF_Mat
  INTERFACE inv_OF_Mat
    MODULE PROCEDURE QDUtil_inv_OF_RMat_TO
    MODULE PROCEDURE QDUtil_inv_OF_CMat_TO
  END INTERFACE
  INTERFACE inv_OF_Mat_TO
    MODULE PROCEDURE QDUtil_inv_OF_RMat_TO
    MODULE PROCEDURE QDUtil_inv_OF_CMat_TO
  END INTERFACE

  PUBLIC inv_OF_Mat_TO_Mat_inv
  INTERFACE inv_OF_Mat_TO_Mat_inv
    MODULE PROCEDURE QDUtil_inv_OF_RMat_TO_RMat_inv
    MODULE PROCEDURE QDUtil_inv_OF_CMat_TO_CMat_inv
  END INTERFACE

  PUBLIC LinearSys_Solve
  INTERFACE LinearSys_Solve
    MODULE PROCEDURE QDUtil_RLinearSystem_Solve
    MODULE PROCEDURE QDUtil_CLinearSystem_Solve
      END INTERFACE

  PUBLIC Det_OF
  INTERFACE Det_OF
    MODULE PROCEDURE QDUtil_Det_OF_RMat
    MODULE PROCEDURE QDUtil_Det_OF_CMat
  END INTERFACE

  PUBLIC Identity_Mat
  INTERFACE Identity_Mat
    MODULE PROCEDURE QDUtil_Identity_RMat
  END INTERFACE

  PUBLIC LU_solve
  INTERFACE LU_solve
    MODULE PROCEDURE QDUtil_Driver_LU_solve_cplx
  END INTERFACE

  PUBLIC LU_decomp
  INTERFACE LU_decomp
    MODULE PROCEDURE QDUtil_Driver_LU_decomp_cplx
  END INTERFACE

  PUBLIC Ortho_GramSchmidt
  INTERFACE Ortho_GramSchmidt
    MODULE PROCEDURE QDUtil_Ortho_GramSchmidt_RMat
    MODULE PROCEDURE QDUtil_Ortho_GramSchmidt_CMat
  END INTERFACE
  
  PUBLIC :: Test_QDUtil_Matrix
  CONTAINS
  !================================================================
  !   Inversion of a real matrix Rmat : RMat_inv = Rmat^-1
  !   Function and subroutine
  !================================================================
  FUNCTION QDUtil_inv_OF_RMat_TO(Rmat,inv_type,epsi) RESULT(RMat_inv)
    USE QDUtil_NumParameters_m
    IMPLICIT NONE

    real(kind=Rkind), allocatable :: RMat_inv(:,:)

    real(kind=Rkind), intent(in)             :: RMat(:,:)
    integer,          intent(in), optional   :: inv_type
    real(kind=Rkind), intent(in), optional   :: epsi

    ! local variables
    integer ,         allocatable :: indx(:)
    real(kind=Rkind), allocatable :: work(:),m1w(:,:)
    real(kind=Rkind), allocatable :: vv(:,:)
    real(kind=Rkind), allocatable :: b(:)

    real(kind=Rkind) :: wmax,wmin
    real(kind=Rkind) :: d
    integer          :: j,n

    integer            :: inv_type_loc
    integer, parameter :: inv_type_default = 0
    real(kind=Rkind)   :: epsi_loc


    IF (size(RMat,dim=1) /= size(RMat,dim=2)) THEN
      write(out_unit,*) ' ERROR in QDUtil_inv_OF_RMat_TO'
      write(out_unit,*) ' Rmat is not a square matrix.'
      write(out_unit,*) ' shape(Rmat)',shape(Rmat)
      STOP 'ERROR in QDUtil_inv_OF_RMat_TO: Rmat is not a square matrix'
    END IF

    IF (present(inv_type)) THEN
      inv_type_loc = inv_type
    ELSE
      inv_type_loc = inv_type_default
    END IF
    IF (present(epsi)) THEN
      epsi_loc = epsi
    ELSE
      epsi_loc = ONETENTH**10
    END IF

    n = size(Rmat,dim=1)

    RMat_inv = Identity_Mat(n)
    m1w      = Rmat

    SELECT CASE (inv_type_loc)
    CASE (0) ! ludcmp ...
      allocate(work(n))
      allocate(indx(n))

      CALL QDUtil_ludcmp(m1w,n,work,indx,d)
      DO j=1,n
        CALL QDUtil_lubksb(m1w,n,indx,RMat_inv(:,j))
      END DO

      deallocate(work)
      deallocate(indx)
    CASE (1) ! svd

      allocate(work(n))
      allocate(b(n))
      allocate(vv(n,n))

      CALL QDUtil_SVDCMP(m1w,n,n,work,vv,n)
      ! Find maximum singular value
      !write(out_unit,*) 'SVD : epsi_loc',epsi_loc
      !write(out_unit,*) 'SVD : work',work

      wmax = maxval(work(:))
      wmin = wmax * epsi_loc
      !write(out_unit,*) 'SVD : count non zero',count(work >= wmin)
      ! Zero the "small" singular values
      WHERE (abs(work) < WMIN) work = ZERO

      DO j=1,n
        b(:) = RMat_inv(:,j)
        CALL QDUtil_SVBKSB(m1w,work,vv,n,n,b,RMat_inv(:,j),n)
      END DO

      deallocate(work)
      deallocate(b)
      deallocate(vv)
    CASE Default
      write(out_unit,*) ' ERROR in QDUtil_inv_OF_RMat_TO'
      write(out_unit,*) ' Problem with inv_type',inv_type_loc
      write(out_unit,*) ' Check the Fortran code!'
      STOP 'ERROR in QDUtil_inv_OF_RMat_TO: Problem with inv_type'
    END SELECT

  END FUNCTION QDUtil_inv_OF_RMat_TO
  SUBROUTINE QDUtil_inv_OF_RMat_TO_RMat_inv(RMat,RMat_inv,inv_type,epsi)
    USE QDUtil_NumParameters_m
    IMPLICIT NONE

    real(kind=Rkind), intent(inout), allocatable :: RMat_inv(:,:)
    real(kind=Rkind), intent(in)                 :: RMat(:,:)
    integer,          intent(in),    optional    :: inv_type
    real(kind=Rkind), intent(in),    optional    :: epsi

    integer            :: inv_type_loc
    integer, parameter :: inv_type_default = 0
    real(kind=Rkind)   :: epsi_loc

    IF (present(inv_type)) THEN
      inv_type_loc = inv_type
    ELSE
      inv_type_loc = inv_type_default
    END IF
    IF (present(epsi)) THEN
      epsi_loc = epsi
    ELSE
      epsi_loc = ONETENTH**10
    END IF


    RMat_inv = inv_OF_Mat_TO(Rmat,inv_type_loc,epsi_loc)

  END SUBROUTINE QDUtil_inv_OF_RMat_TO_RMat_inv
  !================================================================
  !   Inversion of a real matrix Cmat : CMat_inv = Cmat^-1
  !   Function and subroutine
  !================================================================
  FUNCTION QDUtil_inv_OF_CMat_TO(CMat,inv_type,epsi) RESULT(CMat_inv)
    USE QDUtil_NumParameters_m
    USE QDUtil_RW_MatVec_m
    IMPLICIT NONE

    complex(kind=Rkind), allocatable            :: CMat_inv(:,:)

    complex(kind=Rkind), intent(in)             :: CMat(:,:)
    integer,             intent(in), optional   :: inv_type
    real(kind=Rkind),    intent(in), optional   :: epsi


    integer,             allocatable :: indx(:)
    complex(kind=Rkind), allocatable :: work(:),m1w(:,:)

    complex(kind=Rkind) :: d
    integer             :: j,n

    integer            :: inv_type_loc
    integer, parameter :: inv_type_default = 0
    real(kind=Rkind)   :: epsi_loc

    logical, parameter :: debug = .FALSE.
    !logical, parameter :: debug = .TRUE.

    IF (size(CMat,dim=1) /= size(CMat,dim=2)) THEN
      write(out_unit,*) ' ERROR in QDUtil_inv_OF_CMat_TO'
      write(out_unit,*) ' Rmat is not a square matrix.'
      write(out_unit,*) ' shape(Rmat)',shape(Cmat)
      STOP 'ERROR in QDUtil_inv_OF_RMat_TO: Cmat is not a square matrix'
    END IF


    IF (present(inv_type)) THEN
      inv_type_loc = inv_type
    ELSE
      inv_type_loc = inv_type_default
    END IF
    IF (present(epsi)) THEN
      epsi_loc = epsi
    ELSE
      epsi_loc = ONETENTH**10
    END IF

    n = size(Cmat,dim=1)

    IF (debug) THEN
      write(out_unit,*) ' BEGINNING QDUtil_inv_OF_CMat_TO'
      write(out_unit,*) ' n',n
      flush(out_unit)
      CALL Write_Mat(CMat,out_unit,5,info='CMat')
      flush(out_unit)
    END IF

    CMat_inv = Identity_Mat(n)

    IF (debug) THEN
      CALL Write_Mat(CMat_inv,out_unit,5,info='Id?')
    END IF

    m1w      = CMat


    SELECT CASE (inv_type_loc)
    CASE (0) ! ludcmp ...
      allocate(work(n))
      allocate(indx(n))

      CALL QDUtil_ludcmp_cplx(m1w,n,work,indx,d)
      DO j=1,n
        CALL QDUtil_lubksb_cplx(m1w,n,indx,CMat_inv(:,j))
      END DO

      deallocate(work)
      deallocate(indx)
    CASE (1) ! svd
        STOP 'SVD not yet in complex'
    CASE Default ! ludcmp ...
      write(out_unit,*) ' ERROR in QDUtil_inv_OF_CMat_TO'
      write(out_unit,*) ' Problem with inv_type',inv_type_loc
      write(out_unit,*) ' Check the Fortran code!'
      STOP 'ERROR in QDUtil_inv_OF_CMat_TO: Problem with inv_type'
    END SELECT

    IF (debug) THEN
      CALL Write_Mat(CMat_inv,out_unit,5,info='CMat_inv')
     write(out_unit,*) ' END QDUtil_inv_OF_CMat_TO'
     flush(out_unit)
   END IF

  END FUNCTION QDUtil_inv_OF_CMat_TO
  SUBROUTINE QDUtil_inv_OF_CMat_TO_CMat_inv(CMat,CMat_inv,inv_type,epsi)
    USE QDUtil_NumParameters_m
    IMPLICIT NONE

    complex(kind=Rkind), allocatable            :: CMat_inv(:,:)

    complex(kind=Rkind), intent(in)             :: CMat(:,:)
    integer,             intent(in), optional   :: inv_type
    real(kind=Rkind),    intent(in), optional   :: epsi

    integer            :: inv_type_loc
    integer, parameter :: inv_type_default = 0
    real(kind=Rkind)   :: epsi_loc

    IF (present(inv_type)) THEN
      inv_type_loc = inv_type
    ELSE
      inv_type_loc = inv_type_default
    END IF
    IF (present(epsi)) THEN
      epsi_loc = epsi
    ELSE
      epsi_loc = ONETENTH**10
    END IF

    CMat_inv = inv_OF_Mat_TO(Cmat,inv_type_loc,epsi_loc)

  END SUBROUTINE QDUtil_inv_OF_CMat_TO_CMat_inv
  !================================================================
  !    Determinant of a matrix: Function
  !================================================================
  FUNCTION QDUtil_Det_OF_RMat(Rmat) RESULT (det)
    USE QDUtil_NumParameters_m, ONLY : Rkind,out_unit
    IMPLICIT NONE

    real(kind=Rkind), intent(in)  :: Rmat(:,:)
    real(kind=Rkind)              :: det

    integer,          allocatable :: Luindex(:)
    real(kind=Rkind), allocatable :: work(:),m1w(:,:)

    real(kind=Rkind) :: d
    integer          :: j,n


    IF (size(Rmat,dim=1) /= size(Rmat,dim=2)) THEN
      write(out_unit,*) ' ERROR in QDUtil_Det_OF_RMat'
      write(out_unit,*) ' Rmat is not a square matrix.'
      write(out_unit,*) ' shape(Rmat)',shape(Rmat)
      STOP 'ERROR in QDUtil_Det_OF_RMat: Rmat is not a square matrix'
    END IF

    m1w = Rmat

    n = size(Rmat,dim=1)
    allocate(Luindex(n))
    allocate(work(n))

    CALL QDUtil_ludcmp(m1w,n,work,Luindex,d)

    det = d
    DO j=1,n
      det = det * m1w(j,j)
    END DO

    deallocate(Luindex)
    deallocate(work)
    deallocate(m1w)

  END FUNCTION QDUtil_Det_OF_RMat
  !================================================================
  !    Determinant of a matrix: Function
  !================================================================
  FUNCTION QDUtil_Det_OF_CMat(Cmat) RESULT (det)
    USE QDUtil_NumParameters_m, ONLY : Rkind,out_unit
    IMPLICIT NONE

    complex(kind=Rkind), intent(in)  :: Cmat(:,:)
    complex(kind=Rkind)              :: det

    integer,             allocatable :: indx(:)
    complex(kind=Rkind), allocatable :: work(:),m1w(:,:)

    complex(kind=Rkind) :: d
    integer          :: j,n


    IF (size(Cmat,dim=1) /= size(Cmat,dim=2)) THEN
      write(out_unit,*) ' ERROR in QDUtil_Det_OF_CMat'
      write(out_unit,*) ' Rmat is not a square matrix.'
      write(out_unit,*) ' shape(Rmat)',shape(Cmat)
      STOP 'ERROR in QDUtil_Det_OF_CMat: Rmat is not a square matrix'
    END IF

    m1w = Cmat

    n = size(Cmat,dim=1)

    allocate(work(n))
    allocate(indx(n))

    CALL QDUtil_ludcmp_cplx(m1w,n,work,indx,d)

    det = d
    DO j=1,n
      det = det * m1w(j,j)
    END DO

    deallocate(work)
    deallocate(indx)

  END FUNCTION QDUtil_Det_OF_CMat
  !================================================================
  !   Solve, RVecL: RMat . RVecL= RVec
  !   Function 
  !================================================================
  !================================================================
  FUNCTION QDUtil_RLinearSystem_Solve(RMat,RVec,LS_type,epsi) RESULT (RVecL)
    USE QDUtil_NumParameters_m
    IMPLICIT NONE

    real(kind=Rkind), allocatable :: RVecL(:)

    real(kind=Rkind), intent(in)             :: RMat(:,:)
    real(kind=Rkind), intent(in)             :: RVec(:)
    integer,          intent(in), optional   :: LS_type
    real(kind=Rkind), intent(in), optional   :: epsi

    ! local variables
    integer ,         allocatable :: indx(:)
    real(kind=Rkind), allocatable :: work(:),m1w(:,:)
    real(kind=Rkind), allocatable :: vv(:,:)
    real(kind=Rkind), allocatable :: b(:)

    real(kind=Rkind) :: wmax,wmin
    real(kind=Rkind) :: d
    integer          :: j,n

    integer            :: LS_type_loc
    integer, parameter :: LS_type_default = 0
    real(kind=Rkind)   :: epsi_loc


    IF (size(RMat,dim=1) /= size(RMat,dim=2)) THEN
      write(out_unit,*) ' ERROR in QDUtil_RLinearSystem_Solve'
      write(out_unit,*) ' Rmat is not a square matrix.'
      write(out_unit,*) ' shape(Rmat)',shape(Rmat)
      STOP 'ERROR in QDUtil_RLinearSystem_Solve: Rmat is not a square matrix'
    END IF
    IF (size(RVec) /= size(RMat,dim=2)) THEN
      write(out_unit,*) ' ERROR in QDUtil_RLinearSystem_Solve'
      write(out_unit,*) ' Incompatible RVec and RMat shapes'
      write(out_unit,*) ' shape(Rmat)',shape(Rmat)
      write(out_unit,*) ' shape(RVec)',shape(RVec)
      STOP 'ERROR in QDUtil_RLinearSystem_Solve: Incompatible RVec and RMat shapes'
    END IF

    IF (present(LS_type)) THEN
      LS_type_loc = LS_type
    ELSE
      LS_type_loc = LS_type_default
    END IF
    IF (present(epsi)) THEN
      epsi_loc = epsi
    ELSE
      epsi_loc = ONETENTH**10
    END IF

    n = size(Rmat,dim=1)

    RVecL = RVec
    m1w  = RMat

    SELECT CASE (LS_type_loc)
    CASE (0) ! ludcmp ...
      allocate(work(n))
      allocate(indx(n))
 
      CALL QDUtil_ludcmp(m1w,n,work,indx,d)
      CALL QDUtil_lubksb(m1w,n,indx,RVecL)
 
      deallocate(work)
      deallocate(indx)
    CASE (1) ! svd
 
      allocate(work(n))
      allocate(b(n))
      allocate(vv(n,n))
 
      CALL QDUtil_SVDCMP(m1w,n,n,work,vv,n)
      ! Find maximum singular value
      !write(out_unit,*) 'SVD : epsi_loc',epsi_loc
      !write(out_unit,*) 'SVD : work',work
 
      wmax = maxval(work)
      wmin = wmax * epsi_loc
      !write(out_unit,*) 'SVD : count non zero',count(work >= wmin)
      ! Zero the "small" singular values
      WHERE (abs(work) < WMIN) work = ZERO
 
      b(:) = RVecL
      CALL QDUtil_SVBKSB(m1w,work,vv,n,n,b,RVecL,n)
 
      deallocate(work)
      deallocate(b)
      deallocate(vv)
    CASE Default
      write(out_unit,*) ' ERROR in QDUtil_RLinearSystem_Solve'
      write(out_unit,*) ' Problem with LS_type',LS_type_loc
      write(out_unit,*) ' Check the Fortran code!'
      STOP 'ERROR in QDUtil_RLinearSystem_Solve: Problem with LS_type_loc'
    END SELECT

  END FUNCTION QDUtil_RLinearSystem_Solve
  FUNCTION QDUtil_CLinearSystem_Solve(CMat,CVec,LS_type,epsi) RESULT (CVecL)
    USE QDUtil_NumParameters_m
    IMPLICIT NONE

    complex(kind=Rkind), allocatable :: CVecL(:)

    complex(kind=Rkind), intent(in)             :: CMat(:,:)
    complex(kind=Rkind), intent(in)             :: CVec(:)
    integer,             intent(in), optional   :: LS_type
    real(kind=Rkind),    intent(in), optional   :: epsi

    ! local variables
    integer ,         allocatable :: indx(:)
    complex(kind=Rkind), allocatable :: work(:),m1w(:,:)
    complex(kind=Rkind), allocatable :: vv(:,:)
    complex(kind=Rkind), allocatable :: b(:)

    real(kind=Rkind) :: wmax,wmin
    complex(kind=Rkind) :: d
    integer          :: j,n

    integer            :: LS_type_loc
    integer, parameter :: LS_type_default = 0
    real(kind=Rkind)   :: epsi_loc


    IF (size(CMat,dim=1) /= size(CMat,dim=2)) THEN
      write(out_unit,*) ' ERROR in QDUtil_CLinearSystem_Solve'
      write(out_unit,*) ' Rmat is not a square matrix.'
      write(out_unit,*) ' shape(CMat)',shape(CMat)
      STOP 'ERROR in QDUtil_CLinearSystem_Solve: CMat is not a square matrix'
    END IF
    IF (size(CVec) /= size(CMat,dim=2)) THEN
      write(out_unit,*) ' ERROR in QDUtil_CLinearSystem_Solve'
      write(out_unit,*) ' Incompatible CVec and CMat shapes'
      write(out_unit,*) ' shape(Cmat)',shape(Cmat)
      write(out_unit,*) ' shape(CVec)',shape(CVec)
      STOP 'ERROR in QDUtil_CLinearSystem_Solve: Incompatible CVec and CMat shapes'
    END IF

    IF (present(LS_type)) THEN
      LS_type_loc = LS_type
    ELSE
      LS_type_loc = LS_type_default
    END IF
    IF (present(epsi)) THEN
      epsi_loc = epsi
    ELSE
      epsi_loc = ONETENTH**10
    END IF

    n = size(Cmat,dim=1)

    CVecL = CVec
    m1w  = CMat

    SELECT CASE (LS_type_loc)
    CASE (0) ! ludcmp ...
      allocate(indx(n))

      CALL QDUtil_Driver_LU_decomp_cplx(m1w,n,indx,d,lu_type=1)
      CALL QDUtil_Driver_LU_solve_cplx(m1w,n,indx,CVecL,lu_type=1)
      !CALL QDUtil_ludcmp_cplx(m1w,n,work,indx,d)
      !CALL QDUtil_lubksb_cplx(m1w,n,indx,CVecL)

      deallocate(indx)
    CASE (3) ! ludcmp ...
      allocate(indx(n))

      CALL QDUtil_Driver_LU_decomp_cplx(m1w,n,indx,d,lu_type=3)
      CALL QDUtil_Driver_LU_solve_cplx(m1w,n,indx,CVecL,lu_type=3)

      deallocate(indx)
    CASE (1) ! svd
        STOP 'SVD not yet in complex'
    CASE Default ! ludcmp ...
      write(out_unit,*) ' ERROR in QDUtil_CLinearSystem_Solve'
      write(out_unit,*) ' Problem with inv_type',LS_type_loc
      write(out_unit,*) ' Check the Fortran code!'
      STOP 'ERROR in QDUtil_CLinearSystem_Solve: Problem with LS_type'
    END SELECT

  END FUNCTION QDUtil_CLinearSystem_Solve
  !================================================================
  !    ameliore la solution d un systeme d equations
  !    par une iteration
  !================================================================
  SUBROUTINE QDUtil_mprove(A,ALUD,N,INDX,B,X)
    USE QDUtil_NumParameters_m
    IMPLICIT NONE

    integer          :: n
    real(kind=Rkind) :: a(n,n)
    real(kind=Rkind) :: alud(n,n)
    real(kind=Rkind) :: b(n),x(n),r(n)
    integer          :: indx(n)
    integer          :: i,j

    DO I=1,N
      R(I) = -B(I) + dot_product(A(I,:),X(:))
    END DO
    CALL QDUtil_LUBKSB(ALUD,N,INDX,R)

    X(:) = X(:) - R(:)

  END SUBROUTINE QDUtil_mprove
  !================================================================
  !    resolution de a*x=b apres la procedure ludcmp
  !================================================================
  SUBROUTINE QDUtil_lubksb(a,n,index,b)
    USE QDUtil_NumParameters_m
    IMPLICIT NONE

    integer,          intent(in)    :: n
    real(kind=Rkind), intent(inout) :: b(n)
    real(kind=Rkind), intent(in)    :: a(n,n)
    integer,          intent(in)    :: index(n)


    real(kind=Rkind)  :: sum
    integer           :: i,j,ii,ll

    ii=0
    DO i=1,n
      ll=index(i)
      sum=b(ll)
      b(ll)=b(i)
      IF (ii /= 0) THEN
         DO j=ii,i-1
           sum=sum-a(i,j)*b(j)
         END DO
      ELSE IF (sum /= ZERO) THEN
        ii=i
      ENDIF
      b(i)=sum
    END DO
    DO i=n,1,-1
      sum=b(i)
      DO j=i+1,n
        sum=sum-a(i,j)*b(j)
      END DO
      b(i)=sum/a(i,i)
    END DO

  END SUBROUTINE QDUtil_lubksb
  !================================================================
  !    decomposition de a=l*u (pour la resolution d un systeme d equations
  !     l matrice triangulaire inferieur
  !     u matrice triangulaire superieur
  !
  !    a l u matrices n*n
  !
  !================================================================
  SUBROUTINE QDUtil_ludcmp(a,n,vv,index,d)
    USE QDUtil_NumParameters_m
    USE QDUtil_RW_MatVec_m
    IMPLICIT NONE

    integer,          intent(in)    :: n
    real(kind=Rkind), intent(inout) :: a(n,n),vv(n)
    integer,          intent(inout) :: index(n)
    real(kind=Rkind), intent(inout) :: d

    real(kind=Rkind), parameter :: tiny = ONETENTH**20

    real(kind=Rkind) :: aamax,sum,dum
    integer           :: i,j,k,imax

    d=ONE
    DO i=1,n
      aamax=ZERO
      DO j=1,n
        IF (abs(a(i,j)) > aamax) aamax=abs(a(i,j))
      END DO
      IF (aamax < tiny) STOP "matrice singuliere"
      vv(i)=ONE/aamax
    END DO


    DO j=1,n

      DO i=1,j-1
        sum=a(i,j)
        DO k=1,i-1
          sum=sum-a(i,k)*a(k,j)
        END DO
        a(i,j)=sum
      END DO

      aamax=ZERO
      imax=0
      DO i=j,n
        sum=a(i,j)
        DO k=1,j-1
          sum=sum-a(i,k)*a(k,j)
        END DO
        a(i,j)=sum
        dum=vv(i)*abs(sum)
        IF (dum >= aamax) THEN
         imax=i
         aamax=dum
        ENDIF
      END DO
      IF (imax ==0) THEN
        write(out_unit,*) ' ERROR in ludcmp'
        write(out_unit,*) ' imax = 0 !!!'
        write(out_unit,*) ' matrix a:'
        CALL Write_Mat(a,out_unit,4)
        STOP
      END IF

      IF (j /= imax) THEN
        DO k=1,n
          dum=a(imax,k)
          a(imax,k)=a(j,k)
          a(j,k)=dum
        END DO
        d=-d
        vv(imax)=vv(j)
      ENDIF

      index(j)=imax
      IF (a(j,j) == ZERO) a(j,j)=tiny
      IF (j /= n) THEN
        dum=ONE/a(j,j)
        DO i=j+1,n
          a(i,j)=a(i,j)*dum
        END DO
      ENDIF

    END DO

  END SUBROUTINE QDUtil_ludcmp

  SUBROUTINE QDUtil_SVDCMP(A,M,N,W,V,max_n)
    USE QDUtil_NumParameters_m
    IMPLICIT NONE

      integer max_n,N,M
      real (kind=Rkind) :: A(max_n,max_n),V(max_n,max_n)
      real (kind=Rkind) :: W(max_n),RV1(max_n)
      real (kind=Rkind) :: G,SCALE,ANORM,S,F,H,C,Y,Z,X

      integer I,K,J,NM,JJ,L,ITS


      G=ZERO
      SCALE=ZERO
      ANORM=ZERO
      DO I=1,N
        L=I+1
        RV1(I)=SCALE*G
        G=ZERO
        S=ZERO
        SCALE=ZERO
        IF (I.LE.M) THEN
          DO K=I,M
            SCALE=SCALE+ABS(A(K,I))
          END DO
          IF (SCALE.NE.ZERO) THEN
            DO K=I,M
              A(K,I)=A(K,I)/SCALE
              S=S+A(K,I)*A(K,I)
            END DO
            F=A(I,I)
            G=-SIGN(sqrt(S),F)
            H=F*G-S
            A(I,I)=F-G
            IF (I.NE.N) THEN
              DO J=L,N
                S=ZERO
                DO K=I,M
                  S=S+A(K,I)*A(K,J)
                END DO
                F=S/H
                DO K=I,M
                  A(K,J)=A(K,J)+F*A(K,I)
                END DO
              END DO
            ENDIF
            DO K= I,M
              A(K,I)=SCALE*A(K,I)
            END DO
          ENDIF
        ENDIF
        W(I)=SCALE *G
        G=ZERO
        S=ZERO
        SCALE=ZERO
        IF ((I.LE.M).AND.(I.NE.N)) THEN
          DO K=L,N
            SCALE=SCALE+ABS(A(I,K))
          END DO
          IF (SCALE.NE.ZERO) THEN
            DO K=L,N
              A(I,K)=A(I,K)/SCALE
              S=S+A(I,K)*A(I,K)
            END DO
            F=A(I,L)
            G=-SIGN(sqrt(S),F)
            H=F*G-S
            A(I,L)=F-G
            DO K=L,N
              RV1(K)=A(I,K)/H
            END DO
            IF (I.NE.M) THEN
              DO J=L,M
                S=ZERO
                DO K=L,N
                  S=S+A(J,K)*A(I,K)
                END DO
                DO K=L,N
                  A(J,K)=A(J,K)+S*RV1(K)
                END DO
              END DO
            ENDIF
            DO K=L,N
              A(I,K)=SCALE*A(I,K)
            END DO
          ENDIF
        ENDIF
        ANORM=MAX(ANORM,(ABS(W(I))+ABS(RV1(I))))
      END DO
      DO I=N,1,-1
        IF (I.LT.N) THEN
          IF (G.NE.ZERO) THEN
            DO J=L,N
              V(J,I)=(A(I,J)/A(I,L))/G
            END DO
            DO J=L,N
              S=ZERO
              DO K=L,N
                S=S+A(I,K)*V(K,J)
              END DO
              DO K=L,N
                V(K,J)=V(K,J)+S*V(K,I)
              END DO
            END DO
          ENDIF
          DO J=L,N
            V(I,J)=ZERO
            V(J,I)=ZERO
          END DO
        ENDIF
        V(I,I)=ONE
        G=RV1(I)
        L=I
      END DO
      DO I=N,1,-1
        L=I+1
        G=W(I)
        IF (I.LT.N) THEN
          DO J=L,N
            A(I,J)=ZERO
          END DO
        ENDIF
        IF (G.NE.ZERO) THEN
          G=ONE/G
          IF (I.NE.N) THEN
            DO J=L,N
              S=ZERO
              DO K=L,M
                S=S+A(K,I)*A(K,J)
              END DO
              F=(S/A(I,I))*G
              DO K=I,M
                A(K,J)=A(K,J)+F*A(K,I)
              END DO
            END DO
          ENDIF
          DO J=I,M
            A(J,I)=A(J,I)*G
          END DO
        ELSE
          DO J= I,M
            A(J,I)=ZERO
          END DO
        ENDIF
        A(I,I)=A(I,I)+ONE
      END DO
      DO 49 K=N,1,-1
        DO 48 ITS=1,30
          DO L=K,1,-1
            NM=L-1
            IF ((ABS(RV1(L))+ANORM) == ANORM)  GO TO 2
            IF ((ABS(W(NM))+ANORM) == ANORM)  GO TO 1
          END DO
1         C=ZERO
          S=ONE
          DO I=L,K
            F=S*RV1(I)
            IF ((ABS(F)+ANORM) /= ANORM) THEN
              G=W(I)
              H=sqrt(F*F+G*G)
              W(I)=H
              H=ONE/H
              C= (G*H)
              S=-(F*H)
              DO J=1,M
                Y=A(J,NM)
                Z=A(J,I)
                A(J,NM)=(Y*C)+(Z*S)
                A(J,I)=-(Y*S)+(Z*C)
              END DO
            ENDIF
          END DO
2         Z=W(K)
          IF (L.EQ.K) THEN
            IF (Z < ZERO) THEN
              W(K)=-Z
              DO J=1,N
                V(J,K)=-V(J,K) 
              END DO
            ENDIF
            GO TO 3
          ENDIF
          IF (ITS.EQ.50) STOP 'No convergence in 50 iterations'
          X=W(L)
          NM=K-1
          Y=W(NM)
          G=RV1(NM)
          H=RV1(K)
          F=((Y-Z)*(Y+Z)+(G-H)*(G+H))/(TWO*H*Y)
          G=sqrt(F*F+ONE)
          F=((X-Z)*(X+Z)+H*((Y/(F+SIGN(G,F)))-H))/X
          C=ONE
          S=ONE
          DO J=L,NM
            I=J+1
            G=RV1(I)
            Y=W(I)
            H=S*G
            G=C*G
            Z=sqrt(F*F+H*H)
            RV1(J)=Z
            C=F/Z
            S=H/Z
            F= (X*C)+(G*S)
            G=-(X*S)+(G*C)
            H=Y*S
            Y=Y*C
            DO JJ=1,N
              X=V(JJ,J)
              Z=V(JJ,I)
              V(JJ,J)= (X*C)+(Z*S)
              V(JJ,I)=-(X*S)+(Z*C)
            END DO
            Z=sqrt(F*F+H*H)
            W(J)=Z
            IF (Z.NE.ZERO) THEN
              Z=ONE/Z
              C=F*Z
              S=H*Z
            ENDIF
            F= (C*G)+(S*Y)
            X=-(S*G)+(C*Y)
            DO JJ=1,M
              Y=A(JJ,J)
              Z=A(JJ,I)
              A(JJ,J)= (Y*C)+(Z*S)
              A(JJ,I)=-(Y*S)+(Z*C)
            END DO
          END DO
          RV1(L)=ZERO
          RV1(K)=F
          W(K)=X
48      CONTINUE
3       CONTINUE
49    CONTINUE
      RETURN
    END SUBROUTINE QDUtil_SVDCMP


      SUBROUTINE QDUtil_SVBKSB(U,W,V,M,N,B,X,max_n)
      USE QDUtil_NumParameters_m
      IMPLICIT NONE

      integer max_n,M,N
      real (kind=Rkind) :: U(max_n,max_n),V(max_n,max_n)
      real (kind=Rkind) :: W(max_n),B(max_n),X(max_n),TMP(max_n)
      real (kind=Rkind) :: s
      integer I,J,JJ

      DO 12 J=1,N
        S=ZERO
        IF(W(J).NE.ZERO)THEN
          DO 11 I=1,M
            S=S+U(I,J)*B(I)
11        CONTINUE
          S=S/W(J)
        ENDIF
        TMP(J)=S
12    CONTINUE
      DO 14 J=1,N
        S=ZERO
        DO 13 JJ=1,N
          S=S+V(J,JJ)*TMP(JJ)
13      CONTINUE
        X(J)=S
14    CONTINUE
      RETURN

    END SUBROUTINE QDUtil_SVBKSB
!================================================================
!    resolution de a*x=b apres la procedure ludcmp
!================================================================
  SUBROUTINE QDUtil_Driver_LU_solve_cplx(a,n,LU_index,b,lu_type)
  USE, intrinsic :: ISO_FORTRAN_ENV, ONLY : real64,int32
  USE QDUtil_NumParameters_m
  IMPLICIT NONE

  integer,             intent(in)    :: n,lu_type
  complex(kind=Rkind), intent(inout) :: b(n)
  complex(kind=Rkind), intent(in)    :: a(n,n)
  integer,             intent(in)    :: LU_index(n)

  integer               :: err,lu_type_loc
  integer, parameter    :: lu_type_default = 1
  integer(kind=int32)   :: n4,ierr4



    lu_type_loc = lu_type

    !when lapack is used and Rkind /= real64 (not a double)
    IF (Rkind /= real64 .AND. lu_type_loc == 3) lu_type_loc = lu_type_default

#if __LAPACK == 0
    IF (lu_type_loc == 3) lu_type_loc = lu_type_default
#endif

    SELECT CASE (lu_type)
    CASE(1) ! ori
      CALL QDUtil_lubksb_cplx(a,n,LU_index,b)
    CASE(3) ! lapack
#if __LAPACK == 1
      n4     = int(n,kind=int32)
      CALL ZGETRS('No transpose',n4,1,a,n4,LU_index,b,n4,ierr4)
      err = int(ierr4)
      IF (err /= 0) STOP 'LU QDUtil_Driver_LU_solve_cplx'
#elif __LAPACK == 8
      n4     = int(n,kind=int32)
      CALL ZGETRS('No transpose',n,1,a,n,LU_index,b,n,err)
      IF (err /= 0) STOP 'LU QDUtil_Driver_LU_solve_cplx'
#else
      write(out_unit,*) ' ERROR in QDUtil_Driver_LU_solve_cplx'
      write(out_unit,*) '  LAPACK is not linked (LAPACK=0 in the makefile).'
      write(out_unit,*) '  The program should not reach the LAPACK case.'
      write(out_unit,*) '  => Probabely, wrong type_diag_default.'
      write(out_unit,*) '  => CHECK the fortran!!'
      STOP 'ERROR in QDUtil_Driver_LU_solve_cplx: LAPACK case impossible'
#endif
    CASE Default
      CALL QDUtil_lubksb_cplx(a,n,LU_index,b)
    END SELECT

  END SUBROUTINE QDUtil_Driver_LU_solve_cplx
  SUBROUTINE QDUtil_Driver_LU_decomp_cplx(a,n,LU_index,d,lu_type)
  USE, intrinsic :: ISO_FORTRAN_ENV, ONLY : real64,int32
  USE QDUtil_NumParameters_m
  IMPLICIT NONE

  integer,             intent(in)    :: n,lu_type
  complex(kind=Rkind), intent(inout) :: d,a(n,n)
  integer,             intent(inout) :: LU_index(n)

  integer               :: err,lu_type_loc
  integer, parameter    :: lu_type_default = 1
  integer(kind=int32)   :: n4,ierr4
  complex(kind=Rkind), allocatable :: work(:)



  lu_type_loc = lu_type

    !when lapack is used and Rkind /= real64 (not a double)
    IF (Rkind /= real64 .AND. lu_type_loc == 3) lu_type_loc = lu_type_default

#if __LAPACK == 0
    IF ( lu_type_loc == 3) lu_type_loc = lu_type_default
#endif

    SELECT CASE (lu_type)
    CASE(1) ! ori
      allocate(work(n))
      CALL QDUtil_ludcmp_cplx(a,n,work,LU_index,d)
      deallocate(work)
    CASE(3) ! lapack
#if __LAPACK == 1
      n4     = int(n,kind=int32)
      CALL ZGETRF(n4,n4,a,n4,LU_index,ierr4)
      err = int(ierr4)
      IF (err /= 0) STOP 'QDUtil_Driver_LU_decomp_cplx'
#elif __LAPACK == 8
      CALL ZGETRF(n,n,a,n,LU_index,err)
      IF (err /= 0) STOP 'QDUtil_Driver_LU_decomp_cplx'
#else
      write(out_unit,*) ' ERROR in QDUtil_Driver_LU_decomp_cplx'
      write(out_unit,*) '  LAPACK is not linked (LAPACK=0 in the makefile).'
      write(out_unit,*) '  The program should not reach the LAPACK case.'
      write(out_unit,*) '  => Probabely, wrong lu_type_default.'
      write(out_unit,*) '  => CHECK the fortran!!'
      STOP 'ERROR in QDUtil_Driver_LU_decomp_cplx: LAPACK case impossible'
#endif
    CASE Default
      allocate(work(n))
      CALL QDUtil_ludcmp_cplx(a,n,work,LU_index,d)
      deallocate(work)
    END SELECT

  END SUBROUTINE QDUtil_Driver_LU_decomp_cplx
  SUBROUTINE QDUtil_lubksb_cplx(a,n,index,b)
      USE QDUtil_NumParameters_m
      IMPLICIT NONE

       integer n
       complex(kind=Rkind) a(n,n),b(n)
       integer index(n)
       complex(kind=Rkind)  sum

       integer i,j,ii,ll

       ii=0
       DO 12 i=1,n
         ll=index(i)
         sum=b(ll)
         b(ll)=b(i)
         IF (II .NE. 0) THEN
            DO 11 j=ii,i-1
              sum=sum-a(i,j)*b(j)
 11         CONTINUE
         ELSE IF (abs(sum) .NE. ZERO) THEN
                ii=i
              ENDIF
         b(i)=sum
 12    CONTINUE
       DO 14 i=n,1,-1
         sum=b(i)
         DO 13 j=i+1,n
           sum=sum-a(i,j)*b(j)
 13      CONTINUE
         b(i)=sum/a(i,i)
 14    CONTINUE

       RETURN
     end subroutine QDUtil_lubksb_cplx
!================================================================
!    decomposition de a=l*u (pour la resolution d un systeme d equations
!     l matrice triangulaire inferieur
!     u matrice triangulaire superieur
!
!    a l u matrices n*n
!
!================================================================

      SUBROUTINE QDUtil_ludcmp_cplx(a,n,vv,index,d)
      USE QDUtil_NumParameters_m
      IMPLICIT NONE

       integer n
       real(kind=Rkind)   tiny
       parameter (tiny=ONETENTH**20)
       complex(kind=Rkind) a(n,n),vv(n)
       complex(kind=Rkind) aamax,sum,dum,d
       integer index(n)

       integer i,j,k,imax

       d=CONE
       DO 12 i=1,n
        aamax=CZERO
        DO 11 j=1,n
          IF (abs(a(i,j)) .GT. abs(aamax)) aamax=cmplx(abs(a(i,j)),kind=Rkind)
 11     CONTINUE
        IF (abs(aamax) < tiny) STOP "matrice singuliere"
        vv(i)=CONE/aamax
 12    CONTINUE


       DO 19 j=1,n

        DO 14 i=1,j-1
         sum=a(i,j)
         DO 13 k=1,i-1
          sum=sum-a(i,k)*a(k,j)
 13      CONTINUE
         a(i,j)=sum
 14     CONTINUE

        aamax=CZERO
        imax = 0
        DO 16 i=j,n
         sum=a(i,j)
         DO 15 k=1,j-1
          sum=sum-a(i,k)*a(k,j)
 15      CONTINUE
         a(i,j)=sum
         dum=vv(i)*cmplx(abs(sum),kind=Rkind)
         IF (abs(dum) .GE. abs(aamax)) THEN
           imax=i
           aamax=dum
         ENDIF
 16     CONTINUE

        IF (j .NE. imax) THEN
          DO 17 k=1,n
           dum=a(imax,k)
           a(imax,k)=a(j,k)
           a(j,k)=dum
 17       CONTINUE
          d=-d
          vv(imax)=vv(j)
        ENDIF

        index(j)=imax
        IF (abs(a(j,j)) .EQ. ZERO) a(j,j)=cmplx(tiny,kind=Rkind)
        IF (j .NE. n) THEN
          dum=CONE/a(j,j)
          DO 18 i=j+1,n
            a(i,j)=a(i,j)*dum
 18       CONTINUE
        ENDIF

 19    CONTINUE


       RETURN
       end subroutine QDUtil_ludcmp_cplx


  !=====================================================================
  !
  ! ++   A = Id =>  A(i,i)=ONE
  !      A : square matrix
  !
  !=====================================================================
  FUNCTION QDUtil_Identity_RMat(n) RESULT(RMat)
    USE QDUtil_NumParameters_m, ONLY : Rkind,ZERO,ONE
    IMPLICIT NONE

    integer,          intent(in)  :: n
    real(kind=Rkind)              :: RMat(n,n)

    integer           :: i

    RMat(:,:) = ZERO
    DO i=1,n
     RMat(i,i) = ONE
    END DO

  END FUNCTION QDUtil_Identity_RMat
  FUNCTION QDUtil_Identity_CMat(n) RESULT(CMat)
    USE QDUtil_NumParameters_m, ONLY : Rkind,CZERO,CONE
    IMPLICIT NONE

    integer,          intent(in)  :: n
    complex(kind=Rkind)           :: CMat(n,n)

    integer           :: i

    CMat(:,:) = CZERO
    DO i=1,n
      CMat(i,i) = CONE
    END DO

  END FUNCTION QDUtil_Identity_CMat

  FUNCTION QDUtil_Ortho_GramSchmidt_RMat(V,epsi) RESULT(Vortho)
    USE QDUtil_NumParameters_m
    IMPLICIT NONE
    real(kind=Rkind), allocatable                        :: Vortho(:,:)
    real(kind=Rkind),             intent(in)             :: V(:,:)
    real(kind=Rkind),             intent(in), optional   :: epsi

    integer          :: i,j,n,k
    real(kind=Rkind) :: Norm

    real(kind=Rkind)   :: epsi_loc

    IF (present(epsi)) THEN
      epsi_loc = epsi
    ELSE
      epsi_loc = ONETENTH**10
    END IF

    n = size(V,dim=1)
    k = size(V,dim=2)

    allocate(Vortho(n,k))
    Vortho = ZERO

    Norm   = sqrt(dot_product(V(:,1),V(:,1)))
    Vortho(:,1) = V(:,1)/Norm

    DO i=2,k
      Vortho(:,i) = V(:,i)
      DO j=1,i-1
        Vortho(:,i) = Vortho(:,i) - dot_product(Vortho(:,j),Vortho(:,i))*Vortho(:,j)
      END DO
      Norm   = sqrt(dot_product(Vortho(:,i),Vortho(:,i)))
      IF (Norm > epsi_loc) THEN 
        Vortho(:,i) = Vortho(:,i) / Norm
      ELSE
        Vortho(:,i) = ZERO
      END IF
    END DO

  END FUNCTION QDUtil_Ortho_GramSchmidt_RMat
  FUNCTION QDUtil_Ortho_GramSchmidt_CMat(V,epsi) RESULT(Vortho)
    USE QDUtil_NumParameters_m
    IMPLICIT NONE
    complex(kind=Rkind), allocatable                        :: Vortho(:,:)
    complex(kind=Rkind),             intent(in)             :: V(:,:)
    real(kind=Rkind),                intent(in), optional   :: epsi

    integer          :: i,j,n,k
    real(kind=Rkind) :: Norm

    real(kind=Rkind)   :: epsi_loc

    IF (present(epsi)) THEN
      epsi_loc = epsi
    ELSE
      epsi_loc = ONETENTH**10
    END IF

    n = size(V,dim=1)
    k = size(V,dim=2)

    allocate(Vortho(n,k))
    Vortho = CZERO

    Norm   = sqrt(real(dot_product(V(:,1),V(:,1)),kind=Rkind))
    Vortho(:,1) = V(:,1)/Norm

    DO i=2,k

      Vortho(:,i) = V(:,i)
      DO j=1,i-1
        Vortho(:,i) = Vortho(:,i) - dot_product(Vortho(:,j),Vortho(:,i))*Vortho(:,j)
      END DO

      Norm   = sqrt(real(dot_product(Vortho(:,i),Vortho(:,i)),kind=Rkind))

      IF (Norm > epsi_loc) THEN 
        Vortho(:,i) = Vortho(:,i) / Norm
      ELSE
        Vortho(:,i) = ZERO
      END IF

    END DO

  END FUNCTION QDUtil_Ortho_GramSchmidt_CMat
  SUBROUTINE Test_QDUtil_Matrix()
    USE QDUtil_Test_m
    USE QDUtil_NumParameters_m
    USE QDUtil_RW_MatVec_m
    IMPLICIT NONE

    TYPE (test_t)                    :: test_var
    logical                          :: res_test
    real (kind=Rkind),   parameter   :: ZeroTresh    = ONETENTH**10

    integer                          :: io,ioerr
    real(kind=Rkind),    allocatable :: R1Mat(:,:),R1Vec(:),R11Mat(:,:)
    complex(kind=Rkind), allocatable :: C1Mat(:,:),C1Vec(:),C11Mat(:,:)
    real(kind=Rkind),    allocatable :: R2Mat(:,:),R2Vec(:)

    complex(kind=Rkind), allocatable :: C2Mat(:,:),C2Vec(:)
    real(kind=Rkind),    allocatable :: R3Mat(:,:),R3Vec(:)
    complex(kind=Rkind), allocatable :: C3Mat(:,:),C3Vec(:)

    !====================================================================
    ! Tests for the identity matrix
    !
    ! define the matrices
    R1Mat = reshape([ONE,ZERO,ZERO,                              &
                     ZERO,ONE,ZERO,                              &
                     ZERO,ZERO,ONE],shape=[3,3])
    R2Mat = Identity_Mat(3)

    C1Mat = reshape([CONE,CZERO,CZERO,                              &
                     CZERO,CONE,CZERO,                              &
                     CZERO,CZERO,CONE],shape=[3,3])
    C2Mat = Identity_Mat(3)

    ! tests
    CALL Initialize_Test(test_var,test_name='Matrix')


    res_test = all(abs(R1Mat-R2Mat) < ZeroTresh)
    CALL Logical_Test(test_var,test1=res_test,info='Identity_RMat')

    res_test = all(abs(C1Mat-C2Mat) < ZeroTresh)
    CALL Logical_Test(test_var,test1=res_test,info='Identity_CMat')

    CALL Flush_Test(test_var)
    !====================================================================

    !====================================================================
    ! test for the determinant
    !
    ! define the matrices
    R1Mat = reshape([ONE,HALF,ZERO,                             &
                     HALF,ONE,HALF,                             &
                     ZERO,HALF,ONE],shape=[3,3])

    C1Mat = EYE * R1Mat

    res_test = (abs(Det_OF(R1Mat)-HALF) < ZeroTresh)
    CALL Logical_Test(test_var,test1=res_test,info='Determinant of RMat')
    IF (.NOT. res_test) write(out_unit,*) 'Det',Det_OF(R1Mat),HALF


    res_test = (abs(Det_OF(C1Mat)-(-EYE*HALF)) < ZeroTresh)
    CALL Logical_Test(test_var,test1=res_test,info='Determinant of CMat')
    IF (.NOT. res_test) write(out_unit,*) 'Det',Det_OF(C1Mat),EYE*HALF

    CALL Flush_Test(test_var)
    !====================================================================


    !====================================================================
    ! test for the inversion
    !
    ! define the matrices
    R1Mat = reshape([ONE,HALF,ZERO,                             &
                     HALF,ONE,HALF,                             &
                     ZERO,HALF,ONE],shape=[3,3])
    R2Mat = reshape([THREE,-TWO,ONE,                            &
                     -TWO,FOUR,-TWO,                            &
                     ONE,-TWO,THREE],shape=[3,3]) * HALF

    C1Mat =  EYE * R1Mat
    C2Mat = -EYE * R2Mat


    res_test = all(abs(inv_OF_Mat_TO(R1Mat)-R2Mat) < ZeroTresh)
    CALL Logical_Test(test_var,test1=res_test,info='Inversion of R1Mat')
    IF (.NOT. res_test) THEN
      CALL Write_Mat(R1Mat,out_unit,5,info='R1Mat')
      CALL Write_Mat(R2Mat,out_unit,5,info='R2Mat')
      CALL Write_Mat(matmul(R1Mat,R2Mat),out_unit,5,info='Id?')
    END IF
    CALL Flush_Test(test_var)

    res_test = all(abs(inv_OF_Mat_TO(R1Mat,inv_type=0)-R2Mat) < ZeroTresh)
    CALL Logical_Test(test_var,test1=res_test,info='Inversion (#0) of R1Mat')
    IF (.NOT. res_test) THEN
      CALL Write_Mat(R1Mat,out_unit,5,info='R1Mat')
      CALL Write_Mat(R2Mat,out_unit,5,info='R2Mat')
      CALL Write_Mat(matmul(R1Mat,R2Mat),out_unit,5,info='Id?')
    END IF
    CALL Flush_Test(test_var)

    res_test = all(abs(inv_OF_Mat_TO(R1Mat,inv_type=1)-R2Mat) < ZeroTresh)
    CALL Logical_Test(test_var,test1=res_test,info='Inversion (#1) of R1Mat')
    IF (.NOT. res_test) THEN
      CALL Write_Mat(R1Mat,out_unit,5,info='R1Mat')
      CALL Write_Mat(R2Mat,out_unit,5,info='R2Mat')
      CALL Write_Mat(matmul(R1Mat,R2Mat),out_unit,5,info='Id?')
    END IF
    CALL Flush_Test(test_var)


    res_test = all(abs(inv_OF_Mat_TO(C1Mat)-C2Mat) < ZeroTresh)
    CALL Logical_Test(test_var,test1=res_test,info='Inversion of CMat')
    CALL Flush_Test(test_var)
    IF (.NOT. res_test) THEN
      CALL Write_Mat(C1Mat,out_unit,5,info='C1Mat')
      CALL Write_Mat(C2Mat,out_unit,5,info='C2Mat')
      CALL Write_Mat(matmul(C1Mat,C2Mat),out_unit,5,info='Id?')
    END IF
    res_test = all(abs(inv_OF_Mat_TO(C1Mat,inv_type=0)-C2Mat) < ZeroTresh)
    CALL Logical_Test(test_var,test1=res_test,info='Inversion (#0) of CMat')
    CALL Flush_Test(test_var)
    IF (.NOT. res_test) THEN
      CALL Write_Mat(C1Mat,out_unit,5,info='C1Mat')
      CALL Write_Mat(C2Mat,out_unit,5,info='C2Mat')
      CALL Write_Mat(matmul(C1Mat,C2Mat),out_unit,5,info='Id?')
    END IF

    CALL inv_OF_Mat_TO_Mat_inv(R1Mat,R11Mat,0,ZERO)
    res_test = all(abs(R11Mat-R2Mat) < ZeroTresh)
    CALL Logical_Test(test_var,test1=res_test,info='Inversion of R1Mat (#0 sub)')
    IF (.NOT. res_test) THEN
      CALL Write_Mat(R1Mat,out_unit,5,info='R1Mat')
      CALL Write_Mat(R2Mat,out_unit,5,info='R2Mat')
      CALL Write_Mat(matmul(R1Mat,R2Mat),out_unit,5,info='Id?')
    END IF
    CALL inv_OF_Mat_TO_Mat_inv(C1Mat,C11Mat,0,ZERO)
    res_test = all(abs(C11Mat-C2Mat) < ZeroTresh)
    CALL Logical_Test(test_var,test1=res_test,info='Inversion of C1Mat (#0 sub)')
    IF (.NOT. res_test) THEN
      CALL Write_Mat(C1Mat,out_unit,5,info='C1Mat')
      CALL Write_Mat(C2Mat,out_unit,5,info='C2Mat')
      CALL Write_Mat(matmul(C1Mat,C2Mat),out_unit,5,info='Id?')
    END IF
    CALL Flush_Test(test_var)

    !====================================================================


    !====================================================================
    ! test for solving a system of linear equations
    !
    ! define the matrices
    R1Mat = reshape([ONE,HALF,ZERO,                             &
                     HALF,ONE,HALF,                             &
                     ZERO,HALF,ONE],shape=[3,3])
    R2Mat = reshape([THREE,-TWO,ONE,                            &
                     -TWO,FOUR,-TWO,                            &
                     ONE,-TWO,THREE],shape=[3,3]) * HALF

    C1Mat =  EYE * R1Mat
    C2Mat = -EYE * R2Mat
    R1Vec = [ONE,ONE,ONE]
    C1Vec = [CONE,CONE,CONE]

    R2Vec = LinearSys_Solve(R1Mat,R1Vec)
    res_test = all(abs(matmul(R1Mat,R2Vec)-R1Vec) < ZeroTresh)
    CALL Logical_Test(test_var,test1=res_test,info='LinearSys_Solve of R1Mat')
    IF (.NOT. res_test) THEN
      CALL Write_Mat(R1Mat,out_unit,5,info='R1Mat')
      CALL Write_Vec(R1Vec,out_unit,5,info='R1Vec')
      CALL Write_Vec(R2Vec,out_unit,5,info='R2Vec')
      CALL Write_Vec(matmul(R1Mat,R2Vec)-R1Vec,out_unit,5,info='Error')
    END IF
    CALL Flush_Test(test_var)

    R2Vec = LinearSys_Solve(R1Mat,R1Vec,LS_type=0)
    res_test = all(abs(matmul(R1Mat,R2Vec)-R1Vec) < ZeroTresh)
    CALL Logical_Test(test_var,test1=res_test,info='LinearSys_Solve (#0) of R1Mat')
    IF (.NOT. res_test) THEN
      CALL Write_Mat(R1Mat,out_unit,5,info='R1Mat')
      CALL Write_Vec(R1Vec,out_unit,5,info='R1Vec')
      CALL Write_Vec(R2Vec,out_unit,5,info='R2Vec')
      CALL Write_Vec(matmul(R1Mat,R2Vec)-R1Vec,out_unit,5,info='Error')
    END IF
    CALL Flush_Test(test_var)

    R2Vec = LinearSys_Solve(R1Mat,R1Vec,LS_type=0)
    res_test = all(abs(matmul(R1Mat,R2Vec)-R1Vec) < ZeroTresh)
    CALL Logical_Test(test_var,test1=res_test,info='LinearSys_Solve (#1) of R1Mat')
    IF (.NOT. res_test) THEN
      CALL Write_Mat(R1Mat,out_unit,5,info='R1Mat')
      CALL Write_Vec(R1Vec,out_unit,5,info='R1Vec')
      CALL Write_Vec(R2Vec,out_unit,5,info='R2Vec')
      CALL Write_Vec(matmul(R1Mat,R2Vec)-R1Vec,out_unit,5,info='Error')
    END IF
    CALL Flush_Test(test_var)

    C2Vec = LinearSys_Solve(C1Mat,C1Vec)
    res_test = all(abs(matmul(C1Mat,C2Vec)-C1Vec) < ZeroTresh)
    CALL Logical_Test(test_var,test1=res_test,info='LinearSys_Solve of C1Mat')
    IF (.NOT. res_test) THEN
      CALL Write_Mat(R1Mat,out_unit,5,info='C1Mat')
      CALL Write_Vec(R1Vec,out_unit,5,info='C1Vec')
      CALL Write_Vec(R2Vec,out_unit,5,info='C2Vec')
      CALL Write_Vec(matmul(C1Mat,C2Vec)-C1Vec,out_unit,5,info='Error')
    END IF
    CALL Flush_Test(test_var)

    C2Vec = LinearSys_Solve(C1Mat,C1Vec,LS_type=0)
    res_test = all(abs(matmul(C1Mat,C2Vec)-C1Vec) < ZeroTresh)
    CALL Logical_Test(test_var,test1=res_test,info='LinearSys_Solve (#0) of C1Mat')
    IF (.NOT. res_test) THEN
      CALL Write_Mat(R1Mat,out_unit,5,info='C1Mat')
      CALL Write_Vec(R1Vec,out_unit,5,info='C1Vec')
      CALL Write_Vec(R2Vec,out_unit,5,info='C2Vec')
      CALL Write_Vec(matmul(C1Mat,C2Vec)-C1Vec,out_unit,5,info='Error')
    END IF
    CALL Flush_Test(test_var)
#if __LAPACK != 0
    C2Vec = LinearSys_Solve(C1Mat,C1Vec,LS_type=3)
    res_test = all(abs(matmul(C1Mat,C2Vec)-C1Vec) < ZeroTresh)
    CALL Logical_Test(test_var,test1=res_test,info='LinearSys_Solve (#3) of C1Mat')
    IF (.NOT. res_test) THEN
      CALL Write_Mat(R1Mat,out_unit,5,info='C1Mat')
      CALL Write_Vec(R1Vec,out_unit,5,info='C1Vec')
      CALL Write_Vec(R2Vec,out_unit,5,info='C2Vec')
      CALL Write_Vec(matmul(C1Mat,C2Vec)-C1Vec,out_unit,5,info='Error')
    END IF
    CALL Flush_Test(test_var)
#endif
    !====================================================================

    !====================================================================
    ! test for solving a system of linear equations
    !
    ! define the matrices
    R1Mat = reshape([ONE,HALF,ZERO,                             &
                     HALF,ONE,HALF,                             &
                     ZERO,HALF,ONE],shape=[3,3])
    R2Mat = reshape([TWO/sqrt(FIVE),ONE/sqrt(FIVE),ZERO,        &
                    -THREE/sqrt(70._Rkind),sqrt(18._Rkind/35._Rkind),sqrt(FIVE/14._Rkind),                            &
                    sqrt(ONE/14._Rkind),-sqrt(TWO/SEVEN),THREE/sqrt(14._Rkind)],shape=[3,3])
    !{{2/Sqrt[5], 1/Sqrt[5], 0}, {-(3/Sqrt[70]), 3 Sqrt[2/35], Sqrt[5/14]}, {1/Sqrt[14], -Sqrt[(2/7)], 3/Sqrt[14]}}
    !atomic_add
    R3Mat = Ortho_GramSchmidt(R1Mat)
    res_test = all(abs(R2Mat-R3Mat) < ZeroTresh)
    CALL Logical_Test(test_var,test1=res_test,info='GramSmidt ortho of R1Mat')
    !CALL Write_Mat(R2Mat,out_unit,5,info='R2Mat')
    !CALL Write_Mat(R3Mat,out_unit,5,info='R3Mat')
    IF (.NOT. res_test) THEN
      CALL Write_Mat(R2Mat,out_unit,5,info='R2Mat')
      CALL Write_Mat(R3Mat,out_unit,5,info='R3Mat')
    END IF
    CALL Flush_Test(test_var)

    ! define the matrices
    C1Mat = reshape([CONE,CHALF,CZERO,                             &
                     CHALF,CONE,CHALF,                             &
                     CZERO,CHALF,EYE],shape=[3,3])
    C2Mat = reshape([CTWO/sqrt(FIVE),CONE/sqrt(FIVE),CZERO,       &
                    cmplx(-THREE/sqrt(70._Rkind),kind=Rkind),     &
                    cmplx(sqrt(18._Rkind/35._Rkind),kind=Rkind),  &
                    cmplx(sqrt(FIVE/14._Rkind),kind=Rkind),       &
                    cmplx(-ONE,THREE,kind=Rkind)/sqrt(140._Rkind), &
                    cmplx(ONE,-THREE,kind=Rkind)/sqrt(35._Rkind), &
                    cmplx(-THREE,NINE,kind=Rkind)/sqrt(140._Rkind)],shape=[3,3])
    !atomic_add
    !CALL Write_Mat(C1Mat,out_unit,5,info='C1Mat')

    C3Mat = Ortho_GramSchmidt(C1Mat)
    res_test = all(abs(C2Mat-C3Mat) < ZeroTresh)
    CALL Logical_Test(test_var,test1=res_test,info='GramSmidt ortho of C1Mat')
    !CALL Write_Mat(C2Mat,out_unit,5,info='C2Mat')
    !CALL Write_Mat(C3Mat,out_unit,5,info='C3Mat')
    IF (.NOT. res_test) THEN
      CALL Write_Mat(C2Mat,out_unit,5,info='C2Mat')
      CALL Write_Mat(C3Mat,out_unit,5,info='C3Mat')
    END IF
    CALL Flush_Test(test_var)
!====================================================================

    ! finalize the tests
    CALL Finalize_Test(test_var)
  END SUBROUTINE Test_QDUtil_Matrix
END MODULE QDUtil_Matrix_m

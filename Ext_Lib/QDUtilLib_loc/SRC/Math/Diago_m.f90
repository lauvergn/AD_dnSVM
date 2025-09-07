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
MODULE QDUtil_diago_m
  IMPLICIT NONE

  PRIVATE

  PUBLIC diagonalization
  INTERFACE diagonalization
    MODULE PROCEDURE QDUtil_Rdiagonalization,QDUtil_Cdiagonalization,QDUtil_Cdiagonalization_Her
  END INTERFACE

  PUBLIC :: Test_QDUtil_Diago

  CONTAINS
  RECURSIVE SUBROUTINE QDUtil_Cdiagonalization(CMat,CEigVal,CEigVec,nb_diago,diago_type,sort,phase)
    USE, intrinsic :: ISO_FORTRAN_ENV, ONLY : real64,int32
    USE QDUtil_NumParameters_m
    IMPLICIT NONE

    complex(kind=Rkind), intent(in)              :: CMat(:,:)
    complex(kind=Rkind), intent(inout)           :: CEigVal(:),CEigVec(:,:)
    integer,          intent(in),       optional :: nb_diago ! when nb_diago < size(REigVal), 
                                                             !only nb_diago eigenvavlues and  eigenREigVectors are calculated

    integer,          intent(in),       optional :: diago_type,sort
    logical,          intent(in),       optional :: phase


    !local variables
    integer            :: diago_type_loc
    integer            :: diago_type_default = 2 ! tred+tql
    !                                   tred+tql
    integer, parameter :: list_type(*) = [2]

    complex(kind=Rkind), allocatable :: CMat_save(:,:)
    integer :: n_size,n_vect,n

    integer              :: i,ierr
    complex(kind=Rkind), allocatable :: work(:)


    !----- for debuging --------------------------------------------------
    character (len=*), parameter :: name_sub='QDUtil_Cdiagonalization'
    logical, parameter :: debug = .FALSE.
    !logical, parameter :: debug = .TRUE.
    !-----------------------------------------------------------

    n_size = size(CEigVal)
    IF (present(nb_diago)) THEN
      n = nb_diago
    ELSE
      n = n_size
    END IF

    IF (n_size /= size(CMat,dim=1) .OR. n_size /= size(CEigVec,dim=1)) THEN
      write(out_unit,*) ' ERROR in ',name_sub
      write(out_unit,*) ' The matrix or eigenvector sizes are not consistant'
      write(out_unit,*) '   size(REigVal):  ',size(CEigVal)
      write(out_unit,*) '   size(RMat):     ',size(CMat,dim=1)
      write(out_unit,*) '   size(REigVec):  ',size(CEigVec,dim=1)
      write(out_unit,*) '  => CHECK the fortran!!'
      STOP 'ERROR in QDUtil_Cdiagonalization: The matrix or eigenvector sizes are not consistant.'
    END IF
    IF (n < 1) THEN
      write(out_unit,*) ' ERROR in ',name_sub
      write(out_unit,"(a,i0,a)") ' n < 1. It MUST be in the range [1,',n_size,']'
      write(out_unit,*) '   n:              ',n
      write(out_unit,*) '   size(CEigVal):  ',size(CEigVal)
      write(out_unit,*) '  => CHECK the fortran!!'
      STOP 'ERROR in QDUtil_Cdiagonalization:  n < 1.'
    END IF
    n_vect = min(n,n_size)

    IF (present(diago_type)) THEN
      diago_type_loc = diago_type
    ELSE
      diago_type_loc = diago_type_default
    END IF

    IF (count(list_type == diago_type_loc) == 0) THEN
      write(out_unit,*) ' ERROR in ',name_sub
      write(out_unit,*) ' diago_type is out-of-range.'
      write(out_unit,*) '   diago_type:      ',diago_type_loc
      write(out_unit,*) '   Possible values:',list_type(:)
      write(out_unit,*) '  => CHECK the fortran!!'
      STOP 'ERROR in QDUtil_Cdiagonalization: diago_type is out-of-range.'
    END IF


    SELECT CASE (diago_type_loc)
    CASE (2) ! tred+tql
      IF (debug) write(out_unit,*) 'tred+tql: complex symetric'
      allocate(work(n))

      CMat_save = CMat

      CALL QDUtil_cTred2(n,n,CMat_save,CEigVal,work,CEigVec)
      CALL QDUtil_cTql2(n,n,CEigVal,work,CEigVec,ierr)
      IF (debug) write(out_unit,*)'ierr=',ierr

      deallocate(work)
      deallocate(CMat_save)

    CASE DEFAULT
      write(out_unit,*) ' ERROR in ',name_sub
      write(out_unit,*) ' The default CASE is not defined.'
      write(out_unit,*) '  => CHECK the fortran!!'
      STOP 'ERROR in QDUtil_Cdiagonalization: default case impossible'
    END SELECT

  END SUBROUTINE QDUtil_Cdiagonalization
!============================================================
!
!   Driver for the diagonalization
!      Default: tred2+tql2 (diago_type=2)
!            Other possibilities: Jacobi (diago_type=1) or Lapack (diago_type=3)
!      Sort: the eigenvalues/eigenvectors:
!            sort=1:  ascending (default)
!            sort=-1: descending
!            sort=2:  ascending on the absolute eigenvalues
!     phase:
!============================================================
!
  RECURSIVE SUBROUTINE QDUtil_Rdiagonalization(RMat,REigVal,REigVec,nb_diago,diago_type,sort,phase,IEigVec)
    USE, intrinsic :: ISO_FORTRAN_ENV, ONLY : real64,int32
    USE QDUtil_NumParameters_m
    IMPLICIT NONE

    real(kind=Rkind), intent(in)              :: RMat(:,:)
    real(kind=Rkind), intent(inout)           :: REigVal(:),REigVec(:,:)
    integer,          intent(in),    optional :: nb_diago ! when nb_diago < size(REigVal), 
                                                          !only nb_diago eigenvavlues and  eigenREigVectors are calculated
    real(kind=Rkind), intent(inout), optional :: IEigVec(:)

    integer,          intent(in),    optional :: diago_type,sort
    logical,          intent(in),    optional :: phase


    !local variables
    integer            :: diago_type_loc
    integer            :: diago_type_default = 2 ! tred+tql
    !                                    Jacobi tred+tql DSYEV  DGEEV Lanczos
    integer, parameter :: list_type(7) = [1,    2,       3,377, 4,477,   5]

    real(kind=Rkind), allocatable :: RMat_save(:,:)
    integer :: n_size,n_vect,n

    !for lapack
    integer              :: i
    integer              :: lwork ,lda ,ldvr ,ierr
    integer(kind=int32)  :: n4,lwork4,lda4,ldvr4,ierr4
    real(kind=Rkind), allocatable :: work(:)
    real(kind=Rkind), allocatable :: IEigVec_loc(:)

    real(kind=Rkind) :: dummy(1,1)

    !----- for debuging --------------------------------------------------
    character (len=*), parameter :: name_sub='QDUtil_Rdiagonalization'
    logical, parameter :: debug = .FALSE.
    !logical, parameter :: debug = .TRUE.
    !-----------------------------------------------------------

    n_size = size(REigVal)
    IF (present(nb_diago)) THEN
      n = nb_diago
    ELSE
      n = n_size
    END IF

    IF (n_size /= size(RMat,dim=1) .OR. n_size /= size(REigVec,dim=1)) THEN
      write(out_unit,*) ' ERROR in ',name_sub
      write(out_unit,*) ' The matrix or eigenvector sizes are not consistant'
      write(out_unit,*) '   size(REigVal):  ',size(REigVal)
      write(out_unit,*) '   size(RMat):     ',size(RMat,dim=1)
      write(out_unit,*) '   size(REigVec):  ',size(REigVec,dim=1)
      write(out_unit,*) '  => CHECK the fortran!!'
      STOP 'ERROR in QDUtil_Rdiagonalization: The matrix or eigenvector sizes are not consistant.'
    END IF
    IF (n < 1) THEN
      write(out_unit,*) ' ERROR in ',name_sub
      write(out_unit,"(a,i0,a)") ' n < 1. It MUST be in the range [1,',n_size,']'
      write(out_unit,*) '   n:              ',n
      write(out_unit,*) '   size(REigVal):  ',size(REigVal)
      write(out_unit,*) '  => CHECK the fortran!!'
      STOP 'ERROR in QDUtil_Rdiagonalization:  n < 1.'
    END IF
    n_vect = min(n,n_size)

    IF (present(diago_type)) THEN
      diago_type_loc = diago_type
    ELSE
      diago_type_loc = diago_type_default
    END IF

    !when lapack is used and Rkind /= real64 (not a double)
    IF (Rkind /= real64 .AND. diago_type_loc == 3) diago_type_loc = diago_type_default

#if __LAPACK == 0
    IF (debug) write(out_unit,*) '  Lapack library is not linked'
    IF (count([3,377,395] == diago_type_loc) == 1) diago_type_loc = diago_type_default
    IF (count([4,477] == diago_type_loc) == 1) THEN
      !diago_type_loc = 0
      write(out_unit,*) ' ERROR in ',name_sub
      write(out_unit,*) ' The diagonalization of non-symmetric needs LAPACK.'
      write(out_unit,*) '  Try to link LAPACK with the code (use LAPACK=1 in the makefile).'
      write(out_unit,*) '   diago_type:      ',diago_type_loc
      write(out_unit,*) '  => CHECK the fortran!!'
      STOP 'ERROR in QDUtil_Rdiagonalization: Problem with non-symmetric matrix.'
    END IF
#else
    IF (debug) write(out_unit,*) '  Lapack library is linked'
#endif

    IF (count(list_type == diago_type_loc) == 0) THEN
      write(out_unit,*) ' ERROR in ',name_sub
      write(out_unit,*) ' diago_type is out-of-range.'
      write(out_unit,*) '   diago_type:      ',diago_type_loc
      write(out_unit,*) '   Possible values:',list_type(:)
      write(out_unit,*) '  => CHECK the fortran!!'
      STOP 'ERROR in QDUtil_Rdiagonalization: diago_type is out-of-range.'
    END IF


    SELECT CASE (diago_type_loc)
    CASE(1) ! jacobi
      IF (debug) write(out_unit,*) 'Jacobi (symmetric)'
      RMat_save = RMat ! save RMat

      CALL QDUtil_JACOBI2(RMat_save,n,REigVal,REigVec)

      deallocate(RMat_save)
    CASE (2) ! tred+tql
      IF (debug) write(out_unit,*) 'tred+tql, new version (symmetric)'
      allocate(work(n))

      REigVec = RMat
      CALL QDUtil_TRED2_EISPACK(REigVec,n,n,REigVal,work)
      CALL QDUtil_TQLI_EISPACK(REigVal,work,n,n,REigVec)

      deallocate(work)
    CASE(3,377) ! lapack77
      IF (debug) write(out_unit,*) 'lapack77: DSYEV (symmetric)'

#if __LAPACK == 1
      lwork = 3*n-1
      allocate(work(lwork))
      REigVec(:,:) = RMat(:,:)

      ! lapack subroutines need integer (kind=4 or int32), therefore, we add a conversion, otherwise
      ! it fails when integers (kind=8 or int64) are used (at the compilation).
      n4     = int(n,kind=int32)
      lwork4 = int(lwork,kind=int32)
      CALL DSYEV('V','U',n4,REigVec,n4,REigVal,work,lwork4,ierr4)

      IF (debug) write(out_unit,*) 'ierr=',ierr4
      flush(out_unit)

      IF (ierr4 /= 0_int32) THEN
         write(out_unit,*) ' ERROR in ',name_sub
         write(out_unit,*) ' DSYEV lapack subroutine has FAILED!'
         STOP 'ERROR in QDUtil_Rdiagonalization: DSYEV lapack subroutine has FAILED!'
      END IF

      deallocate(work)
#elif __LAPACK == 8
      lwork = 3*n-1
      allocate(work(lwork))
      REigVec(:,:) = RMat(:,:)

      ! with mkl (ifort), lapack lib can linked with -i8 option (int64), so that we don't need to convert integer in int32
      CALL DSYEV('V','U',n,REigVec,n,REigVal,work,lwork,ierr)

      IF (debug) write(out_unit,*) 'ierr=',ierr
      flush(out_unit)

      IF (ierr /= 0) THEN
         write(out_unit,*) ' ERROR in ',name_sub
         write(out_unit,*) ' DSYEV lapack subroutine has FAILED!'
         STOP 'ERROR in QDUtil_Rdiagonalization: DSYEV lapack subroutine has FAILED!'
      END IF

      deallocate(work)
#else
      write(out_unit,*) ' ERROR in ',name_sub
      write(out_unit,*) '  LAPACK is not linked (LAPACK=0 in the makefile).'
      write(out_unit,*) '  The program should not reach the LAPACK case.'
      write(out_unit,*) '  => Probabely, wrong diago_type_default.'
      write(out_unit,*) '  => CHECK the fortran!!'
      STOP 'ERROR in QDUtil_Rdiagonalization: LAPACK case impossible'
#endif


    CASE(4,477) ! lapack77 (non-symmetric)
#if __LAPACK == 1
      IF (debug) write(out_unit,*) 'lapack77: DGEEV (non-symmetric)'
      flush(out_unit)

      RMat_save = RMat ! save RMat


      lwork = (2+64)*n
      ldvr  = n
      lda   = n

      allocate(work(lwork))
      allocate(IEigVec_loc(n))

      n4     = int(n,kind=int32)
      lwork4 = int(lwork,kind=int32)
      lda4   = int(lda,kind=int32)
      ldvr4  = int(ldvr,kind=int32)

      CALL DGEEV('N','V',n4,RMat_save,lda4,REigVal,IEigVec_loc,dummy,          &
                 int(1,kind=int32),REigVec,ldvr4,work,lwork4,ierr4)
      IF (debug) write(out_unit,*)'ierr=',ierr4
      IF (ierr4 /= 0_int32) THEN
        write(out_unit,*) ' ERROR in ',name_sub
        write(out_unit,*) ' DGEEV lapack subroutine has FAILED!'
        STOP 'ERROR in QDUtil_Rdiagonalization: DGEEV lapack subroutine has FAILED!'
      END IF

      DO i=1,n
        write(out_unit,*) 'Eigenvalue(', i, ') = ', REigVal(i),'+I ',IEigVec_loc(i)
      END DO
      IF (present(IEigVec)) IEigVec = IEigVec_loc

      deallocate(IEigVec_loc)
      deallocate(work)
      deallocate(RMat_save)
#elif __LAPACK == 8
      IF (debug) write(out_unit,*) 'lapack77: DGEEV (non-symmetric)'
      flush(out_unit)

      RMat_save = RMat ! save RMat

      lwork = (2+64)*n
      ldvr  = n
      lda   = n

      allocate(work(lwork))
      allocate(IEigVec_loc(n))

      CALL DGEEV('N','V',n,RMat_save,lda,REigVal,IEigVec_loc,dummy,1,REigVec,ldvr,work,lwork,ierr)
      IF (debug) write(out_unit,*)'ierr=',ierr
      IF (ierr /= 0) THEN
        write(out_unit,*) ' ERROR in ',name_sub
        write(out_unit,*) ' DGEEV lapack subroutine has FAILED!'
        STOP 'ERROR in QDUtil_Rdiagonalization: DGEEV lapack subroutine has FAILED!'
      END IF

      DO i=1,n
        write(out_unit,*) 'Eigenvalue(', i, ') = ', REigVal(i),'+I ',IEigVec_loc(i)
      END DO
      IF (present(IEigVec)) IEigVec = IEigVec_loc

      deallocate(IEigVec_loc)
      deallocate(work)
      deallocate(RMat_save)
#else
      write(out_unit,*) ' ERROR in ',name_sub
      write(out_unit,*) '  LAPACK is not linked (LAPACK=0 in the makefile).'
      write(out_unit,*) '  The program should not reach the LAPACK case.'
      write(out_unit,*) '  => Probabely, wrong diago_type_default.'
      write(out_unit,*) '  => CHECK the fortran!!'
      STOP 'ERROR in QDUtil_Rdiagonalization: LAPACK case impossible'
#endif

    CASE(5) ! lanczos

      CALL QDUtil_Lanczos(RMat,n_vect,REigVal,REigVec,epsi=ONETENTH**6,max_it=100)

    CASE DEFAULT
      write(out_unit,*) ' ERROR in ',name_sub
      write(out_unit,*) ' The default CASE is not defined.'
      write(out_unit,*) '  => CHECK the fortran!!'
      STOP 'ERROR in QDUtil_Rdiagonalization: default case impossible'
    END SELECT


    IF (present(sort)) THEN
        SELECT CASE (sort)
        CASE(1)
          CALL QDUtil_sort(REigVal,REigVec)
          CALL QDUtil_rota_denerated(REigVal,REigVec)
        CASE(-1)
          REigVal = -REigVal
          CALL QDUtil_sort(REigVal,REigVec)
          REigVal = -REigVal
          CALL QDUtil_rota_denerated(REigVal,REigVec)
        CASE(2)
          CALL QDUtil_sort_abs(REigVal,REigVec)
        CASE DEFAULT ! no sort
          CONTINUE
        END SELECT
    ELSE
      CALL QDUtil_sort(REigVal,REigVec)
      CALL QDUtil_rota_denerated(REigVal,REigVec)
    END IF

    IF (present(phase)) THEN
      IF (phase) CALL QDUtil_Unique_phase(REigVec)
    ELSE
      CALL QDUtil_Unique_phase(REigVec)
    END IF

  END SUBROUTINE QDUtil_Rdiagonalization


  SUBROUTINE QDUtil_Cdiagonalization_Her(Mat,Eig,Vec,diago_type,sort,phase)
    USE QDUtil_NumParameters_m
    IMPLICIT NONE

    complex(kind=Rkind), intent(in)              :: Mat(:,:)
    complex(kind=Rkind), intent(inout)           :: Vec(:,:)
    real(kind=Rkind),    intent(inout)           :: Eig(:)

    integer,             intent(in),  optional   :: diago_type,sort
    logical,             intent(in),  optional   :: phase


    integer            :: n,n_size,n_vect
    integer            :: diago_type_loc

    integer            :: diago_type_default = 3 ! lapack

    integer          :: ierr
    integer          :: i,lwork
    complex(kind=Rkind), allocatable :: work(:),saveMat(:,:)
    real(kind=Rkind),    allocatable :: rwork(:)

    integer(kind=Ik4)  :: n4,lwork4,ierr4


!----- for debuging --------------------------------------------------
    character (len=*), parameter :: name_sub='QDUtil_Cdiagonalization_Her'
    logical, parameter :: debug = .FALSE.
!      logical, parameter :: debug = .TRUE.
!-----------------------------------------------------------


    n_size = size(Eig)
    n = n_size

    IF (n_size /= size(Mat,dim=1) .OR. n_size /= size(Vec,dim=1)) THEN
      write(out_unit,*) ' ERROR in ',name_sub
      write(out_unit,*) ' The matrix or eigenvector sizes are not consistant'
      write(out_unit,*) '   size(Eig):  ',size(Eig)
      write(out_unit,*) '   size(Mat):  ',size(Mat,dim=1)
      write(out_unit,*) '   size(Vec):  ',size(Vec,dim=1)
      write(out_unit,*) '  => CHECK the fortran!!'
      STOP 'ERROR in QDUtil_Rdiagonalization: The matrix or eigenvector sizes are not consistant.'
    END IF
    IF (n < 1) THEN
      write(out_unit,*) ' ERROR in ',name_sub
      write(out_unit,"(a,i0,a)") ' n < 1. It MUST be in the range [1,',n_size,']'
      write(out_unit,*) '   n:              ',n
      write(out_unit,*) '   size(REigVal):  ',size(Eig)
      write(out_unit,*) '  => CHECK the fortran!!'
      STOP 'ERROR in QDUtil_Rdiagonalization:  n < 1.'
    END IF
    n_vect = min(n,n_size)

    IF (present(diago_type)) THEN
      diago_type_loc = diago_type
    ELSE
      diago_type_loc = diago_type_default
    END IF

    SELECT CASE (diago_type_loc)
    CASE(1)
      stop ' type_diag=1 not yet in QDUtil_Cdiagonalization_Her'
    CASE(2)
      stop ' type_diag=2 not yet in QDUtil_Cdiagonalization_Her'
    CASE(3) ! lapack77

#if __LAPACK == 1
      lwork = (5+1)*n ! max(1,2*n-1)

      allocate(work(lwork))
      allocate(rwork(max(1, 3*n-2)))


      Vec(:,:) = Mat(:,:)

      ! lapack subroutines need integer (kind=int32 or 4), therefore, we add a conversion, otherwise
      ! it fails when integers (kind=int64 or 8) are used (at the compilation).
      n4     = int(n,kind=Ik4)
      lwork4 = int(lwork,kind=Ik4)
      CALL ZHEEV('V','U',n4,Vec,n4,Eig, work,lwork4, rwork, ierr4)

      IF (debug) write(out_unit,*)'ierr=',ierr4
      IF (ierr4 /= 0_Ik4) THEN
         write(out_unit,*) ' ERROR in ',name_sub
         write(out_unit,*) ' ZHEEV lapack subroutine has FAILED!'
         STOP
      END IF

      deallocate(work)
      deallocate(rwork)

#else
      stop ' no lapack in QDUtil_Cdiagonalization_Her'

#endif

    CASE DEFAULT
      stop ' no default in QDUtil_Cdiagonalization_Her'
    END SELECT


    SELECT CASE (sort)
    CASE(1)
      CALL QDUtil_sort_VecCplx_EneR(Eig,Vec)
    CASE(-1)
      Eig = -Eig
      CALL QDUtil_sort_VecCplx_EneR(Eig,Vec)
      Eig = -Eig
    CASE(2)
      CALL QDUtil_sort_abs_VecCplx_EneR(Eig,Vec)
    CASE DEFAULT ! no sort
      CONTINUE
    END SELECT

    DO i=1,n
      Vec(:,i) = Vec(:,i)/sqrt(dot_product(Vec(:,i),Vec(:,i)))
    END DO

    IF (present(phase)) THEN
      IF (phase) CALL QDUtil_Unique_phase_cplx(Vec)
    ELSE
      CALL QDUtil_Unique_phase_cplx(Vec)
    END IF

  END SUBROUTINE QDUtil_Cdiagonalization_Her
  SUBROUTINE QDUtil_JACOBI2(A,N,D,V)
      USE QDUtil_NumParameters_m
      IMPLICIT NONE

      integer            :: N
      real(kind=Rkind)   :: A(N,N),V(N,N),D(N)


      integer, parameter :: max_it = 500
      real(kind=Rkind)   :: B(N),Z(N)


      real(kind=Rkind)   :: h,t,g,sm,tresh,tau,s,theta,c

      integer            :: i,j,iq,nrot,ip

!     V(:,:) = Id(:,:)
      V(:,:) = ZERO
      DO IP=1,N
        V(IP,IP)=ONE
      END DO

!     initialization
      DO IP=1,N
        B(IP)=A(IP,IP)
        D(IP)=B(IP)
        Z(IP)=ZERO
      END DO

      NROT=0
      DO I=1,max_it ! main loop

        ! SM value
        SM = ZERO
        DO IP=1,N-1
          DO IQ=IP+1,N
            SM = SM+abs(A(IP,IQ))
          END DO
        END DO
        IF(SM == ZERO) RETURN

        ! TRESH value
        IF(I < 4)THEN
          TRESH = TWOTENTHS*SM/N**2
        ELSE
          TRESH = ZERO
        ENDIF

        DO IP=1,N-1
          DO IQ=IP+1,N
            G = HUNDRED*abs(A(IP,IQ))
            IF ( I > 4 .AND. ABS(D(IP))+G == ABS(D(IP))                 &
               .AND. ABS(D(IQ))+G == ABS(D(IQ)) ) THEN
              A(IP,IQ)=ZERO
            ELSE IF ( ABS(A(IP,IQ)) > TRESH ) THEN
              H=D(IQ)-D(IP)
              IF ( ABS(H)+G == ABS(H) ) THEN
                T=A(IP,IQ)/H
              ELSE
                THETA=HALF*H/A(IP,IQ)
                T=ONE/(ABS(THETA)+sqrt(ONE+THETA**2))
                IF ( THETA < ZERO) T=-T
              ENDIF
              C=ONE/sqrt(ONE+T**2)
              S=T*C
              TAU=S/(ONE+C)

              H=T*A(IP,IQ)

              Z(IP)=Z(IP)-H
              Z(IQ)=Z(IQ)+H
              D(IP)=D(IP)-H
              D(IQ)=D(IQ)+H
              A(IP,IQ)=ZERO
              DO J=1,IP-1
                G=A(J,IP)
                H=A(J,IQ)
                A(J,IP)=G-S*(H+G*TAU)
                A(J,IQ)=H+S*(G-H*TAU)
              END DO
              DO J=IP+1,IQ-1
                G=A(IP,J)
                H=A(J,IQ)
                A(IP,J)=G-S*(H+G*TAU)
                A(J,IQ)=H+S*(G-H*TAU)
              END DO
              DO J=IQ+1,N
                G=A(IP,J)
                H=A(IQ,J)
                A(IP,J)=G-S*(H+G*TAU)
                A(IQ,J)=H+S*(G-H*TAU)
              END DO
              DO J=1,N
                G=V(J,IP)
                H=V(J,IQ)
                V(J,IP)=G-S*(H+G*TAU)
                V(J,IQ)=H+S*(G-H*TAU)
              END DO
              NROT=NROT+1
            ENDIF
          END DO
        END DO

        DO IP=1,N
          B(IP)=B(IP)+Z(IP)
          D(IP)=B(IP)
          Z(IP)=ZERO
        END DO

      END DO ! end main loop

      write(out_unit,*) max_it,' iterations should never happen'
      STOP

  end subroutine QDUtil_JACOBI2
!
!============================================================
!
!   diagonalisation trigonalisation puis diagonalisation
!
!============================================================
!
  SUBROUTINE QDUtil_TRED2_EISPACK(A,N,NP,D,E)
      USE QDUtil_NumParameters_m
      IMPLICIT NONE

      integer          :: N,NP
      real(kind=Rkind) :: A(NP,NP),D(NP),E(NP)

      !local variables
      integer          :: I,J,K,L
      real(kind=Rkind) :: F,G,H,HH,SCALE

      IF(N > 1)THEN
        DO 18 I=N,2,-1
          L=I-1
          H=ZERO
          SCALE=ZERO
          IF(L > 1)THEN
            DO 11 K=1,L
              SCALE=SCALE+ABS(A(I,K))
11          CONTINUE
            IF(SCALE  ==  ZERO)THEN
              E(I)=A(I,L)
            ELSE
              DO 12 K=1,L
                A(I,K)=A(I,K)/SCALE
                H=H+A(I,K)**2
12            CONTINUE
              F=A(I,L)
              G=-SIGN(SQRT(H),F)
              E(I)=SCALE*G
              H=H-F*G
              A(I,L)=F-G
              F=ZERO
              DO 15 J=1,L
                A(J,I)=A(I,J)/H
                G=ZERO
                DO 13 K=1,J
                  G=G+A(J,K)*A(I,K)
13              CONTINUE
                IF(L > J)THEN
                  DO 14 K=J+1,L
                    G=G+A(K,J)*A(I,K)
14                CONTINUE
                ENDIF
                E(J)=G/H
                F=F+E(J)*A(I,J)
15            CONTINUE
              HH=F/(H+H)
              DO 17 J=1,L
                F=A(I,J)
                G=E(J)-HH*F
                E(J)=G
                DO 16 K=1,J
                  A(J,K)=A(J,K)-F*E(K)-G*A(I,K)
16              CONTINUE
17            CONTINUE
            ENDIF
          ELSE
            E(I)=A(I,L)
          ENDIF
          D(I)=H
18      CONTINUE
      ENDIF
      D(1)=ZERO
      E(1)=ZERO
      DO 23 I=1,N
        L=I-1
        IF(D(I) /=  ZERO)THEN
          DO 21 J=1,L
            G=ZERO
            DO 19 K=1,L
              G=G+A(I,K)*A(K,J)
19          CONTINUE
            DO 20 K=1,L
              A(K,J)=A(K,J)-G*A(K,I)
20          CONTINUE
21        CONTINUE
        ENDIF
        D(I)=A(I,I)
        A(I,I)=ONE
        IF(L >= 1)THEN
          DO 22 J=1,L
            A(I,J)=ZERO
            A(J,I)=ZERO
22        CONTINUE
        ENDIF
23    CONTINUE
      RETURN
  END SUBROUTINE QDUtil_TRED2_EISPACK

  SUBROUTINE QDUtil_TQLI_EISPACK(D,E,N,NP,Z)
      USE QDUtil_NumParameters_m
      IMPLICIT NONE

      integer          :: N,NP
      real(kind=Rkind) :: D(NP),E(NP),Z(NP,NP)

      !local variables
      integer          :: I,K,L,M,ITER
      real(kind=Rkind) :: G,R,S,C,P,F,B,DD

      IF (N > 1) THEN
        DO I=2,N
          E(I-1)=E(I)
        END DO
        E(N)=ZERO
        DO 15 L=1,N
          ITER=0
1         DO 12 M=L,N-1
            DD=ABS(D(M))+ABS(D(M+1))
            IF (ABS(E(M))+DD == DD) GO TO 2
12        CONTINUE
          M=N
2         IF(M /= L)THEN
            IF(ITER == 30) STOP 'too many iterations'
            ITER=ITER+1
            G=(D(L+1)-D(L))/(TWO*E(L))
            R=SQRT(G**2+ONE)
            G=D(M)-D(L)+E(L)/(G+SIGN(R,G))
            S=ONE
            C=ONE
            P=ZERO
            DO 14 I=M-1,L,-1
              F=S*E(I)
              B=C*E(I)
              IF(ABS(F) >= ABS(G))THEN
                C=G/F
                R=SQRT(C**2+ONE)
                E(I+1)=F*R
                S=ONE/R
                C=C*S
              ELSE
                S=F/G
                R=SQRT(S**2+ONE)
                E(I+1)=G*R
                C=ONE/R
                S=S*C
              ENDIF
              G=D(I+1)-P
              R=(D(I)-G)*S+TWO*C*B
              P=S*R
              D(I+1)=G+P
              G=C*R-B
              DO 13 K=1,N
                F=Z(K,I+1)
                Z(K,I+1)=S*Z(K,I)+C*F
                Z(K,I)=C*Z(K,I)-S*F
13            CONTINUE
14          CONTINUE
            D(L)=D(L)-P
            E(L)=G
            E(M)=ZERO
            GO TO 1
          ENDIF
15      CONTINUE
      ENDIF
      RETURN
  END SUBROUTINE QDUtil_TQLI_EISPACK

!
!============================================================
!
!   Sort the eigenvalues and the eigenvectors
!
!============================================================
!
  SUBROUTINE QDUtil_sort_tab(nb_niv,ene,max_niv)
      USE QDUtil_NumParameters_m
      IMPLICIT NONE

      integer nb_niv,max_niv
      real(kind=Rkind) ene(max_niv)
      real(kind=Rkind) a

        integer i,j,k

      DO i=1,nb_niv
      DO j=i+1,nb_niv
       IF (ene(i)  >  ene(j)) THEN
!             permutation
          a=ene(i)
          ene(i)=ene(j)
          ene(j)=a
        END IF
      END DO
      END DO

  end subroutine QDUtil_sort_tab

  SUBROUTINE QDUtil_sort(ene,psi)
      USE QDUtil_NumParameters_m
      IMPLICIT NONE

      real(kind=Rkind), intent(inout) :: ene(:),psi(:,:)

      real(kind=Rkind) :: a
      integer          :: i,j,k

      DO i=1,size(ene)
      DO j=i+1,size(ene)
       IF (ene(i) > ene(j)) THEN
!	      permutation
          a=ene(i)
          ene(i)=ene(j)
          ene(j)=a
          DO k=1,size(psi,dim=1)
            a=psi(k,i)
            psi(k,i)=psi(k,j)
            psi(k,j)=a
          END DO
        END IF
      END DO
      END DO

  END SUBROUTINE QDUtil_sort
  SUBROUTINE QDUtil_sort_abs(ene,psi)
      USE QDUtil_NumParameters_m
      IMPLICIT NONE

      real(kind=Rkind), intent(inout) :: ene(:),psi(:,:)

      real(kind=Rkind) :: a
      integer          :: i,j,k


        DO i=1,size(ene)
          DO j=i+1,size(ene)
            IF (abs(ene(i)) > abs(ene(j))) THEN
!             permutation
              a=ene(i)
              ene(i)=ene(j)
              ene(j)=a
              DO k=1,size(psi,dim=1)
                a=psi(k,i)
                psi(k,i)=psi(k,j)
                psi(k,j)=a
              END DO
            END IF
          END DO
        END DO

  end subroutine QDUtil_sort_abs
  SUBROUTINE QDUtil_sort_VecCplx_EneR(ene,psi)
    USE QDUtil_NumParameters_m
    IMPLICIT NONE

    real(kind=Rkind),    intent(inout) :: ene(:)
    complex(kind=Rkind), intent(inout) :: psi(:,:)

    real(kind=Rkind)    :: a
    complex(kind=Rkind) :: ca
    integer             :: i,j,k


    DO i=1,size(ene)
    DO j=i+1,size(ene)
     IF (ene(i) > ene(j)) THEN
        ! permutation
        a=ene(i)
        ene(i)=ene(j)
        ene(j)=a
        DO k=1,size(ene)
          ca=psi(k,i)
          psi(k,i)=psi(k,j)
          psi(k,j)=ca
        END DO
      END IF
    END DO
    END DO

END SUBROUTINE QDUtil_sort_VecCplx_EneR
SUBROUTINE QDUtil_sort_abs_VecCplx_EneR(ene,psi)
  USE QDUtil_NumParameters_m
  IMPLICIT NONE

  real(kind=Rkind),    intent(inout) :: ene(:)
  complex(kind=Rkind), intent(inout) :: psi(:,:)

  real(kind=Rkind)    :: a
  complex(kind=Rkind) :: ca
  integer             :: i,j,k


  DO i=1,size(ene)
  DO j=i+1,size(ene)
   IF (abs(ene(i)) > abs(ene(j))) THEN
      ! permutation
      a=ene(i)
      ene(i)=ene(j)
      ene(j)=a
      DO k=1,size(ene)
        ca=psi(k,i)
        psi(k,i)=psi(k,j)
        psi(k,j)=ca
      END DO
    END IF
  END DO
  END DO

END SUBROUTINE QDUtil_sort_abs_VecCplx_EneR
!
!============================================================
!
!   Change the phase of Vec(:,i) shuch its largest coeficient is positive
!
!============================================================
!
  SUBROUTINE QDUtil_Unique_phase(Vec)
      USE QDUtil_NumParameters_m
      IMPLICIT NONE

      real(kind=Rkind), intent(inout) :: Vec(:,:)

      integer          :: i,jloc

      DO i=lbound(Vec,dim=2),ubound(Vec,dim=2)
        jloc           = maxloc(abs(Vec(:,i)),dim=1)
        IF (abs(Vec(jloc,i)) < ONETENTH**6 ) CYCLE
        IF (Vec(jloc,i) < ZERO) Vec(:,i) = -Vec(:,i)
      END DO

  END SUBROUTINE QDUtil_Unique_phase
  SUBROUTINE QDUtil_Unique_phase_cplx(Vec)
    USE QDUtil_NumParameters_m
    IMPLICIT NONE
  
    complex(kind=Rkind), intent(inout) :: Vec(:,:)

        real(kind=Rkind)    :: max_val
        complex(kind=Rkind) :: val
        integer             :: i,j

        DO i=lbound(Vec,dim=2),ubound(Vec,dim=2)
          max_val        = ZERO
          DO j=lbound(Vec,dim=1),ubound(Vec,dim=1)
            IF (abs(Vec(j,i)) > max_val) THEN
              max_val = abs(Vec(j,i))
              val     = Vec(j,i)
            END IF
          END DO
          val = conjg(val/max_val)
          IF (max_val > ZERO) THEN
            Vec(:,i) = Vec(:,i)*val
          END IF
        END DO
  
  END SUBROUTINE QDUtil_Unique_phase_cplx
!=====================================================================
!
!   c_new(:,i) =  cos(th) c(:,i) + sin(th) c(:,j)
!   c_new(:,j) = -sin(th) c(:,j) + cos(th) c(:,j)
!
!    the angle is obtained such ...
!
!      it is working only if 2 vectors are degenerated !!!!
!
!=====================================================================
  SUBROUTINE QDUtil_rota_denerated(v,c)
      USE QDUtil_NumParameters_m
      IMPLICIT NONE

      real (kind=Rkind), intent(in)    :: v(:)
      real (kind=Rkind), intent(inout) :: c(:,:)

      integer           :: i,j,k,kloc
      real (kind=Rkind) :: ai,aj,norm,cc,ss

      real (kind=Rkind), parameter :: epsi = ONETENTH**10

!---------------------------------------------------------------------
      logical, parameter :: debug = .FALSE.
      !logical, parameter :: debug = .TRUE.
!---------------------------------------------------------------------
      IF (debug) THEN
      write(out_unit,*) 'BEGINNING QDUtil_rota_denerated'
      write(out_unit,*) 'v',v
      !write(out_unit,*) 'c',c
      END IF
!---------------------------------------------------------------------
      DO i=1,size(v)-1

        j = i+1
        IF ( abs(v(i)-v(j)) < epsi) THEN
          !write(out_unit,*) 'i,j',i,j
          !write(out_unit,*) 'vec i',c(:,i)
          !write(out_unit,*) 'vec j',c(:,j)


          kloc = maxloc((c(:,i)**2+c(:,j)**2),dim=1)

          cc   =  c(kloc,i)
          ss   = -c(kloc,j)
          !write(out_unit,*) i,j,'cos sin',kloc,cc,ss
          norm = sqrt(cc*cc+ss*ss)
          cc   = cc/norm
          ss   = ss/norm
          !write(out_unit,*) i,j,'cos sin',cc,ss
          IF (abs(cc) < epsi .OR. abs(ss) < epsi) CYCLE

          DO k=1,size(c,dim=1)
           ai = c(k,i)
           aj = c(k,j)

           c(k,i) =  cc * ai + ss * aj
           c(k,j) = -ss * ai + cc * aj

          END DO

        END IF
      END DO

!---------------------------------------------------------------------
      IF (debug) THEN
      write(out_unit,*) 'new c',c
      write(out_unit,*) 'END QDUtil_rota_denerated'
      END IF
!---------------------------------------------------------------------

  end subroutine QDUtil_rota_denerated

!============================================================
!
!      *******       Extension to complex symmetrical matrices of
!      *******       the <Tql2> Eispack routine, implemented by
!      *******       Claude Leforestier.
!
!
!     on input-
!        n is the order of the matrix,
!        d contains the diagonal elements of the input matrix,
!        e contains the subdiagonal elements of the input matrix
!          in its last n-1 positions.  e(1) is arbitrary,
!        z should have been initialized to the identity matrix.
!
!      on output-
!        d contains the eigenvalues in ascending order.  if an
!          error exit is made, the eigenvalues are correct but
!          unordered for indices 1,2,...,ierr-1,
!        e has been destroyed,
!        z contains orthonormal eigenvectors of the symmetric
!          tridiagonal (or full) matrix.  if an error exit is made,
!          z contains the eigenvectors associated with the stored
!          eigenvalues,
!        ierr is set to
!          zero       for normal return,
!          j          if the j-th eigenvalue has not been
!                     determined after nbiter iterations.
!
!============================================================
!
  Subroutine QDUtil_cTql2(nZ,n,D,E,Z,ierr)
      USE QDUtil_NumParameters_m
      IMPLICIT NONE

      Integer :: i,j,k,l,m,n,ii,Nbiter,nm,mml,nZ,ierr
      Data Nbiter/60/
      complex(kind=Rkind) :: D(n),E(n),Z(nZ,n)
      real (kind=Rkind) :: machep,norm1,norm2,rsign
      complex(kind=Rkind) :: b,c,f,g,p,r,s


      machep=10.d-16
      ierr=0
!     initialize z to e one.
      do i=2,n
         e(i-1)=e(i)
      enddo
      e(n)=zero
      do 240 l=1,n
      j=0
!     ********** look for small sub-diagonal element **********
  105 do 110 m=l,n
      if(m == n) goto 120
      norm1=abs(real(e(m),kind=Rkind))+abs(aimag(e(m)))
      norm2=abs(real(d(m),kind=Rkind))+abs(aimag(d(m))) +               &
            abs(real(d(m+1),kind=Rkind))+abs(aimag(d(m+1)))
      if(norm1.le.machep*norm2) goto 120
  110 continue
  120 p=d(l)
      if(m == l) goto 240
      if(j == nbiter) goto 1000
      j=j+1
!     ********** form shift **********
      g=(d(l+1)-p)/(TWO*e(l))
      r=sqrt(g*g+ONE)
      rsign=1.
      if(real(g,kind=Rkind).lt.0.) rsign=-ONE
      g=d(m)-p+e(l)/(g+rsign*r)
      s=ONE
      c=ONE
      p=ZERO
      mml=m-l
!     ********** for i=m-1 step -1 until l do -- **********
      do 200 ii=1,mml
      i=m-ii
      f=s*e(i)
      b=c*e(i)
      norm1=f*conjg(f)
      norm2=g*conjg(g)
      if(norm1.lt.norm2) goto 150
      c=g/f
      r=sqrt(c*c+ONE)
      e(i+1)=f*r
      s=ONE/r
      c=c*s
      go to 160
  150 s=f/g
      r=sqrt(s*s+ONE)
      e(i+1)=g*r
      c=ONE/r
      s=s*c
  160 g=d(i+1)-p
      r=(d(i)-g)*s+TWO*c*b
      p=s*r
      d(i+1)=g+p
      g=c*r-b
!     ********** form vector **********
      do k=1,n
      f=z(k,i+1)
      z(k,i+1)=s*z(k,i)+c*f
      z(k,i)=c*z(k,i)-s*f
      enddo
  200 continue
      d(l)=d(l)-p
      e(l)=g
      e(m)=ZERO
      go to 105
  240 continue
!     ********** order eigenvalues and eigenvectors **********
      do 300 ii=2,n
      i=ii-1
      k=i
      p=d(i)
      do 260 j=ii,n
      if(real(d(j),kind=Rkind) >= real(p,kind=Rkind)) goto 260
      k=j
      p=d(j)
  260 continue
      if(k == i) goto 300
      d(k)=d(i)
      d(i)=p
      do m=1,n
         p=z(m,i)
         z(m,i)=z(m,k)
         z(m,k)=p
      enddo
  300 continue
      goto 1001
!     ********** set error -- no convergence to an
!                eigenvalue after Nbiter iterations **********
 1000 write (out_unit,1010) l
 1010 format(//10x,'$$$ <Cmtql2> return code :',i5,' $$$')
      stop
 1001 Return
  end subroutine QDUtil_cTql2

  Subroutine QDUtil_cTred2(nm,n,A,d,e,Z)
      USE QDUtil_NumParameters_m
      IMPLICIT NONE

      INTEGER I,II,J,JP1,K,L,N,NM
      complex(kind=Rkind) A(nm,n),Z(nm,n),d(n),e(n)                     &
                ,f,g,h,hh
      real (kind=Rkind) :: scale,rsign
!     real (kind=Rkind) :: one,scale,rsign,zero
!     Data zero/0./,one/1./

      do i=1,n
         do j=1,i
            Z(i,j)=A(i,j)
         enddo
      enddo
!
!     FOR I=N STEP -1 UNTIL 2 DO --
      do ii=2,n
         I=N+2-II
         L=I-1
         H=ZERO
         SCALE=ZERO
         IF(L.LT.2) GOTO 130
!     SCALE ROW (ALGOL TOL THEN NOT NEEDED)
         DO 120 K=1,L
  120    SCALE=SCALE+ABS(Z(I,K))
         IF(SCALE /= ZERO) GOTO 140
  130    E(I)=Z(I,L)
         GOTO 290
  140    DO 150 K=1,L
         Z(I,K)=Z(I,K)/SCALE
         H=H+Z(I,K)*Z(I,K)
  150 CONTINUE
         F=Z(I,L)
         rsign=ONE
         if( real(F,kind=Rkind).lt.0.) rsign=-ONE
         G=-rsign*SQRT(H)
         E(I)=SCALE*G
         H=H-F*G
         Z(I,L)=F-G
         F=ZERO
         DO 240 J=1,L
         Z(J,I)=Z(I,J)/(SCALE*H)
         G=ZERO
!     FORM ELEMENT OF A*U
         DO 180 K=1,J
  180    G=G+Z(J,K)*Z(I,K)
         JP1=J+1
         IF(L.LT.JP1) GOTO 220
         DO 200 K=JP1,L
  200    G=G+Z(K,J)*Z(I,K)
!     FORM ELEMENT OF PP
  220    E(J)=G/H
         F=F+E(J)*Z(I,J)
  240    CONTINUE
         HH=F/(H+H)
!     FORM REDUCED A
         DO 260 J=1,L
         F=Z(I,J)
         G=E(J)-HH*F
         E(J)=G
         DO 260 K=1,J
         Z(J,K)=Z(J,K)-F*E(K)-G*Z(I,K)
  260 CONTINUE
         DO 280 K=1,L
  280    Z(I,K)=SCALE*Z(I,K)
  290    D(I) = H
      enddo
  320 D(1)=ZERO
      E(1)=ZERO
!     ACCUMULATION OF TRANSFORMATION MATRICES
      DO 500 I=1,N
      L=I-1
      IF(abs(D(I)) == ZERO) GOTO 380
      DO 360 J=1,L
      G=ZERO
      DO 340 K=1,L
  340 G=G+Z(I,K)*Z(K,J)
      DO 360 K=1,L
      Z(K,J)=Z(K,J)-G*Z(K,I)
  360 CONTINUE
  380 D(I)=Z(I,I)
      Z(I,I)=ONE
      IF(L.LT.1) GOTO 500
      DO 400 J=1,L
      Z(I,J)=ZERO
      Z(J,I)=ZERO
  400 CONTINUE
  500 CONTINUE
      RETURN
  end subroutine QDUtil_cTred2

  SUBROUTINE QDUtil_Lanczos(A,n_vect,D,V,epsi,max_it)
      USE QDUtil_NumParameters_m
      USE QDUtil_RW_MatVec_m
      IMPLICIT NONE

      integer,          intent(in)    :: n_vect
      real(kind=Rkind), intent(in)    :: A(:,:)
      real(kind=Rkind), intent(inout) :: V(:,:),D(:)
      real(kind=Rkind), intent(in)    :: epsi
      integer,          intent(in)    :: max_it



      real(kind=Rkind), allocatable :: Krylov_vectors(:,:)
      real(kind=Rkind), allocatable :: M_Krylov(:,:)
      real(kind=Rkind), allocatable :: V_Krylov(:,:)
      real(kind=Rkind), allocatable :: E0_Krylov(:)
      real(kind=Rkind), allocatable :: E1_Krylov(:)
      real(kind=Rkind)              :: y,maxdiff

      integer :: i,it,n_size

      ! Begin Lanczos scheme
      n_size = size(A,dim=1)
      write(out_unit,*) 'shape(A)',shape(A)
      write(out_unit,*) 'shape(V)',shape(V)
      write(out_unit,*) 'shape(D)',shape(D)
      write(out_unit,*) 'n_size',n_size
      write(out_unit,*) 'n_vect',n_vect
      write(out_unit,*) 'max_it',max_it
      write(out_unit,*) 'epsi',epsi

      allocate(Krylov_vectors(n_size,0:max_it))
      Krylov_vectors(:,:) = ZERO
      CALL random_number(Krylov_vectors(1:n_vect,0))
      Krylov_vectors(:,0) = Krylov_vectors(:,0) /                               &
                      sqrt(dot_product(Krylov_vectors(:,0),Krylov_vectors(:,0)))
      write(out_unit,*) 'Krylov_vectors (guess)',Krylov_vectors(:,0)


      allocate(M_Krylov(max_it,max_it))
      allocate(V_Krylov(max_it,max_it))
      allocate(E0_Krylov(max_it))
      allocate(E1_Krylov(max_it))
      maxdiff = huge(ONE)

      DO it=1,max_it

         Krylov_vectors(:,it) = matmul(A,Krylov_vectors(:,it-1))

         DO i=0,it-1
            M_Krylov(i+1, it) = dot_product(Krylov_vectors(:,i),Krylov_vectors(:,it))
            M_Krylov(it, i+1) = M_Krylov(i+1,it)
         END DO
         CALL Write_Mat(M_Krylov(1:it,1:it),out_unit,5)

         ! Orthogonalize vectors
         DO i=0,it-1
            y = dot_product(Krylov_vectors(:,i),Krylov_vectors(:,it))
            Krylov_vectors(:,it) = Krylov_vectors(:,it) - y * Krylov_vectors(:,i)
         END DO
         ! Normalize vector
          Krylov_vectors(:,it) =   Krylov_vectors(:,it) /                      &
                  sqrt(dot_product(Krylov_vectors(:,it),Krylov_vectors(:,it)))

         IF (it >= n_vect) THEN
            CALL diagonalization(M_Krylov(1:it,1:it),E1_Krylov(1:it),V_Krylov(1:it,1:it),it,3,1,.FALSE.)
            write(out_unit,*) 'it eig',it,E1_Krylov(1:n_vect)
         END IF
         IF (it > n_vect) THEN
            maxdiff = maxval(abs(E1_Krylov(1:n_vect) - E0_Krylov(1:n_vect)))
            E0_Krylov(1:n_vect) = E1_Krylov(1:n_vect)
         ELSE
            maxdiff = huge(ONE)
         END IF
         write(out_unit,*) 'it maxdiff,epsi,exit?',it,maxdiff,epsi,(maxdiff < epsi)

         IF (maxdiff < epsi) EXIT

      END DO
stop
  end subroutine QDUtil_Lanczos

  SUBROUTINE Test_QDUtil_Diago()
    USE QDUtil_Test_m
    USE QDUtil_NumParameters_m
    USE QDUtil_String_m
    USE QDUtil_RW_MatVec_m
    IMPLICIT NONE

    TYPE (test_t)                    :: test_var
    logical                          :: res_test
    real (kind=Rkind),   parameter   :: ZeroTresh    = ONETENTH**10

    integer                          :: i,n,io,ioerr,diago_type
    real(kind=Rkind),    allocatable :: RMat(:,:),REigVal(:),RVec(:,:)
    real(kind=Rkind),    allocatable :: R2Mat(:,:),Diag(:,:)
    character (len=:),   allocatable :: info

    !====================================================================
    ! Tests for the matrix digonalization
    !
    ! define the matrices
    n = 3
    RMat =  reshape([ONE,HALF,ZERO,                             &
                     HALF,ONE,HALF,                             &
                     ZERO,HALF,ONE],shape=[3,3])

    allocate(REigVal(n))
    allocate(RVec(n,n))
    allocate(Diag(n,n))

    ! tests
    CALL Initialize_Test(test_var,test_name='Diago')

    CALL diagonalization(RMat,REigVal,RVec,n)

    Diag = ZERO
    DO i=1,n
      Diag(i,i) = REigVal(i)
    END DO

    R2Mat = matmul(RVec,matmul(Diag,transpose(RVec)))

    res_test = all(abs(R2Mat-RMat) < ZeroTresh)
    CALL Logical_Test(test_var,test1=res_test,info='diago')
    IF (.NOT. res_test) THEN
      CALL Write_Mat(RMat,out_unit,5,info='RMat')
      CALL Write_Mat(RVec,out_unit,5,info='RVec')
      CALL Write_Vec(REigVal,out_unit,5,info='REigVal')

      CALL Write_Mat(R2Mat,out_unit,5,info='R2Mat')
    END IF
    CALL Flush_Test(test_var)

    DO diago_type=1,4
      info = 'diago (#' // TO_string(diago_type) // ')'

#if __LAPACK == 0
      IF (diago_type == 4) THEN
        write(out_unit,*) 'WARNING: LAPACK and BLAS are not linked'
        write(out_unit,*) '=> diago with type=4 is not possible'
        CYCLE
      END IF
#endif

      CALL diagonalization(RMat,REigVal,RVec,n,diago_type=diago_type)
      Diag = ZERO
      DO i=1,n
        Diag(i,i) = REigVal(i)
      END DO
      R2Mat = matmul(RVec,matmul(Diag,transpose(RVec)))
  
      res_test = all(abs(R2Mat-RMat) < ZeroTresh)
      CALL Logical_Test(test_var,test1=res_test,info='diago')
      IF (.NOT. res_test) THEN
        CALL Write_Mat(RMat,out_unit,5,info='RMat')
        CALL Write_Mat(RVec,out_unit,5,info='RVec')
        CALL Write_Vec(REigVal,out_unit,5,info='REigVal')
  
        CALL Write_Mat(R2Mat,out_unit,5,info='R2Mat')
      END IF
      CALL Flush_Test(test_var)
    END DO
    !====================================================================

    ! finalize the tests
    CALL Finalize_Test(test_var)
  END  SUBROUTINE Test_QDUtil_Diago
END MODULE QDUtil_diago_m

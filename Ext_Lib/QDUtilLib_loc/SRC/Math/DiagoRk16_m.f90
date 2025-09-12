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
MODULE QDUtil_diagoRk16_m
#ifndef __WITHRK16
#define __WITHRK16 1
#endif
  USE, intrinsic :: ISO_FORTRAN_ENV, ONLY : Rkind => real128, out_unit => OUTPUT_UNIT
  IMPLICIT NONE

  PRIVATE
#if __WITHRK16 == 1
  PUBLIC diagonalizationRk16
  INTERFACE diagonalizationRk16
    MODULE PROCEDURE QDUtil_Rk16diagonalization
  END INTERFACE


  real(kind=Rkind), parameter :: ZERO      = 0._Rkind
  real(kind=Rkind), parameter :: ONE       = 1._Rkind
  real(kind=Rkind), parameter :: TWO       = 2._Rkind
  real(kind=Rkind), parameter :: TEN       = 10._Rkind
  real(kind=Rkind), parameter :: HUNDRED   = 100._Rkind

  real(kind=Rkind), parameter :: HALF      = 0.5_Rkind
  real(kind=Rkind), parameter :: ONETENTH  = 0.1_Rkind
  real(kind=Rkind), parameter :: TWOTENTHS = TWO/TEN

CONTAINS

  SUBROUTINE QDUtil_Rk16diagonalization(RMat,REigVal,REigVec,nb_diago,diago_type,sort,phase)
    IMPLICIT NONE

    real(kind=Rkind), intent(in)              :: RMat(:,:)
    real(kind=Rkind), intent(inout)           :: REigVal(:),REigVec(:,:)
    integer,          intent(in),    optional :: nb_diago ! when nb_diago < size(REigVal), 
                                                            !only nb_diago eigenvavlues and  eigenREigVectors are calculated

    integer,          intent(in),    optional :: diago_type,sort
    logical,          intent(in),    optional :: phase


    !local variables
    integer            :: diago_type_loc
    integer            :: diago_type_default = 2 ! tred+tql

    real(kind=Rkind), allocatable :: RMat_save(:,:)
    real(kind=Rkind), allocatable :: work(:)
    integer :: n_size,n_vect,n

    !----- for debuging --------------------------------------------------
    character (len=*), parameter :: name_sub='QDUtil_Rk16diagonalization'
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
      STOP 'ERROR in QDUtil_Rk16diagonalization: The matrix or eigenvector sizes are not consistant.'
    END IF
    IF (n < 1) THEN
      write(out_unit,*) ' ERROR in ',name_sub
      write(out_unit,"(a,i0,a)") ' n < 1. It MUST be in the range [1,',n_size,']'
      write(out_unit,*) '   n:              ',n
      write(out_unit,*) '   size(REigVal):  ',size(REigVal)
      write(out_unit,*) '  => CHECK the fortran!!'
      STOP 'ERROR in QDUtil_Rk16diagonalization:  n < 1.'
    END IF
    n_vect = min(n,n_size)

    IF (present(diago_type)) THEN
      diago_type_loc = diago_type
    ELSE
      diago_type_loc = diago_type_default
    END IF

    !Only one posibility
    diago_type_loc = diago_type_default



    SELECT CASE (diago_type_loc)
    CASE (2) ! tred+tql
      IF (debug) write(out_unit,*) 'tred+tql, new version (symmetric)'
      allocate(work(n))

      REigVec = RMat
      CALL QDUtil_TRED2_EISPACK_Rk16(REigVec,n,n,REigVal,work)
      CALL QDUtil_TQLI_EISPACK_RK16(REigVal,work,n,n,REigVec)

      deallocate(work)
 
    CASE DEFAULT
      write(out_unit,*) ' ERROR in ',name_sub
      write(out_unit,*) ' The default CASE is not defined.'
      write(out_unit,*) '  => CHECK the fortran!!'
      STOP 'ERROR in QDUtil_Rk16diagonalization: default case impossible'
    END SELECT

    IF (present(sort)) THEN
        SELECT CASE (sort)
        CASE(1)
          CALL QDUtil_sort_Rk16(REigVal,REigVec)
          CALL QDUtil_rota_denerated_Rk16(REigVal,REigVec)
        CASE(-1)
          REigVal = -REigVal
          CALL QDUtil_sort_Rk16(REigVal,REigVec)
          REigVal = -REigVal
          CALL QDUtil_rota_denerated_Rk16(REigVal,REigVec)
        CASE(2)
          CALL QDUtil_sort_abs_Rk16(REigVal,REigVec)
        CASE DEFAULT ! no sort
          CONTINUE
        END SELECT
    ELSE
      CALL QDUtil_sort_Rk16(REigVal,REigVec)
      CALL QDUtil_rota_denerated_Rk16(REigVal,REigVec)
    END IF

    IF (present(phase)) THEN
      IF (phase) CALL QDUtil_Unique_phase_Rk16(REigVec)
    ELSE
      CALL QDUtil_Unique_phase_Rk16(REigVec)
    END IF

  END SUBROUTINE QDUtil_Rk16diagonalization
 
  SUBROUTINE QDUtil_JACOBI2_Rk16(A,N,D,V)
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

  end subroutine QDUtil_JACOBI2_Rk16
!
!============================================================
!
!   diagonalization:  trigonalization then diagonalization
!
!============================================================
!
  SUBROUTINE QDUtil_TRED2_EISPACK_Rk16(A,N,NP,D,E)
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
  END SUBROUTINE QDUtil_TRED2_EISPACK_Rk16
 
  SUBROUTINE QDUtil_TQLI_EISPACK_Rk16(D,E,N,NP,Z)
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
  END SUBROUTINE QDUtil_TQLI_EISPACK_Rk16
!
!============================================================
!
!   Sort the eigenvalues and the eigenvectors
!
!============================================================
!
  SUBROUTINE QDUtil_sort_tab_Rk16(nb_niv,ene,max_niv)
      IMPLICIT NONE

      integer nb_niv,max_niv
      real(kind=Rkind) ene(max_niv)
      real(kind=Rkind) a

        integer i,j,k

      DO i=1,nb_niv
      DO j=i+1,nb_niv
       IF (ene(i)  >  ene(j)) THEN
          !permutation
          a=ene(i)
          ene(i)=ene(j)
          ene(j)=a
        END IF
      END DO
      END DO

  end subroutine QDUtil_sort_tab_Rk16

  SUBROUTINE QDUtil_sort_Rk16(ene,psi)
    IMPLICIT NONE

    real(kind=Rkind), intent(inout) :: ene(:),psi(:,:)

    real(kind=Rkind) :: a
    integer          :: i,j,k

    DO i=1,size(ene)
    DO j=i+1,size(ene)
     IF (ene(i) > ene(j)) THEN
  	    !permutation
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

  END SUBROUTINE QDUtil_sort_Rk16
  SUBROUTINE QDUtil_sort_abs_Rk16(ene,psi)
    IMPLICIT NONE

    real(kind=Rkind), intent(inout) :: ene(:),psi(:,:)

    real(kind=Rkind) :: a
    integer          :: i,j,k

    DO i=1,size(ene)
      DO j=i+1,size(ene)
        IF (abs(ene(i)) > abs(ene(j))) THEN
          ! permutation
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

  end subroutine QDUtil_sort_abs_Rk16

!============================================================
!
!   Change the phase of Vec(:,i) shuch its largest coeficient is positive
!
!============================================================
!
  SUBROUTINE QDUtil_Unique_phase_Rk16(Vec)
      IMPLICIT NONE

      real(kind=Rkind), intent(inout) :: Vec(:,:)

      integer          :: i,jloc

      DO i=lbound(Vec,dim=2),ubound(Vec,dim=2)
        jloc           = maxloc(abs(Vec(:,i)),dim=1)
        IF (abs(Vec(jloc,i)) < ONETENTH**6 ) CYCLE
        IF (Vec(jloc,i) < ZERO) Vec(:,i) = -Vec(:,i)
      END DO

  END SUBROUTINE QDUtil_Unique_phase_Rk16

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
  SUBROUTINE QDUtil_rota_denerated_Rk16(v,c)
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

  end subroutine QDUtil_rota_denerated_Rk16
#endif

END MODULE QDUtil_diagoRk16_m
#if __WITHRK16 == 1
SUBROUTINE Test_QDUtil_DiagoRk16()
  USE, intrinsic :: ISO_FORTRAN_ENV, ONLY : Rkind => real128, out_unit => OUTPUT_UNIT
  USE QDUtil_Test_m
  USE QDUtil_String_m
  USE QDUtil_RW_MatVec_m
  USE QDUtil_diagoRk16_m
  IMPLICIT NONE


  real(kind=Rkind), parameter :: ZERO      = 0._Rkind
  real(kind=Rkind), parameter :: ONE       = 1._Rkind
  real(kind=Rkind), parameter :: TWO       = 2._Rkind
  real(kind=Rkind), parameter :: TEN       = 10._Rkind
  real(kind=Rkind), parameter :: HUNDRED   = 100._Rkind

  real(kind=Rkind), parameter :: HALF      = 0.5_Rkind
  real(kind=Rkind), parameter :: ONETENTH  = 0.1_Rkind
  real(kind=Rkind), parameter :: TWOTENTHS = TWO/TEN

  TYPE (test_t)                    :: test_var
  logical                          :: res_test
  real (kind=Rkind),   parameter   :: ZeroTresh    = TEN**2*epsilon(ONE)

  integer                          :: i,n,io,ioerr,diago_type
  real(kind=Rkind),    allocatable :: RMat(:,:),REigVal(:),RVec(:,:),R2Mat(:,:),RDiag(:,:),RDiffMat(:,:)
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
    allocate(RDiag(n,n))

    ! tests
    CALL Initialize_Test(test_var,test_name='DiagoRk16')

    CALL diagonalizationRk16(RMat,REigVal,RVec,n)

    RDiag = ZERO
    DO i=1,n
      RDiag(i,i) = REigVal(i)
    END DO

    R2Mat = matmul(RVec,matmul(RDiag,transpose(RVec)))

    RDiffMat = abs(R2Mat-RMat)
    res_test = all(RDiffMat < ZeroTresh)
    CALL Logical_Test(test_var,test1=res_test,info='diago')
    IF (.NOT. res_test) THEN
      CALL Write_Mat(RMat,out_unit,5,info='RMat')
      write(out_unit,*)
      CALL Write_Mat(RVec,out_unit,5,info='RVec')
      write(out_unit,*)
      CALL Write_Vec(REigVal,out_unit,5,info='REigVal')
      write(out_unit,*)

      CALL Write_Mat(R2Mat,out_unit,5,info='R2Mat')
      write(out_unit,*)
      CALL Write_Mat(RDiffMat,out_unit,5,info='RDiffMat')
      write(out_unit,*)
      flush(out_unit)
    END IF
    CALL Flush_Test(test_var)

    !====================================================================

    ! finalize the tests
    CALL Finalize_Test(test_var)
  END  SUBROUTINE Test_QDUtil_DiagoRk16
#endif

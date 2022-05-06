!===============================================================================
!===============================================================================
!This file is part of AD_dnSVM.
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
MODULE ADLib_diago_m
!$ USE omp_lib
  IMPLICIT NONE

   PRIVATE
   PUBLIC diagonalization

  INTERFACE diagonalization
     MODULE PROCEDURE AD_diagonalization
  END INTERFACE

   CONTAINS
!============================================================
!
!   Driver for the diagonalization
!      Default: tred2+tql2 (type_diag=2)
!            Other possibilities: Jacobi (type_diag=1) or Lapack (type_diag=3)
!            Rk: Lapack diago is not possible
!      Sort: the eigenvalues/eigenvectors:
!            sort=1:  ascending (default)
!            sort=-1: descending
!            sort=2:  ascending on the absolute eigenvalues
!     phase:
!============================================================
!
  RECURSIVE SUBROUTINE AD_diagonalization(Mat,REig,Vec,n,type_diag,sort,phase,IEig)
    USE, intrinsic :: ISO_FORTRAN_ENV, ONLY : real64,int32
    USE ADLib_NumParameters_m
    IMPLICIT NONE

    integer,          intent(in)              :: n ! when n < size(REig), only n eigenvectors and  eigenvectors are calculated
    real(kind=Rkind), intent(in)              :: Mat(:,:)
    real(kind=Rkind), intent(inout)           :: REig(:),Vec(:,:)
    real(kind=Rkind), intent(inout), optional :: IEig(:)

    integer,          intent(in),    optional :: type_diag,sort
    logical,          intent(in),    optional :: phase


    !local variables
    integer            :: type_diag_loc
    integer            :: type_diag_default = 2 ! tred+tql
    !                                    Jacobi tred+tql DSYEV  DGEEV Lanczos
    integer, parameter :: list_type(7) = [1,    2,       3,377, 4,477,   5]

    real(kind=Rkind), allocatable :: trav(:),Mat_save(:,:)
    integer :: n_size,n_vect

    !for lapack
    integer              :: i
    integer              :: lwork ,lda ,ldvr ,ierr
    integer(kind=int32)  :: n4,lwork4,lda4,ldvr4,ierr4
    real(kind=Rkind), allocatable :: work(:)
    real(kind=Rkind), allocatable :: IEig_loc(:)

    real(kind=Rkind) :: dummy(1,1)





    !----- for debuging --------------------------------------------------
    character (len=*), parameter :: name_sub='AD_diagonalization'
    logical, parameter :: debug = .FALSE.
    !logical, parameter :: debug = .TRUE.
    !-----------------------------------------------------------

    n_size = size(REig)
    IF (n_size /= size(Mat,dim=1) .OR. n_size /= size(Vec,dim=1)) THEN
      write(out_unitp,*) ' ERROR in ',name_sub
      write(out_unitp,*) ' The matrix or vector sizes are not consistant'
      write(out_unitp,*) '   size(REig):     ',size(REig)
      write(out_unitp,*) '   size(Mat):      ',size(Mat,dim=1)
      write(out_unitp,*) '   size(Vec):      ',size(Vec,dim=1)
      write(out_unitp,*) '  => CHECK the fortran!!'
      STOP 'ERROR in AD_diagonalization: The matrix or vector sizes are not consistant.'
    END IF
    IF (n < 1) THEN
      write(out_unitp,*) ' ERROR in ',name_sub
      write(out_unitp,"(a,i0,a)") ' n < 1. It MUST be in the range [1,',n_size,']'
      write(out_unitp,*) '   n:              ',n
      write(out_unitp,*) '   size(REig):     ',size(REig)
      write(out_unitp,*) '  => CHECK the fortran!!'
      STOP 'ERROR in AD_diagonalization: The matrix or vector sizes are not consistant.'
    END IF
    n_vect = min(n,n_size)

    IF (present(type_diag)) THEN
      type_diag_loc = type_diag
    ELSE
      type_diag_loc = type_diag_default
    END IF

    !when lapack is used and Rkind /= real64 (not a double)
    IF (Rkind /= real64 .AND. type_diag_loc == 3) type_diag_loc = type_diag_default

    IF (count(list_type == type_diag_loc) == 0) THEN
      write(out_unitp,*) ' ERROR in ',name_sub
      write(out_unitp,*) ' type_diag is out-of-range.'
      write(out_unitp,*) '   type_diag:      ',type_diag_loc
      write(out_unitp,*) '   Possible values:',list_type(:)
      write(out_unitp,*) '  => CHECK the fortran!!'
      STOP 'ERROR in AD_diagonalization: type_diag is out-of-range.'
    END IF


    SELECT CASE (type_diag_loc)
    CASE(1) ! jacobi
      IF (debug) write(out_unitp,*) 'Jacobi (symmetric)'
      allocate(Mat_save(n,n))
      Mat_save = Mat ! save mat

      CALL AD_JACOBI2(Mat_save,n,REig,Vec)

      deallocate(Mat_save)
    CASE (2) ! tred+tql
      IF (debug) write(out_unitp,*) 'tred+tql, new version (symmetric)'
      allocate(trav(n))

      Vec = Mat
      CALL AD_TRED2_EISPACK(Vec,n,n,REig,trav)
      CALL AD_TQLI_EISPACK(REig,trav,n,n,Vec)

      deallocate(trav)
    CASE(3,377) ! lapack77
      IF (debug) write(out_unitp,*) 'lapack77: DSYEV (symmetric)'
      lwork = 3*n-1
      allocate(work(lwork))
      Vec(:,:) = Mat(:,:)

      ! lapack subroutines need integer (kind=4 or int32), therefore, we add a conversion, otherwise
      ! it fails when integers (kind=8 or int64) are used (at the compilation).
      n4     = int(n,kind=int32)
      lwork4 = int(lwork,kind=int32)
      CALL DSYEV('V','U',n4,Vec,n4,REig,work,lwork4,ierr4)

      IF (debug) write(out_unitp,*) 'ierr=',ierr4
      flush(out_unitp)

      IF (ierr4 /= 0_int32) THEN
         write(out_unitp,*) ' ERROR in ',name_sub
         write(out_unitp,*) ' DSYEV lapack subroutine has FAILED!'
         STOP
      END IF


      deallocate(work)
!      CASE(395) ! lapack95
!        IF (debug) write(out_unitp,*) 'lapack95: LA_SYEVD'
!        flush(out_unitp)
!        Vec(:,:) = Mat
!        CALL LA_SYEVD(Vec,Eig)

    CASE(4,477) ! lapack77 (non-symmetric)
      IF (debug) write(out_unitp,*) 'lapack77: DGEEV (non-symmetric)'
      flush(out_unitp)

      allocate(Mat_save(n,n))
      Mat_save = Mat ! save mat


      lwork = (2+64)*n
      ldvr  = n
      lda   = n
      allocate(work(lwork))


      n4     = int(n,kind=int32)
      lwork4 = int(lwork,kind=int32)
      lda4   = int(lda,kind=int32)
      ldvr4  = int(ldvr,kind=int32)

      IF (present(IEig)) THEN
        CALL DGEEV('N','V',n4,Mat_save,lda4,REig,IEig,dummy,              &
                   int(1,kind=int32),Vec,ldvr4,work,lwork4,ierr4)
        IF (debug) write(out_unitp,*)'ierr=',ierr4
        IF (ierr4 /= 0_int32) THEN
           write(out_unitp,*) ' ERROR in ',name_sub
           write(out_unitp,*) ' DGEEV lapack subroutine has FAILED!'
           STOP
        END IF

        IF (debug) THEN
          DO i=1,n
            write(out_unitp,*) 'Eigenvalue(', i, ') = ', REig(i),'+I ',IEig(i)
          END DO
        END IF
      ELSE
        allocate(IEig_loc(n))

        CALL DGEEV('N','V',n4,Mat_save,lda4,REig,IEig_loc,dummy,        &
                   int(1,kind=int32),Vec,ldvr4,work,lwork4,ierr4)
        IF (debug) write(out_unitp,*)'ierr=',ierr4
        IF (ierr4 /= 0_int32) THEN
           write(out_unitp,*) ' ERROR in ',name_sub
           write(out_unitp,*) ' DGEEV lapack subroutine has FAILED!'
           STOP
        END IF

        DO i=1,n
          write(out_unitp,*) 'Eigenvalue(', i, ') = ', REig(i),'+I ',IEig_loc(i)
        END DO

        deallocate(IEig_loc)
      END IF

      deallocate(work)
      deallocate(Mat_save)

    CASE(5) ! lanczos

      CALL AD_Lanczos(Mat,n_vect,REig,Vec,epsi=ONETENTH**6,max_it=100)

    CASE DEFAULT
      write(out_unitp,*) ' ERROR in ',name_sub
      write(out_unitp,*) ' The default CASE is not defined.'
      write(out_unitp,*) '  => CHECK the fortran!!'
      STOP 'ERROR in AD_diagonalization: default case impossible'
    END SELECT


    IF (present(sort)) THEN
        SELECT CASE (sort)
        CASE(1)
          CALL AD_sort(REig,Vec)
          CALL AD_rota_denerated(REig,Vec)
        CASE(-1)
          REig = -REig
          CALL AD_sort(REig,Vec)
          REig = -REig
          CALL AD_rota_denerated(REig,Vec)
        CASE(2)
          CALL AD_sort_abs(REig,Vec)
        CASE DEFAULT ! no sort
          CONTINUE
        END SELECT
    ELSE
      CALL AD_sort(REig,Vec)
      CALL AD_rota_denerated(REig,Vec)
    END IF

    IF (present(phase)) THEN
      IF (phase) CALL AD_Unique_phase(Vec)
    ELSE
      CALL AD_Unique_phase(Vec)
    END IF

  END SUBROUTINE AD_diagonalization

  SUBROUTINE AD_JACOBI2(A,N,D,V)
      USE ADLib_NumParameters_m
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
        IF(SM == ZERO)RETURN

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

      write(out_unitp,*) max_it,' iterations should never happen'
      STOP

  end subroutine AD_JACOBI2
!
!============================================================
!
!   diagonalisation trigonalisation puis diagonalisation
!
!============================================================
!
  SUBROUTINE AD_TRED2_EISPACK(A,N,NP,D,E)
      USE ADLib_NumParameters_m
      IMPLICIT NONE

      integer          :: N,NP
      real(kind=Rkind) :: A(NP,NP),D(NP),E(NP)

      !local variables
      integer          :: I,J,K,L
      real(kind=Rkind) :: F,G,H,HH,SCALE

      IF(N.GT.1)THEN
        DO 18 I=N,2,-1
          L=I-1
          H=ZERO
          SCALE=ZERO
          IF(L.GT.1)THEN
            DO 11 K=1,L
              SCALE=SCALE+ABS(A(I,K))
11          CONTINUE
            IF(SCALE .EQ. ZERO)THEN
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
                IF(L.GT.J)THEN
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
        IF(D(I).NE. ZERO)THEN
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
        IF(L.GE.1)THEN
          DO 22 J=1,L
            A(I,J)=ZERO
            A(J,I)=ZERO
22        CONTINUE
        ENDIF
23    CONTINUE
      RETURN
  END SUBROUTINE AD_TRED2_EISPACK

  SUBROUTINE AD_TQLI_EISPACK(D,E,N,NP,Z)
      USE ADLib_NumParameters_m
      IMPLICIT NONE

      integer          :: N,NP
      real(kind=Rkind) :: D(NP),E(NP),Z(NP,NP)

      !local variables
      integer          :: I,K,L,M,ITER
      real(kind=Rkind) :: G,R,S,C,P,F,B,DD

      IF (N.GT.1) THEN
        DO 11 I=2,N
          E(I-1)=E(I)
11      CONTINUE
        E(N)=ZERO
        DO 15 L=1,N
          ITER=0
1         DO 12 M=L,N-1
            DD=ABS(D(M))+ABS(D(M+1))
            IF (ABS(E(M))+DD.EQ.DD) GO TO 2
12        CONTINUE
          M=N
2         IF(M.NE.L)THEN
            IF(ITER.EQ.30) STOP 'too many iterations'
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
              IF(ABS(F).GE.ABS(G))THEN
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
  END SUBROUTINE AD_TQLI_EISPACK

!
!============================================================
!
!   Sort the eigenvalues and the eigenvectors
!
!============================================================
!
  SUBROUTINE AD_sort_tab(nb_niv,ene,max_niv)
      USE ADLib_NumParameters_m
      IMPLICIT NONE

      integer nb_niv,max_niv
      real(kind=Rkind) ene(max_niv)
      real(kind=Rkind) a

        integer i,j,k

      DO i=1,nb_niv
      DO j=i+1,nb_niv
       IF (ene(i) .GT. ene(j)) THEN
!             permutation
          a=ene(i)
          ene(i)=ene(j)
          ene(j)=a
        END IF
      END DO
      END DO

  end subroutine AD_sort_tab

  SUBROUTINE AD_sort(ene,psi)
      USE ADLib_NumParameters_m
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

  END SUBROUTINE AD_sort
  SUBROUTINE AD_sort_abs(ene,psi)
      USE ADLib_NumParameters_m
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

  end subroutine AD_sort_abs
!
!============================================================
!
!   Change the phase of Vec(:,i) shuch its largest coeficient is positive
!
!============================================================
!
  SUBROUTINE AD_Unique_phase(Vec)
      USE ADLib_NumParameters_m
      IMPLICIT NONE

      real(kind=Rkind), intent(inout) :: Vec(:,:)

      integer          :: i,jloc

      DO i=lbound(Vec,dim=2),ubound(Vec,dim=2)
        jloc           = maxloc(abs(Vec(:,i)),dim=1)
        IF (abs(Vec(jloc,i)) < ONETENTH**6 ) CYCLE
        IF (Vec(jloc,i) < ZERO) Vec(:,i) = -Vec(:,i)
      END DO

      END SUBROUTINE AD_Unique_phase
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
  SUBROUTINE AD_rota_denerated(v,c)
      USE ADLib_NumParameters_m
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
      write(out_unitp,*) 'BEGINNING AD_rota_denerated'
      write(out_unitp,*) 'v',v
      !write(out_unitp,*) 'c',c
      END IF
!---------------------------------------------------------------------
      DO i=1,size(v)-1

        j = i+1
        IF ( abs(v(i)-v(j)) < epsi) THEN
          !write(6,*) 'i,j',i,j
          !write(6,*) 'vec i',c(:,i)
          !write(6,*) 'vec j',c(:,j)


          kloc = maxloc((c(:,i)**2+c(:,j)**2),dim=1)

          cc   =  c(kloc,i)
          ss   = -c(kloc,j)
          !write(6,*) i,j,'cos sin',kloc,cc,ss
          norm = sqrt(cc*cc+ss*ss)
          cc   = cc/norm
          ss   = ss/norm
          !write(6,*) i,j,'cos sin',cc,ss

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
      write(out_unitp,*) 'new c',c
      write(out_unitp,*) 'END AD_rota_denerated'
      END IF
!---------------------------------------------------------------------

  end subroutine AD_rota_denerated

  SUBROUTINE AD_Lanczos(A,n_vect,D,V,epsi,max_it)
      USE ADLib_NumParameters_m
      USE ADLib_Util_m
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
      write(6,*) 'shape(A)',shape(A)
      write(6,*) 'shape(V)',shape(V)
      write(6,*) 'shape(D)',shape(D)
      write(6,*) 'n_size',n_size
      write(6,*) 'n_vect',n_vect
      write(6,*) 'max_it',max_it
      write(6,*) 'epsi',epsi

      allocate(Krylov_vectors(n_size,0:max_it))
      Krylov_vectors(:,:) = ZERO
      CALL random_number(Krylov_vectors(1:n_vect,0))
      Krylov_vectors(:,0) = Krylov_vectors(:,0) /                               &
                      sqrt(dot_product(Krylov_vectors(:,0),Krylov_vectors(:,0)))
      write(6,*) 'Krylov_vectors (guess)',Krylov_vectors(:,0)


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
         CALL Write_RMat(M_Krylov(1:it,1:it),out_unitp,5)

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
            write(6,*) 'it eig',it,E1_Krylov(1:n_vect)
         END IF
         IF (it > n_vect) THEN
            maxdiff = maxval(abs(E1_Krylov(1:n_vect) - E0_Krylov(1:n_vect)))
            E0_Krylov(1:n_vect) = E1_Krylov(1:n_vect)
         ELSE
            maxdiff = huge(ONE)
         END IF
         write(6,*) 'it maxdiff,epsi,exit?',it,maxdiff,epsi,(maxdiff < epsi)

         IF (maxdiff < epsi) EXIT

      END DO
stop
  end subroutine AD_Lanczos


END MODULE ADLib_diago_m

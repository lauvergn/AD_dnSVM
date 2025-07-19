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
MODULE QDUtil_RW_MatVec_m
  USE QDUtil_NumParameters_m, ONLY : Name_longlen
  IMPLICIT NONE

  PRIVATE

  INTERFACE Write_VecMat
    MODULE PROCEDURE QDUtil_Write_Rk4Mat,QDUtil_Write_Ck4Mat,QDUtil_Write_Rk4Mat_string
    MODULE PROCEDURE QDUtil_Write_Rk8Mat,QDUtil_Write_Ck8Mat,QDUtil_Write_Rk8Mat_string
    MODULE PROCEDURE QDUtil_Write_Rk16Mat,QDUtil_Write_Ck16Mat,QDUtil_Write_Rk16Mat_string
    MODULE PROCEDURE QDUtil_Write_Rk4Vec,QDUtil_Write_Ck4Vec
    MODULE PROCEDURE QDUtil_Write_Rk8Vec,QDUtil_Write_Ck8Vec
    MODULE PROCEDURE QDUtil_Write_Rk16Vec,QDUtil_Write_Ck16Vec
    MODULE PROCEDURE QDUtil_Write_Ik4Mat,QDUtil_Write_Ik4Vec
    MODULE PROCEDURE QDUtil_Write_Ik8Mat,QDUtil_Write_Ik8Vec
  END INTERFACE
  INTERFACE Write_Mat
    MODULE PROCEDURE QDUtil_Write_Rk4Mat,QDUtil_Write_Ck4Mat,QDUtil_Write_Rk4Mat_string
    MODULE PROCEDURE QDUtil_Write_Rk8Mat,QDUtil_Write_Ck8Mat,QDUtil_Write_Rk8Mat_string
    MODULE PROCEDURE QDUtil_Write_Rk16Mat,QDUtil_Write_Ck16Mat,QDUtil_Write_Rk16Mat_string
    MODULE PROCEDURE QDUtil_Write_Ik4Mat,QDUtil_Write_Ik8Mat
  END INTERFACE
  INTERFACE Write_Vec
    MODULE PROCEDURE QDUtil_Write_Rk4Vec,QDUtil_Write_Ck4Vec
    MODULE PROCEDURE QDUtil_Write_Rk8Vec,QDUtil_Write_Ck8Vec
    MODULE PROCEDURE QDUtil_Write_Rk16Vec,QDUtil_Write_Ck16Vec
    MODULE PROCEDURE QDUtil_Write_Ik4Vec,QDUtil_Write_Ik8Vec
  END INTERFACE
  INTERFACE Read_Mat
    MODULE PROCEDURE QDUtil_Read_Rk4Mat,QDUtil_Read_Ck4Mat
    MODULE PROCEDURE QDUtil_Read_Rk8Mat,QDUtil_Read_Ck8Mat
    MODULE PROCEDURE QDUtil_Read_Rk16Mat,QDUtil_Read_Ck16Mat
    MODULE PROCEDURE QDUtil_Read_Ik4Mat,QDUtil_Read_Ik8Mat
  END INTERFACE
  INTERFACE Read_Vec
    MODULE PROCEDURE QDUtil_Read_Rk4Vec,QDUtil_Read_Ck4Vec
    MODULE PROCEDURE QDUtil_Read_Rk8Vec,QDUtil_Read_Ck8Vec
    MODULE PROCEDURE QDUtil_Read_Rk16Vec,QDUtil_Read_Ck16Vec
    MODULE PROCEDURE QDUtil_Read_Ik4Vec,QDUtil_Read_Ik8Vec
  END INTERFACE

  PUBLIC :: Write_VecMat, Write_Mat, Write_Vec, Read_Mat, Read_Vec
  PUBLIC :: Test_QDUtil_RW_MatVec
  PUBLIC :: RMatIO_format, CMatIO_format

  character (len=Name_longlen) :: RMatIO_format = "f18.10"
  character (len=Name_longlen) :: CMatIO_format = "'(',f15.7,',',f15.7,')'"

  CONTAINS

  !!@description: Defined a format to write a matrix line 
  !!@param: TODO
  SUBROUTINE QDUtil_Format_OF_Line(wformat,nb_line,max_col,cplx,Rformat,info)
    USE QDUtil_String_m
    USE QDUtil_NumParameters_m, ONLY : Rk8,out_unit
    IMPLICIT NONE

    character (len=:), allocatable, intent(inout)  :: wformat
    integer,                        intent(in)     :: nb_line,max_col
    logical,                        intent(in)     :: cplx
    character (len=*), optional,    intent(in)     :: Rformat
    character (len=*), optional,    intent(in)     :: info


    ! local variables
    character (len=:), allocatable :: NMatformat,wformat_loc
    integer                        :: ilen

    !$OMP  CRITICAL (QDUtil_Format_OF_Line_CRIT)

    IF (allocated(wformat)) deallocate(wformat)

    IF (present(info)) THEN
      wformat_loc = '(2x,"' // trim(adjustl(info)) // ' ",'
    ELSE
      wformat_loc = '('
    END IF

    IF (present(Rformat)) THEN
      IF (len_trim(Rformat) > 10) THEN
        write(out_unit,*) ' ERROR in QDUtil_Format_OF_Line'
        write(out_unit,*) ' The format (len_trim) in "Rformat" is too long',len_trim(Rformat)
        write(out_unit,*) ' Rformat: ',Rformat
        STOP
      END IF
        IF (cplx) THEN
          NMatformat = "'('," // trim(adjustl(Rformat)) //           &
                       ",' +i'," // trim(adjustl(Rformat)) // ",')'"
        ELSE
          NMatformat = trim(adjustl(Rformat))
        END IF
    ELSE
      IF (cplx) THEN
        NMatformat = trim(adjustl(CMatIO_format))
      ELSE
        NMatformat = trim(adjustl(RMatIO_format))
      END IF
    END IF

    IF (nb_line > 0) THEN

        !ilen = int(log10(real(nb_line,kind=Rk8)))+1
        ! ensure compatible with very small system in test
        ilen = MAX(int(log10(real(nb_line,kind=Rk8)))+1,2)

        !write(*,*) 'max_col check:',max_col,ilen

        wformat_loc = wformat_loc // '1x,i' //                       &
                    TO_string(ilen) // ',2x,' //                   &
                    TO_string(max_col) // '(' //                   &
                    trim(adjustl(NMatformat)) // ',1x))'


    ELSE

        wformat_loc = wformat_loc // TO_string(max_col) // '(' //  &
                      trim(adjustl(NMatformat)) // ',1x))'


    END IF
    !write(out_unit,*) 'NMatformat: ',NMatformat
    !write(out_unit,*) 'wformat: ',wformat
    !flush(out_unit)

    wformat = wformat_loc

    deallocate(NMatformat)
    deallocate(wformat_loc)
    !$OMP  END CRITICAL (QDUtil_Format_OF_Line_CRIT)

    !write(out_unit,*) 'format?: ',trim(wformat)
  END SUBROUTINE QDUtil_Format_OF_Line

  !!@description:  write a rectangular real or complex matrix, mat(nl,nc),
  !!   with a specific format selected with Format_OF_Line
  !!@param: TODO
  SUBROUTINE QDUtil_Write_Rk8Mat(Mat,nio,nbcol,Rformat,info,iprint)
    USE QDUtil_NumParameters_m, ONLY : out_unit,Rk8
    IMPLICIT NONE

    integer,                     intent(in) :: nio,nbcol
    real(kind=Rk8),              intent(in) :: Mat(:,:)

    character (len=*), optional, intent(in) :: Rformat
    character (len=*), optional, intent(in) :: info
    integer,           optional, intent(in) :: iprint

    integer         :: nl,nc
    integer         :: i,j,nb,nbblocs,nfin,nbcol_loc
    character (len=:), allocatable  :: wformat

    IF (present(iprint)) THEN
      IF (iprint /=0) RETURN ! it was MPI_id in the module mod_MPI
    END IF

    nl = size(Mat,dim=1)
    nc = size(Mat,dim=2)
    !write(out_unit,*) 'nl,nc,nbcol_loc',nl,nc,nbcol_loc
    nbcol_loc = nbcol
    IF (nbcol_loc > 10) nbcol_loc=10
    nbblocs=int(nc/nbcol_loc)
    IF (nbblocs*nbcol_loc == nc) nbblocs=nbblocs-1

    IF (present(Rformat)) THEN
      IF (present(info)) THEN
        CALL QDUtil_Format_OF_Line(wformat,nl,nbcol_loc,.FALSE.,Rformat,info)
      ELSE
        CALL QDUtil_Format_OF_Line(wformat,nl,nbcol_loc,.FALSE.,Rformat=Rformat)
      END IF
    ELSE
      IF (present(info)) THEN
        CALL QDUtil_Format_OF_Line(wformat,nl,nbcol_loc,.FALSE.,info=info)
      ELSE
        CALL QDUtil_Format_OF_Line(wformat,nl,nbcol_loc,.FALSE.)
      END IF
    END IF

      DO nb=0,nbblocs-1
        DO j=1,nl
          write(nio,wformat) j,(Mat(j,i+nb*nbcol_loc),i=1,nbcol_loc)
        END DO
        IF (nl > 1 ) write(nio,*)
      END DO
      DO j=1,nl
        nfin=nc-nbcol_loc*nbblocs
        write(nio,wformat) j,(Mat(j,i+nbcol_loc*nbblocs),i=1,nfin)
      END DO

    deallocate(wformat)

  END SUBROUTINE QDUtil_Write_Rk8Mat
  SUBROUTINE QDUtil_Write_Rk8Mat_string(Mat,string,nbcol,Rformat,info,iprint)
    USE QDUtil_NumParameters_m, ONLY : out_unit,Rk8
    USE QDUtil_String_m,        ONLY : TO_string
    IMPLICIT NONE

    integer,                        intent(in)    :: nbcol
    character (len=:), allocatable, intent(inout) :: string
    real(kind=Rk8),              intent(in)    :: Mat(:,:)

    character (len=*), optional,    intent(in)    :: Rformat
    character (len=*), optional,    intent(in)    :: info
    integer,           optional,    intent(in)    :: iprint

    integer         :: nl,nc
    integer         :: i,j,nb,nbblocs,nfin,nbcol_loc
    character (len=:), allocatable :: BeginString
    character (len=:), allocatable :: Rf

    string = ''

    IF (present(iprint)) THEN
      IF (iprint /=0) RETURN ! it was MPI_id in the module mod_MPI
    END IF

    nl = size(Mat,dim=1)
    nc = size(Mat,dim=2)
 
    nbcol_loc = nbcol
    IF (nbcol_loc > 10) nbcol_loc=10
    nbblocs=int(nc/nbcol_loc)
    IF (nbblocs*nbcol_loc == nc) nbblocs=nbblocs-1

    !write(out_unit,*) 'nl,nc,nbcol_loc',nl,nc,nbcol_loc
    !write(out_unit,*) 'string: ',string ; flush(out_unit)

    IF (present(info)) THEN
      BeginString = trim(info) // ' '
    ELSE
      BeginString = ' '
    END IF

    IF (present(Rformat)) THEN
      Rf = trim(Rformat)
    ELSE
      Rf = RMatIO_format
    END IF

    DO nb=0,nbblocs-1
      DO j=1,nl
        string = string // BeginString // TO_string(j)
        DO i=1,nbcol_loc
          string = string // ' ' // TO_string(Mat(j,i+nb*nbcol_loc),rformat=Rf)
        END DO
        string = string // new_line('a')
      END DO
      IF (nl > 1 ) string = string // new_line('a')
      !write(out_unit,*) 'string: ' // new_line('a'),string ; flush(out_unit)
    END DO

    DO j=1,nl
      nfin=nc-nbcol_loc*nbblocs
      string = string // BeginString // TO_string(j)
      DO i=1,nfin
        string = string // ' ' // TO_string(Mat(j,i+nbcol_loc*nbblocs),rformat=Rf)
      END DO
      string = string // new_line('a')
    END DO

  END SUBROUTINE QDUtil_Write_Rk8Mat_string
  !!@description: TODO
  !!@param: TODO
  SUBROUTINE QDUtil_Write_Ck8Mat(Mat,nio,nbcol,Rformat,info,iprint)
    USE QDUtil_NumParameters_m, ONLY : out_unit,Rk8
    IMPLICIT NONE

    integer,                     intent(in) :: nio,nbcol
    complex(kind=Rk8),        intent(in) :: Mat(:,:)

    character (len=*), optional, intent(in) :: Rformat
    character (len=*), optional, intent(in) :: info
    integer,           optional, intent(in) :: iprint


    integer         :: nl,nc
    integer i,j,nb,nbblocs,nfin,nbcol_loc
    character (len=:), allocatable  :: wformat

    IF (present(iprint)) THEN
      IF (iprint /=0) RETURN ! it was MPI_id in the module mod_MPI
    END IF

    nl = size(Mat,dim=1)
      nc = size(Mat,dim=2)
      !write(out_unit,*) 'nl,nc,nbcol_loc',nl,nc,nbcol_loc
      nbcol_loc = nbcol
      IF (nbcol_loc > 10) nbcol_loc=10
      nbblocs=int(nc/nbcol_loc)
      IF (nbblocs*nbcol_loc == nc) nbblocs=nbblocs-1

      IF (present(Rformat)) THEN
        IF (present(info)) THEN
          CALL QDUtil_Format_OF_Line(wformat,nl,nbcol_loc,.TRUE.,Rformat,info)
        ELSE
          CALL QDUtil_Format_OF_Line(wformat,nl,nbcol_loc,.TRUE.,Rformat=Rformat)
        END IF
      ELSE
        IF (present(info)) THEN
          CALL QDUtil_Format_OF_Line(wformat,nl,nbcol_loc,.TRUE.,info=info)
        ELSE
          CALL QDUtil_Format_OF_Line(wformat,nl,nbcol_loc,.TRUE.)
        END IF
      END IF


      DO nb=0,nbblocs-1
        DO j=1,nl
          write(nio,wformat) j,(Mat(j,i+nb*nbcol_loc),i=1,nbcol_loc)
        END DO
        IF (nl > 1 ) write(nio,*)
      END DO
      DO j=1,nl
        nfin=nc-nbcol_loc*nbblocs
        write(nio,wformat) j,(Mat(j,i+nbcol_loc*nbblocs),i=1,nfin)
      END DO

      deallocate(wformat)

  END SUBROUTINE QDUtil_Write_Ck8Mat

  !!@description: TODO
  !!@param: TODO
  SUBROUTINE QDUtil_Write_Rk8Vec(Vec,nio,nbcol,Rformat,info,iprint)
    USE QDUtil_NumParameters_m, ONLY : out_unit,Rk8
    IMPLICIT NONE

    integer,                     intent(in) :: nio,nbcol
    real(kind=Rk8),            intent(in) :: Vec(:)

    character (len=*), optional, intent(in) :: Rformat
    character (len=*), optional, intent(in) :: info
    integer,           optional, intent(in) :: iprint


    integer           :: n,i,nb,nbblocs,nfin,nbcol_loc
    character (len=:), allocatable  :: wformat

    IF (present(iprint)) THEN
      IF (iprint /=0) RETURN ! it was MPI_id in the module mod_MPI
    END IF

    n = size(Vec)
       !write(out_unit,*) 'n,nbcol_loc',n,nbcol_loc
       nbcol_loc = nbcol
       IF (nbcol_loc > 10) nbcol_loc=10
       nbblocs=int(n/nbcol_loc)
       IF (nbblocs*nbcol_loc == n) nbblocs=nbblocs-1


       IF (present(Rformat)) THEN
         IF (present(info)) THEN
           CALL QDUtil_Format_OF_Line(wformat,0,nbcol_loc,.FALSE.,Rformat,info)
         ELSE
           CALL QDUtil_Format_OF_Line(wformat,0,nbcol_loc,.FALSE.,Rformat=Rformat)
         END IF
       ELSE
         IF (present(info)) THEN
           CALL QDUtil_Format_OF_Line(wformat,0,nbcol_loc,.FALSE.,info=info)
         ELSE
           CALL QDUtil_Format_OF_Line(wformat,0,nbcol_loc,.FALSE.)
         END IF
       END IF

       DO nb=0,nbblocs-1
         write(nio,wformat) (Vec(i+nb*nbcol_loc),i=1,nbcol_loc)
       END DO
       nfin=n-nbcol_loc*nbblocs
       write(nio,wformat) (Vec(i+nbcol_loc*nbblocs),i=1,nfin)

       deallocate(wformat)
  END SUBROUTINE QDUtil_Write_Rk8Vec

  !!@description: TODO
  !!@param: TODO
  SUBROUTINE QDUtil_Write_Ck8Vec(Vec,nio,nbcol,Rformat,info,iprint)
    USE QDUtil_NumParameters_m, ONLY : out_unit,Rk8
    IMPLICIT NONE

    integer,                     intent(in) :: nio,nbcol
    complex(kind=Rk8),         intent(in) :: Vec(:)

    character (len=*), optional, intent(in) :: Rformat
    character (len=*), optional, intent(in) :: info
    integer,           optional, intent(in) :: iprint

    integer           :: n,i,nb,nbblocs,nfin,nbcol_loc
    character (len=:), allocatable  :: wformat

    IF (present(iprint)) THEN
      IF (iprint /=0) RETURN ! it was MPI_id in the module mod_MPI
    END IF

      n = size(Vec)
      !write(out_unit,*) 'n,nbcol_loc',n,nbcol_loc
      nbcol_loc = nbcol
      IF (nbcol_loc > 10) nbcol_loc=10
      nbblocs=int(n/nbcol_loc)
      IF (nbblocs*nbcol_loc == n) nbblocs=nbblocs-1

      IF (present(Rformat)) THEN
        IF (present(info)) THEN
          CALL QDUtil_Format_OF_Line(wformat,0,nbcol_loc,.TRUE.,Rformat,info)
        ELSE
          CALL QDUtil_Format_OF_Line(wformat,0,nbcol_loc,.TRUE.,Rformat=Rformat)
        END IF
      ELSE
        IF (present(info)) THEN
          CALL QDUtil_Format_OF_Line(wformat,0,nbcol_loc,.TRUE.,info=info)
        ELSE
          CALL QDUtil_Format_OF_Line(wformat,0,nbcol_loc,.TRUE.)
        END IF
      END IF

      DO nb=0,nbblocs-1
        write(nio,wformat) (Vec(i+nb*nbcol_loc),i=1,nbcol_loc)
      END DO
      nfin=n-nbcol_loc*nbblocs
      write(nio,wformat) (Vec(i+nbcol_loc*nbblocs),i=1,nfin)

      deallocate(wformat)
  END SUBROUTINE QDUtil_Write_Ck8Vec

  SUBROUTINE QDUtil_Read_Rk8Mat(Mat,nio,nbcol_loc,err)
    USE QDUtil_NumParameters_m, ONLY : out_unit,Rk8
    IMPLICIT NONE

    integer,           intent(in)    :: nio,nbcol_loc
    integer,           intent(inout) :: err
    real(kind=Rk8),    intent(inout) :: Mat(:,:)

     integer i,j,jj,nb,nbblocs,nfin,nl,nc

     nl = size(Mat,dim=1)
     nc = size(Mat,dim=2)
     !write(out_unit,*) 'nl,nc,nbcol_loc',nl,nc,nbcol_loc


     nbblocs=int(nc/nbcol_loc)

     IF (nbblocs*nbcol_loc == nc) nbblocs=nbblocs-1
     err = 0

     !write(out_unit,*) 'nl,nc,nbcol_loc,nbblocs',nl,nc,nbcol_loc,nbblocs


     DO nb=0,nbblocs-1

         DO j=1,nl
           read(nio,*,IOSTAT=err) jj,(Mat(j,i+nb*nbcol_loc),i=1,nbcol_loc)
           IF (err /= 0) EXIT
         END DO

         IF (err /= 0) EXIT

         IF (nl > 1) read(nio,*,IOSTAT=err)
         IF (err /= 0) EXIT

     END DO

     nfin=nc-nbcol_loc*nbblocs
     IF (err == 0) THEN
       DO j=1,nl
         read(nio,*,IOSTAT=err) jj,(Mat(j,i+nbcol_loc*nbblocs),i=1,nfin)
         !write(out_unit,*) err,jj,(Mat(j,i+nbcol_loc*nbblocs),i=1,nfin)
         IF (err /= 0) EXIT
       END DO
     END IF

     IF (err /= 0) THEN
       CALL QDUtil_Write_Rk8Mat(Mat,out_unit,nbcol_loc)
       write(out_unit,*) ' ERROR in QDUtil_Read_Rk8Mat'
       write(out_unit,*) '  while reading a matrix'
       write(out_unit,*) '  end of file or end of record'
       write(out_unit,*) '  The matrix paramters: nl,nc,nbcol_loc',nl,nc,nbcol_loc
       write(out_unit,*) '  Internal paramters: nbblocs,nfin',nbblocs,nfin
       write(out_unit,*) ' Check your data !!'
     END IF

  END SUBROUTINE QDUtil_Read_Rk8Mat
  SUBROUTINE QDUtil_Read_Ck8Mat(Mat,nio,nbcol_loc,err)
    USE QDUtil_NumParameters_m, ONLY : out_unit,Rk8
    IMPLICIT NONE

    integer,              intent(in)    :: nio,nbcol_loc
    complex(kind=Rk8), intent(inout) :: Mat(:,:)
    integer,              intent(inout) :: err

     integer i,j,jj,nb,nbblocs,nfin,nl,nc

     nl = size(Mat,dim=1)
     nc = size(Mat,dim=2)
     !write(out_unit,*) 'nl,nc,nbcol_loc',nl,nc,nbcol_loc


     nbblocs=int(nc/nbcol_loc)
     err = 0
     IF (nbblocs*nbcol_loc == nc) nbblocs=nbblocs-1

     DO nb=0,nbblocs-1

         DO j=1,nl
           read(nio,*,IOSTAT=err) jj,(Mat(j,i+nb*nbcol_loc),i=1,nbcol_loc)
           IF (err /= 0) EXIT
         END DO

         IF (err /= 0) EXIT

         IF (nl > 1) read(nio,*,IOSTAT=err)
         IF (err /= 0) EXIT

     END DO

     IF (err == 0) THEN
       DO j=1,nl
         nfin=nc-nbcol_loc*nbblocs
         read(nio,*,IOSTAT=err) jj,(Mat(j,i+nbcol_loc*nbblocs),i=1,nfin)
         IF (err /= 0) EXIT
       END DO
     END IF

     IF (err /= 0) THEN
       CALL QDUtil_Write_Ck8Mat(Mat,out_unit,nbcol_loc)
       write(out_unit,*) ' ERROR in QDUtil_Read_Ck8Mat'
       write(out_unit,*) '  while reading a matrix'
       write(out_unit,*) '  end of file or end of record'
       write(out_unit,*) '  The matrix paramters: nl,nc,nbcol_loc',nl,nc,nbcol_loc
       write(out_unit,*) ' Check your data !!'
     END IF

  END SUBROUTINE QDUtil_Read_Ck8Mat

  !================================================================
  ! ++    read a vector in line
  !================================================================
  SUBROUTINE QDUtil_Read_Rk8Vec(Vec,nio,nbcol_loc,err)
    USE QDUtil_NumParameters_m, ONLY : out_unit,Rk8
    IMPLICIT NONE

    integer, intent(in)                :: nio,nbcol_loc
     real(kind=Rk8), intent(inout)  :: Vec(:)
     integer, intent(inout)            :: err

     integer :: n,i,nb,nbblocs,nfin

     n = size(Vec,dim=1)
     nbblocs=int(n/nbcol_loc)
     err = 0


     IF (nbblocs*nbcol_loc == n) nbblocs=nbblocs-1

     DO nb=0,nbblocs-1
       read(nio,*,IOSTAT=err) (Vec(i+nb*nbcol_loc),i=1,nbcol_loc)
       IF (err /= 0) EXIT
     END DO

     nfin=n-nbcol_loc*nbblocs
     read(nio,*,IOSTAT=err) (Vec(i+nbcol_loc*nbblocs),i=1,nfin)

     IF (err /= 0) THEN
       write(out_unit,*) ' ERROR in QDUtil_Read_Rk8Vec'
       write(out_unit,*) '  while reading a vector'
       write(out_unit,*) '  end of file or end of record'
       write(out_unit,*) '  The vector paramters: n,nbcol_loc',n,nbcol_loc
       write(out_unit,*) ' Check your data !!'
     END IF

  END SUBROUTINE QDUtil_Read_Rk8Vec
  SUBROUTINE QDUtil_Read_Ck8Vec(Vec,nio,nbcol_loc,err)
    USE QDUtil_NumParameters_m, ONLY : out_unit,Rk8
    IMPLICIT NONE

    integer, intent(in)                 :: nio,nbcol_loc
    complex(kind=Rk8), intent(inout) :: Vec(:)
    integer, intent(inout)              :: err

     integer :: n,i,nb,nbblocs,nfin

     n = size(Vec,dim=1)
     nbblocs=int(n/nbcol_loc)
     err = 0

     IF (nbblocs*nbcol_loc == n) nbblocs=nbblocs-1

     DO nb=0,nbblocs-1
       read(nio,*,IOSTAT=err) (Vec(i+nb*nbcol_loc),i=1,nbcol_loc)
       IF (err /= 0) EXIT
     END DO

     nfin=n-nbcol_loc*nbblocs
     read(nio,*,IOSTAT=err) (Vec(i+nbcol_loc*nbblocs),i=1,nfin)

     IF (err /= 0) THEN
       write(out_unit,*) ' ERROR in QDUtil_Read_Ck8Vec'
       write(out_unit,*) '  while reading a vector'
       write(out_unit,*) '  end of file or end of record'
       write(out_unit,*) '  The vector paramters: n,nbcol_loc',n,nbcol_loc
       write(out_unit,*) ' Check your data !!'
     END IF

  END SUBROUTINE QDUtil_Read_Ck8Vec


  SUBROUTINE QDUtil_Write_Rk4Mat(Mat,nio,nbcol,Rformat,info,iprint)
    USE QDUtil_NumParameters_m, ONLY : out_unit,Rk4
    IMPLICIT NONE

    integer,                     intent(in) :: nio,nbcol
    real(kind=Rk4),              intent(in) :: Mat(:,:)

    character (len=*), optional, intent(in) :: Rformat
    character (len=*), optional, intent(in) :: info
    integer,           optional, intent(in) :: iprint

    integer         :: nl,nc
    integer         :: i,j,nb,nbblocs,nfin,nbcol_loc
    character (len=:), allocatable  :: wformat

    IF (present(iprint)) THEN
      IF (iprint /=0) RETURN ! it was MPI_id in the module mod_MPI
    END IF

    nl = size(Mat,dim=1)
    nc = size(Mat,dim=2)
    !write(out_unit,*) 'nl,nc,nbcol_loc',nl,nc,nbcol_loc
    nbcol_loc = nbcol
    IF (nbcol_loc > 10) nbcol_loc=10
    nbblocs=int(nc/nbcol_loc)
    IF (nbblocs*nbcol_loc == nc) nbblocs=nbblocs-1

    IF (present(Rformat)) THEN
      IF (present(info)) THEN
        CALL QDUtil_Format_OF_Line(wformat,nl,nbcol_loc,.FALSE.,Rformat,info)
      ELSE
        CALL QDUtil_Format_OF_Line(wformat,nl,nbcol_loc,.FALSE.,Rformat=Rformat)
      END IF
    ELSE
      IF (present(info)) THEN
        CALL QDUtil_Format_OF_Line(wformat,nl,nbcol_loc,.FALSE.,info=info)
      ELSE
        CALL QDUtil_Format_OF_Line(wformat,nl,nbcol_loc,.FALSE.)
      END IF
    END IF

      DO nb=0,nbblocs-1
        DO j=1,nl
          write(nio,wformat) j,(Mat(j,i+nb*nbcol_loc),i=1,nbcol_loc)
        END DO
        IF (nl > 1 ) write(nio,*)
      END DO
      DO j=1,nl
        nfin=nc-nbcol_loc*nbblocs
        write(nio,wformat) j,(Mat(j,i+nbcol_loc*nbblocs),i=1,nfin)
      END DO

    deallocate(wformat)

  END SUBROUTINE QDUtil_Write_Rk4Mat
  SUBROUTINE QDUtil_Write_Rk4Mat_string(Mat,string,nbcol,Rformat,info,iprint)
    USE QDUtil_NumParameters_m, ONLY : out_unit,Rk4
    USE QDUtil_String_m,        ONLY : TO_string
    IMPLICIT NONE

    integer,                        intent(in)    :: nbcol
    character (len=:), allocatable, intent(inout) :: string
    real(kind=Rk4),              intent(in)    :: Mat(:,:)

    character (len=*), optional,    intent(in)    :: Rformat
    character (len=*), optional,    intent(in)    :: info
    integer,           optional,    intent(in)    :: iprint

    integer         :: nl,nc
    integer         :: i,j,nb,nbblocs,nfin,nbcol_loc
    character (len=:), allocatable :: BeginString
    character (len=:), allocatable :: Rf

    string = ''

    IF (present(iprint)) THEN
      IF (iprint /=0) RETURN ! it was MPI_id in the module mod_MPI
    END IF

    nl = size(Mat,dim=1)
    nc = size(Mat,dim=2)
 
    nbcol_loc = nbcol
    IF (nbcol_loc > 10) nbcol_loc=10
    nbblocs=int(nc/nbcol_loc)
    IF (nbblocs*nbcol_loc == nc) nbblocs=nbblocs-1

    !write(out_unit,*) 'nl,nc,nbcol_loc',nl,nc,nbcol_loc
    !write(out_unit,*) 'string: ',string ; flush(out_unit)

    IF (present(info)) THEN
      BeginString = trim(info) // ' '
    ELSE
      BeginString = ' '
    END IF

    IF (present(Rformat)) THEN
      Rf = trim(Rformat)
    ELSE
      Rf = RMatIO_format
    END IF

    DO nb=0,nbblocs-1
      DO j=1,nl
        string = string // BeginString // TO_string(j)
        DO i=1,nbcol_loc
          string = string // ' ' // TO_string(Mat(j,i+nb*nbcol_loc),rformat=Rf)
        END DO
        string = string // new_line('a')
      END DO
      IF (nl > 1 ) string = string // new_line('a')
      !write(out_unit,*) 'string: ' // new_line('a'),string ; flush(out_unit)
    END DO

    DO j=1,nl
      nfin=nc-nbcol_loc*nbblocs
      string = string // BeginString // TO_string(j)
      DO i=1,nfin
        string = string // ' ' // TO_string(Mat(j,i+nbcol_loc*nbblocs),rformat=Rf)
      END DO
      string = string // new_line('a')
    END DO

  END SUBROUTINE QDUtil_Write_Rk4Mat_string
  !!@description: TODO
  !!@param: TODO
  SUBROUTINE QDUtil_Write_Ck4Mat(Mat,nio,nbcol,Rformat,info,iprint)
    USE QDUtil_NumParameters_m, ONLY : out_unit,Rk4
    IMPLICIT NONE

    integer,                     intent(in) :: nio,nbcol
    complex(kind=Rk4),        intent(in) :: Mat(:,:)

    character (len=*), optional, intent(in) :: Rformat
    character (len=*), optional, intent(in) :: info
    integer,           optional, intent(in) :: iprint


    integer         :: nl,nc
    integer i,j,nb,nbblocs,nfin,nbcol_loc
    character (len=:), allocatable  :: wformat

    IF (present(iprint)) THEN
      IF (iprint /=0) RETURN ! it was MPI_id in the module mod_MPI
    END IF

    nl = size(Mat,dim=1)
      nc = size(Mat,dim=2)
      !write(out_unit,*) 'nl,nc,nbcol_loc',nl,nc,nbcol_loc
      nbcol_loc = nbcol
      IF (nbcol_loc > 10) nbcol_loc=10
      nbblocs=int(nc/nbcol_loc)
      IF (nbblocs*nbcol_loc == nc) nbblocs=nbblocs-1

      IF (present(Rformat)) THEN
        IF (present(info)) THEN
          CALL QDUtil_Format_OF_Line(wformat,nl,nbcol_loc,.TRUE.,Rformat,info)
        ELSE
          CALL QDUtil_Format_OF_Line(wformat,nl,nbcol_loc,.TRUE.,Rformat=Rformat)
        END IF
      ELSE
        IF (present(info)) THEN
          CALL QDUtil_Format_OF_Line(wformat,nl,nbcol_loc,.TRUE.,info=info)
        ELSE
          CALL QDUtil_Format_OF_Line(wformat,nl,nbcol_loc,.TRUE.)
        END IF
      END IF


      DO nb=0,nbblocs-1
        DO j=1,nl
          write(nio,wformat) j,(Mat(j,i+nb*nbcol_loc),i=1,nbcol_loc)
        END DO
        IF (nl > 1 ) write(nio,*)
      END DO
      DO j=1,nl
        nfin=nc-nbcol_loc*nbblocs
        write(nio,wformat) j,(Mat(j,i+nbcol_loc*nbblocs),i=1,nfin)
      END DO

      deallocate(wformat)

  END SUBROUTINE QDUtil_Write_Ck4Mat

  !!@description: TODO
  !!@param: TODO
  SUBROUTINE QDUtil_Write_Rk4Vec(Vec,nio,nbcol,Rformat,info,iprint)
    USE QDUtil_NumParameters_m, ONLY : out_unit,Rk4
    IMPLICIT NONE

    integer,                     intent(in) :: nio,nbcol
    real(kind=Rk4),           intent(in) :: Vec(:)

    character (len=*), optional, intent(in) :: Rformat
    character (len=*), optional, intent(in) :: info
    integer,           optional, intent(in) :: iprint


    integer           :: n,i,nb,nbblocs,nfin,nbcol_loc
    character (len=:), allocatable  :: wformat

    IF (present(iprint)) THEN
      IF (iprint /=0) RETURN ! it was MPI_id in the module mod_MPI
    END IF

    n = size(Vec)
       !write(out_unit,*) 'n,nbcol_loc',n,nbcol_loc
       nbcol_loc = nbcol
       IF (nbcol_loc > 10) nbcol_loc=10
       nbblocs=int(n/nbcol_loc)
       IF (nbblocs*nbcol_loc == n) nbblocs=nbblocs-1


       IF (present(Rformat)) THEN
         IF (present(info)) THEN
           CALL QDUtil_Format_OF_Line(wformat,0,nbcol_loc,.FALSE.,Rformat,info)
         ELSE
           CALL QDUtil_Format_OF_Line(wformat,0,nbcol_loc,.FALSE.,Rformat=Rformat)
         END IF
       ELSE
         IF (present(info)) THEN
           CALL QDUtil_Format_OF_Line(wformat,0,nbcol_loc,.FALSE.,info=info)
         ELSE
           CALL QDUtil_Format_OF_Line(wformat,0,nbcol_loc,.FALSE.)
         END IF
       END IF

       DO nb=0,nbblocs-1
         write(nio,wformat) (Vec(i+nb*nbcol_loc),i=1,nbcol_loc)
       END DO
       nfin=n-nbcol_loc*nbblocs
       write(nio,wformat) (Vec(i+nbcol_loc*nbblocs),i=1,nfin)

       deallocate(wformat)
  END SUBROUTINE QDUtil_Write_Rk4Vec

  !!@description: TODO
  !!@param: TODO
  SUBROUTINE QDUtil_Write_Ck4Vec(Vec,nio,nbcol,Rformat,info,iprint)
    USE QDUtil_NumParameters_m, ONLY : out_unit,Rk4
    IMPLICIT NONE

    integer,                     intent(in) :: nio,nbcol
    complex(kind=Rk4),         intent(in) :: Vec(:)

    character (len=*), optional, intent(in) :: Rformat
    character (len=*), optional, intent(in) :: info
    integer,           optional, intent(in) :: iprint

    integer           :: n,i,nb,nbblocs,nfin,nbcol_loc
    character (len=:), allocatable  :: wformat

    IF (present(iprint)) THEN
      IF (iprint /=0) RETURN ! it was MPI_id in the module mod_MPI
    END IF

      n = size(Vec)
      !write(out_unit,*) 'n,nbcol_loc',n,nbcol_loc
      nbcol_loc = nbcol
      IF (nbcol_loc > 10) nbcol_loc=10
      nbblocs=int(n/nbcol_loc)
      IF (nbblocs*nbcol_loc == n) nbblocs=nbblocs-1

      IF (present(Rformat)) THEN
        IF (present(info)) THEN
          CALL QDUtil_Format_OF_Line(wformat,0,nbcol_loc,.TRUE.,Rformat,info)
        ELSE
          CALL QDUtil_Format_OF_Line(wformat,0,nbcol_loc,.TRUE.,Rformat=Rformat)
        END IF
      ELSE
        IF (present(info)) THEN
          CALL QDUtil_Format_OF_Line(wformat,0,nbcol_loc,.TRUE.,info=info)
        ELSE
          CALL QDUtil_Format_OF_Line(wformat,0,nbcol_loc,.TRUE.)
        END IF
      END IF

      DO nb=0,nbblocs-1
        write(nio,wformat) (Vec(i+nb*nbcol_loc),i=1,nbcol_loc)
      END DO
      nfin=n-nbcol_loc*nbblocs
      write(nio,wformat) (Vec(i+nbcol_loc*nbblocs),i=1,nfin)

      deallocate(wformat)
  END SUBROUTINE QDUtil_Write_Ck4Vec

  SUBROUTINE QDUtil_Read_Rk4Mat(Mat,nio,nbcol_loc,err)
    USE QDUtil_NumParameters_m, ONLY : out_unit,Rk4
    IMPLICIT NONE

    integer,            intent(in)    :: nio,nbcol_loc
     integer,           intent(inout) :: err
     real(kind=Rk4), intent(inout) :: Mat(:,:)

     integer i,j,jj,nb,nbblocs,nfin,nl,nc

     nl = size(Mat,dim=1)
     nc = size(Mat,dim=2)
     !write(out_unit,*) 'nl,nc,nbcol_loc',nl,nc,nbcol_loc


     nbblocs=int(nc/nbcol_loc)

     IF (nbblocs*nbcol_loc == nc) nbblocs=nbblocs-1
     err = 0

     !write(out_unit,*) 'nl,nc,nbcol_loc,nbblocs',nl,nc,nbcol_loc,nbblocs


     DO nb=0,nbblocs-1

         DO j=1,nl
           read(nio,*,IOSTAT=err) jj,(Mat(j,i+nb*nbcol_loc),i=1,nbcol_loc)
           IF (err /= 0) EXIT
         END DO

         IF (err /= 0) EXIT

         IF (nl > 1) read(nio,*,IOSTAT=err)
         IF (err /= 0) EXIT

     END DO

     nfin=nc-nbcol_loc*nbblocs
     IF (err == 0) THEN
       DO j=1,nl
         read(nio,*,IOSTAT=err) jj,(Mat(j,i+nbcol_loc*nbblocs),i=1,nfin)
         !write(out_unit,*) err,jj,(Mat(j,i+nbcol_loc*nbblocs),i=1,nfin)
         IF (err /= 0) EXIT
       END DO
     END IF

     IF (err /= 0) THEN
       CALL QDUtil_Write_Rk4Mat(Mat,out_unit,nbcol_loc)
       write(out_unit,*) ' ERROR in QDUtil_Read_Rk4Mat'
       write(out_unit,*) '  while reading a matrix'
       write(out_unit,*) '  end of file or end of record'
       write(out_unit,*) '  The matrix paramters: nl,nc,nbcol_loc',nl,nc,nbcol_loc
       write(out_unit,*) '  Internal paramters: nbblocs,nfin',nbblocs,nfin
       write(out_unit,*) ' Check your data !!'
     END IF

  END SUBROUTINE QDUtil_Read_Rk4Mat
  SUBROUTINE QDUtil_Read_Ck4Mat(Mat,nio,nbcol_loc,err)
    USE QDUtil_NumParameters_m, ONLY : out_unit,Rk4
    IMPLICIT NONE

    integer,             intent(in)    :: nio,nbcol_loc
    complex(kind=Rk4), intent(inout) :: Mat(:,:)
    integer,             intent(inout) :: err

     integer i,j,jj,nb,nbblocs,nfin,nl,nc

     nl = size(Mat,dim=1)
     nc = size(Mat,dim=2)
     !write(out_unit,*) 'nl,nc,nbcol_loc',nl,nc,nbcol_loc


     nbblocs=int(nc/nbcol_loc)
     err = 0
     IF (nbblocs*nbcol_loc == nc) nbblocs=nbblocs-1

     DO nb=0,nbblocs-1

         DO j=1,nl
           read(nio,*,IOSTAT=err) jj,(Mat(j,i+nb*nbcol_loc),i=1,nbcol_loc)
           IF (err /= 0) EXIT
         END DO

         IF (err /= 0) EXIT

         IF (nl > 1) read(nio,*,IOSTAT=err)
         IF (err /= 0) EXIT

     END DO

     IF (err == 0) THEN
       DO j=1,nl
         nfin=nc-nbcol_loc*nbblocs
         read(nio,*,IOSTAT=err) jj,(Mat(j,i+nbcol_loc*nbblocs),i=1,nfin)
         IF (err /= 0) EXIT
       END DO
     END IF

     IF (err /= 0) THEN
       CALL QDUtil_Write_Ck4Mat(Mat,out_unit,nbcol_loc)
       write(out_unit,*) ' ERROR in QDUtil_Read_Ck4Mat'
       write(out_unit,*) '  while reading a matrix'
       write(out_unit,*) '  end of file or end of record'
       write(out_unit,*) '  The matrix paramters: nl,nc,nbcol_loc',nl,nc,nbcol_loc
       write(out_unit,*) ' Check your data !!'
     END IF

  END SUBROUTINE QDUtil_Read_Ck4Mat

  !================================================================
  ! ++    read a vector in line
  !================================================================
  SUBROUTINE QDUtil_Read_Rk4Vec(Vec,nio,nbcol_loc,err)
    USE QDUtil_NumParameters_m, ONLY : out_unit,Rk4
    IMPLICIT NONE

    integer, intent(in)                :: nio,nbcol_loc
     real(kind=Rk4), intent(inout)  :: Vec(:)
     integer, intent(inout)            :: err

     integer :: n,i,nb,nbblocs,nfin

     n = size(Vec,dim=1)
     nbblocs=int(n/nbcol_loc)
     err = 0


     IF (nbblocs*nbcol_loc == n) nbblocs=nbblocs-1

     DO nb=0,nbblocs-1
       read(nio,*,IOSTAT=err) (Vec(i+nb*nbcol_loc),i=1,nbcol_loc)
       IF (err /= 0) EXIT
     END DO

     nfin=n-nbcol_loc*nbblocs
     read(nio,*,IOSTAT=err) (Vec(i+nbcol_loc*nbblocs),i=1,nfin)

     IF (err /= 0) THEN
       write(out_unit,*) ' ERROR in QDUtil_Read_Rk4Vec'
       write(out_unit,*) '  while reading a vector'
       write(out_unit,*) '  end of file or end of record'
       write(out_unit,*) '  The vector paramters: n,nbcol_loc',n,nbcol_loc
       write(out_unit,*) ' Check your data !!'
     END IF

  END SUBROUTINE QDUtil_Read_Rk4Vec
  SUBROUTINE QDUtil_Read_Ck4Vec(Vec,nio,nbcol_loc,err)
    USE QDUtil_NumParameters_m, ONLY : out_unit,Rk4
    IMPLICIT NONE

    integer, intent(in)                 :: nio,nbcol_loc
    complex(kind=Rk4), intent(inout) :: Vec(:)
    integer, intent(inout)              :: err

     integer :: n,i,nb,nbblocs,nfin

     n = size(Vec,dim=1)
     nbblocs=int(n/nbcol_loc)
     err = 0

     IF (nbblocs*nbcol_loc == n) nbblocs=nbblocs-1

     DO nb=0,nbblocs-1
       read(nio,*,IOSTAT=err) (Vec(i+nb*nbcol_loc),i=1,nbcol_loc)
       IF (err /= 0) EXIT
     END DO

     nfin=n-nbcol_loc*nbblocs
     read(nio,*,IOSTAT=err) (Vec(i+nbcol_loc*nbblocs),i=1,nfin)

     IF (err /= 0) THEN
       write(out_unit,*) ' ERROR in QDUtil_Read_Ck4Vec'
       write(out_unit,*) '  while reading a vector'
       write(out_unit,*) '  end of file or end of record'
       write(out_unit,*) '  The vector paramters: n,nbcol_loc',n,nbcol_loc
       write(out_unit,*) ' Check your data !!'
     END IF

  END SUBROUTINE QDUtil_Read_Ck4Vec



  SUBROUTINE QDUtil_Write_Rk16Mat(Mat,nio,nbcol,Rformat,info,iprint)
    USE QDUtil_NumParameters_m, ONLY : out_unit,Rk16
    IMPLICIT NONE

    integer,                     intent(in) :: nio,nbcol
    real(kind=Rk16),          intent(in) :: Mat(:,:)

    character (len=*), optional, intent(in) :: Rformat
    character (len=*), optional, intent(in) :: info
    integer,           optional, intent(in) :: iprint

    integer         :: nl,nc
    integer         :: i,j,nb,nbblocs,nfin,nbcol_loc
    character (len=:), allocatable  :: wformat

    IF (present(iprint)) THEN
      IF (iprint /=0) RETURN ! it was MPI_id in the module mod_MPI
    END IF

    nl = size(Mat,dim=1)
    nc = size(Mat,dim=2)
    !write(out_unit,*) 'nl,nc,nbcol_loc',nl,nc,nbcol_loc
    nbcol_loc = nbcol
    IF (nbcol_loc > 10) nbcol_loc=10
    nbblocs=int(nc/nbcol_loc)
    IF (nbblocs*nbcol_loc == nc) nbblocs=nbblocs-1

    IF (present(Rformat)) THEN
      IF (present(info)) THEN
        CALL QDUtil_Format_OF_Line(wformat,nl,nbcol_loc,.FALSE.,Rformat,info)
      ELSE
        CALL QDUtil_Format_OF_Line(wformat,nl,nbcol_loc,.FALSE.,Rformat=Rformat)
      END IF
    ELSE
      IF (present(info)) THEN
        CALL QDUtil_Format_OF_Line(wformat,nl,nbcol_loc,.FALSE.,info=info)
      ELSE
        CALL QDUtil_Format_OF_Line(wformat,nl,nbcol_loc,.FALSE.)
      END IF
    END IF

      DO nb=0,nbblocs-1
        DO j=1,nl
          write(nio,wformat) j,(Mat(j,i+nb*nbcol_loc),i=1,nbcol_loc)
        END DO
        IF (nl > 1 ) write(nio,*)
      END DO
      DO j=1,nl
        nfin=nc-nbcol_loc*nbblocs
        write(nio,wformat) j,(Mat(j,i+nbcol_loc*nbblocs),i=1,nfin)
      END DO

    deallocate(wformat)

  END SUBROUTINE QDUtil_Write_Rk16Mat
  SUBROUTINE QDUtil_Write_Rk16Mat_string(Mat,string,nbcol,Rformat,info,iprint)
    USE QDUtil_NumParameters_m, ONLY : out_unit,Rk16
    USE QDUtil_String_m,        ONLY : TO_string
    IMPLICIT NONE

    integer,                        intent(in)    :: nbcol
    character (len=:), allocatable, intent(inout) :: string
    real(kind=Rk16),              intent(in)    :: Mat(:,:)

    character (len=*), optional,    intent(in)    :: Rformat
    character (len=*), optional,    intent(in)    :: info
    integer,           optional,    intent(in)    :: iprint

    integer         :: nl,nc
    integer         :: i,j,nb,nbblocs,nfin,nbcol_loc
    character (len=:), allocatable :: BeginString
    character (len=:), allocatable :: Rf

    string = ''

    IF (present(iprint)) THEN
      IF (iprint /=0) RETURN ! it was MPI_id in the module mod_MPI
    END IF

    nl = size(Mat,dim=1)
    nc = size(Mat,dim=2)
 
    nbcol_loc = nbcol
    IF (nbcol_loc > 10) nbcol_loc=10
    nbblocs=int(nc/nbcol_loc)
    IF (nbblocs*nbcol_loc == nc) nbblocs=nbblocs-1

    !write(out_unit,*) 'nl,nc,nbcol_loc',nl,nc,nbcol_loc
    !write(out_unit,*) 'string: ',string ; flush(out_unit)

    IF (present(info)) THEN
      BeginString = trim(info) // ' '
    ELSE
      BeginString = ' '
    END IF

    IF (present(Rformat)) THEN
      Rf = trim(Rformat)
    ELSE
      Rf = RMatIO_format
    END IF

    DO nb=0,nbblocs-1
      DO j=1,nl
        string = string // BeginString // TO_string(j)
        DO i=1,nbcol_loc
          string = string // ' ' // TO_string(Mat(j,i+nb*nbcol_loc),rformat=Rf)
        END DO
        string = string // new_line('a')
      END DO
      IF (nl > 1 ) string = string // new_line('a')
      !write(out_unit,*) 'string: ' // new_line('a'),string ; flush(out_unit)
    END DO

    DO j=1,nl
      nfin=nc-nbcol_loc*nbblocs
      string = string // BeginString // TO_string(j)
      DO i=1,nfin
        string = string // ' ' // TO_string(Mat(j,i+nbcol_loc*nbblocs),rformat=Rf)
      END DO
      string = string // new_line('a')
    END DO

  END SUBROUTINE QDUtil_Write_Rk16Mat_string
  !!@description: TODO
  !!@param: TODO
  SUBROUTINE QDUtil_Write_Ck16Mat(Mat,nio,nbcol,Rformat,info,iprint)
    USE QDUtil_NumParameters_m, ONLY : out_unit,Rk16
    IMPLICIT NONE

    integer,                      intent(in) :: nio,nbcol
    complex(kind=Rk16),        intent(in) :: Mat(:,:)

    character (len=*), optional, intent(in) :: Rformat
    character (len=*), optional, intent(in) :: info
    integer,           optional, intent(in) :: iprint


    integer         :: nl,nc
    integer i,j,nb,nbblocs,nfin,nbcol_loc
    character (len=:), allocatable  :: wformat

    IF (present(iprint)) THEN
      IF (iprint /=0) RETURN ! it was MPI_id in the module mod_MPI
    END IF

    nl = size(Mat,dim=1)
      nc = size(Mat,dim=2)
      !write(out_unit,*) 'nl,nc,nbcol_loc',nl,nc,nbcol_loc
      nbcol_loc = nbcol
      IF (nbcol_loc > 10) nbcol_loc=10
      nbblocs=int(nc/nbcol_loc)
      IF (nbblocs*nbcol_loc == nc) nbblocs=nbblocs-1

      IF (present(Rformat)) THEN
        IF (present(info)) THEN
          CALL QDUtil_Format_OF_Line(wformat,nl,nbcol_loc,.TRUE.,Rformat,info)
        ELSE
          CALL QDUtil_Format_OF_Line(wformat,nl,nbcol_loc,.TRUE.,Rformat=Rformat)
        END IF
      ELSE
        IF (present(info)) THEN
          CALL QDUtil_Format_OF_Line(wformat,nl,nbcol_loc,.TRUE.,info=info)
        ELSE
          CALL QDUtil_Format_OF_Line(wformat,nl,nbcol_loc,.TRUE.)
        END IF
      END IF


      DO nb=0,nbblocs-1
        DO j=1,nl
          write(nio,wformat) j,(Mat(j,i+nb*nbcol_loc),i=1,nbcol_loc)
        END DO
        IF (nl > 1 ) write(nio,*)
      END DO
      DO j=1,nl
        nfin=nc-nbcol_loc*nbblocs
        write(nio,wformat) j,(Mat(j,i+nbcol_loc*nbblocs),i=1,nfin)
      END DO

      deallocate(wformat)

  END SUBROUTINE QDUtil_Write_Ck16Mat

  !!@description: TODO
  !!@param: TODO
  SUBROUTINE QDUtil_Write_Rk16Vec(Vec,nio,nbcol,Rformat,info,iprint)
    USE QDUtil_NumParameters_m, ONLY : out_unit,Rk16
    IMPLICIT NONE

    integer,                     intent(in) :: nio,nbcol
    real(kind=Rk16),            intent(in) :: Vec(:)

    character (len=*), optional, intent(in) :: Rformat
    character (len=*), optional, intent(in) :: info
    integer,           optional, intent(in) :: iprint


    integer           :: n,i,nb,nbblocs,nfin,nbcol_loc
    character (len=:), allocatable  :: wformat

    IF (present(iprint)) THEN
      IF (iprint /=0) RETURN ! it was MPI_id in the module mod_MPI
    END IF

    n = size(Vec)
       !write(out_unit,*) 'n,nbcol_loc',n,nbcol_loc
       nbcol_loc = nbcol
       IF (nbcol_loc > 10) nbcol_loc=10
       nbblocs=int(n/nbcol_loc)
       IF (nbblocs*nbcol_loc == n) nbblocs=nbblocs-1


       IF (present(Rformat)) THEN
         IF (present(info)) THEN
           CALL QDUtil_Format_OF_Line(wformat,0,nbcol_loc,.FALSE.,Rformat,info)
         ELSE
           CALL QDUtil_Format_OF_Line(wformat,0,nbcol_loc,.FALSE.,Rformat=Rformat)
         END IF
       ELSE
         IF (present(info)) THEN
           CALL QDUtil_Format_OF_Line(wformat,0,nbcol_loc,.FALSE.,info=info)
         ELSE
           CALL QDUtil_Format_OF_Line(wformat,0,nbcol_loc,.FALSE.)
         END IF
       END IF

       DO nb=0,nbblocs-1
         write(nio,wformat) (Vec(i+nb*nbcol_loc),i=1,nbcol_loc)
       END DO
       nfin=n-nbcol_loc*nbblocs
       write(nio,wformat) (Vec(i+nbcol_loc*nbblocs),i=1,nfin)

       deallocate(wformat)
  END SUBROUTINE QDUtil_Write_Rk16Vec

  !!@description: TODO
  !!@param: TODO
  SUBROUTINE QDUtil_Write_Ck16Vec(Vec,nio,nbcol,Rformat,info,iprint)
    USE QDUtil_NumParameters_m, ONLY : out_unit,Rk16
    IMPLICIT NONE

    integer,                     intent(in) :: nio,nbcol
    complex(kind=Rk16),       intent(in) :: Vec(:)

    character (len=*), optional, intent(in) :: Rformat
    character (len=*), optional, intent(in) :: info
    integer,           optional, intent(in) :: iprint

    integer           :: n,i,nb,nbblocs,nfin,nbcol_loc
    character (len=:), allocatable  :: wformat

    IF (present(iprint)) THEN
      IF (iprint /=0) RETURN ! it was MPI_id in the module mod_MPI
    END IF

      n = size(Vec)
      !write(out_unit,*) 'n,nbcol_loc',n,nbcol_loc
      nbcol_loc = nbcol
      IF (nbcol_loc > 10) nbcol_loc=10
      nbblocs=int(n/nbcol_loc)
      IF (nbblocs*nbcol_loc == n) nbblocs=nbblocs-1

      IF (present(Rformat)) THEN
        IF (present(info)) THEN
          CALL QDUtil_Format_OF_Line(wformat,0,nbcol_loc,.TRUE.,Rformat,info)
        ELSE
          CALL QDUtil_Format_OF_Line(wformat,0,nbcol_loc,.TRUE.,Rformat=Rformat)
        END IF
      ELSE
        IF (present(info)) THEN
          CALL QDUtil_Format_OF_Line(wformat,0,nbcol_loc,.TRUE.,info=info)
        ELSE
          CALL QDUtil_Format_OF_Line(wformat,0,nbcol_loc,.TRUE.)
        END IF
      END IF

      DO nb=0,nbblocs-1
        write(nio,wformat) (Vec(i+nb*nbcol_loc),i=1,nbcol_loc)
      END DO
      nfin=n-nbcol_loc*nbblocs
      write(nio,wformat) (Vec(i+nbcol_loc*nbblocs),i=1,nfin)

      deallocate(wformat)
  END SUBROUTINE QDUtil_Write_Ck16Vec

  SUBROUTINE QDUtil_Read_Rk16Mat(Mat,nio,nbcol_loc,err)
    USE QDUtil_NumParameters_m, ONLY : out_unit,Rk16
    IMPLICIT NONE

    integer,             intent(in)    :: nio,nbcol_loc
     integer,            intent(inout) :: err
     real(kind=Rk16), intent(inout) :: Mat(:,:)

     integer i,j,jj,nb,nbblocs,nfin,nl,nc

     nl = size(Mat,dim=1)
     nc = size(Mat,dim=2)
     !write(out_unit,*) 'nl,nc,nbcol_loc',nl,nc,nbcol_loc


     nbblocs=int(nc/nbcol_loc)

     IF (nbblocs*nbcol_loc == nc) nbblocs=nbblocs-1
     err = 0

     !write(out_unit,*) 'nl,nc,nbcol_loc,nbblocs',nl,nc,nbcol_loc,nbblocs


     DO nb=0,nbblocs-1

         DO j=1,nl
           read(nio,*,IOSTAT=err) jj,(Mat(j,i+nb*nbcol_loc),i=1,nbcol_loc)
           IF (err /= 0) EXIT
         END DO

         IF (err /= 0) EXIT

         IF (nl > 1) read(nio,*,IOSTAT=err)
         IF (err /= 0) EXIT

     END DO

     nfin=nc-nbcol_loc*nbblocs
     IF (err == 0) THEN
       DO j=1,nl
         read(nio,*,IOSTAT=err) jj,(Mat(j,i+nbcol_loc*nbblocs),i=1,nfin)
         !write(out_unit,*) err,jj,(Mat(j,i+nbcol_loc*nbblocs),i=1,nfin)
         IF (err /= 0) EXIT
       END DO
     END IF

     IF (err /= 0) THEN
       CALL QDUtil_Write_Rk16Mat(Mat,out_unit,nbcol_loc)
       write(out_unit,*) ' ERROR in QDUtil_Read_Rk16Mat'
       write(out_unit,*) '  while reading a matrix'
       write(out_unit,*) '  end of file or end of record'
       write(out_unit,*) '  The matrix paramters: nl,nc,nbcol_loc',nl,nc,nbcol_loc
       write(out_unit,*) '  Internal paramters: nbblocs,nfin',nbblocs,nfin
       write(out_unit,*) ' Check your data !!'
     END IF

  END SUBROUTINE QDUtil_Read_Rk16Mat
  SUBROUTINE QDUtil_Read_Ck16Mat(Mat,nio,nbcol_loc,err)
    USE QDUtil_NumParameters_m, ONLY : out_unit,Rk16
    IMPLICIT NONE

    integer,               intent(in)    :: nio,nbcol_loc
    complex(kind=Rk16), intent(inout) :: Mat(:,:)
    integer,               intent(inout) :: err

     integer i,j,jj,nb,nbblocs,nfin,nl,nc

     nl = size(Mat,dim=1)
     nc = size(Mat,dim=2)
     !write(out_unit,*) 'nl,nc,nbcol_loc',nl,nc,nbcol_loc


     nbblocs=int(nc/nbcol_loc)
     err = 0
     IF (nbblocs*nbcol_loc == nc) nbblocs=nbblocs-1

     DO nb=0,nbblocs-1

         DO j=1,nl
           read(nio,*,IOSTAT=err) jj,(Mat(j,i+nb*nbcol_loc),i=1,nbcol_loc)
           IF (err /= 0) EXIT
         END DO

         IF (err /= 0) EXIT

         IF (nl > 1) read(nio,*,IOSTAT=err)
         IF (err /= 0) EXIT

     END DO

     IF (err == 0) THEN
       DO j=1,nl
         nfin=nc-nbcol_loc*nbblocs
         read(nio,*,IOSTAT=err) jj,(Mat(j,i+nbcol_loc*nbblocs),i=1,nfin)
         IF (err /= 0) EXIT
       END DO
     END IF

     IF (err /= 0) THEN
       CALL QDUtil_Write_Ck16Mat(Mat,out_unit,nbcol_loc)
       write(out_unit,*) ' ERROR in QDUtil_Read_Ck16Mat'
       write(out_unit,*) '  while reading a matrix'
       write(out_unit,*) '  end of file or end of record'
       write(out_unit,*) '  The matrix paramters: nl,nc,nbcol_loc',nl,nc,nbcol_loc
       write(out_unit,*) ' Check your data !!'
     END IF

  END SUBROUTINE QDUtil_Read_Ck16Mat

  !================================================================
  ! ++    read a vector in line
  !================================================================
  SUBROUTINE QDUtil_Read_Rk16Vec(Vec,nio,nbcol_loc,err)
    USE QDUtil_NumParameters_m, ONLY : out_unit,Rk16
    IMPLICIT NONE

    integer, intent(in)                 :: nio,nbcol_loc
     real(kind=Rk16), intent(inout)  :: Vec(:)
     integer, intent(inout)             :: err

     integer :: n,i,nb,nbblocs,nfin

     n = size(Vec,dim=1)
     nbblocs=int(n/nbcol_loc)
     err = 0


     IF (nbblocs*nbcol_loc == n) nbblocs=nbblocs-1

     DO nb=0,nbblocs-1
       read(nio,*,IOSTAT=err) (Vec(i+nb*nbcol_loc),i=1,nbcol_loc)
       IF (err /= 0) EXIT
     END DO

     nfin=n-nbcol_loc*nbblocs
     read(nio,*,IOSTAT=err) (Vec(i+nbcol_loc*nbblocs),i=1,nfin)

     IF (err /= 0) THEN
       write(out_unit,*) ' ERROR in QDUtil_Read_Rk16Vec'
       write(out_unit,*) '  while reading a vector'
       write(out_unit,*) '  end of file or end of record'
       write(out_unit,*) '  The vector paramters: n,nbcol_loc',n,nbcol_loc
       write(out_unit,*) ' Check your data !!'
     END IF

  END SUBROUTINE QDUtil_Read_Rk16Vec
  SUBROUTINE QDUtil_Read_Ck16Vec(Vec,nio,nbcol_loc,err)
    USE QDUtil_NumParameters_m, ONLY : out_unit,Rk16
    IMPLICIT NONE

    integer, intent(in)                  :: nio,nbcol_loc
    complex(kind=Rk16), intent(inout)    :: Vec(:)
    integer, intent(inout)               :: err

     integer :: n,i,nb,nbblocs,nfin

     n = size(Vec,dim=1)
     nbblocs=int(n/nbcol_loc)
     err = 0

     IF (nbblocs*nbcol_loc == n) nbblocs=nbblocs-1

     DO nb=0,nbblocs-1
       read(nio,*,IOSTAT=err) (Vec(i+nb*nbcol_loc),i=1,nbcol_loc)
       IF (err /= 0) EXIT
     END DO

     nfin=n-nbcol_loc*nbblocs
     read(nio,*,IOSTAT=err) (Vec(i+nbcol_loc*nbblocs),i=1,nfin)

     IF (err /= 0) THEN
       write(out_unit,*) ' ERROR in QDUtil_Read_Ck16Vec'
       write(out_unit,*) '  while reading a vector'
       write(out_unit,*) '  end of file or end of record'
       write(out_unit,*) '  The vector paramters: n,nbcol_loc',n,nbcol_loc
       write(out_unit,*) ' Check your data !!'
     END IF

  END SUBROUTINE QDUtil_Read_Ck16Vec

  SUBROUTINE QDUtil_Write_Ik4Mat(Mat,nio,nbcol,Iformat,info,iprint)
    USE QDUtil_NumParameters_m, ONLY : out_unit,Ik4,Rk8
    USE QDUtil_String_m,        ONLY : TO_String
    IMPLICIT NONE
  
    integer,                     intent(in) :: nio,nbcol
    integer(kind=Ik4),           intent(in) :: Mat(:,:)

    character (len=*), optional, intent(in) :: Iformat
    character (len=*), optional, intent(in) :: info
    integer,           optional, intent(in) :: iprint


    integer         :: nl,nc
    integer         :: i,j,nb,nbblocs,nfin,nbcol_loc,ilen
    character (len=:), allocatable  :: wformat,Iformat_loc

    IF (present(iprint)) THEN
      IF (iprint /=0) RETURN ! it was MPI_id in the module mod_MPI
    END IF
    IF (present(Iformat)) THEN
      Iformat_loc = trim(adjustl(Iformat))
    ELSE
      Iformat_loc = 'i5'
    END IF

    nl = size(Mat,dim=1)
    nc = size(Mat,dim=2)
 
   !write(out_unit,*) 'nl,nc,nbcol_loc',nl,nc,nbcol_loc
    nbcol_loc = nbcol
    IF (nbcol_loc > 10) nbcol_loc=10
    nbblocs=int(nc/nbcol_loc)
    IF (nbblocs*nbcol_loc == nc) nbblocs=nbblocs-1

    IF (present(info)) THEN
      wformat = '(2x,"' // trim(adjustl(info)) // ' ",'
    ELSE
      wformat = '('
    END IF

    IF (nl > 0) THEN

      !ilen = int(log10(real(nb_line,kind=Rk8)))+1
      ! ensure compatible with very small system in test
      ilen = MAX(int(log10(real(nl,kind=Rk8)))+1,2)

      !write(*,*) 'nbcol_loc check:',nbcol_loc,ilen

      wformat = wformat // '1x,i' //                       &
          TO_String(ilen) // ',2x,' //                     &
          TO_String(nbcol_loc) // '(' //                   &
          Iformat_loc // ',1x))'

    ELSE

      wformat = wformat // TO_String(nbcol_loc) // '(' //  &
                    Iformat_loc // ',1x))'

    END IF

      DO nb=0,nbblocs-1
        DO j=1,nl
          write(nio,wformat) j,(Mat(j,i+nb*nbcol_loc),i=1,nbcol_loc)
        END DO
        IF (nl > 1 ) write(nio,*)
      END DO
      DO j=1,nl
        nfin=nc-nbcol_loc*nbblocs
        write(nio,wformat) j,(Mat(j,i+nbcol_loc*nbblocs),i=1,nfin)
      END DO

    deallocate(wformat)
    deallocate(Iformat_loc)

  END SUBROUTINE QDUtil_Write_Ik4Mat
  SUBROUTINE QDUtil_Read_Ik4Mat(Mat,nio,nbcol_loc,err)
    USE QDUtil_NumParameters_m, ONLY : out_unit,Ik4
    IMPLICIT NONE

    integer,           intent(in)    :: nio,nbcol_loc
    integer,           intent(inout) :: err
    integer(kind=Ik4), intent(inout) :: Mat(:,:)

     integer i,j,jj,nb,nbblocs,nfin,nl,nc

     nl = size(Mat,dim=1)
     nc = size(Mat,dim=2)
     !write(out_unit,*) 'nl,nc,nbcol_loc',nl,nc,nbcol_loc


     nbblocs=int(nc/nbcol_loc)

     IF (nbblocs*nbcol_loc == nc) nbblocs=nbblocs-1
     err = 0

     !write(out_unit,*) 'nl,nc,nbcol_loc,nbblocs',nl,nc,nbcol_loc,nbblocs


     DO nb=0,nbblocs-1

         DO j=1,nl
           read(nio,*,IOSTAT=err) jj,(Mat(j,i+nb*nbcol_loc),i=1,nbcol_loc)
           IF (err /= 0) EXIT
         END DO

         IF (err /= 0) EXIT

         IF (nl > 1) read(nio,*,IOSTAT=err)
         IF (err /= 0) EXIT

     END DO

     nfin=nc-nbcol_loc*nbblocs
     IF (err == 0) THEN
       DO j=1,nl
         read(nio,*,IOSTAT=err) jj,(Mat(j,i+nbcol_loc*nbblocs),i=1,nfin)
         !write(out_unit,*) err,jj,(Mat(j,i+nbcol_loc*nbblocs),i=1,nfin)
         IF (err /= 0) EXIT
       END DO
     END IF

     IF (err /= 0) THEN
       CALL QDUtil_Write_Ik4Mat(Mat,out_unit,nbcol_loc)
       write(out_unit,*) ' ERROR in QDUtil_Read_Ik4Mat'
       write(out_unit,*) '  while reading a matrix'
       write(out_unit,*) '  end of file or end of record'
       write(out_unit,*) '  The matrix paramters: nl,nc,nbcol_loc',nl,nc,nbcol_loc
       write(out_unit,*) '  Internal paramters: nbblocs,nfin',nbblocs,nfin
       write(out_unit,*) ' Check your data !!'
     END IF

  END SUBROUTINE QDUtil_Read_Ik4Mat
  SUBROUTINE QDUtil_Write_Ik4Vec(Vec,nio,nbcol,Iformat,info,iprint)
    USE QDUtil_NumParameters_m, ONLY : out_unit,Ik4,Rk8
    USE QDUtil_String_m,        ONLY : TO_String
    IMPLICIT NONE

    integer,                     intent(in) :: nio,nbcol
    integer(kind=Ik4),           intent(in) :: Vec(:)

    character (len=*), optional, intent(in) :: Iformat
    character (len=*), optional, intent(in) :: info
    integer,           optional, intent(in) :: iprint


    integer           :: n,i,nb,nbblocs,nfin,nbcol_loc,ilen
    character (len=:), allocatable  :: wformat,Iformat_loc

    IF (present(iprint)) THEN
      IF (iprint /=0) RETURN ! it was MPI_id in the module mod_MPI
    END IF

    n = size(Vec)
    !write(out_unit,*) 'n,nbcol_loc',n,nbcol_loc
    nbcol_loc = nbcol
    IF (nbcol_loc > 10) nbcol_loc=10
    nbblocs=int(n/nbcol_loc)
    IF (nbblocs*nbcol_loc == n) nbblocs=nbblocs-1

    IF (present(Iformat)) THEN
      Iformat_loc = trim(adjustl(Iformat))
    ELSE
      Iformat_loc = 'i5'
    END IF

    IF (present(info)) THEN
      wformat = '(2x,"' // trim(adjustl(info)) // ' ",'
    ELSE
      wformat = '('
    END IF

    wformat = wformat // TO_String(nbcol_loc) // '(' // Iformat_loc // ',1x))'

    DO nb=0,nbblocs-1
      write(nio,wformat) (Vec(i+nb*nbcol_loc),i=1,nbcol_loc)
    END DO
    nfin=n-nbcol_loc*nbblocs
    write(nio,wformat) (Vec(i+nbcol_loc*nbblocs),i=1,nfin)

    deallocate(Iformat_loc)
    deallocate(wformat)

  END SUBROUTINE QDUtil_Write_Ik4Vec
  SUBROUTINE QDUtil_Read_Ik4Vec(Vec,nio,nbcol_loc,err)
    USE QDUtil_NumParameters_m, ONLY : out_unit,Ik4
    IMPLICIT NONE

    integer(kind=Ik4), intent(inout)  :: Vec(:)
    integer,           intent(in)     :: nio,nbcol_loc
    integer,           intent(inout)  :: err


     integer :: n,i,nb,nbblocs,nfin

     n = size(Vec,dim=1)
     nbblocs=int(n/nbcol_loc)
     err = 0

     IF (nbblocs*nbcol_loc == n) nbblocs=nbblocs-1

     DO nb=0,nbblocs-1
       read(nio,*,IOSTAT=err) (Vec(i+nb*nbcol_loc),i=1,nbcol_loc)
       IF (err /= 0) EXIT
     END DO

     nfin=n-nbcol_loc*nbblocs
     read(nio,*,IOSTAT=err) (Vec(i+nbcol_loc*nbblocs),i=1,nfin)

     IF (err /= 0) THEN
       write(out_unit,*) ' ERROR in QDUtil_Read_Ik4Vec'
       write(out_unit,*) '  while reading a vector'
       write(out_unit,*) '  end of file or end of record'
       write(out_unit,*) '  The vector paramters: n,nbcol_loc',n,nbcol_loc
       write(out_unit,*) ' Check your data !!'
     END IF

  END SUBROUTINE QDUtil_Read_Ik4Vec


  SUBROUTINE QDUtil_Write_Ik8Mat(Mat,nio,nbcol,Iformat,info,iprint)
    USE QDUtil_NumParameters_m, ONLY : out_unit,Ik8,Rk8
    USE QDUtil_String_m,        ONLY : TO_String
    IMPLICIT NONE
  
    integer,                     intent(in) :: nio,nbcol
    integer(kind=Ik8),           intent(in) :: Mat(:,:)

    character (len=*), optional, intent(in) :: Iformat
    character (len=*), optional, intent(in) :: info
    integer,           optional, intent(in) :: iprint


    integer         :: nl,nc
    integer         :: i,j,nb,nbblocs,nfin,nbcol_loc,ilen
    character (len=:), allocatable  :: wformat,Iformat_loc

    IF (present(iprint)) THEN
      IF (iprint /=0) RETURN ! it was MPI_id in the module mod_MPI
    END IF
    IF (present(Iformat)) THEN
      Iformat_loc = trim(adjustl(Iformat))
    ELSE
      Iformat_loc = 'i5'
    END IF

    nl = size(Mat,dim=1)
    nc = size(Mat,dim=2)
 
   !write(out_unit,*) 'nl,nc,nbcol_loc',nl,nc,nbcol_loc
    nbcol_loc = nbcol
    IF (nbcol_loc > 10) nbcol_loc=10
    nbblocs=int(nc/nbcol_loc)
    IF (nbblocs*nbcol_loc == nc) nbblocs=nbblocs-1

    IF (present(info)) THEN
      wformat = '(2x,"' // trim(adjustl(info)) // ' ",'
    ELSE
      wformat = '('
    END IF

    IF (nl > 0) THEN

      !ilen = int(log10(real(nb_line,kind=Rk8)))+1
      ! ensure compatible with very small system in test
      ilen = MAX(int(log10(real(nl,kind=Rk8)))+1,2)

      !write(*,*) 'nbcol_loc check:',nbcol_loc,ilen

      wformat = wformat // '1x,i' //                       &
          TO_String(ilen) // ',2x,' //                     &
          TO_String(nbcol_loc) // '(' //                   &
          Iformat_loc // ',1x))'

    ELSE

      wformat = wformat // TO_String(nbcol_loc) // '(' //  &
                    Iformat_loc // ',1x))'

    END IF

      DO nb=0,nbblocs-1
        DO j=1,nl
          write(nio,wformat) j,(Mat(j,i+nb*nbcol_loc),i=1,nbcol_loc)
        END DO
        IF (nl > 1 ) write(nio,*)
      END DO
      DO j=1,nl
        nfin=nc-nbcol_loc*nbblocs
        write(nio,wformat) j,(Mat(j,i+nbcol_loc*nbblocs),i=1,nfin)
      END DO

    deallocate(wformat)
    deallocate(Iformat_loc)

  END SUBROUTINE QDUtil_Write_Ik8Mat
  SUBROUTINE QDUtil_Read_Ik8Mat(Mat,nio,nbcol_loc,err)
    USE QDUtil_NumParameters_m, ONLY : out_unit,Ik8
    IMPLICIT NONE

    integer,           intent(in)    :: nio,nbcol_loc
    integer,           intent(inout) :: err
    integer(kind=Ik8), intent(inout) :: Mat(:,:)

     integer i,j,jj,nb,nbblocs,nfin,nl,nc

     nl = size(Mat,dim=1)
     nc = size(Mat,dim=2)
     !write(out_unit,*) 'nl,nc,nbcol_loc',nl,nc,nbcol_loc


     nbblocs=int(nc/nbcol_loc)

     IF (nbblocs*nbcol_loc == nc) nbblocs=nbblocs-1
     err = 0

     !write(out_unit,*) 'nl,nc,nbcol_loc,nbblocs',nl,nc,nbcol_loc,nbblocs


     DO nb=0,nbblocs-1

         DO j=1,nl
           read(nio,*,IOSTAT=err) jj,(Mat(j,i+nb*nbcol_loc),i=1,nbcol_loc)
           IF (err /= 0) EXIT
         END DO

         IF (err /= 0) EXIT

         IF (nl > 1) read(nio,*,IOSTAT=err)
         IF (err /= 0) EXIT

     END DO

     nfin=nc-nbcol_loc*nbblocs
     IF (err == 0) THEN
       DO j=1,nl
         read(nio,*,IOSTAT=err) jj,(Mat(j,i+nbcol_loc*nbblocs),i=1,nfin)
         !write(out_unit,*) err,jj,(Mat(j,i+nbcol_loc*nbblocs),i=1,nfin)
         IF (err /= 0) EXIT
       END DO
     END IF

     IF (err /= 0) THEN
       CALL QDUtil_Write_Ik8Mat(Mat,out_unit,nbcol_loc)
       write(out_unit,*) ' ERROR in QDUtil_Read_Ik8Mat'
       write(out_unit,*) '  while reading a matrix'
       write(out_unit,*) '  end of file or end of record'
       write(out_unit,*) '  The matrix paramters: nl,nc,nbcol_loc',nl,nc,nbcol_loc
       write(out_unit,*) '  Internal paramters: nbblocs,nfin',nbblocs,nfin
       write(out_unit,*) ' Check your data !!'
     END IF

  END SUBROUTINE QDUtil_Read_Ik8Mat
  SUBROUTINE QDUtil_Write_Ik8Vec(Vec,nio,nbcol,Iformat,info,iprint)
    USE QDUtil_NumParameters_m, ONLY : out_unit,Ik8,Rk8
    USE QDUtil_String_m,        ONLY : TO_String
    IMPLICIT NONE

    integer,                     intent(in) :: nio,nbcol
    integer(kind=Ik8),           intent(in) :: Vec(:)

    character (len=*), optional, intent(in) :: Iformat
    character (len=*), optional, intent(in) :: info
    integer,           optional, intent(in) :: iprint


    integer           :: n,i,nb,nbblocs,nfin,nbcol_loc,ilen
    character (len=:), allocatable  :: wformat,Iformat_loc

    IF (present(iprint)) THEN
      IF (iprint /=0) RETURN ! it was MPI_id in the module mod_MPI
    END IF

    n = size(Vec)
    !write(out_unit,*) 'n,nbcol_loc',n,nbcol_loc
    nbcol_loc = nbcol
    IF (nbcol_loc > 10) nbcol_loc=10
    nbblocs=int(n/nbcol_loc)
    IF (nbblocs*nbcol_loc == n) nbblocs=nbblocs-1

    IF (present(Iformat)) THEN
      Iformat_loc = trim(adjustl(Iformat))
    ELSE
      Iformat_loc = 'i5'
    END IF

    IF (present(info)) THEN
      wformat = '(2x,"' // trim(adjustl(info)) // ' ",'
    ELSE
      wformat = '('
    END IF

    wformat = wformat // TO_String(nbcol_loc) // '(' // Iformat_loc // ',1x))'

    DO nb=0,nbblocs-1
      write(nio,wformat) (Vec(i+nb*nbcol_loc),i=1,nbcol_loc)
    END DO
    nfin=n-nbcol_loc*nbblocs
    write(nio,wformat) (Vec(i+nbcol_loc*nbblocs),i=1,nfin)

    deallocate(Iformat_loc)
    deallocate(wformat)

  END SUBROUTINE QDUtil_Write_Ik8Vec
  SUBROUTINE QDUtil_Read_Ik8Vec(Vec,nio,nbcol_loc,err)
    USE QDUtil_NumParameters_m, ONLY : out_unit,Ik8
    IMPLICIT NONE

    integer(kind=Ik8), intent(inout)  :: Vec(:)
    integer,           intent(in)     :: nio,nbcol_loc
    integer,           intent(inout)  :: err


     integer :: n,i,nb,nbblocs,nfin

     n = size(Vec,dim=1)
     nbblocs=int(n/nbcol_loc)
     err = 0

     IF (nbblocs*nbcol_loc == n) nbblocs=nbblocs-1

     DO nb=0,nbblocs-1
       read(nio,*,IOSTAT=err) (Vec(i+nb*nbcol_loc),i=1,nbcol_loc)
       IF (err /= 0) EXIT
     END DO

     nfin=n-nbcol_loc*nbblocs
     read(nio,*,IOSTAT=err) (Vec(i+nbcol_loc*nbblocs),i=1,nfin)

     IF (err /= 0) THEN
       write(out_unit,*) ' ERROR in QDUtil_Read_Ik8Vec'
       write(out_unit,*) '  while reading a vector'
       write(out_unit,*) '  end of file or end of record'
       write(out_unit,*) '  The vector paramters: n,nbcol_loc',n,nbcol_loc
       write(out_unit,*) ' Check your data !!'
     END IF

  END SUBROUTINE QDUtil_Read_Ik8Vec


  SUBROUTINE Test_QDUtil_RW_MatVec()
    USE QDUtil_Test_m
    USE QDUtil_NumParameters_m
    IMPLICIT NONE

    TYPE (test_t)                    :: test_var
    logical                          :: res_test
    real (kind=Rkind),   parameter   :: ZeroTresh    = ONETENTH**10

    integer                          :: io,ioerr
    real(kind=Rkind),    allocatable :: R1Mat(:,:),R1Vec(:)
    complex(kind=Rkind), allocatable :: C1Mat(:,:),C1Vec(:)
    real(kind=Rkind),    allocatable :: R2Mat(:,:),R2Vec(:)
    complex(kind=Rkind), allocatable :: C2Mat(:,:),C2Vec(:)
    character (len=:),   allocatable :: string
    integer,    allocatable :: I1Mat(:,:),I1Vec(:)
    integer,    allocatable :: I2Mat(:,:),I2Vec(:)

    ! define the matrices and the vectors
    I1Mat = reshape([0,1,2,3,4,5,                              &
                     0,1,2,3,4,5,                              &
                     0,1,2,3,4,5,                              &
                     0,1,2,3,4,5,                              &
                     0,1,2,3,4,5],shape=[6,5])

    I1Vec = [0,1,2,3,4,5]

                     ! define the matrices and the vectors
    R1Mat = reshape([ZERO,ONE,TWO,THREE,FOUR,FIVE,                              &
                     ZERO,ONE,TWO,THREE,FOUR,FIVE,                              &
                     ZERO,ONE,TWO,THREE,FOUR,FIVE,                              &
                     ZERO,ONE,TWO,THREE,FOUR,FIVE,                              &
                     ZERO,ONE,TWO,THREE,FOUR,FIVE],shape=[6,5])
    C1Mat = R1Mat + EYE*R1Mat
    allocate(C2Mat(6,5))
    allocate(R2Mat(6,5))
    allocate(I2Mat(6,5))

    R1Vec = [ZERO,ONE,TWO,THREE,FOUR,FIVE]
    C1Vec = R1Vec-EYE*R1Vec
    allocate(R2Vec(6))
    allocate(C2Vec(6))
    allocate(I2Vec(6))

    ! tests
    CALL Initialize_Test(test_var,test_name='RW_MatVec')

    ! Test1 for the real matrix
    open(newunit=io,file='test_io_file.txt')
    CALL Write_Mat(R1Mat,io,4) ; close(io)

    open(newunit=io,file='test_io_file.txt')
    CALL Read_Mat(R2Mat,io,4,ioerr) ; close(io)

    IF (ioerr /= 0) THEN
      write(out_unit,*) 'ERROR while reading R2Mat'
      res_test = .FALSE.
    ELSE
      res_test = all(abs(R1Mat-R2Mat) < ZeroTresh)
    END IF
    CALL Logical_Test(test_var,test1=res_test,info='Read-Write Rk8Mat')

   ! Test1bis for the real matrix (in a string)
    open(newunit=io,file='test_io_file.txt')
    CALL Write_Mat(R1Mat,string,4)
    write(io,'(a)') string  ; close(io)

    open(newunit=io,file='test_io_file.txt')
    CALL Read_Mat(R2Mat,io,4,ioerr) ; close(io)

    IF (ioerr /= 0) THEN
      write(out_unit,*) 'ERROR while reading R2Mat'
      res_test = .FALSE.
    ELSE
      res_test = all(abs(R1Mat-R2Mat) < ZeroTresh)
    END IF
    CALL Logical_Test(test_var,test1=res_test,info='Read-Write_string Rk8Mat')

    ! Test2 for the complex matrix
    open(newunit=io,file='test_io_file.txt')
    CALL Write_Mat(C1Mat,io,4) ; close(io)

    open(newunit=io,file='test_io_file.txt')
    CALL Read_Mat(C2Mat,io,4,ioerr) ; close(io)

    IF (ioerr /= 0) THEN
      write(out_unit,*) 'ERROR while reading C2Mat'
      res_test = .FALSE.
    ELSE
      res_test = all(abs(C1Mat-C2Mat) < ZeroTresh)
    END IF
    CALL Logical_Test(test_var,test1=res_test,info='Read-Write Ck8Mat')


    ! Test3 for the real vector
    open(newunit=io,file='test_io_file.txt')
    CALL Write_Vec(R1Vec,io,4) ; close(io)

    open(newunit=io,file='test_io_file.txt')
    CALL Read_Vec(R2Vec,io,4,ioerr) ; close(io)

    IF (ioerr /= 0) THEN
      write(out_unit,*) 'ERROR while reading R2Vec'
      res_test = .FALSE.
    ELSE
      res_test = all(abs(R1Vec-R2Vec) < ZeroTresh)
    END IF
    CALL Logical_Test(test_var,test1=res_test,info='Read-Write Rk8Vec')

    ! Test4 for the complex vector
    open(newunit=io,file='test_io_file.txt')
    CALL Write_Vec(C1Vec,io,4) ; close(io)

    open(newunit=io,file='test_io_file.txt')
    CALL Read_Vec(C2Vec,io,4,ioerr) ; close(io)

    IF (ioerr /= 0) THEN
      write(out_unit,*) 'ERROR while reading C2Vec'
      res_test = .FALSE.
    ELSE
      res_test = all(abs(C1Vec-C2Vec) < ZeroTresh)
    END IF
    CALL Logical_Test(test_var,test1=res_test,info='Read-Write Ck8Vec')


    ! Test4 for the integer matrix
    open(newunit=io,file='test_io_file.txt')
    CALL Write_Mat(I1Mat,io,4) ; close(io)

    open(newunit=io,file='test_io_file.txt')
    CALL Read_Mat(I2Mat,io,4,ioerr) ; close(io)

    IF (ioerr /= 0) THEN
      write(out_unit,*) 'ERROR while reading I2Mat'
      res_test = .FALSE.
    ELSE
      res_test = all(abs(I1Mat-I2Mat) < ZeroTresh)
    END IF
    CALL Logical_Test(test_var,test1=res_test,info='Read-Write Ik4Mat')
    ! finalize the tests

    open(newunit=io,file='test_io_file.txt')
    CALL Write_Vec(I1Vec,io,4) ; close(io)

    open(newunit=io,file='test_io_file.txt')
    CALL Read_Vec(I2Vec,io,4,ioerr) ; close(io)

    IF (ioerr /= 0) THEN
      write(out_unit,*) 'ERROR while reading I2Vec'
      res_test = .FALSE.
    ELSE
      res_test = all(abs(I1Vec-I2Vec) < ZeroTresh)
    END IF
    CALL Logical_Test(test_var,test1=res_test,info='Read-Write Ik4Vec')

    !! test with size = 1
    R1Mat = reshape([ONE],shape=[1,1])
    R2Mat = R1Mat ; R2Mat = ZERO
    open(newunit=io,file='test_io_file.txt')
    CALL Write_Mat(R1Mat,io,4) ; close(io)

    open(newunit=io,file='test_io_file.txt')
    CALL Read_Mat(R2Mat,io,4,ioerr) ; close(io)

    IF (ioerr /= 0) THEN
      write(out_unit,*) 'ERROR while reading R2Mat'
      res_test = .FALSE.
    ELSE
      res_test = all(abs(R1Mat-R2Mat) < ZeroTresh)
    END IF
    CALL Logical_Test(test_var,test1=res_test,info='Read-Write Rk8Mat(1,1)')

    R1Vec = [ONE]
    R2Vec = [ZERO]
    open(newunit=io,file='test_io_file.txt')
    CALL Write_Vec(R1Vec,io,4) ; close(io)

    open(newunit=io,file='test_io_file.txt')
    CALL Read_Vec(R2Vec,io,4,ioerr) ; close(io)

    IF (ioerr /= 0) THEN
      write(out_unit,*) 'ERROR while reading R2Vec'
      res_test = .FALSE.
    ELSE
      res_test = all(abs(R1Vec-R2Vec) < ZeroTresh)
    END IF
    CALL Logical_Test(test_var,test1=res_test,info='Read-Write Rk8Vec(1)')

    ! finalize the tests
    CALL Finalize_Test(test_var)
  END SUBROUTINE Test_QDUtil_RW_MatVec
END MODULE QDUtil_RW_MatVec_m

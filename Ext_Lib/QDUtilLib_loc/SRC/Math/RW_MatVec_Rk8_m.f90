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
MODULE QDUtil_RW_MatVec_Rk8_m
  USE QDUtil_RW_MatVec_base_m
  IMPLICIT NONE

  INTERFACE Write_VecMat
    MODULE PROCEDURE QDUtil_Write_Rk8Mat,QDUtil_Write_Ck8Mat,QDUtil_Write_Rk8Mat_string
    MODULE PROCEDURE QDUtil_Write_Rk8Vec,QDUtil_Write_Ck8Vec
  END INTERFACE
  INTERFACE Write_Mat
    MODULE PROCEDURE QDUtil_Write_Rk8Mat,QDUtil_Write_Ck8Mat,QDUtil_Write_Rk8Mat_string
  END INTERFACE
  INTERFACE Write_Vec
    MODULE PROCEDURE QDUtil_Write_Rk8Vec,QDUtil_Write_Ck8Vec
  END INTERFACE
  INTERFACE Read_Mat
    MODULE PROCEDURE QDUtil_Read_Rk8Mat,QDUtil_Read_Ck8Mat
  END INTERFACE
  INTERFACE Read_Vec
    MODULE PROCEDURE QDUtil_Read_Rk8Vec,QDUtil_Read_Ck8Vec
  END INTERFACE

  PRIVATE :: QDUtil_Write_Rk8Mat,QDUtil_Write_Ck8Mat,QDUtil_Write_Rk8Mat_string
  PRIVATE :: QDUtil_Read_Rk8Mat,QDUtil_Read_Ck8Mat
  PRIVATE :: QDUtil_Write_Rk8Vec,QDUtil_Write_Ck8Vec
  PRIVATE :: QDUtil_Read_Rk8Vec,QDUtil_Read_Ck8Vec

CONTAINS

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
    real(kind=Rk8),           intent(in) :: Vec(:)

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

    integer,            intent(in)    :: nio,nbcol_loc
     integer,           intent(inout) :: err
     real(kind=Rk8), intent(inout) :: Mat(:,:)

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

    integer,             intent(in)    :: nio,nbcol_loc
    complex(kind=Rk8), intent(inout) :: Mat(:,:)
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

END MODULE QDUtil_RW_MatVec_Rk8_m

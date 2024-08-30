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
MODULE QDUtil_File_m
  USE QDUtil_NumParameters_m
  IMPLICIT NONE

  PRIVATE

  !!@description: TODO
  !!@param: TODO
  TYPE File_t
    character (len=:), allocatable :: name
    integer                        :: unit      = 0       ! unit of the file
    logical                        :: formatted = .TRUE.
    logical                        :: append    =.FALSE.
    logical                        :: old       =.FALSE.
    logical                        :: seq       = .TRUE.
    integer                        :: frecl     = 0
    logical                        :: init      = .FALSE.

    ! to store/use data of several files using threads
    integer                               :: nb_thread      = 0
    character (len=:), allocatable        :: tab_name_th(:)
    integer,           allocatable        :: tab_unit(:)
  CONTAINS
    PROCEDURE, PRIVATE, PASS(file1) :: QDUtil_file2TOfile1
    GENERIC,   PUBLIC  :: assignment(=) => QDUtil_file2TOfile1
  END TYPE File_t

  PUBLIC :: File_t,file_GetUnit, file_set, file_open, file_open2
  PUBLIC :: file_close, file_delete, file_dealloc, file_write
  PUBLIC :: err_FileName,check_file_exist_WITH_FileName
  PUBLIC :: flush_perso
  PUBLIC :: Test_QDUtil_File

  INTERFACE file_GetUnit
    MODULE PROCEDURE QDUtil_file_GetUnit
  END INTERFACE
  INTERFACE file_set
    MODULE PROCEDURE QDUtil_file_set
  END INTERFACE
  INTERFACE file_open
    MODULE PROCEDURE QDUtil_file_open
  END INTERFACE
  INTERFACE file_open2
    MODULE PROCEDURE QDUtil_file_open2
  END INTERFACE
  INTERFACE file_close
    MODULE PROCEDURE QDUtil_file_close
  END INTERFACE
  INTERFACE file_delete
    MODULE PROCEDURE QDUtil_file_delete
  END INTERFACE
  INTERFACE file_dealloc
    MODULE PROCEDURE QDUtil_file_dealloc
  END INTERFACE
  INTERFACE file_write
    MODULE PROCEDURE QDUtil_file_write
  END INTERFACE

  INTERFACE err_FileName
    MODULE PROCEDURE QDUtil_err_FileName
  END INTERFACE
  INTERFACE check_file_exist_WITH_FileName
    MODULE PROCEDURE QDUtil_check_file_exist_WITH_FileName
  END INTERFACE

  INTERFACE flush_perso
    MODULE PROCEDURE QDUtil_flush_perso
  END INTERFACE

CONTAINS

  SUBROUTINE QDUtil_file2TOfile1(file1,file2)
    IMPLICIT NONE

    CLASS(File_t), intent(inout)  :: file1
    TYPE(File_t),  intent(in)     :: file2

    IF (allocated(file2%name)) file1%name      = file2%name
    file1%unit      = file2%unit
    file1%formatted = file2%formatted
    file1%append    = file2%append
    file1%old       = file2%old
    file1%seq       = file2%seq
    file1%frecl     = file2%frecl
    file1%init      = file2%init

    file1%nb_thread = file2%nb_thread

    IF (allocated(file2%tab_unit))    file1%tab_unit       = file2%tab_unit
    IF (allocated(file2%tab_name_th)) file1%tab_name_th    = file2%tab_name_th

  END SUBROUTINE QDUtil_file2TOfile1

  FUNCTION QDUtil_err_FileName(FileName,name_sub) RESULT(err_FileName)
    USE QDUtil_String_m, ONLY : string_IS_empty
    IMPLICIT NONE

    integer                                    :: err_FileName
    character (len=*),    intent(in)           :: FileName
    character (len=*),    intent(in), optional :: name_sub


    IF (string_IS_empty(FileName) ) THEN
      IF (present(name_sub)) THEN
        write(out_unit,*) ' ERROR in err_FileName, called from: ',name_sub
      ELSE
        write(out_unit,*) ' ERROR in err_FileName'
      END IF
      write(out_unit,*) '   FileName is empty'
      err_FileName = 1
    ELSE
      err_FileName = 0
    END IF

  END FUNCTION QDUtil_err_FileName
  FUNCTION QDUtil_check_file_exist_WITH_FileName(FileName,err_file,name_sub) RESULT(file_exist)
    IMPLICIT NONE

    logical                                    :: file_exist
    character (len=*), intent(in)              :: FileName
    character (len=*), intent(in)   , optional :: name_sub
    integer,           intent(inout), optional :: err_file

    integer :: err_file_loc

    IF (present(name_sub)) THEN
      err_file_loc = err_FileName(FileName,name_sub)
    ELSE
      err_file_loc = err_FileName(FileName)
    END IF

    IF (present(err_file)) err_file = err_file_loc

    IF (err_file_loc == 0) THEN
      INQUIRE(FILE=trim(FileName),EXIST=file_exist)
    ELSE ! err_file_loc = 1
      file_exist = .FALSE.
    END IF

  END FUNCTION QDUtil_check_file_exist_WITH_FileName

  FUNCTION QDUtil_file_GetUnit(ffile,err_file) RESULT(file_GetUnit)
      !$ USE omp_lib, only : OMP_GET_THREAD_NUM
    IMPLICIT NONE

    integer                               :: file_GetUnit
    TYPE(File_t), intent(in)              :: ffile
    integer,      intent(inout), optional :: err_file

    logical                  :: unit_opened
    integer                  :: ithread,err_file_loc

    !write(out_unit,*) 'BEGINNING file_GetUnit'
    IF (.NOT. ffile%init) THEN
      file_GetUnit = 0 ! the file is not opened
      RETURN
    END IF

    !- check if the file is already open ------------------
    IF (ffile%nb_thread > 1) THEN
      ithread         = 0
      !$ ithread      = OMP_GET_THREAD_NUM()

      err_file_loc = err_FileName(ffile%tab_name_th(ithread),'file_GetUnit')
      IF (.NOT. present(err_file) .AND. err_file_loc /= 0) THEN
        write(out_unit,*) 'ERROR in file_GetUnit'
        CALL file_Write(ffile)
        write(out_unit,*) 'The file name is empty, with ithread=',ithread
        STOP ' ERROR in file_GetUnit, the file name is empty!'
      END IF
      IF (present(err_file)) err_file = err_file_loc


      inquire(FILE=ffile%tab_name_th(ithread),OPENED=unit_opened)
      IF (.NOT. unit_opened) THEN ! the file is not open
        file_GetUnit = 0 ! the file is not opened
      ELSE
        file_GetUnit = ffile%tab_unit(ithread)
      END IF

    ELSE

      IF (.NOT. allocated(ffile%name)) THEN
        err_file_loc = 1
      ELSE
        err_file_loc = err_FileName(ffile%name,'file_GetUnit')
      END IF
      IF (.NOT. present(err_file) .AND. err_file_loc /= 0) THEN
        write(out_unit,*) 'ERROR in file_GetUnit'
        CALL file_Write(ffile)
        write(out_unit,*) 'The file name is empty'
        STOP ' ERROR in file_GetUnit, the file name is empty!'
      END IF
      IF (present(err_file)) err_file = err_file_loc

      inquire(FILE=ffile%name,OPENED=unit_opened)
      IF (.NOT. unit_opened) THEN ! the file is not open
        file_GetUnit = 0 ! the file is not opened
      ELSE
        file_GetUnit = ffile%unit
      END IF
    END IF

  END FUNCTION QDUtil_file_GetUnit

  FUNCTION QDUtil_GetUnit_NewFile(FileName,err_file) RESULT(GetUnit_NewFile)
    IMPLICIT NONE

    integer                                    :: GetUnit_NewFile
    character (len=*), intent(in)              :: FileName
    integer,           intent(inout), optional :: err_file


    logical                  :: unit_opened
    integer                  :: iunit,err_file_loc

    !write(out_unit,*) 'BEGINNING GetUnit_NewFile'

    err_file_loc = err_FileName(FileName,'GetUnit_NewFile')
    IF (.NOT. present(err_file) .AND. err_file_loc /= 0) THEN
      write(out_unit,*) 'ERROR in GetUnit_NewFile'
      write(out_unit,*) 'The file name is empty "',FileName,'"'
      STOP ' ERROR, the file name is empty!'
    END IF
    IF (present(err_file)) err_file = err_file_loc


    !- check if the file is already open ------------------
    inquire(FILE=FileName,NUMBER=iunit,OPENED=unit_opened)
    IF (.NOT. unit_opened) THEN ! the file is not open

      !- the file is not open, find an unused UNIT ---------
      open(newunit=GetUnit_NewFile,file=FileName)

    ELSE
      GetUnit_NewFile = 0
    END IF

  END FUNCTION QDUtil_GetUnit_NewFile

  SUBROUTINE QDUtil_file_set(ffile,FileName,lformatted,append,old,seq,lrecl,nb_thread,err_file)
    USE QDUtil_String_m, ONLY : TO_String
    IMPLICIT NONE

    TYPE(File_t),     intent(inout)           :: ffile
    character(len=*), intent(in),    optional :: FileName
    integer,          intent(in),    optional :: nb_thread
    integer,          intent(in),    optional :: lrecl
    logical,          intent(in),    optional :: lformatted,append,old,seq
    integer,          intent(inout), optional :: err_file


    character (len=:), allocatable :: fform,fstatus,fposition,faccess
    logical                        :: unit_opened
    integer                        :: ith,err_file_loc,clen

    integer :: err_mem,memory

    !write(out_unit,*) 'BEGINNING QDUtil_file_set'

    !- test if optional arguments are present ---------
    IF (ffile%init) THEN ! file is init, optional arguments are just here for modification
      ! only append can be changed
      IF (present(append)) ffile%append = append
    ELSE
      IF (present(nb_thread)) THEN
        ffile%nb_thread = nb_thread
      ELSE
        ffile%nb_thread = 0
      END IF
  
      IF (present(lformatted)) THEN
        ffile%formatted = lformatted
      ELSE
        ffile%formatted = .TRUE.
      END IF
  
      IF (present(old)) THEN
        ffile%old = old
      ELSE
        ffile%old = .FALSE.
      END IF
  
      IF (present(append)) THEN
        ffile%append = append
      ELSE
        ffile%append = .FALSE.
      END IF
  
      IF (present(seq)) THEN
        IF (seq) THEN
          ffile%seq = .TRUE.
        ELSE
          ffile%seq = .FALSE.
          IF (present(lrecl)) THEN
            ffile%frecl = lrecl
          ELSE
            write(out_unit,*) 'ERROR in file_open'
            write(out_unit,*) 'The file access is DIRECT but lrecl is not present!'
            STOP 'ERROR in file_set: The file access is DIRECT but lrecl is not present'
          END IF
        END IF
      ELSE
        ffile%seq = .TRUE.
      END IF
      IF (.NOT. ffile%seq) ffile%nb_thread = 0

      IF (.NOT. present(FileName)) THEN
        err_file_loc = 1
      ELSE
        err_file_loc = err_FileName(FileName,'file_set')
      END IF
      IF (.NOT. present(err_file) .AND. err_file_loc /= 0) THEN
        write(out_unit,*) 'ERROR in file_set'
        CALL file_Write(ffile)
        write(out_unit,*) 'The FileName is empty or not present'
        STOP ' ERROR in file_set, the FileName is empty!'
      END IF
      IF (present(err_file)) err_file = err_file_loc
      IF (err_file_loc == 0) ffile%name = FileName
  
      IF (ffile%nb_thread > 1 .AND. err_file_loc == 0) THEN
  
        IF (allocated(ffile%tab_unit)) deallocate(ffile%tab_unit,stat=err_mem)
        allocate(ffile%tab_unit(0:ffile%nb_thread-1),stat=err_mem)
  
        IF (allocated(ffile%tab_name_th)) deallocate(ffile%tab_name_th,stat=err_mem)
        clen = len(ffile%name // "." // TO_String(ffile%nb_thread-1))
        allocate(character(len=clen) :: ffile%tab_name_th(0:ffile%nb_thread-1),stat=err_mem)
  
        DO ith=0,ffile%nb_thread-1
  
          ffile%tab_name_th(ith) = ffile%name // "." // TO_String(ith)
  
        END DO
      END IF
      ffile%init  = (err_file_loc == 0)

    END IF
    !CALL file_Write(ffile)
    !write(out_unit,*) 'END GetUnit_file_set'
    !flush(out_unit)

  END SUBROUTINE QDUtil_file_set

  !!@description: TODO
  !!@param: TODO
  SUBROUTINE QDUtil_file_open(ffile,iunit,lformatted,append,old,seq,lrecl,err_file)
    USE QDUtil_String_m, ONLY : TO_String
    IMPLICIT NONE

    TYPE(File_t), intent(inout)           :: ffile
    integer,      intent(inout)           :: iunit
    integer,      intent(in),    optional :: lrecl
    logical,      intent(in),    optional :: lformatted,append,old,seq
    integer,      intent(inout), optional :: err_file


    character (len=:), allocatable :: fform,fstatus,fposition,faccess
    logical                        :: unit_opened
    integer                        :: ith,err_file_loc,clen

    integer :: err_mem,memory

    !write(out_unit,*) 'BEGINNING QDUtil_file_open'

    !- test if optional arguments are present ---------
    IF (.NOT. ffile%init) THEN  ! IF init=.T., those parameters are already set-up


      IF (present(lformatted)) THEN
        ffile%formatted = lformatted
      ELSE
        ffile%formatted = .TRUE.
      END IF

      IF (present(old)) THEN
        ffile%old = old
      ELSE
        ffile%old = .FALSE.
      END IF

      IF (present(append)) THEN
        ffile%append = append
      ELSE
        ffile%append = .FALSE.
      END IF


      IF (present(seq)) THEN
        IF (seq) THEN
          ffile%seq = .TRUE.
        ELSE
          ffile%seq = .FALSE.
          IF (present(lrecl)) THEN
            ffile%frecl = lrecl
          ELSE
            write(out_unit,*) 'ERROR in file_open'
            write(out_unit,*) 'The file access is DIRECT but lrecl is not present!'
            STOP 'ERROR in file_open: The file access is DIRECT but lrecl is not present'
          END IF
        END IF
      ELSE
        ffile%seq = .TRUE.
      END IF

      ffile%init  = .TRUE.
    END IF
    !-------------------------------------

    !-------------------------------------
    IF (ffile%formatted) THEN
      fform = 'formatted'
    ELSE
      fform = 'unformatted'
    END IF

    IF (ffile%old) THEN
      fstatus = 'old'
    ELSE
      fstatus = 'unknown'
    END IF

    IF (ffile%append) THEN
      fposition = 'append'
    ELSE
      fposition = 'asis'
    END IF

    IF (ffile%seq) THEN
      faccess = 'sequential'
    ELSE
      faccess = 'direct'
    END IF
    !-------------------------------------
    !CALL file_Write(ffile)

    IF (.NOT. ffile%seq) ffile%nb_thread = 0

    IF (.NOT. allocated(ffile%name)) THEN
      err_file_loc = 1
    ELSE
      err_file_loc = err_FileName(ffile%name,'file_open')
    END IF
    ffile%init  = (err_file_loc == 0)

    IF (.NOT. present(err_file) .AND. err_file_loc /= 0) THEN
      write(out_unit,*) 'ERROR in file_open'
      CALL file_Write(ffile)
      write(out_unit,*) 'The file name is empty'
      STOP ' ERROR in file_open, the file name is empty!'
    END IF
    IF (present(err_file)) err_file = err_file_loc
    IF (err_file_loc /= 0) RETURN

    !- check if the file is already open ------------------
    inquire(FILE=ffile%name,NUMBER=iunit,OPENED=unit_opened)
    !write(out_unit,*) 'unit_opened ?',unit_opened

    IF (.NOT. unit_opened) THEN ! the file is not open
        !-- open the file
      IF (ffile%seq) THEN
        IF (present(err_file)) THEN
          open(NEWUNIT=ffile%unit,FILE=ffile%name,FORM=fform,STATUS=fstatus,POSITION=fposition,ACCESS='SEQUENTIAL',IOSTAT=err_file)
          IF (err_file /= 0) RETURN
        ELSE
          open(NEWUNIT=ffile%unit,FILE=ffile%name,FORM=fform,STATUS=fstatus,POSITION=fposition,ACCESS='SEQUENTIAL')
        END IF
      ELSE
        IF (present(err_file)) THEN
          open(NEWUNIT=ffile%unit,FILE=ffile%name,FORM=fform,STATUS=fstatus,ACCESS='DIRECT',RECL=ffile%frecl,IOSTAT=err_file)
          IF (err_file /= 0) RETURN
        ELSE
          open(NEWUNIT=ffile%unit,FILE=ffile%name,FORM=fform,STATUS=fstatus,ACCESS='DIRECT',RECL=ffile%frecl)
        END IF
      END IF
    ELSE
      ffile%unit = iunit
    END IF
    !write(out_unit,*) 'open ',iunit,ffile%name

    IF (ffile%nb_thread > 1) THEN

      IF (allocated(ffile%tab_unit)) deallocate(ffile%tab_unit,stat=err_mem)
      allocate(ffile%tab_unit(0:ffile%nb_thread-1),stat=err_mem)

      IF (allocated(ffile%tab_name_th)) deallocate(ffile%tab_name_th,stat=err_mem)
      clen = len(ffile%name // "." // TO_String(ffile%nb_thread-1))
      allocate(character(len=clen) :: ffile%tab_name_th(0:ffile%nb_thread-1),stat=err_mem)

      DO ith=0,ffile%nb_thread-1

        ffile%tab_name_th(ith) = ffile%name // "." // TO_String(ith)

        inquire(FILE=ffile%tab_name_th(ith),NUMBER=iunit,OPENED=unit_opened)
        IF (.NOT. unit_opened) THEN ! the file is not open

          !-- open the file
          IF (ffile%seq) THEN
            open(NEWUNIT=iunit,FILE=ffile%tab_name_th(ith),FORM=fform,STATUS=fstatus,POSITION=fposition,ACCESS='SEQUENTIAL')
          ELSE
            open(NEWUNIT=iunit,FILE=ffile%tab_name_th(ith),FORM=fform,STATUS=fstatus,ACCESS='DIRECT',RECL=ffile%frecl)
          END IF

          ffile%tab_unit(ith) = iunit
        ELSE
          ffile%tab_unit(ith) = iunit
        END IF

      END DO
    END IF

    iunit = ffile%unit
    !write(out_unit,*) 'END QDUtil_file_open'

  END SUBROUTINE QDUtil_file_open

  SUBROUTINE QDUtil_file_close(ffile)
    IMPLICIT NONE

    TYPE(File_t), intent(inout)  :: ffile

    integer                   :: ith
    logical                   :: op

    inquire(unit=ffile%unit,OPENED=op)
    IF (op) close(ffile%unit)
    ffile%unit = 0

    IF (ffile%nb_thread > 1) THEN
      DO ith=0,ffile%nb_thread-1
        inquire(unit=ffile%tab_unit(ith),OPENED=op)
        IF (op)  close(ffile%tab_unit(ith))
        ffile%tab_unit(ith) = 0
      END DO
    END IF
    ffile%init = .FALSE.

  END SUBROUTINE QDUtil_file_close

  !!@description: TODO
  !!@param: TODO
  SUBROUTINE QDUtil_file_open2(name_file,iunit,lformatted,append,old,err_file)
    IMPLICIT NONE

    character (len=*), intent(in)              :: name_file
    integer,           intent(inout)           :: iunit
    logical,           intent(in),    optional :: lformatted,append,old
    integer,           intent(inout), optional :: err_file

    character (len=:), allocatable :: fform,fstatus,fposition
    logical                        :: unit_opened
    integer                        :: err_file_loc


    !- test if optional arguments are present ---------
    IF (present(lformatted)) THEN
      IF (.NOT. lformatted) THEN
        fform = 'unformatted'
      ELSE
        fform = 'formatted'
      END IF
    ELSE
      fform = 'formatted'
    END IF

    IF (present(append)) THEN
      IF (append) THEN
        fposition = 'append'
      ELSE
        fposition = 'asis'
      END IF
    ELSE
      fposition = 'asis'
    END IF

    IF (present(old)) THEN
      IF (old) THEN
        fstatus = 'old'
      ELSE
        fstatus = 'unknown'
      END IF
    ELSE
        fstatus = 'unknown'
    END IF

    err_file_loc = err_FileName(name_file,'file_open2')
    IF (.NOT. present(err_file) .AND. err_file_loc /= 0) THEN
      write(out_unit,*) 'ERROR in file_open2'
      write(out_unit,*) 'The FileName is empty'
      STOP ' ERROR in file_open2, the FileName is empty!'
    END IF
    IF (present(err_file)) err_file = err_file_loc

    !- check if the file is already open ------------------
    !write(out_unit,*) 'name_file,iunit ',name_file,iunit ; flush(out_unit)

    inquire(FILE=name_file,NUMBER=iunit,OPENED=unit_opened)
    !write(out_unit,*) 'name,unit,unit_opened ',name_file,unit,unit_opened


    !- the file is not open, find an unused UNIT ---------
    IF (unit_opened) RETURN ! the file is already open

    !-- open the file
    IF (present(err_file)) THEN
      open(NEWUNIT=iunit,FILE=name_file,FORM=fform,STATUS=fstatus,POSITION=fposition,ACCESS='SEQUENTIAL',IOSTAT=err_file)
    ELSE
      open(NEWUNIT=iunit,FILE=name_file,FORM=fform,STATUS=fstatus,POSITION=fposition,ACCESS='SEQUENTIAL')
    END IF

    !write(out_unit,*) 'open ',name_file,iunit

  END SUBROUTINE QDUtil_file_open2

  !!@description: TODO
  !!@param: TODO
  SUBROUTINE QDUtil_file_delete(ffile)
    IMPLICIT NONE

    TYPE(File_t), intent(inout) :: ffile

    integer           :: unit
    integer           :: ithread,nio

    !write(out_unit,*) 'BEGINNING file_delete'

    IF (len_trim(ffile%name) == 0) RETURN
    CALL file_open(ffile,unit)

    close(unit,status='delete')
    ffile%unit = 0
    !write(out_unit,*) 'delete file: ',unit,file%name

    IF (ffile%nb_thread > 1) THEN
      DO ithread=0,ffile%nb_thread-1
        nio = ffile%tab_unit(ithread)
        close(nio,status='delete')
        ffile%tab_unit(ithread) = 0
        !write(out_unit,*) 'delete file: ',nio,ffile%tab_name_th(ithread)
      END DO
    END IF
    ffile%init = .FALSE.

    !write(out_unit,*) 'END file_delete'

  END SUBROUTINE QDUtil_file_delete

  SUBROUTINE QDUtil_file_dealloc(ffile)
    IMPLICIT NONE

    TYPE(File_t), intent(inout)  :: ffile

    !write(out_unit,*) 'BEGINNING file_dealloc'

    ! first close the file
    CALL file_close(ffile)

    ffile%init = .FALSE.

    ffile%nb_thread = 0
    IF (allocated(ffile%tab_unit))    deallocate(ffile%tab_unit)
    IF (allocated(ffile%tab_name_th)) deallocate(ffile%tab_name_th)

    !write(out_unit,*) 'END file_dealloc'

  END SUBROUTINE QDUtil_file_dealloc

  SUBROUTINE QDUtil_file_Write(ffile)
    IMPLICIT NONE

    TYPE(File_t), intent(in)  :: ffile

    integer :: ith

    write(out_unit,*) 'BEGINNING file_Write'
    IF (allocated(ffile%name)) THEN
      write(out_unit,*) 'name:    ',ffile%name
    ELSE
      write(out_unit,*) 'name is not defined (not allocated)'
    END IF
    write(out_unit,*) 'unit     ',ffile%unit
    write(out_unit,*) 'formatted',ffile%formatted
    write(out_unit,*) 'append   ',ffile%append

    write(out_unit,*) 'old      ',ffile%old
    write(out_unit,*) 'seq      ',ffile%seq
    write(out_unit,*) 'frecl    ',ffile%frecl
    write(out_unit,*) 'init     ',ffile%init
    write(out_unit,*) 'nb_thread',ffile%nb_thread

    IF (allocated(ffile%tab_name_th) .AND. allocated(ffile%tab_unit)) THEN
      write(out_unit,*) 'tab_name_th,tab_unit'
      DO ith=0,ffile%nb_thread-1
        write(out_unit,*) 'name,unit: ',                             &
                           trim(adjustl(ffile%tab_name_th(ith))),' ', &
                           ffile%tab_unit(ith)
      END DO
    END IF

    write(out_unit,*) 'END file_Write'

  END SUBROUTINE QDUtil_file_Write

  !!@description: TODO
  !!@param: TODO
  SUBROUTINE QDUtil_flush_perso(nio)
    IMPLICIT NONE

    integer, intent(in) :: nio

    flush(nio)

  END  SUBROUTINE QDUtil_flush_perso
  SUBROUTINE Test_QDUtil_File
    USE QDUtil_Test_m
    USE QDUtil_NumParameters_m, only : Rkind, out_unit
    IMPLICIT NONE

    TYPE(File_t)              :: file1,file2
    integer                   :: i,iunit,err_file,frecl

    TYPE (test_t)                    :: test_var
    logical                          :: res_test
    character (len=:), allocatable   :: FileName

    !----- for debuging --------------------------------------------------
    character (len=*), parameter :: name_sub='Test_QDUtil_File'
    !logical, parameter :: debug = .FALSE.
    logical, parameter :: debug = .TRUE.
    !-----------------------------------------------------------

    CALL Initialize_Test(test_var,test_name='File')

    !--------------------------------------------------------------
    ! test 1: wrong file name
    CALL file_open(file1, iunit, err_file=err_file) ! here an error occurs because file1%name does not exist
    res_test = (err_file == 0)
    CALL Logical_Test(test_var,test1=res_test,info='file1%name is not allocated',test2=.FALSE.)
    !CALL file_Write(file1)
    !--------------------------------------------------------------

    !--------------------------------------------------------------
    ! test 2: sequential + formatted file
    CALL file_set(file1,FileName='file.txt', err_file=err_file)
    CALL file_open(file1, iunit, err_file=err_file)
    res_test = (err_file == 0)
    CALL Logical_Test(test_var,test1=res_test,info='Seq+Formatted file',test2=.TRUE.)
    write(iunit,*) 'test'
    CALL flush_perso(iunit)
    CALL file_delete(file1) ! the file is deleted
    !CALL file_Write(file1)
    !--------------------------------------------------------------

    !--------------------------------------------------------------
    ! test 3: sequential + unformatted file
    CALL file_set(file1,FileName='file.txt',lformatted=.FALSE.,err_file=err_file)
    CALL file_open(file1, iunit, err_file=err_file) ! here an error occurs because file1%name does not exist
    res_test = (err_file == 0)
    CALL Logical_Test(test_var,test1=res_test,info='Seq+UnFormatted file',test2=.TRUE.)
    write(iunit) 'test'
    CALL file_close(file1) ! the file is closed

    CALL file_set(file1,FileName='file.txt',lformatted=.FALSE.,append=.TRUE.,err_file=err_file)
    CALL file_open(file1, iunit, err_file=err_file)
    write(iunit) 'test2'

    CALL file_close(file1) ! the file is closed
    !--------------------------------------------------------------

    !--------------------------------------------------------------
    ! test 4: direct acces + unformatted file
    INQUIRE(iolength=frecl) i
    CALL file_set(file1,FileName='file.bin',lformatted=.FALSE.,seq=.FALSE.,lrecl=frecl,err_file=err_file)
    CALL file_open(file1, iunit, err_file=err_file)
    res_test = (err_file == 0)
    CALL Logical_Test(test_var,test1=res_test,info='direct+UnFormatted file',test2=.TRUE.)
    write(iunit,rec=1) 1
    write(iunit,rec=1) 2
    CALL file_close(file1) ! the file is closed
    CALL file_dealloc(file1) ! variable are deallocated
    !--------------------------------------------------------------


    ! finalize the tests
    CALL Finalize_Test(test_var)
  END SUBROUTINE Test_QDUtil_File
END MODULE QDUtil_File_m

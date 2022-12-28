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
MODULE ADLib_Util_m
  USE QDUtil_m, out_unitp => out_unit
  IMPLICIT NONE

  PRIVATE

  character (len=Line_len), public :: File_path = ''
  integer :: print_level = 1        ! 0 minimal, 1 default, 2 large, -1 nothing

  PUBLIC :: gamma_perso
  !PUBLIC :: time_perso
  !PUBLIC :: err_file_name,make_FileName,file_open2
  PUBLIC :: print_level


INTERFACE gamma_perso
  MODULE PROCEDURE AD_gamma_perso
END INTERFACE

INTERFACE time_perso
  MODULE PROCEDURE AD_time_perso
END INTERFACE
INTERFACE err_file_name
  MODULE PROCEDURE AD_err_file_name
END INTERFACE
INTERFACE make_FileName
  MODULE PROCEDURE AD_make_FileName
END INTERFACE
INTERFACE file_open2
  MODULE PROCEDURE AD_file_open2
END INTERFACE

CONTAINS

  ELEMENTAL FUNCTION AD_gamma_perso(n) result(gamma_perso)
  IMPLICIT NONE
  real(kind=Rkind)           :: gamma_perso
  integer,        intent(in) :: n

  real(kind=Rkind)  :: a
  integer           :: i

    a = ONE
    DO i=1,n-1
     a = a * i
    END DO
    gamma_perso = a

  END FUNCTION AD_gamma_perso


  SUBROUTINE AD_time_perso(name)
  IMPLICIT NONE

    character (len=*) :: name


    integer           :: tab_time(8) = 0
    real (kind=Rkind) :: t_real
    integer           :: count,count_work,freq
    real              :: t_cpu

    integer, save     :: count_old,count_ini
    real, save        :: t_cpu_old,t_cpu_ini
    integer           :: seconds,minutes,hours,days
    logical, save     :: begin = .TRUE.


!$OMP    CRITICAL (AD_time_perso_CRIT)

    CALL date_and_time(values=tab_time)
    write(out_unitp,21) name,tab_time(5:8),tab_time(3:1:-1)
 21 format('     Time and date in ',a,' : ',i2,'h:',                &
            i2,'m:',i2,'.',i3,'s, the ',i2,'/',i2,'/',i4)

     CALL system_clock(count=count,count_rate=freq)
     call cpu_time(t_cpu)

     IF (begin) THEN
       begin = .FALSE.
       count_old = count
       count_ini = count
       t_cpu_old = t_cpu
       t_cpu_ini = t_cpu
     END IF


     !============================================
     !cpu time in the subroutine: "name"

     count_work = count-count_old
     seconds = count_work/freq

     minutes = seconds/60
     seconds = mod(seconds,60)
     hours   = minutes/60
     minutes = mod(minutes,60)
     days    = hours/24
     hours   = mod(hours,24)


     t_real = real(count_work,kind=Rkind)/real(freq,kind=Rkind)
     write(out_unitp,31) t_real,name
 31  format('        real (s): ',f18.3,' in ',a)
     write(out_unitp,32) days,hours,minutes,seconds,name
 32  format('        real    : ',i3,'d ',i2,'h ',i2,'m ',i2,'s in ',a)

     write(out_unitp,33) t_cpu-t_cpu_old,name
 33  format('        cpu (s): ',f18.3,' in ',a)


     !============================================
     !Total cpu time

     count_work = count-count_ini
     seconds = count_work/freq

     minutes = seconds/60
     seconds = mod(seconds,60)
     hours   = minutes/60
     minutes = mod(minutes,60)
     days    = hours/24
     hours   = mod(hours,24)

     t_real = real(count_work,kind=Rkind)/real(freq,kind=Rkind)
     write(out_unitp,41) t_real
 41  format('  Total real (s): ',f18.3)
     write(out_unitp,42) days,hours,minutes,seconds
 42  format('  Total real    : ',i3,'d ',i2,'h ',i2,'m ',i2,'s')
     write(out_unitp,43) t_cpu-t_cpu_ini
 43  format('  Total cpu (s): ',f18.3)

 51  format(a,i10,a,a)


     flush(out_unitp)
     !============================================

     count_old = count
     t_cpu_old = t_cpu

!$OMP   END CRITICAL (AD_time_perso_CRIT)


  END SUBROUTINE AD_time_perso

  FUNCTION AD_err_file_name(file_name,name_sub)
  integer                                 :: AD_err_file_name
  character (len=*), intent(in)           :: file_name
  character (len=*), intent(in), optional :: name_sub

    IF (string_IS_empty(file_name) ) THEN
      IF (present(name_sub)) THEN
        write(out_unitp,*) ' ERROR in AD_err_file_name, called from: ',name_sub
      ELSE
        write(out_unitp,*) ' ERROR in AD_err_file_name'
      END IF
      write(out_unitp,*) '   The file name is empty'
      AD_err_file_name = 1
    ELSE
      AD_err_file_name = 0
    END IF

  END FUNCTION AD_err_file_name

  FUNCTION AD_make_FileName(FileName)
    character(len=*), intent(in)    :: FileName

    character (len=:), allocatable  :: AD_make_FileName
    integer :: ilast_char

    ilast_char = len_trim(File_path)

    IF (FileName(1:1) == "/" .OR. FileName(1:1) == "" .OR. ilast_char == 0) THEN
      AD_make_FileName = trim(adjustl(FileName))
    ELSE
      IF (File_path(ilast_char:ilast_char) == "/") THEN
        AD_make_FileName = trim(adjustl(File_path)) // trim(adjustl(FileName))
      ELSE
        AD_make_FileName = trim(adjustl(File_path)) // '/' // trim(adjustl(FileName))
      END IF
    END IF
    !stop
  END FUNCTION AD_make_FileName
  SUBROUTINE AD_file_open2(name_file,iunit,lformatted,append,old,err_file)

  character (len=*),   intent(in)              :: name_file
  integer,             intent(inout)           :: iunit
  logical,             intent(in),    optional :: lformatted,append,old
  integer,             intent(out),   optional :: err_file

  character (len=20)   :: fform,fstatus,fposition
  logical              :: unit_opened
  integer              :: err_file_loc

! - default for the open ---------------------------

! - test if optional arguments are present ---------
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

  err_file_loc = err_file_name(name_file,'AD_file_open2')
  IF (.NOT. present(err_file) .AND. err_file_loc /= 0) STOP ' ERROR, the file name is empty!'
  IF (present(err_file)) err_file = err_file_loc

! - check if the file is already open ------------------
! write(out_unitp,*) 'name,unit,unit_opened ',name_file,unit,unit_opened

  inquire(FILE=name_file,NUMBER=iunit,OPENED=unit_opened)
! write(out_unitp,*) 'name,unit,unit_opened ',name_file,unit,unit_opened


! - the file is not open, find an unused UNIT ---------
  IF (unit_opened) RETURN ! the file is already open

  iunit = 9
  DO
    iunit = iunit + 1
    inquire(UNIT=iunit,OPENED=unit_opened)
!   write(out_unitp,*) 'name,iunit,unit_opened ',name_file,iunit,unit_opened
    IF (.NOT. unit_opened) exit
  END DO


! -- open the file
  IF (present(err_file)) THEN
    open(UNIT=iunit,FILE=name_file,FORM=fform,STATUS=fstatus,       &
         POSITION=fposition,ACCESS='SEQUENTIAL',IOSTAT=err_file)
  ELSE
    open(UNIT=iunit,FILE=name_file,FORM=fform,STATUS=fstatus,       &
         POSITION=fposition,ACCESS='SEQUENTIAL')
  END IF

! write(out_unitp,*) 'open ',name_file,iunit

  END SUBROUTINE AD_file_open2
END MODULE ADLib_Util_m

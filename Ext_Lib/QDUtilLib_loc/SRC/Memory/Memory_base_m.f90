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
MODULE QDUtil_Memory_base_m
  USE QDUtil_NumParameters_m, only : Rkind, Ik4, Ik8, out_unit
  IMPLICIT NONE

  PRIVATE

  PUBLIC :: Memory_t
  PUBLIC :: Write_error_NOT_null, Write_error_null
  PUBLIC :: sub_test_tab_ub, sub_test_tab_lb
  PUBLIC :: error_memo_allo
  PUBLIC :: Check_mem, UnCheck_mem
  PUBLIC :: convertMem
  PUBLIC :: Write_mem_file,Write_mem_tot

  TYPE Memory_t
    integer (kind=Ik8)              :: max_mem      =  4000000000_Ik8/Rkind   ! (8GO) max_memory

    integer (kind=Ik8)              :: max_mem_used =  0   ! the maximal memory used
    integer (kind=Ik8)              :: mem_tot      =  0   ! memory used
    integer (kind=Ik8)              :: memory       =  0   ! asked memory
  
    integer                         :: nb_alloc     =  0   ! nb of allocations
    integer                         :: nb_dealloc   =  0   ! nb of deallocations
  
    logical                         :: mem_debug    = .FALSE.
    logical                         :: mem_print    = .FALSE.
  
    integer                         :: mem_unit     = -1
    character (len=:), allocatable  :: mem_file

  END TYPE Memory_t

  INTERFACE Write_error_NOT_null
    MODULE PROCEDURE QDUtil_Write_error_NOT_null
  END INTERFACE
  INTERFACE Write_error_null
    MODULE PROCEDURE QDUtil_Write_error_null
  END INTERFACE

  INTERFACE sub_test_tab_ub
    MODULE PROCEDURE QDUtil_sub_test_tab_ub_Ik4,QDUtil_sub_test_tab_ub_Ik8
  END INTERFACE
  INTERFACE sub_test_tab_lb
    MODULE PROCEDURE QDUtil_sub_test_tab_lb_Ik4,QDUtil_sub_test_tab_lb_Ik8
  END INTERFACE

  INTERFACE error_memo_allo
    MODULE PROCEDURE QDUtil_error_memo_allo_Ik8,QDUtil_error_memo_allo_Ik4
  END INTERFACE

  INTERFACE Check_mem
    MODULE PROCEDURE QDUtil_Check_mem
  END INTERFACE
  INTERFACE UnCheck_mem
    MODULE PROCEDURE QDUtil_UnCheck_mem
  END INTERFACE

  INTERFACE convertMem
    MODULE PROCEDURE QDUtil_convertMem
  END INTERFACE

  INTERFACE Write_mem_file
    MODULE PROCEDURE QDUtil_Write_mem_file
  END INTERFACE
  INTERFACE Write_mem_tot
    MODULE PROCEDURE QDUtil_Write_mem_tot
  END INTERFACE

  TYPE (Memory_t), save, public :: para_mem

  CONTAINS

  SUBROUTINE QDUtil_Write_error_NOT_null(name_sub_alloc,name_var,name_sub)
      IMPLICIT NONE


      character (len=*), intent(in) :: name_var,name_sub,name_sub_alloc

      write(out_unit,*) ' ERROR in ',name_sub_alloc
      write(out_unit,*) '   CALLED from "',name_sub,'" for the array "',name_var,'"'
      write(out_unit,*) '   The array IS ALREADY allocated'
      write(out_unit,*) '               OR'
      write(out_unit,*) '   The pointer (array) IS ALREADY associated'
      write(out_unit,*) '   => it cannot be allocated!'
      write(out_unit,*) ' CHECK the fortran! '
      STOP

  END SUBROUTINE QDUtil_Write_error_NOT_null
  SUBROUTINE QDUtil_Write_error_null(name_sub_alloc,name_var,name_sub)
      IMPLICIT NONE


      character (len=*), intent(in) :: name_var,name_sub,name_sub_alloc

      write(out_unit,*) ' ERROR in ',name_sub_alloc
      write(out_unit,*) '   CALLED from "',name_sub,'" for the array "',name_var,'"'
      write(out_unit,*) '   The array IS NOT allocated'
      write(out_unit,*) '               OR'
      write(out_unit,*) '   The pointer (array) IS NOT associated'
      write(out_unit,*) '   => it cannot be used!'
      write(out_unit,*) '   or it cannot be deallocated!'
      write(out_unit,*) ' CHECK the fortran! '
      STOP

      END SUBROUTINE QDUtil_Write_error_null
      SUBROUTINE QDUtil_sub_test_tab_ub_Ik4(tab_ub,ndim,name_sub_alloc,name_var,name_sub)
      IMPLICIT NONE

      integer,            intent(in) :: ndim
      integer (kind=Ik4), intent(in) :: tab_ub(:)
      character (len=*),  intent(in) :: name_var,name_sub,name_sub_alloc

       IF (sum(shape(tab_ub)) /= ndim) THEN
         write(out_unit,*) ' ERROR in ',name_sub_alloc
         write(out_unit,*) '   CALLED from "',name_sub,'" for the array "',name_var,'"'
         write(out_unit,*) '   The number of element(s) of tab_ub(:) MUST be ',ndim
         write(out_unit,*) '   The actual number IS ',shape(tab_ub)
         write(out_unit,*) '      tab_ub(:) ',tab_ub(:)

         write(out_unit,*) ' CHECK the fortran! '

         STOP
       END IF

      END SUBROUTINE QDUtil_sub_test_tab_ub_Ik4
      SUBROUTINE QDUtil_sub_test_tab_lb_Ik4(tab_lb,ndim,name_sub_alloc,name_var,name_sub)
      IMPLICIT NONE

      integer,            intent(in) :: ndim
      integer (kind=Ik4), intent(in) :: tab_lb(:)
      character (len=*),  intent(in) :: name_var,name_sub,name_sub_alloc

       IF (sum(shape(tab_lb)) /= ndim) THEN
         write(out_unit,*) ' ERROR in ',name_sub_alloc
          write(out_unit,*) '   CALLED from "',name_sub,'" for the array "',name_var,'"'
         write(out_unit,*) '   The number of element(s) of tab_lb(:) MUST be ',ndim
         write(out_unit,*) '   The actual number IS ',shape(tab_lb)
         write(out_unit,*) '      tab_lb(:) ',tab_lb(:)

         write(out_unit,*) ' CHECK the fortran! '

         STOP
       END IF

      END SUBROUTINE QDUtil_sub_test_tab_lb_Ik4
      SUBROUTINE QDUtil_sub_test_tab_ub_Ik8(tab_ub,ndim,name_sub_alloc,name_var,name_sub)
      IMPLICIT NONE

      integer,               intent(in) :: ndim
      integer (kind=Ik8),    intent(in) :: tab_ub(:)
      character (len=*),     intent(in) :: name_var,name_sub,name_sub_alloc

       IF (sum(shape(tab_ub)) /= ndim) THEN
         write(out_unit,*) ' ERROR in ',name_sub_alloc
         write(out_unit,*) '   CALLED from "',name_sub,'" for the array "',name_var,'"'
         write(out_unit,*) '   The number of element(s) of tab_ub(:) MUST be ',ndim
         write(out_unit,*) '   The actual number IS ',shape(tab_ub)
         write(out_unit,*) '      tab_ub(:) ',tab_ub(:)

         write(out_unit,*) ' CHECK the fortran! '

         STOP
       END IF

      END SUBROUTINE QDUtil_sub_test_tab_ub_Ik8
      SUBROUTINE QDUtil_sub_test_tab_lb_Ik8(tab_lb,ndim,name_sub_alloc,name_var,name_sub)
      IMPLICIT NONE

      integer,               intent(in) :: ndim
      integer (kind=Ik8),    intent(in) :: tab_lb(:)
      character (len=*),     intent(in) :: name_var,name_sub,name_sub_alloc

       IF (sum(shape(tab_lb)) /= ndim) THEN
         write(out_unit,*) ' ERROR in ',name_sub_alloc
          write(out_unit,*) '   CALLED from "',name_sub,'" for the array "',name_var,'"'
         write(out_unit,*) '   The number of element(s) of tab_lb(:) MUST be ',ndim
         write(out_unit,*) '   The actual number IS ',shape(tab_lb)
         write(out_unit,*) '      tab_lb(:) ',tab_lb(:)

         write(out_unit,*) ' CHECK the fortran! '

         STOP
       END IF

      END SUBROUTINE QDUtil_sub_test_tab_lb_Ik8

      SUBROUTINE QDUtil_error_memo_allo_Ik4(err,memory,name_var,name_sub,var_type)
      IMPLICIT NONE

      integer,            intent(in) :: err
      integer (kind=Ik4), intent(in) :: memory
      character (len=*),  intent(in) :: name_var,name_sub
      character (len=*),  intent(in), optional :: var_type


      logical :: memory_test

!----- for debuging --------------------------------------------------
      logical,parameter :: debug=.FALSE.
      ! logical,parameter :: debug=.TRUE.

      IF (err /= 0) THEN
        write(out_unit,*) ' ERROR in  the routine "',name_sub,'"'
        write(out_unit,*) ' cannot allocate or deallocate the ',       &
                                             'variable: "',name_var,'"'
        IF (present(var_type) ) write(out_unit,*) 'Variable TYPE: ',var_type

        write(out_unit,*) 'err_mem',err
        write(out_unit,*) 'mem_tot,memory',para_mem%mem_tot,memory
        STOP
      END IF

      IF (.NOT. debug .AND. .NOT. para_mem%mem_debug) RETURN

!$OMP CRITICAL (QDUtil_error_memo_allo_Ik4_CRIT)
      CALL QDUtil_open_mem_file()

      IF (present(var_type)) THEN
        write(para_mem%mem_unit,*) para_mem%mem_tot,para_mem%mem_tot+int(memory,kind=Ik8),&
                memory,' var_type=',var_type,' name_var=',name_var,' ',name_sub
      ELSE
        write(para_mem%mem_unit,*) para_mem%mem_tot,para_mem%mem_tot+int(memory,kind=Ik8),&
                         memory,' no_var_type name_var=',name_var,' ',name_sub
      END IF
      flush(para_mem%mem_unit)

      IF (memory > 0) THEN
        memory_test = ( para_mem%mem_tot > huge(1_Ik8)-int(memory,kind=Ik8) )
        IF (memory_test) THEN
          write(out_unit,*) ' ERROR in QDUtil_error_memo_allo_Ik4'
          write(out_unit,*) ' Variable and subroutine: ',                      &
                                name_var,' in ',name_sub
          IF (present(var_type) ) write(out_unit,*) 'Variable TYPE: ',var_type

          write(out_unit,*) ' mem_tot WILL be larger than the largest integer!'
          write(out_unit,*) ' mem_tot,memory,huge(int)',para_mem%mem_tot,      &
                                                         memory,huge(1_Ik8)
          write(out_unit,*) ' huge(int)-memory',huge(1_Ik8)-int(memory,kind=Ik8)
          write(out_unit,*) ' => calculation TOO large or memory leak'
          STOP
        END IF
      END IF

      para_mem%mem_tot = para_mem%mem_tot + int(memory,kind=Ik8)
      IF (para_mem%mem_tot > para_mem%max_mem_used)                     &
                               para_mem%max_mem_used = para_mem%mem_tot
      IF (memory > 0) THEN
        para_mem%nb_alloc = para_mem%nb_alloc + 1
      ELSE IF (memory < 0) THEN
        para_mem%nb_dealloc = para_mem%nb_dealloc + 1
      END IF

      IF (para_mem%mem_print) THEN
        IF (memory == 0 .AND. err == 0) THEN
          write(out_unit,*) ' WARNING memory = 0'
          write(out_unit,*) 'old_mem_tot,new_mem_tot,memory',          &
                              para_mem%mem_tot-int(memory,kind=Ik8),para_mem%mem_tot, &
                              memory,'nothing: ',name_var,' in ',name_sub
          IF (present(var_type) ) write(out_unit,*) 'Variable TYPE: ',var_type
        END IF

        IF ((debug .OR. para_mem%mem_debug) .AND. memory < 0 .AND. err == 0) THEN
          write(out_unit,*) 'old_mem_tot,new_mem_tot,memory',          &
                              para_mem%mem_tot-int(memory,kind=Ik8),para_mem%mem_tot, &
                              memory,' dealloc of var name "',name_var, &
                              '" in routine "',name_sub,'"'
          IF (present(var_type) ) write(out_unit,*) 'Variable TYPE: ',var_type
        END IF
        IF ((debug .OR. para_mem%mem_debug) .AND. memory > 0 .AND. err == 0) THEN
          write(out_unit,*) 'old_mem_tot,new_mem_tot,memory',          &
                              para_mem%mem_tot-int(memory,kind=Ik8),para_mem%mem_tot, &
                              memory,' alloc of var name "',name_var,   &
                              '" in routine "',name_sub,'"'
          IF (present(var_type) ) write(out_unit,*) 'Variable TYPE: ',var_type
        END IF

        IF ((debug .OR. para_mem%mem_debug) .AND. err == 0) THEN
          write(out_unit,*) 'max_mem_used,mem_tot,nb_alloc,nb_dealloc',&
                                para_mem%max_mem_used,para_mem%mem_tot, &
                                 para_mem%nb_alloc,para_mem%nb_dealloc, &
                 ' of var name "',name_var,'" in routine "',name_sub,'"'
        END IF

      END IF

!$OMP END CRITICAL (QDUtil_error_memo_allo_Ik4_CRIT)
      END SUBROUTINE QDUtil_error_memo_allo_Ik4

      SUBROUTINE QDUtil_error_memo_allo_Ik8(err,memory,name_var,name_sub,var_type)
      IMPLICIT NONE

      integer,               intent(in)            :: err
      integer (kind=Ik8),    intent(in)            :: memory
      character (len=*),     intent(in)            :: name_var,name_sub
      character (len=*),     intent(in), optional :: var_type


      logical :: memory_test

!----- for debuging --------------------------------------------------
      logical,parameter :: debug=.FALSE.
!      logical,parameter :: debug=.TRUE.

      IF (err /= 0) THEN
        write(out_unit,*) ' ERROR in  the routine "',name_sub,'"'
        write(out_unit,*) ' cannot allocate or deallocate the ',       &
                                             'variable: "',name_var,'"'
        IF (present(var_type) ) write(out_unit,*) 'Variable TYPE: ',var_type

        write(out_unit,*) 'err_mem',err
        write(out_unit,*) 'mem_tot,memory',para_mem%mem_tot,memory
        STOP
      END IF

      IF (.NOT. debug .AND. .NOT. para_mem%mem_debug) RETURN

!$OMP CRITICAL (QDUtil_error_memo_allo_Ik8_CRIT)

      CALL QDUtil_open_mem_file()

      IF (present(var_type)) THEN
        write(para_mem%mem_unit,*) para_mem%mem_tot,para_mem%mem_tot+memory,memory,&
                        ' var_type=',var_type,' name_var=',name_var,' ',name_sub
      ELSE
        write(para_mem%mem_unit,*) para_mem%mem_tot,para_mem%mem_tot+memory,memory,&
                        ' no_var_type name_var=',name_var,' ',name_sub
      END IF
      flush(para_mem%mem_unit)

      IF (memory > 0) THEN
        memory_test = ( para_mem%mem_tot > huge(1_Ik8)-memory )
        IF (memory_test) THEN
          write(out_unit,*) ' ERROR in QDUtil_error_memo_allo_Ik8'
          write(out_unit,*) ' Variable and subroutine: ',                      &
                                name_var,' in ',name_sub
          IF (present(var_type) ) write(out_unit,*) 'Variable TYPE: ',var_type

          write(out_unit,*) ' mem_tot WILL be larger than the largest integer!'
          write(out_unit,*) ' mem_tot,memory,huge(int)',para_mem%mem_tot,      &
                                                         memory,huge(1_Ik8)
          write(out_unit,*) ' huge(int)-memory',huge(1_Ik8)-memory
          write(out_unit,*) ' => calculation TOO large or memory leak'
          STOP
        END IF
      END IF

      para_mem%mem_tot = para_mem%mem_tot + memory
      IF (para_mem%mem_tot > para_mem%max_mem_used)                     &
                               para_mem%max_mem_used = para_mem%mem_tot
      IF (memory > 0) THEN
        para_mem%nb_alloc = para_mem%nb_alloc + 1
      ELSE IF (memory < 0) THEN
        para_mem%nb_dealloc = para_mem%nb_dealloc + 1
      END IF

      IF (para_mem%mem_print) THEN
        IF (memory == 0 .AND. err == 0) THEN
          write(out_unit,*) ' WARNING memory = 0'
          write(out_unit,*) 'old_mem_tot,new_mem_tot,memory',          &
                              para_mem%mem_tot-memory,para_mem%mem_tot, &
                              memory,'nothing: ',name_var,' in ',name_sub
          IF (present(var_type) ) write(out_unit,*) 'Variable TYPE: ',var_type
        END IF

        IF ((debug .OR. para_mem%mem_debug) .AND. memory < 0 .AND. err == 0) THEN
          write(out_unit,*) 'old_mem_tot,new_mem_tot,memory',          &
                              para_mem%mem_tot-memory,para_mem%mem_tot, &
                              memory,' dealloc of var name "',name_var, &
                              '" in routine "',name_sub,'"'
          IF (present(var_type) ) write(out_unit,*) 'Variable TYPE: ',var_type
        END IF
        IF ((debug .OR. para_mem%mem_debug) .AND. memory > 0 .AND. err == 0) THEN
          write(out_unit,*) 'old_mem_tot,new_mem_tot,memory',          &
                              para_mem%mem_tot-memory,para_mem%mem_tot, &
                              memory,' alloc of var name "',name_var,   &
                              '" in routine "',name_sub,'"'
          IF (present(var_type) ) write(out_unit,*) 'Variable TYPE: ',var_type
        END IF

        IF ((debug .OR. para_mem%mem_debug) .AND. err == 0) THEN
          write(out_unit,*) 'max_mem_used,mem_tot,nb_alloc,nb_dealloc',&
                                para_mem%max_mem_used,para_mem%mem_tot, &
                                 para_mem%nb_alloc,para_mem%nb_dealloc, &
                 ' of var name "',name_var,'" in routine "',name_sub,'"'
        END IF

      END IF

!$OMP END CRITICAL (QDUtil_error_memo_allo_Ik8_CRIT)
  END SUBROUTINE QDUtil_error_memo_allo_Ik8


  SUBROUTINE QDUtil_Check_mem()
    IMPLICIT NONE

    para_mem%mem_print = .FALSE.
    para_mem%mem_debug = .TRUE.

    write(out_unit,*) '=============================================='
    write(out_unit,*) '========= CHECK MEMORY ======================='
    write(out_unit,*) '=============================================='
    write(out_unit,*) 'mem_tot,max_mem_used',para_mem%mem_tot,para_mem%max_mem_used
    write(out_unit,*) 'nb_alloc,nb_dealloc',para_mem%nb_alloc,para_mem%nb_dealloc
    write(out_unit,*) '=============================================='

  END SUBROUTINE QDUtil_Check_mem
  SUBROUTINE QDUtil_UnCheck_mem()
    IMPLICIT NONE

    para_mem%mem_print = .TRUE.
    para_mem%mem_debug = .FALSE.

    write(out_unit,*) '=============================================='
    write(out_unit,*) '========= UNCHECK MEMORY ====================='
    write(out_unit,*) '=============================================='
    write(out_unit,*) 'mem_tot,max_mem_used',para_mem%mem_tot,para_mem%max_mem_used
    write(out_unit,*) 'nb_alloc,nb_dealloc',para_mem%nb_alloc,para_mem%nb_dealloc
    write(out_unit,*) '=============================================='

  END SUBROUTINE QDUtil_UnCheck_mem

  SUBROUTINE QDUtil_Write_mem_tot(info)
    IMPLICIT NONE
    character (len=*), optional, intent(in) :: info
   
   
   !$OMP CRITICAL (Write_mem_tot_CRIT)
      IF (present(info)) THEN
        write(out_unit,*) 'max_mem_used,mem_tot,nb_alloc,nb_dealloc',     &
                                   para_mem%max_mem_used,para_mem%mem_tot, &
                                    para_mem%nb_alloc,para_mem%nb_dealloc, &
                                    ' info: ',info
      ELSE
        write(out_unit,*) 'max_mem_used,mem_tot,nb_alloc,nb_dealloc',     &
                                   para_mem%max_mem_used,para_mem%mem_tot, &
                                    para_mem%nb_alloc,para_mem%nb_dealloc
      END IF
   !$OMP END CRITICAL (Write_mem_tot_CRIT)
  END SUBROUTINE QDUtil_Write_mem_tot
   
  SUBROUTINE QDUtil_Write_mem_file(info)
    IMPLICIT NONE
   
    character (len=*), intent(in) :: info
   
    IF (.NOT. para_mem%mem_debug) RETURN
   
    !$OMP CRITICAL (write_mem_file_CRIT)
    CALL QDUtil_open_mem_file()
   
    write(para_mem%mem_unit,*) "--------------------------------------------------"
    write(para_mem%mem_unit,*) "--------------------------------------------------"
    write(para_mem%mem_unit,*)
    write(para_mem%mem_unit,*) "Memory analysis: ",info
    write(para_mem%mem_unit,*)
    write(para_mem%mem_unit,*) 'max_mem_used,mem_tot,nb_alloc,nb_dealloc',         &
                               para_mem%max_mem_used,para_mem%mem_tot,             &
                                para_mem%nb_alloc,para_mem%nb_dealloc
    write(para_mem%mem_unit,*) "--------------------------------------------------"
    write(para_mem%mem_unit,*) "--------------------------------------------------"
   
    flush(para_mem%mem_unit)
   
    !$OMP END CRITICAL (write_mem_file_CRIT)
   
   END SUBROUTINE QDUtil_Write_mem_file
   
  SUBROUTINE QDUtil_open_mem_file()
    IMPLICIT NONE
   
    IF (para_mem%mem_unit == -1) THEN
      open(newunit=para_mem%mem_unit,file='EVRT_mem.log')
      write(out_unit,*) 'Memory file: "EVRT_mem.log", unit:',para_mem%mem_unit
      IF (para_mem%mem_unit == -1) STOP 'ERROR cannot open the file: "EVRT_mem.log"'
    END IF
   
  END SUBROUTINE QDUtil_open_mem_file
   
  SUBROUTINE QDUtil_convertMem(mem,MemUnit)
    IMPLICIT NONE

    real(kind=Rkind),   intent(inout) :: mem
    character (len=2),  intent(inout) :: MemUnit
   
      IF (mem < 1024_Rkind) THEN
        MemUnit='O'
      ELSE IF (mem < 1024_Rkind**2) THEN
        mem = mem / 1024_Rkind
        MemUnit='kO'
      ELSE IF (mem < 1024_Rkind**3) THEN
        mem = mem / 1024_Rkind**2
        MemUnit='MO'
      ELSE
        mem = mem / 1024_Rkind**3
        MemUnit='GO'
      END IF
  END SUBROUTINE QDUtil_convertMem

END MODULE QDUtil_Memory_base_m

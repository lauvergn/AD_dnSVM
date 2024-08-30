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
MODULE QDUtil_Time_m
  IMPLICIT NONE

  PRIVATE
  TYPE Time_t
    integer :: count_old,count_ini
    real    :: t_cpu_old,t_cpu_ini
    logical :: begin = .TRUE.
  END TYPE Time_t 


  PUBLIC :: Time_t,time_perso,DeltaTime,Delta_RealTime

  INTERFACE time_perso
    MODULE PROCEDURE QDUtil_time_perso
  END INTERFACE
  INTERFACE DeltaTime
    MODULE PROCEDURE QDUtil_DeltaTime
  END INTERFACE
  INTERFACE Delta_RealTime
    MODULE PROCEDURE QDUtil_Delta_RealTime
  END INTERFACE
  INTERFACE DeltaTime_withParam_time
    MODULE PROCEDURE QDUtil_DeltaTime_withParam_time
  END INTERFACE
  
  CONTAINS

  !!@description: TODO
  !!@param: TODO
  SUBROUTINE QDUtil_time_perso(name_sub,openmpi,MPI_id)
    USE QDUtil_NumParameters_m, ONLY : Rkind, out_unit
    IMPLICIT NONE

    character (len=*), intent(in)           :: name_sub
    logical,           intent(in), optional :: openmpi
    integer,           intent(in), optional :: MPI_id

    !local variables
    integer           :: tab_time(8) = 0
    real (kind=Rkind) :: dt_real,t_real
    real              :: dt_cpu,t_cpu
    integer           :: seconds,minutes,hours,days

    logical           :: openmpi_loc
    integer           :: MPI_id_loc

    IF (present(openmpi)) THEN
      openmpi_loc = openmpi
    ELSE
      openmpi_loc = .FALSE.
    END IF
    IF (present(MPI_id)) THEN
      MPI_id_loc = MPI_id
    ELSE
      MPI_id_loc = -1
    END IF

    CALL date_and_time(values=tab_time)
    write(out_unit,21) name_sub,tab_time(5:8),(tab_time(3:1:-1))
21  format('     Time and date in ',a,' : ',i2,'h:',                &
           i2,'m:',i2,'.',i3,'s, the ',i2,'/',i2,'/',i4)

    CALL DeltaTime(dt_real,t_real,dt_cpu,t_cpu)

    !============================================
    !real and cpu delta times in the subroutine: "name_sub"
    seconds = int(dt_real)
    minutes = seconds/60
    seconds = mod(seconds,60)
    hours   = minutes/60
    minutes = mod(minutes,60)
    days    = hours/24
    hours   = mod(hours,24)

    IF(openmpi_loc) THEN
      write(out_unit,30) dt_real,name_sub,MPI_id_loc
30        format('        real (s): ',f18.3,' in ',a, ' from MPI id ',i4)
    ELSE
      write(out_unit,31) dt_real,name_sub
31        format('        real (s): ',f18.3,' in ',a)
    ENDIF

    write(out_unit,32) days,hours,minutes,seconds,name_sub
32      format('        real    : ',i3,'d ',i2,'h ',i2,'m ',i2,'s in ',a)

    write(out_unit,33) dt_cpu,name_sub
33      format('        cpu (s): ',f18.3,' in ',a)


    !============================================
    !real and cpu total time
    seconds = int(t_real)
    minutes = seconds/60
    seconds = mod(seconds,60)
    hours   = minutes/60
    minutes = mod(minutes,60)
    days    = hours/24
    hours   = mod(hours,24)

    IF(openmpi_loc) THEN
      write(out_unit,40) t_real,MPI_id_loc
40        format('  Total real (s): ',f18.3,' from MPI id ',i4)
    ELSE
      write(out_unit,41) t_real
41        format('  Total real (s): ',f18.3)
    ENDIF

    write(out_unit,42) days,hours,minutes,seconds
42      format('  Total real    : ',i3,'d ',i2,'h ',i2,'m ',i2,'s')
    write(out_unit,43) t_cpu
43      format('  Total cpu (s): ',f18.3)

    flush(out_unit)

  END SUBROUTINE QDUtil_time_perso

  SUBROUTINE QDUtil_DeltaTime(dt_real,t_real,dt_cpu,t_cpu,LocalTime)
    USE QDUtil_NumParameters_m, ONLY : Rkind
    IMPLICIT NONE

    real (kind=Rkind), intent(inout)           :: dt_real,t_real
    real,              intent(inout)           :: dt_cpu,t_cpu
    TYPE (Time_t),     intent(inout), optional :: LocalTime


    integer       :: count,count_work,freq

    TYPE (Time_t), save :: MainTime

    IF (present(LocalTime)) THEN
      CALL DeltaTime_withParam_time(dt_real,t_real,dt_cpu,t_cpu,LocalTime)
    ELSE
      CALL DeltaTime_withParam_time(dt_real,t_real,dt_cpu,t_cpu,MainTime)
    END IF

  END SUBROUTINE QDUtil_DeltaTime

  SUBROUTINE QDUtil_DeltaTime_withParam_time(dt_real,t_real,dt_cpu,t_cpu,LocalTime)
    USE QDUtil_NumParameters_m, ONLY : Rkind
    IMPLICIT NONE

    real (kind=Rkind), intent(inout) :: dt_real,t_real
    real,              intent(inout) :: dt_cpu,t_cpu
    TYPE (Time_t),     intent(inout) :: LocalTime


    integer       :: count,count_work,freq,count_max

    CALL system_clock(count=count,count_rate=freq,count_max=count_max)
    call cpu_time(t_cpu)

    IF (LocalTime%begin) THEN
      LocalTime%begin     = .FALSE.
      LocalTime%count_old = count
      LocalTime%count_ini = count
      LocalTime%t_cpu_old = t_cpu
      LocalTime%t_cpu_ini = t_cpu
    END IF


    ! real time
    !count_work = count-LocalTime%count_old
    count_work=merge(count-LocalTime%count_old,count-LocalTime%count_old+count_max,&
                     count>=LocalTime%count_old)
    dt_real    = real(count_work,kind=Rkind)/real(freq,kind=Rkind)
    !count_work = count-LocalTime%count_ini
    count_work=merge(count-LocalTime%count_ini,count-LocalTime%count_ini+count_max,&
                     count>=LocalTime%count_ini)
    t_real     = real(count_work,kind=Rkind)/real(freq,kind=Rkind)

    ! cpu time
    dt_cpu  = t_cpu-LocalTime%t_cpu_old
    t_cpu   = t_cpu-LocalTime%t_cpu_ini

    ! change the save variable
    LocalTime%count_old = count
    LocalTime%t_cpu_old = t_cpu

  END SUBROUTINE QDUtil_DeltaTime_withParam_time
  FUNCTION QDUtil_Delta_RealTime(LocalTime) RESULT(dt_real)
    USE QDUtil_NumParameters_m, ONLY : Rkind
    IMPLICIT NONE

    TYPE (Time_t), intent(inout), optional :: LocalTime


    real (kind=Rkind) :: dt_real,t_real
    real              :: dt_cpu,t_cpu

    IF (present(LocalTime)) THEN
      CALL DeltaTime(dt_real,t_real,dt_cpu,t_cpu,LocalTime)
    ELSE
      CALL DeltaTime(dt_real,t_real,dt_cpu,t_cpu)
    END IF

  END FUNCTION QDUtil_Delta_RealTime

END MODULE QDUtil_Time_m

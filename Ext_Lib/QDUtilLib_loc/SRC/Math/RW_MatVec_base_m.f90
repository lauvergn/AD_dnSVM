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
MODULE QDUtil_RW_MatVec_base_m
  USE QDUtil_NumParameters_m, ONLY : Name_longlen
  IMPLICIT NONE

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

END MODULE QDUtil_RW_MatVec_base_m

PROGRAM testreal128
  USE, intrinsic :: ISO_FORTRAN_ENV, ONLY : real128
  IMPLICIT NONE
  IF (real128 < 0) THEN
    write(*,'(a)') '0'
  ELSE
    write(*,'(a)') '1'
  END IF
END PROGRAM testreal128

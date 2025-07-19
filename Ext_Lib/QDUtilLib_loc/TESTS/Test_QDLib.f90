PROGRAM Test_QDLib
  USE QDUtil_m
  IMPLICIT NONE

!  #if __LAPACK == 0
!    write(out_unit,*) '  Lapack library is not linked'
!  #else
!    write(out_unit,*) '  Lapack library is linked'
!  #endif

  CALL Test_QDUtil_NumParameters()  
  CALL Test_QDUtil_MathUtil()
  CALL Test_QDUtil_String()
  CALL Test_QDUtil_RW_MatVec()
  CALL Test_QDUtil_Matrix()
  CALL Test_QDUtil_Vector()
  CALL Test_QDUtil_Diago()
  CALL Test_QDUtil_Frac()
  CALL Test_QDUtil_File()
  CALL Test_QDUtil_Memory()
  CALL Test_QDUtil_FFT_OOURA()

  CALL Test_Quadrature_QDUtil()

END PROGRAM Test_QDLib
PROGRAM Test_QDLib
#ifndef __WITHRK16
#define __WITHRK16 1
#endif
  USE QDUtil_m
  IMPLICIT NONE

  CALL version_QDUtil(Print_Version=.TRUE.)

  CALL Test_QDUtil_NumParameters()
  CALL Test_QDUtil_Frac()
  CALL Test_QDUtil_File()
  CALL Test_QDUtil_Memory()
  CALL Test_QDUtil_String()
  CALL Test_QDUtil_Time()

  CALL Test_QDUtil_RW_MatVec()
  CALL Test_QDUtil_MathUtil()
  CALL Test_QDUtil_Matrix()
  CALL Test_QDUtil_Vector()
  CALL Test_QDUtil_Diago()

  CALL Test_QDUtil_FFT_OOURA()

  CALL Test_Quadrature_QDUtil()

#if __WITHRK16 == 1
  CALL Test_QDUtil_DiagoRk16()
  CALL Test_Quadrature_Rk16_QDUtil()
#endif
END PROGRAM Test_QDLib
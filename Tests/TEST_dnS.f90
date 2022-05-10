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
!!
PROGRAM TEST_dnS
  USE ADLib_NumParameters_m
  USE ADLib_Util_m
  USE ADdnSVM_m
  USE ADLib_Test_m
  IMPLICIT NONE

    TYPE (dnS_t)                     :: dnX,dn2X,dnY,dnZ,Sana,Snum,dnXZ
    TYPE (dnS_t), allocatable        :: Vec_dnS(:)
    TYPE (test_t)                    :: test_var
    logical                          :: val_test,res_test

    real (kind=Rkind)                :: x,y,z,r,th,err,maxdiff,maxdnS
    real (kind=Rkind), allocatable   :: JacNewOld(:,:),JacNewOld_ana(:,:)

    integer                          :: nderiv,nio_test
    real (kind=Rkind)                :: dnSerr_test = FIVE*ONETENTH**4


    real (kind=Rkind), external  :: faplusx,faminusx,fatimex,faoverx
    real (kind=Rkind), external  :: SQRT_perso,ABS_perso,EXP_perso,LOG_perso,LOG10_perso
    real (kind=Rkind), external  :: SIN_perso,ASIN_perso,COS_perso,ACOS_perso,TAN_perso,ATAN_perso
    real (kind=Rkind), external  :: SINH_perso,ASINH_perso,COSH_perso,ACOSH_perso,TANH_perso,ATANH_perso

    character (len=*), parameter :: name_sub='TEST_dnS'


    CALL test_initialyze(test_var,test_name='dnS')

    nderiv = 3
    write(out_unitp,'(a,i2)') "== TESTING dnS module with nderiv=",nderiv

    x       = 0.5_Rkind
    dnX     = variable(x  ,nVar=1,nderiv=nderiv,iVar=1) ! to set up the derivatives
    dn2X    = variable(x+x,nVar=1,nderiv=nderiv,iVar=1) ! to set up the derivatives

    IF (print_level > 0) THEN
      CALL Write_dnS(dnX,test_var%test_log_file_unit,info='dnX')
      CALL Write_dnS(dn2X,test_var%test_log_file_unit,info='dn2X')
    END IF



   write(out_unitp,'(a)') "============================================"
   write(out_unitp,'(a)') "operators: == /= > >= < <="
   write(out_unitp,'(a)') "============================================"

  res_test = (dnX == dnX)
  CALL test_logical(test_var,test1=res_test,                                    &
                   info='dnX == dnX:   T ? ' // logical_TO_char(res_test) )
  res_test = (dnX == dn2X)
  CALL test_logical(test_var,test1=res_test,test2=.FALSE.,                      &
                   info='dnX == dn2X:  F ? ' // logical_TO_char(res_test) )

  res_test = (dnX /= dnX)
  CALL test_logical(test_var,test1=res_test,test2=.FALSE.,                      &
                   info='dnX /= dnX:   F ? ' // logical_TO_char(res_test) )
  res_test = (dnX /= dn2X)
  CALL test_logical(test_var,test1=res_test,                                    &
                   info='dnX /= dn2X:  T ? ' // logical_TO_char(res_test) )

  res_test = (dnX > dnX)
  CALL test_logical(test_var,test1=res_test,test2=.FALSE.,                      &
                   info='dnX  > dnX:   F ? ' // logical_TO_char(res_test) )
  res_test = (dnX  > dn2X)
  CALL test_logical(test_var,test1=res_test,test2=.FALSE.,                      &
                   info='dnX  > dn2X:  F ? ' // logical_TO_char(res_test) )
  res_test = (dn2X > dnX)
  CALL test_logical(test_var,test1=res_test,                                    &
                   info='dn2X > dnX:   T ? ' // logical_TO_char(res_test) )

  res_test = (dnX >= dnX)
  CALL test_logical(test_var,test1=res_test,                                    &
                   info='dnX  >= dnX:  T ? ' // logical_TO_char(res_test) )
  res_test = (dnX  >= dn2X)
  CALL test_logical(test_var,test1=res_test,test2=.FALSE.,                      &
                   info='dnX  >= dn2X: F ? ' // logical_TO_char(res_test) )
  res_test = (dn2X >= dnX)
  CALL test_logical(test_var,test1=res_test,                                    &
                   info='dn2X >= dnX:  T ? ' // logical_TO_char(res_test) )

  res_test = (dnX  < dnX)
  CALL test_logical(test_var,test1=res_test,test2=.FALSE.,                      &
                   info='dnX  < dnX:   F ? ' // logical_TO_char(res_test) )
  res_test = (dnX  < dn2X)
  CALL test_logical(test_var,test1=res_test,                                    &
                   info='dnX  < dn2X:  T ? ' // logical_TO_char(res_test) )
  res_test = (dn2X < dnX)
  CALL test_logical(test_var,test1=res_test,test2=.FALSE.,                      &
                   info='dn2X < dnX:   F ? ' // logical_TO_char(res_test) )

  res_test = (dnX  <= dnX)
  CALL test_logical(test_var,test1=res_test,                                    &
                   info='dnX  <= dnX:  T ? ' // logical_TO_char(res_test) )
  res_test = (dnX  <= dn2X)
  CALL test_logical(test_var,test1=res_test,                                    &
                   info='dnX  <= dn2X: T ? ' // logical_TO_char(res_test) )
  res_test = (dn2X <= dnX)
  CALL test_logical(test_var,test1=res_test,test2=.FALSE.,                      &
                   info='dn2X <= dnX:  F ? ' // logical_TO_char(res_test) )

  write(out_unitp,'(a)') "============================================"
  write(out_unitp,'(a)') "operators: .EQ. .NE. .GT. .GE. .LT. .LE."
  write(out_unitp,'(a)') "============================================"
  res_test = (dnX .EQ. dnX)
  CALL test_logical(test_var,test1=res_test,                                    &
                  info='dnX .EQ. dnX:   T ? ' // logical_TO_char(res_test) )
  res_test = (dnX .NE. dn2X)
  CALL test_logical(test_var,test1=res_test,                                    &
                  info='dnX .NE.dn2X:   T ? ' // logical_TO_char(res_test) )

  res_test = (dnX .GT. dnX)
  CALL test_logical(test_var,test1=res_test,test2=.FALSE.,                      &
                  info='dnX .GT. dnX:   F ? ' // logical_TO_char(res_test) )
  res_test = (dnX .GE. dnX)
  CALL test_logical(test_var,test1=res_test,                                    &
                  info='dnX .GE. dnX:   T ? ' // logical_TO_char(res_test) )

  res_test = (dnX .LT. dnX)
  CALL test_logical(test_var,test1=res_test,test2=.FALSE.,                      &
                  info='dnX .LT. dnX:   F ? ' // logical_TO_char(res_test) )
  res_test = (dnX .LE. dnX)
  CALL test_logical(test_var,test1=res_test,                                    &
                  info='dnX .LE. dnX:   T ? ' // logical_TO_char(res_test) )

  write(out_unitp,'(a)') "============================================"
  write(out_unitp,'(a)') "operators: + - * / **"
  write(out_unitp,'(a)') "============================================"

  Sana = 0.5_Rkind
  Sana = Sana + dnX
  Snum = AD_get_Num_dnS_FROM_f_x(x,faplusx,nderiv=nderiv)
  res_test = AD_Check_dnS_IS_ZERO(Sana-Snum,dnSerr_test)
  CALL test_logical(test_var,test1=res_test,info='a+dnX:         (Sana-Snum)==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,test_var%test_log_file_unit,info='0.5 + dnX')

  Sana = dnX + 0.5_Rkind
  Snum = AD_get_Num_dnS_FROM_f_x(x,faplusx,nderiv=nderiv)
  res_test = AD_Check_dnS_IS_ZERO(Sana-Snum,dnSerr_test)
  CALL test_logical(test_var,test1=res_test,info='dnX+a:         (Sana-Snum)==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,test_var%test_log_file_unit,info='dnX + 0.5')

  Sana = dnX + dnX
  Snum = AD_get_Num_dnS_FROM_f_x(x,fatimex,nderiv=nderiv)
  res_test = AD_Check_dnS_IS_ZERO(Sana-Snum,dnSerr_test)
  CALL test_logical(test_var,test1=res_test,info='dnX+dnX=2*dnX: (Sana-Snum)==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,test_var%test_log_file_unit,info='dnX + dnX')

  Sana = +(0.5_Rkind - dnX)
  Snum = AD_get_Num_dnS_FROM_f_x(x,faminusx,nderiv=nderiv)
  res_test = AD_Check_dnS_IS_ZERO(Sana-Snum,dnSerr_test)
  CALL test_logical(test_var,test1=res_test,info='+(a-dnX):      (Sana-Snum)==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,test_var%test_log_file_unit,info='+(0.5 - dnX)')

  Sana = -(dnX - 0.5_Rkind)
  Snum = AD_get_Num_dnS_FROM_f_x(x,faminusx,nderiv=nderiv)
  res_test = AD_Check_dnS_IS_ZERO(Sana-Snum,dnSerr_test)
  CALL test_logical(test_var,test1=res_test,info='-(dnX-a):      (Sana-Snum)==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,test_var%test_log_file_unit,info='-(dnX - 0.5)')

  Sana = dnX - dnX
  res_test = AD_Check_dnS_IS_ZERO(Sana,dnSerr_test)
  CALL test_logical(test_var,test1=res_test,info='dnX-dnX                   ==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,test_var%test_log_file_unit,info='dnX - dnX')

  Sana = 2._Rkind * dnX
  Snum = AD_get_Num_dnS_FROM_f_x(x,fatimex,nderiv=nderiv)
  res_test = AD_Check_dnS_IS_ZERO(Sana-Snum,dnSerr_test)
  CALL test_logical(test_var,test1=res_test,info='a*dnX:         (Sana-Snum)==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,test_var%test_log_file_unit,info='2. * dnX')

  Sana =  dnX * 2._Rkind
  Snum = AD_get_Num_dnS_FROM_f_x(x,fatimex,nderiv=nderiv)
  res_test = AD_Check_dnS_IS_ZERO(Sana-Snum,dnSerr_test)
  CALL test_logical(test_var,test1=res_test,info='dnX*a:         (Sana-Snum)==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,test_var%test_log_file_unit,info='dnX * 2.')

  Sana =  dnX * dnX - dnX**(2._Rkind)
  res_test = AD_Check_dnS_IS_ZERO(Sana,dnSerr_test)
  CALL test_logical(test_var,test1=res_test,info='dnX*dnX-dnX**2.           ==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,test_var%test_log_file_unit,info='dnX * dnX - dnX**(2.)')

  Sana =  dnX / 0.5_Rkind -dnX*2._Rkind
  res_test = AD_Check_dnS_IS_ZERO(Sana,dnSerr_test)
  CALL test_logical(test_var,test1=res_test,info='dnX/0.5-dnX*2             ==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,test_var%test_log_file_unit,info='dnX/0.5 -dnX*2.')

  Sana =  0.5_Rkind / dnX - 0.5_Rkind*dnX**(-1)
  res_test = AD_Check_dnS_IS_ZERO(Sana,dnSerr_test)
  CALL test_logical(test_var,test1=res_test,info='0.5/dnX-0.5.dnX*^-1       ==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,test_var%test_log_file_unit,info='0.5 / dnX - 0.5*dnX**(-1)')

  Sana =  dnX / dnX - 1._Rkind
  res_test = AD_Check_dnS_IS_ZERO(Sana,dnSerr_test)
  CALL test_logical(test_var,test1=res_test,info='dnX/dnX-1.                ==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,test_var%test_log_file_unit,info='dnX / dnX - 1.')

  Sana =  dnX**0.5_Rkind - sqrt(dnX)
  res_test = AD_Check_dnS_IS_ZERO(Sana,dnSerr_test)
  CALL test_logical(test_var,test1=res_test,info='dnX**0.5-sqrt(dnX)        ==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,test_var%test_log_file_unit,info='dnX**0.5-sqrt(dnX)')

  Sana =  dnX**3 - dnX*dnX*dnX
  res_test = AD_Check_dnS_IS_ZERO(Sana,dnSerr_test)
  CALL test_logical(test_var,test1=res_test,info='dnX**3-dnX*dnX*dnX        ==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,test_var%test_log_file_unit,info='dnX**3 - dnX*dnX*dnX')


  write(out_unitp,'(a)') "============================================"
  write(out_unitp,'(a)') "functions: sqrt, exp, log, ... sin, asin, ... acosh ..."
  write(out_unitp,'(a)') "============================================"

  Sana = sqrt(dnX)
  Snum = AD_get_Num_dnS_FROM_f_x(x,SQRT_perso,nderiv=nderiv)
  res_test = AD_Check_dnS_IS_ZERO(Sana-Snum,dnSerr_test)
  CALL test_logical(test_var,test1=res_test,info='sqrt: (Sana-Snum)==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,test_var%test_log_file_unit,info='sqrt(dnX)')

  Sana = abs(dnX)
  Snum = AD_get_Num_dnS_FROM_f_x(x,ABS_perso,nderiv=nderiv)
  res_test = AD_Check_dnS_IS_ZERO(Sana-Snum,dnSerr_test)
  CALL test_logical(test_var,test1=res_test,info='abs:  (Sana-Snum)==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,test_var%test_log_file_unit,info='abs(dnX)')

  Sana = exp(dnX)
  Snum = AD_get_Num_dnS_FROM_f_x(x,EXP_perso,nderiv=nderiv)
  res_test = AD_Check_dnS_IS_ZERO(Sana-Snum,dnSerr_test)
  CALL test_logical(test_var,test1=res_test,info='exp:  (Sana-Snum)==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,test_var%test_log_file_unit,info='exp(dnX)')

  Sana = log(dnX)
  Snum = AD_get_Num_dnS_FROM_f_x(x,log_perso,nderiv=nderiv)
  res_test = AD_Check_dnS_IS_ZERO(Sana-Snum,dnSerr_test)
  CALL test_logical(test_var,test1=res_test,info='log:  (Sana-Snum)==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,test_var%test_log_file_unit,info='log(dnX)')

  Sana = log10(dnX)
  Snum = AD_get_Num_dnS_FROM_f_x(x,log10_perso,nderiv=nderiv)
  res_test = AD_Check_dnS_IS_ZERO(Sana-Snum,dnSerr_test)
  CALL test_logical(test_var,test1=res_test,info='log10:(Sana-Snum)==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,test_var%test_log_file_unit,info='log10(dnX)')

  Sana = sin(dnX)
  Snum = AD_get_Num_dnS_FROM_f_x(x,sin_perso,nderiv=nderiv)
  res_test = AD_Check_dnS_IS_ZERO(Sana-Snum,dnSerr_test)
  CALL test_logical(test_var,test1=res_test,info='sin:  (Sana-Snum)==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,test_var%test_log_file_unit,info='sin(dnX)')

  Sana = asin(dnX)
  Snum = AD_get_Num_dnS_FROM_f_x(x,asin_perso,nderiv=nderiv)
  res_test = AD_Check_dnS_IS_ZERO(Sana-Snum,dnSerr_test)
  CALL test_logical(test_var,test1=res_test,info='asin: (Sana-Snum)==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,test_var%test_log_file_unit,info='asin(dnX)')

  Sana = cos(dnX)
  Snum = AD_get_Num_dnS_FROM_f_x(x,cos_perso,nderiv=nderiv)
  res_test = AD_Check_dnS_IS_ZERO(Sana-Snum,dnSerr_test)
  CALL test_logical(test_var,test1=res_test,info='cos:  (Sana-Snum)==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,test_var%test_log_file_unit,info='cos(dnX)')

  Sana = acos(dnX)
  Snum = AD_get_Num_dnS_FROM_f_x(x,acos_perso,nderiv=nderiv)
  res_test = AD_Check_dnS_IS_ZERO(Sana-Snum,dnSerr_test)
  CALL test_logical(test_var,test1=res_test,info='acos: (Sana-Snum)==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,test_var%test_log_file_unit,info='acos(dnX)')

  Sana = tan(dnX)
  Snum = AD_get_Num_dnS_FROM_f_x(x,tan_perso,nderiv=nderiv)
  res_test = AD_Check_dnS_IS_ZERO(Sana-Snum,dnSerr_test)
  CALL test_logical(test_var,test1=res_test,info='tan:  (Sana-Snum)==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,test_var%test_log_file_unit,info='tan(dnX)')

  Sana = atan(dnX)
  Snum = AD_get_Num_dnS_FROM_f_x(x,atan_perso,nderiv=nderiv)
  res_test = AD_Check_dnS_IS_ZERO(Sana-Snum,dnSerr_test)
  CALL test_logical(test_var,test1=res_test,info='atan: (Sana-Snum)==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,test_var%test_log_file_unit,info='atan(dnX)')

  Sana = sinh(dnX)
  Snum = AD_get_Num_dnS_FROM_f_x(x,sinh_perso,nderiv=nderiv)
  res_test = AD_Check_dnS_IS_ZERO(Sana-Snum,dnSerr_test)
  CALL test_logical(test_var,test1=res_test,info='sinh: (Sana-Snum)==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,test_var%test_log_file_unit,info='sinh(dnX)')

  Sana = asinh(dnX)
  Snum = AD_get_Num_dnS_FROM_f_x(x,asinh_perso,nderiv=nderiv)
  res_test = AD_Check_dnS_IS_ZERO(Sana-Snum,dnSerr_test)
  CALL test_logical(test_var,test1=res_test,info='asinh:(Sana-Snum)==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,test_var%test_log_file_unit,info='asinh(dnX)')

  Sana = cosh(dnX)
  Snum = AD_get_Num_dnS_FROM_f_x(x,cosh_perso,nderiv=nderiv)
  res_test = AD_Check_dnS_IS_ZERO(Sana-Snum,dnSerr_test)
  CALL test_logical(test_var,test1=res_test,info='cosh: (Sana-Snum)==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,test_var%test_log_file_unit,info='cosh(dnX)')

  dnY = Variable(FOUR*x  ,nVar=1,nderiv=nderiv,iVar=1) ! to set up the derivatives
  Sana = acosh(dnY)
  Snum = AD_get_Num_dnS_FROM_f_x(FOUR*x,acosh_perso,nderiv=nderiv)
  res_test = AD_Check_dnS_IS_ZERO(Sana-Snum,dnSerr_test)
  CALL test_logical(test_var,test1=res_test,info='acosh:(Sana-Snum)==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,test_var%test_log_file_unit,info='acosh(dnY)')

  Sana = tanh(dnX)
  Snum = AD_get_Num_dnS_FROM_f_x(x,tanh_perso,nderiv=nderiv)
  res_test = AD_Check_dnS_IS_ZERO(Sana-Snum,dnSerr_test)
  CALL test_logical(test_var,test1=res_test,info='tanh: (Sana-Snum)==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,test_var%test_log_file_unit,info='tanh(dnX)')

  Sana = atanh(dnX)
  Snum = AD_get_Num_dnS_FROM_f_x(x,atanh_perso,nderiv=nderiv)
  res_test = AD_Check_dnS_IS_ZERO(Sana-Snum,dnSerr_test)
  CALL test_logical(test_var,test1=res_test,info='atanh:(Sana-Snum)==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,test_var%test_log_file_unit,info='atanh(dnX)')


  write(out_unitp,'(a)') "============================================"
  write(out_unitp,'(a)') "new tests: **, composition"
  write(out_unitp,'(a)') "============================================"

  Sana =  dnX**0 - ONE
  res_test = AD_Check_dnS_IS_ZERO(Sana,dnSerr_test)
  CALL test_logical(test_var,test1=res_test,info='dnX**0-ONE                ==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,test_var%test_log_file_unit,info='dnX**0-ONE')

  Sana =  dnX**1 - dnX
  res_test = AD_Check_dnS_IS_ZERO(Sana,dnSerr_test)
  CALL test_logical(test_var,test1=res_test,info='dnX**1-dnX                ==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,test_var%test_log_file_unit,info='dnX**1-dnX')

  Sana =  dnX**2 - dnX*dnX
  res_test = AD_Check_dnS_IS_ZERO(Sana,dnSerr_test)
  CALL test_logical(test_var,test1=res_test,info='dnX**2-dnX*dnX            ==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,test_var%test_log_file_unit,info='dnX**2-dnX*dnX')

  Sana =  dnX**3 - dnX*dnX*dnX
  res_test = AD_Check_dnS_IS_ZERO(Sana,dnSerr_test)
  CALL test_logical(test_var,test1=res_test,info='dnX**3-dnX*dnX*dnX        ==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,test_var%test_log_file_unit,info='dnX**3-dnX*dnX*dnX')

  Sana =  sqrt(dnX**2) - dnX
  res_test = AD_Check_dnS_IS_ZERO(Sana,dnSerr_test)
  CALL test_logical(test_var,test1=res_test,info='sqrt(dnX**2) - dnX        ==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,test_var%test_log_file_unit,info='sqrt(dnX**2) - dnX')


  write(out_unitp,'(a)') "============================================"
  write(out_unitp,'(a)') "new tests: 3D, nderiv=2"
  write(out_unitp,'(a)') "============================================"
  nderiv = 2
  x = 0.5_Rkind
  z = 2.0_Rkind

  dnX     = Variable(x  ,nVar=3,nderiv=nderiv,iVar=1) ! to set up the derivatives
  dnZ     = Variable(z  ,nVar=3,nderiv=nderiv,iVar=3) ! to set up the derivatives
  Sana    = dnX*dnZ ! It is equivalent to 3D function f(x,y,z) = x*z
  CALL set_dnS(dnXZ,d0=     x*z,                             &
                    d1=        [z,   ZERO,x],                &
                    d2=reshape([ZERO,ZERO,ONE,               &
                                ZERO,ZERO,ZERO,              &
                                ONE, ZERO,ZERO],shape=[3,3]))

  res_test = AD_Check_dnS_IS_ZERO(Sana-dnXZ,dnSerr_test)
  CALL test_logical(test_var,test1=res_test,info='dnX*dnZ -dnS_Result       ==0?')
  IF (print_level > 0) THEN
    CALL Write_dnS(dnX,test_var%test_log_file_unit,info='dnX')
    CALL Write_dnS(dnZ,test_var%test_log_file_unit,info='dnZ')
    CALL Write_dnS(Sana,test_var%test_log_file_unit,info='dnX*dnZ (3D)')
  END IF

  nderiv = 2
  x = 0.5_Rkind
  z = 2.0_Rkind
  Vec_dnS = Variable([x,ZERO,z],nderiv=nderiv)

  Sana    = Vec_dnS(1)*Vec_dnS(3) ! It is equivalent to a 3D function f(x,y,z) = x*z
  CALL set_dnS(dnXZ,d0=     x*z,                             &
                    d1=        [z,   ZERO,x],                &
                    d2=reshape([ZERO,ZERO,ONE,               &
                                ZERO,ZERO,ZERO,              &
                                ONE, ZERO,ZERO],shape=[3,3]))

  res_test = AD_Check_dnS_IS_ZERO(Sana-dnXZ,dnSerr_test)
  CALL test_logical(test_var,test1=res_test,info='Vec_dnS(1)*Vec_dnS(3)-dnS_Result==0?')
  IF (print_level > 0) THEN
    CALL Write_dnS(Vec_dnS(1),test_var%test_log_file_unit,info='Vec_dnS(1)')
    CALL Write_dnS(Vec_dnS(3),test_var%test_log_file_unit,info='Vec_dnS(3)')
    CALL Write_dnS(Sana,test_var%test_log_file_unit,info='dnX*dnZ (3D)')
  END IF

  write(out_unitp,'(a)') "============================================"
  write(out_unitp,'(a)') "Jacobian(inew,iold): 2D, nderiv=2"
  write(out_unitp,'(a)') "Polar transformation"
  write(out_unitp,'(a)') "============================================"

  r  = TWO
  th = Pi/3
  Vec_dnS = Variable([r,th], nderiv=2 ) ! Vec(1) : r, Vec(2) : th

  JacNewOld     = get_Jacobian( Vec_dnS(1)*[cos(Vec_dnS(2)),sin(Vec_dnS(2))] ) ! Jacobian of a polar transformation
  JacNewOld_ana = reshape([cos(th),sin(th),-r*sin(th),r*cos(th)],shape(JacNewOld))

  res_test = ( maxval(abs(JacNewOld-JacNewOld_ana)) < dnSerr_test )
  CALL test_logical(test_var,test1=res_test,info='Jacobian-Jacobian_ana     ==0?')

  IF (print_level > 0) THEN
    write(test_var%test_log_file_unit,*) 'Jac(inew,iold)=[ dQinew/dQiold ]:'
    CALL Write_RMat(JacNewOld,test_var%test_log_file_unit,5)
    write(test_var%test_log_file_unit,*) 'analytical: [dx/dr, dx/dth]:   [0.5,     -1.732...]'
    write(test_var%test_log_file_unit,*) 'analytical: [dy/dr, dy/dth]:   [0.866..,  1.      ]'
  END IF

  write(out_unitp,'(a)') "============================================"
  write(out_unitp,'(a)') "new tests : Vec_OF_dnS(1:3), 2D, nderiv=2"
  write(out_unitp,'(a)') "============================================"

  nderiv = 2
  x=0.5_Rkind
  y=1.0_Rkind
  dnX     = Variable(x  ,nVar=2,nderiv=nderiv,iVar=1) ! to set up the derivatives
  dnY     = Variable(y  ,nVar=2,nderiv=nderiv,iVar=2) ! to set up the derivatives

  Vec_dnS = [dnX,dnY,dnX+dnY]
  Sana = dot_product(Vec_dnS,Vec_dnS) ! 2*(dnX**2+dnY**2+dnX*dnY)
  dnXZ = TWO*(dnX**2+dnY**2+dnX*dnY)
  res_test = AD_Check_dnS_IS_ZERO(Sana-dnXZ,dnSerr_test)
  CALL test_logical(test_var,test1=res_test,info='dot_product - dnS_Result  ==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,test_var%test_log_file_unit,info='dot_product (2D)')

  Vec_dnS(:) = [-dnX,-dnY,dnX+dnY]
  Sana = sum(Vec_dnS) ! ZERO
  res_test = AD_Check_dnS_IS_ZERO(Sana,dnSerr_test)
  CALL test_logical(test_var,test1=res_test,info='sum                       ==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,test_var%test_log_file_unit,info='sum (2D)')


  CALL dealloc_dnS(dnZ)
  dnZ = ONE
  Vec_dnS(:) = [dnX**2,ONE/dnX,ONE/dnX]
  Sana = product(Vec_dnS) ! ONE
  res_test = AD_Check_dnS_IS_ZERO(Sana-dnZ,dnSerr_test)
  CALL test_logical(test_var,test1=res_test,info='product - 1               ==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,test_var%test_log_file_unit,info='product - 1 (2D)')


  write(out_unitp,'(a)') "============================================"
  write(out_unitp,'(a)') "new tests : two-argument fuction, 2D, nderiv=2"
  write(out_unitp,'(a)') "============================================"
  nderiv = 2
  x = -0.5_Rkind
  y = -1.0_Rkind
  dnX     = Variable(x  ,nVar=2,nderiv=nderiv,iVar=1) ! to set up the derivatives
  dnY     = Variable(y  ,nVar=2,nderiv=nderiv,iVar=2) ! to set up the derivatives
  Sana = atan2(dnY,dnX)
  Snum = atan(dnY/dnX)-pi
  res_test = AD_Check_dnS_IS_ZERO(Sana-Snum,dnSerr_test)
  CALL test_logical(test_var,test1=res_test,info='atan2 - atan              ==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,test_var%test_log_file_unit,info='atan2 (2D)')
  IF (print_level > 0) CALL Write_dnS(Snum,test_var%test_log_file_unit,info='atan (2D)')

  write(out_unitp,'(a)') "============================================"
  write(out_unitp,'(a)') "new tests : init const, 2D, nderiv=2"
  write(out_unitp,'(a)') "============================================"
  nderiv = 2
  x = 0.5_Rkind
  y = 1.0_Rkind
  dnX     = Variable(x  ,nVar=2,nderiv=nderiv,iVar=1) ! to set up the derivatives

  write(test_var%test_log_file_unit,'(a)') "------------------------------------------------------"
  write(test_var%test_log_file_unit,'(a)') "------------------------------------------------------"
  write(test_var%test_log_file_unit,'(a)') "------------------------------------------------------"
  CALL dealloc_dnS(Sana)
  CALL dealloc_dnS(Snum)
  Sana    = x ! here, Sana%nderiv = -1
  IF (print_level > 0) CALL Write_dnS(Sana,test_var%test_log_file_unit,info='x, constant')
  Sana    = Sana + dnX ! here, Sana%nderiv = dnX%nderiv
  IF (print_level > 0) CALL Write_dnS(Sana,test_var%test_log_file_unit,info='(x+dnX)')
  Snum    = dnX + x    ! here, Snum%nderiv = dnX%nderiv
  IF (print_level > 0) CALL Write_dnS(Snum,test_var%test_log_file_unit,info='(dnX+x)')
  res_test = AD_Check_dnS_IS_ZERO(Sana-Snum,dnSerr_test)
  CALL test_logical(test_var,test1=res_test,info='(x+dnX) - (dnX+x)         ==0?')


  write(test_var%test_log_file_unit,'(a)') "------------------------------------------------------"
  write(test_var%test_log_file_unit,'(a)') "------------------------------------------------------"
  write(test_var%test_log_file_unit,'(a)') "------------------------------------------------------"
  CALL dealloc_dnS(Sana)
  CALL dealloc_dnS(Snum)
  Sana    = x ! here, Sana%nderiv = -1
  IF (print_level > 0) CALL Write_dnS(Sana,test_var%test_log_file_unit,info='x, constant')
  Sana    = Sana * dnX ! here, Sana%nderiv = dnX%nderiv
  IF (print_level > 0) CALL Write_dnS(Sana,test_var%test_log_file_unit,info='(x*dnX)')
  Snum    = dnX * x    ! here, Snum%nderiv = dnX%nderiv
  IF (print_level > 0) CALL Write_dnS(Snum,test_var%test_log_file_unit,info='(dnX*x)')
  res_test = AD_Check_dnS_IS_ZERO(Sana-Snum,dnSerr_test)
  CALL test_logical(test_var,test1=res_test,info='(x*dnX) - (dnX*x)         ==0?')

  write(test_var%test_log_file_unit,'(a)') "------------------------------------------------------"
  write(test_var%test_log_file_unit,'(a)') "------------------------------------------------------"
  write(test_var%test_log_file_unit,'(a)') "------------------------------------------------------"
  CALL dealloc_dnS(Sana)
  CALL dealloc_dnS(Snum)
  Sana    = x ! here, Sana%nderiv = -1
  IF (print_level > 0) CALL Write_dnS(Sana,test_var%test_log_file_unit,info='x, constant')
  Sana    = Sana / dnX ! here, Sana%nderiv = dnX%nderiv
  IF (print_level > 0)CALL Write_dnS(Sana,test_var%test_log_file_unit,info='(x/dnX)')
  Snum    = dnX**(-1) * x    ! here, Snum%nderiv = dnX%nderiv
  IF (print_level > 0)CALL Write_dnS(Snum,test_var%test_log_file_unit,info='(dnX**(-1)*x)')
  res_test = AD_Check_dnS_IS_ZERO(Sana-Snum,dnSerr_test)
  CALL test_logical(test_var,test1=res_test,info='(x/dnX) - (dnX**(-1)*x)   ==0?')

  write(test_var%test_log_file_unit,'(a)') "------------------------------------------------------"
  write(test_var%test_log_file_unit,'(a)') "------------------------------------------------------"
  write(test_var%test_log_file_unit,'(a)') "------------------------------------------------------"
  CALL dealloc_dnS(Sana)
  CALL dealloc_dnS(Snum)
  Sana    = x ! here, Sana%nderiv = -1
  IF (print_level > 0) CALL Write_dnS(Sana,test_var%test_log_file_unit,info='x, constant')
  Sana    = sin(Sana) ! here, Sana%nderiv = -1
  IF (print_level > 0) CALL Write_dnS(Sana,test_var%test_log_file_unit,info='(sin(x)')
  Snum    = dnX ; Snum = ZERO
  Snum    = sin(x)    ! here, Snum%nderiv = dnX%nderiv
  IF (print_level > 0) CALL Write_dnS(Snum,test_var%test_log_file_unit,info='(sin(x))')
  res_test = AD_Check_dnS_IS_ZERO(Sana-Snum,dnSerr_test)
  CALL test_logical(test_var,test1=res_test,info='(sin(x)) - (sin(x))       ==0?')

  ! end the normal tests

  CALL TEST_EXCEPTION()


  ! finalize the tests
  CALL test_finalize(test_var)



CONTAINS
  SUBROUTINE read_arg()
    USE ADLib_NumParameters_m
    IMPLICIT NONE

    character(len=:), allocatable :: arg,arg2
    integer :: iarg,arg_len

    IF (COMMAND_ARGUMENT_COUNT() /= 0 .AND. COMMAND_ARGUMENT_COUNT() /= 2) THEN
      write(out_unitp,*) ' ERROR in read_arg'
      write(out_unitp,*) ' Wrong ',name_sub,' argument number!'
      write(out_unitp,*) 'argument number',COMMAND_ARGUMENT_COUNT()
      write(out_unitp,*) ' You can have 0 or 2 arguments.'
      STOP 'Wrong TEST_dnS argument number'
    END IF


    DO iarg=1,COMMAND_ARGUMENT_COUNT(),2

      CALL GET_COMMAND_ARGUMENT( NUMBER=iarg, LENGTH=arg_len )
      allocate( character(len=arg_len) :: arg )
      CALL GET_COMMAND_ARGUMENT( NUMBER=iarg, VALUE=arg )

      CALL GET_COMMAND_ARGUMENT( NUMBER=iarg+1, LENGTH=arg_len )
      allocate( character(len=arg_len) :: arg2 )
      CALL GET_COMMAND_ARGUMENT( NUMBER=iarg+1, VALUE=arg2 )

      SELECT CASE(arg)
      CASE("-p","--print")
        read(arg2,*) print_level
      CASE Default
        write(out_unitp,*) ' ERROR in read_arg'
        write(out_unitp,*) ' Wrong ',name_sub,' argument!'
        write(out_unitp,*) '   arg: "',arg,'"'
        write(out_unitp,*) ' The possibilities are:'
        write(out_unitp,*) '    -p or --print'
        STOP 'Wrong TEST_dnS argument(s)'
      END SELECT

      IF (print_level > 0) write(out_unitp,*) 'Argument number: ',iarg,' ==> arg: "',arg,'", arg2: "',arg2,'"'

      deallocate(arg)
      deallocate(arg2)
    END DO
    IF (print_level > 0) write(out_unitp,*) '=================================='

  END SUBROUTINE read_arg

  SUBROUTINE TEST_EXCEPTION()
    USE, INTRINSIC :: ieee_exceptions
    IMPLICIT NONE

      logical                          :: flag_NAN


      character (len=*), parameter :: name_sub='TEST_EXCEPTION'

      nderiv = 3
      write(out_unitp,'(a)') "============================================"
      write(out_unitp,'(a,i2)') "== TESTING EXCEPTION dnS module with nderiv=",nderiv
      write(out_unitp,'(a)') "============================================"

      write(test_var%test_log_file_unit,'(a,i2)') "== TESTING EXCEPTION dnS module with nderiv=",nderiv

      x   = -ONE
      dnX = Variable(x  ,nVar=1,nderiv=nderiv,iVar=1) ! to set up the derivatives
      IF (print_level > 0) CALL Write_dnS(dnX,test_var%test_log_file_unit,info='dnX')

      CALL ieee_set_halting_mode(ieee_invalid, .FALSE.)

      dnY = log(dnX)
      CALL  IEEE_GET_FLAG(ieee_invalid, flag_NAN)
      CALL test_logical(test_var,test1=flag_NAN,              info='log(-1.)            =>    NAN')
      IF (print_level > 0) CALL Write_dnS(dnY,test_var%test_log_file_unit,info='log(-1.)')

      ! reset the NAN exception
      CALL  IEEE_SET_FLAG(ieee_invalid, .FALSE.)

      dnY = cos(dnX)
      CALL  IEEE_GET_FLAG(ieee_invalid, flag_NAN)
      CALL test_logical(test_var,test1=flag_NAN,test2=.FALSE.,info='cos(-1.)            => no NAN')
      IF (print_level > 0) CALL Write_dnS(dnY,test_var%test_log_file_unit,info='cos(-1.)')

      ! reset the NAN exception
      CALL  IEEE_SET_FLAG(ieee_invalid, .FALSE.)

      dnY = log(dnX)
      dnY = cos(dnX)
      CALL  IEEE_GET_FLAG(ieee_invalid, flag_NAN)
      CALL test_logical(test_var,test1=flag_NAN,              info='cos(-1.) ; log(-1.) =>    NAN')

      ! reset the NAN exception
      CALL  IEEE_SET_FLAG(ieee_invalid, .FALSE.)
      dnX = Variable(ZERO  ,nVar=1,nderiv=nderiv,iVar=1) ! to set up the derivatives
      !IF (print_level > 0) CALL Write_dnS(dnX,test_var%test_log_file_unit,info='dnX')
      dnY = sqrt(dnX)
      CALL  IEEE_GET_FLAG(ieee_invalid, flag_NAN)
      CALL test_logical(test_var,test1=flag_NAN,              info='sqrt(0.0)           =>    NAN')
      IF (print_level > 0) CALL Write_dnS(dnY,test_var%test_log_file_unit,info='sqrt(0.0)')

  END SUBROUTINE TEST_EXCEPTION

END PROGRAM TEST_dnS


FUNCTION faplusx(x)
  USE ADLib_NumParameters_m
  IMPLICIT NONE

  real (kind=Rkind) :: faplusx
  real (kind=Rkind), intent(in) :: x

  faplusx = 0.5_Rkind + x

END FUNCTION faplusx
FUNCTION faminusx(x)
  USE ADLib_NumParameters_m
  IMPLICIT NONE

  real (kind=Rkind) :: faminusx
  real (kind=Rkind), intent(in) :: x

  faminusx = 0.5_Rkind - x

END FUNCTION faminusx
FUNCTION fatimex(x)
  USE ADLib_NumParameters_m
  IMPLICIT NONE

  real (kind=Rkind) :: fatimex
  real (kind=Rkind), intent(in) :: x

  fatimex = 2._Rkind * x

END FUNCTION fatimex
FUNCTION faoverx(x)
  USE ADLib_NumParameters_m
  IMPLICIT NONE

  real (kind=Rkind) :: faoverx
  real (kind=Rkind), intent(in) :: x

  faoverx = 0.5_Rkind / x

END FUNCTION faoverx
FUNCTION SQRT_perso(x) RESULT(f)
  USE ADLib_NumParameters_m
  IMPLICIT NONE
  real (kind=Rkind) :: f
  real (kind=Rkind), intent(in) :: x

  f = sqrt(x)

END FUNCTION SQRT_perso
FUNCTION ABS_perso(x) RESULT(f)
  USE ADLib_NumParameters_m
  IMPLICIT NONE
  real (kind=Rkind) :: f
  real (kind=Rkind), intent(in) :: x

  f = ABS(x)

END FUNCTION ABS_perso
FUNCTION EXP_perso(x) RESULT(f)
  USE ADLib_NumParameters_m
  IMPLICIT NONE
  real (kind=Rkind) :: f
  real (kind=Rkind), intent(in) :: x

  f = EXP(x)

END FUNCTION EXP_perso
FUNCTION LOG_perso(x) RESULT(f)
  USE ADLib_NumParameters_m
  IMPLICIT NONE
  real (kind=Rkind) :: f
  real (kind=Rkind), intent(in) :: x

  f = LOG(x)

END FUNCTION LOG_perso
FUNCTION LOG10_perso(x) RESULT(f)
  USE ADLib_NumParameters_m
  IMPLICIT NONE
  real (kind=Rkind) :: f
  real (kind=Rkind), intent(in) :: x

  f = LOG10(x)

END FUNCTION LOG10_perso
FUNCTION SIN_perso(x) RESULT(f)
  USE ADLib_NumParameters_m
  IMPLICIT NONE
  real (kind=Rkind) :: f
  real (kind=Rkind), intent(in) :: x

  f = SIN(x)

END FUNCTION SIN_perso
FUNCTION ASIN_perso(x) RESULT(f)
  USE ADLib_NumParameters_m
  IMPLICIT NONE
  real (kind=Rkind) :: f
  real (kind=Rkind), intent(in) :: x

  f = ASIN(x)

END FUNCTION ASIN_perso
FUNCTION COS_perso(x) RESULT(f)
  USE ADLib_NumParameters_m
  IMPLICIT NONE
  real (kind=Rkind) :: f
  real (kind=Rkind), intent(in) :: x

  f = COS(x)

END FUNCTION COS_perso
FUNCTION ACOS_perso(x) RESULT(f)
  USE ADLib_NumParameters_m
  IMPLICIT NONE
  real (kind=Rkind) :: f
  real (kind=Rkind), intent(in) :: x

  f = ACOS(x)

END FUNCTION ACOS_perso
FUNCTION TAN_perso(x) RESULT(f)
  USE ADLib_NumParameters_m
  IMPLICIT NONE
  real (kind=Rkind) :: f
  real (kind=Rkind), intent(in) :: x

  f = TAN(x)

END FUNCTION TAN_perso
FUNCTION ATAN_perso(x) RESULT(f)
  USE ADLib_NumParameters_m
  IMPLICIT NONE
  real (kind=Rkind) :: f
  real (kind=Rkind), intent(in) :: x

  f = ATAN(x)

END FUNCTION ATAN_perso
FUNCTION SINH_perso(x) RESULT(f)
  USE ADLib_NumParameters_m
  IMPLICIT NONE
  real (kind=Rkind) :: f
  real (kind=Rkind), intent(in) :: x

  f = SINH(x)

END FUNCTION SINH_perso
FUNCTION ASINH_perso(x) RESULT(f)
  USE ADLib_NumParameters_m
  IMPLICIT NONE
  real (kind=Rkind) :: f
  real (kind=Rkind), intent(in) :: x

    f = asinh(x)

END FUNCTION ASINH_perso
FUNCTION COSH_perso(x) RESULT(f)
  USE ADLib_NumParameters_m
  IMPLICIT NONE
  real (kind=Rkind) :: f
  real (kind=Rkind), intent(in) :: x

  f = COSH(x)

END FUNCTION COSH_perso
FUNCTION ACOSH_perso(x) RESULT(f)
  USE ADLib_NumParameters_m
  IMPLICIT NONE
  real (kind=Rkind) :: f
  real (kind=Rkind), intent(in) :: x

    f = acosh(x)

END FUNCTION ACOSH_perso
FUNCTION TANH_perso(x) RESULT(f)
  USE ADLib_NumParameters_m
  IMPLICIT NONE
  real (kind=Rkind) :: f
  real (kind=Rkind), intent(in) :: x

  f = TANH(x)

END FUNCTION TANH_perso
FUNCTION ATANH_perso(x) RESULT(f)
  USE ADLib_NumParameters_m
  IMPLICIT NONE
  real (kind=Rkind) :: f
  real (kind=Rkind), intent(in) :: x

    f = atanh(x)

END FUNCTION ATANH_perso

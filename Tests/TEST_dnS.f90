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
  USE QDUtil_m
  USE QDUtil_Test_m
  USE ADdnSVM_m
  IMPLICIT NONE

    TYPE (dnS_t)                     :: dnX,dn2X,dnY,dnZ,Sana,Snum,dnXZ
    TYPE (dnS_t)                     :: dnA,dnB,dnC,dnDiff
    TYPE (dnS_t)                     :: dnMX,dnMXana
    TYPE (dnS_t)                     :: dnF,dnFX,dnFY

    TYPE (dnS_t), allocatable        :: Vec_dnS(:),Vres_dnS(:),Vana_dnS(:)
    TYPE (dnS_t), allocatable        :: Mat_dnS(:,:),MatA_dnS(:,:),MatB_dnS(:,:),Mana_dnS(:,:)

    TYPE (test_t)                    :: test_var
    logical                          :: val_test,res_test

    real (kind=Rkind)                :: x,y,z,r,th,err,maxdiff,maxdnS
    real (kind=Rkind), allocatable   :: JacNewOld(:,:),JacNewOld_ana(:,:)
    real (kind=Rkind), allocatable   :: FlatdnS(:),FlatdnS_ref(:)

    integer                          :: nderiv,nio_test
    real (kind=Rkind)                :: dnSerr_test = FIVE*ONETENTH**4
    real (kind=Rkind)                :: ZeroTresh   = ONETENTH**10

    integer                          :: i,j

    ! Add the functions in the CONTAINS to avoid a compiler bug (?, gfortran 15)
    !real (kind=Rkind), external  :: faplusx,faminusx,fatimex,faoverx
    !real (kind=Rkind), external  :: SQRT_perso,ABS_perso,EXP_perso,LOG_perso,LOG10_perso
    !real (kind=Rkind), external  :: SIN_perso,ASIN_perso,COS_perso,ACOS_perso,TAN_perso,ATAN_perso
    !real (kind=Rkind), external  :: SINH_perso,ASINH_perso,COSH_perso,ACOSH_perso,TANH_perso,ATANH_perso

    character (len=*), parameter :: name_sub='TEST_dnS'

  CALL read_arg()

  CALL Initialize_Test(test_var,test_name='dnS')

  nderiv = 3
  CALL Append_Test(test_var,'== TESTING dnS module with nderiv= ' // int_TO_char(nderiv))

  x       = 0.5_Rkind
  dnX     = variable(x  ,nVar=1,nderiv=nderiv,iVar=1) ! to set up the derivatives
  dn2X    = variable(x+x,nVar=1,nderiv=nderiv,iVar=1) ! to set up the derivatives

  IF (print_level > 0) THEN
    CALL Write_dnS(dnX,  string=test_var%test_log,info='dnX')
    CALL Write_dnS(dn2X, string=test_var%test_log,info='dn2X')
  END IF
  CALL Flush_Test(test_var)

  CALL Append_Test(test_var,'============================================')
  CALL Append_Test(test_var,'operators: == /= > >= < <=')
  CALL Append_Test(test_var,'============================================')

  res_test = (dnX == dnX)
  CALL Logical_Test(test_var,test1=res_test,                                    &
                   info='dnX == dnX:   T ? ' // TO_string(res_test) )
  res_test = (dnX == dn2X)
  CALL Logical_Test(test_var,test1=res_test,test2=.FALSE.,                      &
                   info='dnX == dn2X:  F ? ' // TO_string(res_test) )

  res_test = (dnX /= dnX)
  CALL Logical_Test(test_var,test1=res_test,test2=.FALSE.,                      &
                   info='dnX /= dnX:   F ? ' // TO_string(res_test) )
  res_test = (dnX /= dn2X)
  CALL Logical_Test(test_var,test1=res_test,                                    &
                   info='dnX /= dn2X:  T ? ' // TO_string(res_test) )

  res_test = (dnX > dnX)
  CALL Logical_Test(test_var,test1=res_test,test2=.FALSE.,                      &
                   info='dnX  > dnX:   F ? ' // TO_string(res_test) )
  res_test = (dnX  > dn2X)
  CALL Logical_Test(test_var,test1=res_test,test2=.FALSE.,                      &
                   info='dnX  > dn2X:  F ? ' // TO_string(res_test) )
  res_test = (dn2X > dnX)
  CALL Logical_Test(test_var,test1=res_test,                                    &
                   info='dn2X > dnX:   T ? ' // TO_string(res_test) )

  res_test = (dnX >= dnX)
  CALL Logical_Test(test_var,test1=res_test,                                    &
                   info='dnX  >= dnX:  T ? ' // TO_string(res_test) )
  res_test = (dnX  >= dn2X)
  CALL Logical_Test(test_var,test1=res_test,test2=.FALSE.,                      &
                   info='dnX  >= dn2X: F ? ' // TO_string(res_test) )
  res_test = (dn2X >= dnX)
  CALL Logical_Test(test_var,test1=res_test,                                    &
                   info='dn2X >= dnX:  T ? ' // TO_string(res_test) )

  res_test = (dnX  < dnX)
  CALL Logical_Test(test_var,test1=res_test,test2=.FALSE.,                      &
                   info='dnX  < dnX:   F ? ' // TO_string(res_test) )
  res_test = (dnX  < dn2X)
  CALL Logical_Test(test_var,test1=res_test,                                    &
                   info='dnX  < dn2X:  T ? ' // TO_string(res_test) )
  res_test = (dn2X < dnX)
  CALL Logical_Test(test_var,test1=res_test,test2=.FALSE.,                      &
                   info='dn2X < dnX:   F ? ' // TO_string(res_test) )

  res_test = (dnX  <= dnX)
  CALL Logical_Test(test_var,test1=res_test,                                    &
                   info='dnX  <= dnX:  T ? ' // TO_string(res_test) )
  res_test = (dnX  <= dn2X)
  CALL Logical_Test(test_var,test1=res_test,                                    &
                   info='dnX  <= dn2X: T ? ' // TO_string(res_test) )
  res_test = (dn2X <= dnX)
  CALL Logical_Test(test_var,test1=res_test,test2=.FALSE.,                      &
                   info='dn2X <= dnX:  F ? ' // TO_string(res_test) )

  CALL Flush_Test(test_var)

  CALL Append_Test(test_var,'============================================')
  CALL Append_Test(test_var,'operators: .EQ. .NE. .GT. .GE. .LT. .LE.')
  CALL Append_Test(test_var,'============================================')

  res_test = (dnX .EQ. dnX)
  CALL Logical_Test(test_var,test1=res_test,                                    &
                  info='dnX .EQ. dnX:   T ? ' // TO_string(res_test) )
  res_test = (dnX .NE. dn2X)
  CALL Logical_Test(test_var,test1=res_test,                                    &
                  info='dnX .NE.dn2X:   T ? ' // TO_string(res_test) )

  res_test = (dnX .GT. dnX)
  CALL Logical_Test(test_var,test1=res_test,test2=.FALSE.,                      &
                  info='dnX .GT. dnX:   F ? ' // TO_string(res_test) )
  res_test = (dnX .GE. dnX)
  CALL Logical_Test(test_var,test1=res_test,                                    &
                  info='dnX .GE. dnX:   T ? ' // TO_string(res_test) )

  res_test = (dnX .LT. dnX)
  CALL Logical_Test(test_var,test1=res_test,test2=.FALSE.,                      &
                  info='dnX .LT. dnX:   F ? ' // TO_string(res_test) )
  res_test = (dnX .LE. dnX)
  CALL Logical_Test(test_var,test1=res_test,                                    &
                  info='dnX .LE. dnX:   T ? ' // TO_string(res_test) )

  CALL Flush_Test(test_var)

  CALL Append_Test(test_var,'============================================')
  CALL Append_Test(test_var,'operators: + - * / **')
  CALL Append_Test(test_var,'============================================')

  Sana = 0.5_Rkind
  Sana = Sana + dnX
  Snum = AD_get_Num_dnS_FROM_f_x(x,faplusx,nderiv=nderiv)
  res_test = AD_Check_dnS_IS_ZERO(Sana-Snum,dnSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='a+dnX:         (Sana-Snum)==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,string=test_var%test_log,info='0.5 + dnX')

  Sana = dnX + 0.5_Rkind
  Snum = AD_get_Num_dnS_FROM_f_x(x,faplusx,nderiv=nderiv)
  res_test = AD_Check_dnS_IS_ZERO(Sana-Snum,dnSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='dnX+a:         (Sana-Snum)==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,string=test_var%test_log,info='dnX + 0.5')

  Sana = dnX + dnX
  Snum = AD_get_Num_dnS_FROM_f_x(x,fatimex,nderiv=nderiv)
  res_test = AD_Check_dnS_IS_ZERO(Sana-Snum,dnSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='dnX+dnX=2*dnX: (Sana-Snum)==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,string=test_var%test_log,info='dnX + dnX')

  Sana = +(0.5_Rkind - dnX)
  Snum = AD_get_Num_dnS_FROM_f_x(x,faminusx,nderiv=nderiv)
  res_test = AD_Check_dnS_IS_ZERO(Sana-Snum,dnSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='+(a-dnX):      (Sana-Snum)==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,string=test_var%test_log,info='+(0.5 - dnX)')

  Sana = -(dnX - 0.5_Rkind)
  Snum = AD_get_Num_dnS_FROM_f_x(x,faminusx,nderiv=nderiv)
  res_test = AD_Check_dnS_IS_ZERO(Sana-Snum,dnSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='-(dnX-a):      (Sana-Snum)==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,string=test_var%test_log,info='-(dnX - 0.5)')

  Sana = dnX - dnX
  res_test = AD_Check_dnS_IS_ZERO(Sana,dnSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='dnX-dnX                   ==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,string=test_var%test_log,info='dnX - dnX')

  Sana = 2._Rkind * dnX
  Snum = AD_get_Num_dnS_FROM_f_x(x,fatimex,nderiv=nderiv)
  res_test = AD_Check_dnS_IS_ZERO(Sana-Snum,dnSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='a*dnX:         (Sana-Snum)==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,string=test_var%test_log,info='2. * dnX')

  Sana =  dnX * 2._Rkind
  Snum = AD_get_Num_dnS_FROM_f_x(x,fatimex,nderiv=nderiv)
  res_test = AD_Check_dnS_IS_ZERO(Sana-Snum,dnSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='dnX*a:         (Sana-Snum)==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,string=test_var%test_log,info='dnX * 2.')

  Sana =  dnX * dnX - dnX**(2._Rkind)
  res_test = AD_Check_dnS_IS_ZERO(Sana,dnSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='dnX*dnX-dnX**2.           ==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,string=test_var%test_log,info='dnX * dnX - dnX**(2.)')

  Sana =  dnX / 0.5_Rkind -dnX*2._Rkind
  res_test = AD_Check_dnS_IS_ZERO(Sana,dnSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='dnX/0.5-dnX*2             ==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,string=test_var%test_log,info='dnX/0.5 -dnX*2.')

  Sana =  0.5_Rkind / dnX - 0.5_Rkind*dnX**(-1)
  res_test = AD_Check_dnS_IS_ZERO(Sana,dnSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='0.5/dnX-0.5.dnX*^-1       ==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,string=test_var%test_log,info='0.5 / dnX - 0.5*dnX**(-1)')

  Sana =  dnX / dnX - 1._Rkind
  res_test = AD_Check_dnS_IS_ZERO(Sana,dnSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='dnX/dnX-1.                ==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,string=test_var%test_log,info='dnX / dnX - 1.')

  Sana =  dnX**0.5_Rkind - sqrt(dnX)
  res_test = AD_Check_dnS_IS_ZERO(Sana,dnSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='dnX**0.5-sqrt(dnX)        ==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,string=test_var%test_log,info='dnX**0.5-sqrt(dnX)')

  Sana =  dnX**3 - dnX*dnX*dnX
  res_test = AD_Check_dnS_IS_ZERO(Sana,dnSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='dnX**3-dnX*dnX*dnX        ==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,string=test_var%test_log,info='dnX**3 - dnX*dnX*dnX')

  CALL Flush_Test(test_var)

  CALL Append_Test(test_var,'============================================')
  CALL Append_Test(test_var,'functions: sqrt, exp, log, ... sin, asin, ... acosh ...')
  CALL Append_Test(test_var,'============================================')

  Sana = sqrt(dnX)
  Snum = AD_get_Num_dnS_FROM_f_x(x,SQRT_perso,nderiv=nderiv)
  res_test = AD_Check_dnS_IS_ZERO(Sana-Snum,dnSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='sqrt: (Sana-Snum)==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,string=test_var%test_log,info='sqrt(dnX)')

  Sana = abs(dnX)
  Snum = AD_get_Num_dnS_FROM_f_x(x,ABS_perso,nderiv=nderiv)
  res_test = AD_Check_dnS_IS_ZERO(Sana-Snum,dnSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='abs:  (Sana-Snum)==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,string=test_var%test_log,info='abs(dnX)')

  Sana = exp(dnX)
  Snum = AD_get_Num_dnS_FROM_f_x(x,EXP_perso,nderiv=nderiv)
  res_test = AD_Check_dnS_IS_ZERO(Sana-Snum,dnSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='exp:  (Sana-Snum)==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,string=test_var%test_log,info='exp(dnX)')

  Sana = log(dnX)
  Snum = AD_get_Num_dnS_FROM_f_x(x,log_perso,nderiv=nderiv)
  res_test = AD_Check_dnS_IS_ZERO(Sana-Snum,dnSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='log:  (Sana-Snum)==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,string=test_var%test_log,info='log(dnX)')

  Sana = log10(dnX)
  Snum = AD_get_Num_dnS_FROM_f_x(x,log10_perso,nderiv=nderiv)
  res_test = AD_Check_dnS_IS_ZERO(Sana-Snum,dnSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='log10:(Sana-Snum)==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,string=test_var%test_log,info='log10(dnX)')

  Sana = sin(dnX)
  Snum = AD_get_Num_dnS_FROM_f_x(x,sin_perso,nderiv=nderiv)
  res_test = AD_Check_dnS_IS_ZERO(Sana-Snum,dnSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='sin:  (Sana-Snum)==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,string=test_var%test_log,info='sin(dnX)')

  Sana = asin(dnX)
  Snum = AD_get_Num_dnS_FROM_f_x(x,asin_perso,nderiv=nderiv)
  res_test = AD_Check_dnS_IS_ZERO(Sana-Snum,dnSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='asin: (Sana-Snum)==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,string=test_var%test_log,info='asin(dnX)')

  Sana = cos(dnX)
  Snum = AD_get_Num_dnS_FROM_f_x(x,cos_perso,nderiv=nderiv)
  res_test = AD_Check_dnS_IS_ZERO(Sana-Snum,dnSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='cos:  (Sana-Snum)==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,string=test_var%test_log,info='cos(dnX)')

  Sana = acos(dnX)
  Snum = AD_get_Num_dnS_FROM_f_x(x,acos_perso,nderiv=nderiv)
  res_test = AD_Check_dnS_IS_ZERO(Sana-Snum,dnSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='acos: (Sana-Snum)==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,string=test_var%test_log,info='acos(dnX)')

  Sana = tan(dnX)
  Snum = AD_get_Num_dnS_FROM_f_x(x,tan_perso,nderiv=nderiv)
  res_test = AD_Check_dnS_IS_ZERO(Sana-Snum,dnSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='tan:  (Sana-Snum)==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,string=test_var%test_log,info='tan(dnX)')

  Sana = atan(dnX)
  Snum = AD_get_Num_dnS_FROM_f_x(x,atan_perso,nderiv=nderiv)
  res_test = AD_Check_dnS_IS_ZERO(Sana-Snum,dnSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='atan: (Sana-Snum)==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,string=test_var%test_log,info='atan(dnX)')

  Sana = sinh(dnX)
  Snum = AD_get_Num_dnS_FROM_f_x(x,sinh_perso,nderiv=nderiv)
  res_test = AD_Check_dnS_IS_ZERO(Sana-Snum,dnSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='sinh: (Sana-Snum)==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,string=test_var%test_log,info='sinh(dnX)')

  Sana = asinh(dnX)
  Snum = AD_get_Num_dnS_FROM_f_x(x,asinh_perso,nderiv=nderiv)
  res_test = AD_Check_dnS_IS_ZERO(Sana-Snum,dnSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='asinh:(Sana-Snum)==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,string=test_var%test_log,info='asinh(dnX)')

  Sana = cosh(dnX)
  Snum = AD_get_Num_dnS_FROM_f_x(x,cosh_perso,nderiv=nderiv)
  res_test = AD_Check_dnS_IS_ZERO(Sana-Snum,dnSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='cosh: (Sana-Snum)==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,string=test_var%test_log,info='cosh(dnX)')

  dnY = Variable(FOUR*x  ,nVar=1,nderiv=nderiv,iVar=1) ! to set up the derivatives
  Sana = acosh(dnY)
  Snum = AD_get_Num_dnS_FROM_f_x(FOUR*x,acosh_perso,nderiv=nderiv)
  res_test = AD_Check_dnS_IS_ZERO(Sana-Snum,dnSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='acosh:(Sana-Snum)==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,string=test_var%test_log,info='acosh(dnY)')

  Sana = tanh(dnX)
  Snum = AD_get_Num_dnS_FROM_f_x(x,tanh_perso,nderiv=nderiv)
  res_test = AD_Check_dnS_IS_ZERO(Sana-Snum,dnSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='tanh: (Sana-Snum)==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,string=test_var%test_log,info='tanh(dnX)')

  Sana = atanh(dnX)
  Snum = AD_get_Num_dnS_FROM_f_x(x,atanh_perso,nderiv=nderiv)
  res_test = AD_Check_dnS_IS_ZERO(Sana-Snum,dnSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='atanh:(Sana-Snum)==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,string=test_var%test_log,info='atanh(dnX)')

  CALL Flush_Test(test_var)


  CALL Append_Test(test_var,'============================================')
  CALL Append_Test(test_var,'new tests: **, composition')
  CALL Append_Test(test_var,'============================================')

  Sana =  dnX**0 - ONE
  res_test = AD_Check_dnS_IS_ZERO(Sana,dnSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='dnX**0-ONE                ==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,string=test_var%test_log,info='dnX**0-ONE')

  Sana =  dnX**1 - dnX
  res_test = AD_Check_dnS_IS_ZERO(Sana,dnSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='dnX**1-dnX                ==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,string=test_var%test_log,info='dnX**1-dnX')

  Sana =  dnX**2 - dnX*dnX
  res_test = AD_Check_dnS_IS_ZERO(Sana,dnSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='dnX**2-dnX*dnX            ==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,string=test_var%test_log,info='dnX**2-dnX*dnX')

  Sana =  dnX**3 - dnX*dnX*dnX
  res_test = AD_Check_dnS_IS_ZERO(Sana,dnSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='dnX**3-dnX*dnX*dnX        ==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,string=test_var%test_log,info='dnX**3-dnX*dnX*dnX')

  Sana =  sqrt(dnX**2) - dnX
  res_test = AD_Check_dnS_IS_ZERO(Sana,dnSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='sqrt(dnX**2) - dnX        ==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,string=test_var%test_log,info='sqrt(dnX**2) - dnX')

  CALL Flush_Test(test_var)

  CALL Append_Test(test_var,'============================================')
  CALL Append_Test(test_var,'new tests: 3D, nderiv=2')
  CALL Append_Test(test_var,'============================================')
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
  CALL Logical_Test(test_var,test1=res_test,info='dnX*dnZ -dnS_Result       ==0?')
  IF (print_level > 0) THEN
    CALL Write_dnS(dnX,string=test_var%test_log,info='dnX')
    CALL Write_dnS(dnZ,string=test_var%test_log,info='dnZ')
    CALL Write_dnS(Sana,string=test_var%test_log,info='dnX*dnZ (3D)')
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
  CALL Logical_Test(test_var,test1=res_test,info='Vec_dnS(1)*Vec_dnS(3)-dnS_Result==0?')
  IF (print_level > 0) THEN
    CALL Write_dnS(Vec_dnS(1),string=test_var%test_log,info='Vec_dnS(1)')
    CALL Write_dnS(Vec_dnS(3),string=test_var%test_log,info='Vec_dnS(3)')
    CALL Write_dnS(Sana,string=test_var%test_log,info='dnX*dnZ (3D)')
  END IF
  CALL Flush_Test(test_var)

  Vec_dnS = Variable([x,ZERO],nVar=3,iVar=[1,2],nderiv=nderiv)
  dnZ =     Variable(z,nVar=3,iVar=3,nderiv=nderiv)

  Sana    = Vec_dnS(1)*dnZ ! It is equivalent to a 3D function f(x,y,z) = x*z
  CALL set_dnS(dnXZ,d0=     x*z,                             &
                    d1=        [z,   ZERO,x],                &
                    d2=reshape([ZERO,ZERO,ONE,               &
                                ZERO,ZERO,ZERO,              &
                                ONE, ZERO,ZERO],shape=[3,3]))

  res_test = AD_Check_dnS_IS_ZERO(Sana-dnXZ,dnSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='Vec_dnS(1)*Vec_dnS(3)-dnS_Result==0?')
  IF (print_level > 0) THEN
    CALL Write_dnS(Vec_dnS(1),string=test_var%test_log,info='Vec_dnS(1)')
    CALL Write_dnS(dnZ,string=test_var%test_log,info='dnZ')
    CALL Write_dnS(Sana,string=test_var%test_log,info='dnX*dnZ (3D)')
  END IF


  Vec_dnS = Variable([x,ZERO],nVar=3,nderiv=nderiv)
  dnZ =     Variable(z,nVar=3,iVar=3,nderiv=nderiv)

  Sana    = Vec_dnS(1)*dnZ ! It is equivalent to a 3D function f(x,y,z) = x*z
  CALL set_dnS(dnXZ,d0=     x*z,                             &
                    d1=        [z,   ZERO,x],                &
                    d2=reshape([ZERO,ZERO,ONE,               &
                                ZERO,ZERO,ZERO,              &
                                ONE, ZERO,ZERO],shape=[3,3]))

  res_test = AD_Check_dnS_IS_ZERO(Sana-dnXZ,dnSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='Vec_dnS(1)*Vec_dnS(3)-dnS_Result==0?')
  IF (print_level > 0) THEN
    CALL Write_dnS(Vec_dnS(1),string=test_var%test_log,info='Vec_dnS(1)')
    CALL Write_dnS(dnZ,string=test_var%test_log,info='dnZ')
    CALL Write_dnS(Sana,string=test_var%test_log,info='dnX*dnZ (3D)')
  END IF
  CALL Flush_Test(test_var)

  CALL Append_Test(test_var,'============================================')
  CALL Append_Test(test_var,'Jacobian(inew,iold): 2D, nderiv=2')
  CALL Append_Test(test_var,'Polar transformation')
  CALL Append_Test(test_var,'============================================')
  CALL Flush_Test(test_var)

  r  = TWO
  th = Pi/3
  Vec_dnS = Variable([r,th], nderiv=2 ) ! Vec(1) : r, Vec(2) : th

  JacNewOld     = get_Jacobian( Vec_dnS(1)*[cos(Vec_dnS(2)),sin(Vec_dnS(2))] ) ! Jacobian of a polar transformation
  JacNewOld_ana = reshape([cos(th),sin(th),-r*sin(th),r*cos(th)],shape(JacNewOld))

  res_test = ( maxval(abs(JacNewOld-JacNewOld_ana)) < dnSerr_test )
  CALL Logical_Test(test_var,test1=res_test,info='Jacobian-Jacobian_ana     ==0?')

  IF (print_level > 0) THEN
    CALL Append_Test(test_var,'Jac(inew,iold)=[ dQinew/dQiold ]:',Print_res=.FALSE.)
    CALL Write_Mat(JacNewOld,string=test_var%test_log,nbcol=5)
    CALL Append_Test(test_var,'analytical: [dx/dr, dx/dth]:   [0.5,     -1.732...]',Print_res=.FALSE.)
    CALL Append_Test(test_var,'analytical: [dy/dr, dy/dth]:   [0.866..,  1.      ]',Print_res=.FALSE.)
  END IF
  CALL Flush_Test(test_var)

  CALL Append_Test(test_var,'============================================')
  CALL Append_Test(test_var,'Matmul, transpose: 2D, nderiv=2')
  CALL Append_Test(test_var,'============================================')
  CALL Flush_Test(test_var)

  x = ONE
  y = TWO
  dnX = Variable(x,nVar=2,iVar=1,nderiv=2)
  dnY = Variable(y,nVar=2,iVar=2,nderiv=2)

  Vec_dnS = [dnX,dnY]



  allocate(Mat_dnS(0:1,2:3))
  Mat_dnS(:,2) = [dnX**2,dnX*dnY]
  Mat_dnS(:,3) = [dnX*dnY,dnY**2]

  IF (print_level > 0) THEN
    CALL Write_dnS(Vec_dnS(1),info='Vec_dnS(1)')
    CALL Write_dnS(Vec_dnS(2),info='Vec_dnS(2)')
    DO i=0,1
      DO j=2,3
        write(out_unit,*) 'Mat_dnS',i,j
        CALL Write_dnS(Mat_dnS(i,j))
      END DO
    END DO
  END IF

  Vres_dnS = matmul(Mat_dnS,Vec_dnS)
  Vana_dnS = [dnX**3+dnX*dnY**2,dnX**2*dnY+dnY**3]

  IF (print_level > 0) THEN
    CALL Write_dnS(Vres_dnS(1),info='Vres_dnS(1)')
    CALL Write_dnS(Vres_dnS(2),info='Vres_dnS(2)')

    CALL Write_dnS(Vana_dnS(1),info='Vana_dnS(1)')
    CALL Write_dnS(Vana_dnS(2),info='Vana_dnS(2)')
  END IF

  res_test = all(AD_Check_dnS_IS_ZERO(Vres_dnS-Vana_dnS,dnSerr_test))
  CALL Logical_Test(test_var,test1=res_test,info='matmul     ==0?')
  CALL Flush_Test(test_var)

  dnA = ZERO
  dnB = ONE
  allocate(MatA_dnS(2:3,0:2))
  MatA_dnS(2,:) = [dnX,dnA,dnX]
  MatA_dnS(3,:) = [dnY,dnB,dnY]

  allocate(MatB_dnS(-1:1,0:3))
  MatB_dnS(:,0) = [dnA,dnA,dnA]
  MatB_dnS(:,1) = [dnA,dnA,dnA]
  MatB_dnS(:,2) = [dnX,dnA,dnX]
  MatB_dnS(:,3) = [dnY,dnB,dnY]


  Mat_dnS = matmul(MatA_dnS,MatB_dnS)

  allocate(Mana_dnS(2,4))
  Mana_dnS(1,:) = [dnA,dnA,2*dnX**2,2*dnX*dnY]
  Mana_dnS(2,:) = [dnA,dnA,2*dnX*dnY,1+2*dnY**2]

  res_test = all(AD_Check_dnS_IS_ZERO(Mat_dnS-Mana_dnS,dnSerr_test))

  CALL Logical_Test(test_var,test1=res_test,info='matmul     ==0?')
  CALL Flush_Test(test_var)


  !transpose
  Mat_dnS = transpose(MatA_dnS)

  Mana_dnS = reshape([dnX,dnA,dnX , dnY,dnB,dnY],shape=[3,2])

  res_test = all(AD_Check_dnS_IS_ZERO(Vres_dnS-Vana_dnS,dnSerr_test))

  CALL Logical_Test(test_var,test1=res_test,info='matmul     ==0?')
  CALL Flush_Test(test_var)

  CALL Append_Test(test_var,'============================================')
  CALL Append_Test(test_var,'new tests : Vec_OF_dnS(1:3), 2D, nderiv=2')
  CALL Append_Test(test_var,'============================================')

  nderiv = 2
  x=0.5_Rkind
  y=1.0_Rkind
  dnX     = Variable(x  ,nVar=2,nderiv=nderiv,iVar=1) ! to set up the derivatives
  dnY     = Variable(y  ,nVar=2,nderiv=nderiv,iVar=2) ! to set up the derivatives

  Vec_dnS = [dnX,dnY,dnX+dnY]
  Sana = dot_product(Vec_dnS,Vec_dnS) ! 2*(dnX**2+dnY**2+dnX*dnY)
  dnXZ = TWO*(dnX**2+dnY**2+dnX*dnY)
  res_test = AD_Check_dnS_IS_ZERO(Sana-dnXZ,dnSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='dot_product - dnS_Result  ==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,string=test_var%test_log,info='dot_product (2D)')

  Vec_dnS(:) = [-dnX,-dnY,dnX+dnY]
  Sana = sum(Vec_dnS) ! ZERO
  res_test = AD_Check_dnS_IS_ZERO(Sana,dnSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='sum                       ==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,string=test_var%test_log,info='sum (2D)')


  CALL dealloc_dnS(dnZ)
  dnZ = ONE
  Vec_dnS(:) = [dnX**2,ONE/dnX,ONE/dnX]
  Sana = product(Vec_dnS) ! ONE
  res_test = AD_Check_dnS_IS_ZERO(Sana-dnZ,dnSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='product - 1               ==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,string=test_var%test_log,info='product - 1 (2D)')
  CALL Flush_Test(test_var)

  CALL Append_Test(test_var,'============================================')
  CALL Append_Test(test_var,'new tests : two-argument fuction, 2D, nderiv=2')
  CALL Append_Test(test_var,'============================================')

  nderiv = 2
  x = -0.5_Rkind
  y = -1.0_Rkind
  dnX     = Variable(x  ,nVar=2,nderiv=nderiv,iVar=1) ! to set up the derivatives
  dnY     = Variable(y  ,nVar=2,nderiv=nderiv,iVar=2) ! to set up the derivatives
  Sana = atan2(dnY,dnX)
  Snum = atan(dnY/dnX)-pi
  res_test = AD_Check_dnS_IS_ZERO(Sana-Snum,dnSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='atan2 - atan              ==0?')
  IF (print_level > 0) CALL Write_dnS(Sana,string=test_var%test_log,info='atan2 (2D)')
  IF (print_level > 0) CALL Write_dnS(Snum,string=test_var%test_log,info='atan (2D)')
  CALL Flush_Test(test_var)

  CALL Append_Test(test_var,'============================================')
  CALL Append_Test(test_var,'new tests : init const, 2D, nderiv=2')
  CALL Append_Test(test_var,'============================================')

  nderiv = 2
  x = 0.5_Rkind
  y = 1.0_Rkind
  dnX     = Variable(x  ,nVar=2,nderiv=nderiv,iVar=1) ! to set up the derivatives

  CALL Append_Test(test_var,'------------------------------------------------------',Print_res=.FALSE.)
  CALL Append_Test(test_var,'------------------------------------------------------',Print_res=.FALSE.)
  CALL Append_Test(test_var,'------------------------------------------------------',Print_res=.FALSE.)
  CALL dealloc_dnS(Sana)
  CALL dealloc_dnS(Snum)
  Sana    = x ! here, Sana%nderiv = -1
  IF (print_level > 0) CALL Write_dnS(Sana,string=test_var%test_log,info='x, constant')
  Sana    = Sana + dnX ! here, Sana%nderiv = dnX%nderiv
  IF (print_level > 0) CALL Write_dnS(Sana,string=test_var%test_log,info='(x+dnX)')
  Snum    = dnX + x    ! here, Snum%nderiv = dnX%nderiv
  IF (print_level > 0) CALL Write_dnS(Snum,string=test_var%test_log,info='(dnX+x)')
  res_test = AD_Check_dnS_IS_ZERO(Sana-Snum,dnSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='(x+dnX) - (dnX+x)         ==0?')

  CALL Append_Test(test_var,'------------------------------------------------------',Print_res=.FALSE.)
  CALL Append_Test(test_var,'------------------------------------------------------',Print_res=.FALSE.)
  CALL Append_Test(test_var,'------------------------------------------------------',Print_res=.FALSE.)
  CALL dealloc_dnS(Sana)
  CALL dealloc_dnS(Snum)
  Sana    = x ! here, Sana%nderiv = -1
  IF (print_level > 0) CALL Write_dnS(Sana,string=test_var%test_log,info='x, constant')
  Sana    = Sana * dnX ! here, Sana%nderiv = dnX%nderiv
  IF (print_level > 0) CALL Write_dnS(Sana,string=test_var%test_log,info='(x*dnX)')
  Snum    = dnX * x    ! here, Snum%nderiv = dnX%nderiv
  IF (print_level > 0) CALL Write_dnS(Snum,string=test_var%test_log,info='(dnX*x)')
  res_test = AD_Check_dnS_IS_ZERO(Sana-Snum,dnSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='(x*dnX) - (dnX*x)         ==0?')

  CALL Append_Test(test_var,'------------------------------------------------------',Print_res=.FALSE.)
  CALL Append_Test(test_var,'------------------------------------------------------',Print_res=.FALSE.)
  CALL Append_Test(test_var,'------------------------------------------------------',Print_res=.FALSE.)
  CALL dealloc_dnS(Sana)
  CALL dealloc_dnS(Snum)
  Sana    = x ! here, Sana%nderiv = -1
  IF (print_level > 0) CALL Write_dnS(Sana,string=test_var%test_log,info='x, constant')
  Sana    = Sana / dnX ! here, Sana%nderiv = dnX%nderiv
  IF (print_level > 0)CALL Write_dnS(Sana,string=test_var%test_log,info='(x/dnX)')
  Snum    = dnX**(-1) * x    ! here, Snum%nderiv = dnX%nderiv
  IF (print_level > 0)CALL Write_dnS(Snum,string=test_var%test_log,info='(dnX**(-1)*x)')
  res_test = AD_Check_dnS_IS_ZERO(Sana-Snum,dnSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='(x/dnX) - (dnX**(-1)*x)   ==0?')

  CALL Append_Test(test_var,'------------------------------------------------------',Print_res=.FALSE.)
  CALL Append_Test(test_var,'------------------------------------------------------',Print_res=.FALSE.)
  CALL Append_Test(test_var,'------------------------------------------------------',Print_res=.FALSE.)
  CALL dealloc_dnS(Sana)
  CALL dealloc_dnS(Snum)
  Sana    = x ! here, Sana%nderiv = -1
  IF (print_level > 0) CALL Write_dnS(Sana,string=test_var%test_log,info='x, constant')
  Sana    = sin(Sana) ! here, Sana%nderiv = -1
  IF (print_level > 0) CALL Write_dnS(Sana,string=test_var%test_log,info='(sin(x)')
  Snum    = dnX ; Snum = ZERO
  Snum    = sin(x)    ! here, Snum%nderiv = dnX%nderiv
  IF (print_level > 0) CALL Write_dnS(Snum,string=test_var%test_log,info='(sin(x))')
  res_test = AD_Check_dnS_IS_ZERO(Sana-Snum,dnSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='(sin(x)) - (sin(x))       ==0?')

  CALL Flush_Test(test_var)

  CALL Append_Test(test_var,'============================================')
  CALL Append_Test(test_var,'new tests : flatten dnS and set_d0')
  CALL Append_Test(test_var,'============================================')
  CALL Flush_Test(test_var)

  nderiv = 3
  dnX         = Variable(TWO,nVar=2,nderiv=nderiv,iVar=1) ! to set up the derivatives
  FlatdnS     = get_Flatten(dnX)
  FlatdnS_ref = [TWO,  ONE,ZERO,  ZERO,ZERO,ZERO,ZERO,  ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO]
  res_test = all(abs(FlatdnS-FlatdnS_ref) < ZeroTresh)
  CALL Logical_Test(test_var,test1=res_test,info='FlatdnS-FlatdnS_ref (nderiv=3)  ==0?')
  write(out_unit,*) 'FlatdnS',FlatdnS
  CALL Flush_Test(test_var)

  dnX         = Variable(TWO,nVar=2,nderiv=nderiv,iVar=1) ! to set up the derivatives
  CALL set_d0S(dnX,ZERO)
  FlatdnS     = get_Flatten(dnX)
  FlatdnS_ref = [ZERO,  ONE,ZERO,  ZERO,ZERO,ZERO,ZERO,  ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO]
  res_test = all(abs(FlatdnS-FlatdnS_ref) < ZeroTresh)
  CALL Logical_Test(test_var,test1=res_test,info='FlatdnS-FlatdnS_ref + set_d0 (nderiv=3)  ==0?')
  write(out_unit,*) 'FlatdnS',FlatdnS
  CALL Flush_Test(test_var)

  dnX         = Variable(TWO,nVar=2,nderiv=nderiv,iVar=1) ! to set up the derivatives
  FlatdnS     = get_Flatten(dnX,all_der=.TRUE.)
  FlatdnS_ref = [TWO,  ONE,ZERO,  ZERO,ZERO,ZERO,ZERO,  ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO]
  res_test    = all(abs(FlatdnS-FlatdnS_ref) < ZeroTresh)
  CALL Logical_Test(test_var,test1=res_test,info='FlatdnS-FlatdnS_ref (all)       ==0?')
  write(out_unit,*) 'FlatdnS',FlatdnS
  CALL Flush_Test(test_var)

  FlatdnS     = get_Flatten(dnX,i_der=0)
  FlatdnS_ref = [TWO]
  res_test    = all(abs(FlatdnS-FlatdnS_ref) < ZeroTresh)
  CALL Logical_Test(test_var,test1=res_test,info='FlatdnS-FlatdnS_ref (%d0)        ==0?')
  write(out_unit,*) 'FlatdnS',FlatdnS
  CALL Flush_Test(test_var)


  FlatdnS     = get_Flatten(dnX,i_der=1)
  FlatdnS_ref = [ONE,ZERO]
  res_test    = all(abs(FlatdnS-FlatdnS_ref) < ZeroTresh)
  CALL Logical_Test(test_var,test1=res_test,info='FlatdnS-FlatdnS_ref (%d1)        ==0?')
  write(out_unit,*) 'FlatdnS',FlatdnS
  CALL Flush_Test(test_var)

  FlatdnS     = get_Flatten(dnX,i_der=2)
  FlatdnS_ref = [ZERO,ZERO,ZERO,ZERO]
  res_test    = all(abs(FlatdnS-FlatdnS_ref) < ZeroTresh)
  CALL Logical_Test(test_var,test1=res_test,info='FlatdnS-FlatdnS_ref (%d2)        ==0?')
  write(out_unit,*) 'FlatdnS',FlatdnS
  CALL Flush_Test(test_var)

  FlatdnS     = get_Flatten(dnX,i_der=3)
  FlatdnS_ref = [ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO]
  res_test    = all(abs(FlatdnS-FlatdnS_ref) < ZeroTresh)
  CALL Logical_Test(test_var,test1=res_test,info='FlatdnS-FlatdnS_ref (%d3)        ==0?')
  write(out_unit,*) 'FlatdnS',FlatdnS
  CALL Flush_Test(test_var)

  nderiv = 2
  dnX         = Variable(TWO,nVar=2,nderiv=nderiv,iVar=1) ! to set up the derivatives
  FlatdnS     = get_Flatten(dnX)
  FlatdnS_ref = [TWO,  ONE,ZERO,  ZERO,ZERO,ZERO,ZERO]
  res_test = all(abs(FlatdnS-FlatdnS_ref) < ZeroTresh)
  CALL Logical_Test(test_var,test1=res_test,info='FlatdnS-FlatdnS_ref (nderiv=2)   ==0?')
  write(out_unit,*) 'FlatdnS',FlatdnS
  CALL Flush_Test(test_var)

  CALL dealloc_dnS(dnX)
  FlatdnS     = get_Flatten(dnX)
  res_test    = (size(FlatdnS) == 0)
  CALL Logical_Test(test_var,test1=res_test,info='size(FlatdnS)   ==0?')
  write(out_unit,*) 'size FlatdnS',size(FlatdnS)
  CALL Flush_Test(test_var)

  CALL dealloc_dnS(dnX)
  CALL Append_Test(test_var,'------------------------------------------------------',Print_res=.FALSE.)
  CALL Append_Test(test_var,'------------------------------------------------------',Print_res=.FALSE.)
  CALL Append_Test(test_var,'------------------------------------------------------',Print_res=.FALSE.)

  CALL Append_Test(test_var,'============================================')
  CALL Append_Test(test_var,'new tests : deriv of dnS')
  CALL Append_Test(test_var,'============================================')
  CALL Flush_Test(test_var)

  nderiv = 3
  x=ONE
  y=TWO
  z=THREE
  Vec_dnS     = Variable([x,y,z],nderiv=nderiv)
  dnA         = cos(Vec_dnS(1))+sin(Vec_dnS(2))+Vec_dnS(3)**2
  dnB         = deriv(dnA,ider=1)
  CALL Write_dnS(dnB,string=test_var%test_log,info="cos'(x)")
  CALL set_dnS(dnC,                                        &
               d0=     -sin(x),                            &
               d1=        [-cos(x),   ZERO,ZERO],          &
               d2=reshape([sin(x),ZERO,ZERO,               &
                           ZERO,ZERO,ZERO,                 &
                           ZERO, ZERO,ZERO],shape=[3,3]))

  res_test = (dnB == dnC)
  CALL Logical_Test(test_var,test1=res_test,info='deriv of dnS (nderiv=3)  ==0?')
  CALL Flush_Test(test_var)

  dnA = deriv(dnB,ider=1)
  CALL Write_dnS(dnA,string=test_var%test_log,info="cos''(x)")
  CALL set_dnS(dnC,d0=-cos(x),d1=[sin(x),   ZERO,ZERO])
  res_test = (dnA == dnC)
  CALL Logical_Test(test_var,test1=res_test,info='deriv^2 of dnS (nderiv=3)  ==0?')
  CALL Flush_Test(test_var)



  Vec_dnS     = Variable([x,y,z],nderiv=nderiv)
  dnA         = cos(Vec_dnS(1))+sin(Vec_dnS(2))+Vec_dnS(3)**2
  Vres_dnS    = grad(dnA)
  if (allocated(Vana_dnS)) deallocate(Vana_dnS)
  allocate(Vana_dnS(size(Vres_dnS)))

  CALL Write_dnS(Vres_dnS(1),string=test_var%test_log,info="cos'(x)")
  CALL set_dnS(Vana_dnS(1),                                &
               d0=     -sin(x),                            &
               d1=        [-cos(x),   ZERO,ZERO],          &
               d2=reshape([sin(x),ZERO,ZERO,               &
                           ZERO,ZERO,ZERO,                 &
                           ZERO, ZERO,ZERO],shape=[3,3]))
  
  CALL Write_dnS(Vres_dnS(2),string=test_var%test_log,info="sin'(y)")
  CALL set_dnS(Vana_dnS(2),                                &
               d0=     cos(y),                             &
               d1=        [ZERO,-sin(y),ZERO],             &
               d2=reshape([ZERO,ZERO,ZERO,                 &
                           ZERO,-cos(y),ZERO,                 &
                           ZERO,ZERO,ZERO],shape=[3,3]))

  CALL Write_dnS(Vres_dnS(3),string=test_var%test_log,info="z'")
  CALL set_dnS(Vana_dnS(3),                                &
               d0=     TWO*z,                              &
               d1=        [ZERO,ZERO,TWO],                 &
               d2=reshape([ZERO,ZERO,ZERO,                 &
                           ZERO,ZERO,ZERO,                 &
                           ZERO,ZERO,ZERO],shape=[3,3]))
  
  res_test = (all(Vres_dnS == Vres_dnS))
  CALL Logical_Test(test_var,test1=res_test,info='grad of dnS (nderiv=3)  ==0?')
  CALL Flush_Test(test_var)
  CALL Append_Test(test_var,'------------------------------------------------------',Print_res=.FALSE.)
  CALL Append_Test(test_var,'------------------------------------------------------',Print_res=.FALSE.)
  CALL Append_Test(test_var,'------------------------------------------------------',Print_res=.FALSE.)

  CALL Append_Test(test_var,'============================================')
  CALL Append_Test(test_var,'new tests : mod ... of dnS')
  CALL Append_Test(test_var,'============================================')
  CALL Flush_Test(test_var)

  nderiv = 3
  x=pi*(TEN+HALF)

  dnX         = Variable(x,nderiv=nderiv)
  dnMX        = mod(dnX,TWO*pi)
  dnMXana     = Variable(mod(x,TWO*pi),nderiv=nderiv)
  CALL Append_Test(test_var,'mod(10.5*pi,2pi)=' // to_string(mod(x,TWO*pi)),Print_res=.FALSE.)
  res_test = AD_Check_dnS_IS_ZERO(dnMX - dnMXana,dnSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='mod of dnS (nderiv=3)  ==0?')
  CALL Flush_Test(test_var)

  dnX         = Variable(-x,nderiv=nderiv)
  dnMX        = mod(dnX,TWO*pi)
  dnMXana     = Variable(mod(-x,TWO*pi),nderiv=nderiv)
  CALL Append_Test(test_var,'mod(-10.5*pi,2pi)=' // to_string(mod(-x,TWO*pi)),Print_res=.FALSE.)
  res_test = AD_Check_dnS_IS_ZERO(dnMX - dnMXana,dnSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='mod of dnS (nderiv=3)  ==0?')
  CALL Flush_Test(test_var)

  dnX         = Variable(x,nderiv=nderiv)
  dnMX        = modulo(dnX,TWO*pi)
  dnMXana     = Variable(modulo(x,TWO*pi),nderiv=nderiv)
  CALL Append_Test(test_var,'modulo(10.5*pi,2pi)=' // to_string(modulo(x,TWO*pi)),Print_res=.FALSE.)
  res_test = AD_Check_dnS_IS_ZERO(dnMX - dnMXana,dnSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='modulo of dnS (nderiv=3)  ==0?')

  dnX         = Variable(-x,nderiv=nderiv)
  dnMX        = modulo(dnX,TWO*pi)
  dnMXana     = Variable(modulo(-x,TWO*pi),nderiv=nderiv)
  CALL Append_Test(test_var,'modulo(-10.5*pi,2pi)=' // to_string(modulo(-x,TWO*pi)),Print_res=.FALSE.)
  res_test = AD_Check_dnS_IS_ZERO(dnMX - dnMXana,dnSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='modulo of dnS (nderiv=3)  ==0?')

  CALL Flush_Test(test_var)
  CALL Append_Test(test_var,'------------------------------------------------------',Print_res=.FALSE.)
  CALL Append_Test(test_var,'------------------------------------------------------',Print_res=.FALSE.)
  CALL Append_Test(test_var,'------------------------------------------------------',Print_res=.FALSE.)

  CALL Append_Test(test_var,'============================================')
  CALL Append_Test(test_var,'new tests : reduced derivatives')
  CALL Append_Test(test_var,'============================================')
  CALL Flush_Test(test_var)

  nderiv = 2
  x=HALF

  dnX         = Variable(x,nderiv=nderiv) ! 1D
  CALL Write_dnS(dnX,string=test_var%test_log,info="dnS 1D")

  dnY =  FROM_dnSReducedDer(nVar=3,list=[2],S=dnX)
  CALL Write_dnS(dnY,string=test_var%test_log,info="dnS 1D->3D")

  dnZ = TO_dnSReducedDer(dnY,list=[2])
  CALL Write_dnS(dnZ,string=test_var%test_log,info="dnS 3D->1D")

  res_test = AD_Check_dnS_IS_ZERO(dnZ - dnX,dnSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='Reduced Derivative ok ?')

  CALL Flush_Test(test_var)

  nderiv = 0
  x=HALF

  dnX         = Variable(x,nderiv=nderiv) ! 1D
  CALL Write_dnS(dnX,string=test_var%test_log,info="dnS 1D")

  dnY =  FROM_dnSReducedDer(nVar=3,list=[2],S=dnX)
  CALL Write_dnS(dnY,string=test_var%test_log,info="dnS 1D->3D")

  dnZ = TO_dnSReducedDer(dnY,list=[2])
  CALL Write_dnS(dnZ,string=test_var%test_log,info="dnS 3D->1D")

  res_test = AD_Check_dnS_IS_ZERO(dnZ - dnX,dnSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='Reduced Derivative ok ?')

  CALL Flush_Test(test_var)

  CALL Flush_Test(test_var)
  CALL Append_Test(test_var,'------------------------------------------------------',Print_res=.FALSE.)
  CALL Append_Test(test_var,'------------------------------------------------------',Print_res=.FALSE.)
  CALL Append_Test(test_var,'------------------------------------------------------',Print_res=.FALSE.)


  CALL Append_Test(test_var,'============================================')
  CALL Append_Test(test_var,'new tests : function combination')
  CALL Append_Test(test_var,'============================================')
  CALL Flush_Test(test_var)

  nderiv = 2
  x=ONE

  dnX         = Variable(x,nderiv=nderiv) ! 1D for the function
  dnF         = cos(dnX)

  dnY         = Variable(x,iVar=2,nVar=4,nderiv=nderiv)

  dnFY        = dnF_OF_dnS(dnF,dnX)
  dnFX        = cos(dnY)
  res_test = AD_Check_dnS_IS_ZERO(dnFX - dnFX,dnSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='function combination  ==0?')

  nderiv = 0
  x=ONE

  dnX         = Variable(x,nderiv=nderiv) ! 1D for the function
  dnF         = cos(dnX)

  dnY         = Variable(x,iVar=2,nVar=4,nderiv=nderiv)

  dnFY        = dnF_OF_dnS(dnF,dnX)
  dnFX        = cos(dnY)
  res_test = AD_Check_dnS_IS_ZERO(dnFX - dnFX,dnSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='function combination  ==0?')

  CALL Flush_Test(test_var)

  CALL Flush_Test(test_var)
  CALL Append_Test(test_var,'------------------------------------------------------',Print_res=.FALSE.)
  CALL Append_Test(test_var,'------------------------------------------------------',Print_res=.FALSE.)
  CALL Append_Test(test_var,'------------------------------------------------------',Print_res=.FALSE.)
  ! end the normal tests

  ! end the normal tests

  CALL TEST_EXCEPTION()


  ! finalize the tests
  CALL Finalize_Test(test_var)

CONTAINS
  SUBROUTINE read_arg()
    USE QDUtil_m
    IMPLICIT NONE

    character(len=:), allocatable :: arg,arg2
    integer :: iarg,arg_len,prt_lev
    !logical, parameter :: debug = .TRUE.
    logical, parameter :: debug = .FALSE.

    IF (COMMAND_ARGUMENT_COUNT() /= 0 .AND. COMMAND_ARGUMENT_COUNT() /= 2) THEN
      write(out_unit,*) ' ERROR in read_arg'
      write(out_unit,*) ' Wrong ',name_sub,' argument number!'
      write(out_unit,*) 'argument number',COMMAND_ARGUMENT_COUNT()
      write(out_unit,*) ' You can have 0 or 2 arguments.'
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
        read(arg2,*) prt_lev
        CALL set_print_level(prt_lev,force=.TRUE.)

      CASE Default
        write(out_unit,*) ' ERROR in read_arg'
        write(out_unit,*) ' Wrong ',name_sub,' argument!'
        write(out_unit,*) '   arg: "',arg,'"'
        write(out_unit,*) ' The possibilities are:'
        write(out_unit,*) '    -p or --print'
        STOP 'Wrong TEST_dnS argument(s)'
      END SELECT

      IF (print_level > 0) write(out_unit,*) 'Argument number: ',iarg,' ==> arg: "',arg,'", arg2: "',arg2,'"'

      deallocate(arg)
      deallocate(arg2)
    END DO
    IF (print_level > 0) write(out_unit,*) '=================================='
    flush(out_unit)

  END SUBROUTINE read_arg

  SUBROUTINE TEST_EXCEPTION()
    USE, INTRINSIC :: ieee_exceptions
    IMPLICIT NONE

      logical (kind=Ik4)                          :: flagik4_NAN
      logical                                     :: flag_NAN


      character (len=*), parameter :: name_sub_loc='TEST_EXCEPTION'

      nderiv = 3
      CALL Append_Test(test_var,'============================================')
      CALL Append_Test(test_var,'== TESTING EXCEPTION dnS module with nderiv= ' // int_TO_char(nderiv))
      CALL Append_Test(test_var,'============================================')

      x   = -ONE
      dnX = Variable(x  ,nVar=1,nderiv=nderiv,iVar=1) ! to set up the derivatives
      IF (print_level > 0) CALL Write_dnS(dnX,string=test_var%test_log,info='dnX')

      CALL ieee_set_halting_mode(ieee_invalid,.FALSE._Ik4)
      CALL ieee_set_halting_mode(IEEE_DIVIDE_BY_ZERO,.FALSE._Ik4) ! this is added because nagfor (7.1.29) need that!!! Why?

      dnY = log(dnX)
      CALL  IEEE_GET_FLAG(ieee_invalid, flagik4_NAN) ; flag_NAN = flagik4_NAN
      CALL Logical_Test(test_var,test1=flag_NAN,info='log(-1.)            =>    NAN')
      IF (print_level > 0) CALL Write_dnS(dnY,string=test_var%test_log,info='log(-1.)')

      ! reset the NAN exception
      CALL  IEEE_SET_FLAG(ieee_invalid,.FALSE._Ik4)

      dnY = cos(dnX)
      CALL  IEEE_GET_FLAG(ieee_invalid, flagik4_NAN)  ; flag_NAN = flagik4_NAN
      CALL Logical_Test(test_var,test1=flag_NAN,test2=.FALSE.,info='cos(-1.)            => no NAN')
      IF (print_level > 0) CALL Write_dnS(dnY,string=test_var%test_log,info='cos(-1.)')

      ! reset the NAN exception
      CALL  IEEE_SET_FLAG(ieee_invalid,.FALSE._Ik4)

      dnY = log(dnX)
      dnY = cos(dnX)
      CALL  IEEE_GET_FLAG(ieee_invalid, flagik4_NAN) ; flag_NAN = flagik4_NAN
      CALL Logical_Test(test_var,test1=flag_NAN,              info='cos(-1.) ; log(-1.) =>    NAN')

      ! reset the NAN exception
      CALL  IEEE_SET_FLAG(ieee_invalid,.FALSE._Ik4)
      dnX = Variable(ZERO  ,nVar=1,nderiv=nderiv,iVar=1) ! to set up the derivatives
      !IF (print_level > 0) CALL Write_dnS(dnX,string=test_var%test_log,info='dnX')
      dnY = sqrt(dnX)
      CALL  IEEE_GET_FLAG(ieee_invalid, flagik4_NAN) ; flag_NAN = flagik4_NAN
      CALL Logical_Test(test_var,test1=flag_NAN,              info='sqrt(0.0)           =>    NAN')
      IF (print_level > 0) CALL Write_dnS(dnY,string=test_var%test_log,info='sqrt(0.0)')

      CALL Flush_Test(test_var)


  END SUBROUTINE TEST_EXCEPTION

FUNCTION SQRT_perso(x) RESULT(f)
  USE QDUtil_m
  IMPLICIT NONE
  real (kind=Rkind) :: f
  real (kind=Rkind), intent(in) :: x

  f = sqrt(x)

END FUNCTION SQRT_perso


FUNCTION faplusx(x)
  USE QDUtil_m
  IMPLICIT NONE

  real (kind=Rkind) :: faplusx
  real (kind=Rkind), intent(in) :: x

  faplusx = 0.5_Rkind + x

END FUNCTION faplusx
FUNCTION faminusx(x)
  USE QDUtil_m
  IMPLICIT NONE

  real (kind=Rkind) :: faminusx
  real (kind=Rkind), intent(in) :: x

  faminusx = 0.5_Rkind - x

END FUNCTION faminusx
FUNCTION fatimex(x)
  USE QDUtil_m
  IMPLICIT NONE

  real (kind=Rkind) :: fatimex
  real (kind=Rkind), intent(in) :: x

  fatimex = 2._Rkind * x

END FUNCTION fatimex
FUNCTION faoverx(x)
  USE QDUtil_m
  IMPLICIT NONE

  real (kind=Rkind) :: faoverx
  real (kind=Rkind), intent(in) :: x

  faoverx = 0.5_Rkind / x

END FUNCTION faoverx

FUNCTION ABS_perso(x) RESULT(f)
  USE QDUtil_m
  IMPLICIT NONE
  real (kind=Rkind) :: f
  real (kind=Rkind), intent(in) :: x

  f = ABS(x)

END FUNCTION ABS_perso
FUNCTION EXP_perso(x) RESULT(f)
  USE QDUtil_m
  IMPLICIT NONE
  real (kind=Rkind) :: f
  real (kind=Rkind), intent(in) :: x

  f = EXP(x)

END FUNCTION EXP_perso
FUNCTION LOG_perso(x) RESULT(f)
  USE QDUtil_m
  IMPLICIT NONE
  real (kind=Rkind) :: f
  real (kind=Rkind), intent(in) :: x

  f = LOG(x)

END FUNCTION LOG_perso
FUNCTION LOG10_perso(x) RESULT(f)
  USE QDUtil_m
  IMPLICIT NONE
  real (kind=Rkind) :: f
  real (kind=Rkind), intent(in) :: x

  f = LOG10(x)

END FUNCTION LOG10_perso
FUNCTION SIN_perso(x) RESULT(f)
  USE QDUtil_m
  IMPLICIT NONE
  real (kind=Rkind) :: f
  real (kind=Rkind), intent(in) :: x

  f = SIN(x)

END FUNCTION SIN_perso
FUNCTION ASIN_perso(x) RESULT(f)
  USE QDUtil_m
  IMPLICIT NONE
  real (kind=Rkind) :: f
  real (kind=Rkind), intent(in) :: x

  f = ASIN(x)

END FUNCTION ASIN_perso
FUNCTION COS_perso(x) RESULT(f)
  USE QDUtil_m
  IMPLICIT NONE
  real (kind=Rkind) :: f
  real (kind=Rkind), intent(in) :: x

  f = COS(x)

END FUNCTION COS_perso
FUNCTION ACOS_perso(x) RESULT(f)
  USE QDUtil_m
  IMPLICIT NONE
  real (kind=Rkind) :: f
  real (kind=Rkind), intent(in) :: x

  f = ACOS(x)

END FUNCTION ACOS_perso
FUNCTION TAN_perso(x) RESULT(f)
  USE QDUtil_m
  IMPLICIT NONE
  real (kind=Rkind) :: f
  real (kind=Rkind), intent(in) :: x

  f = TAN(x)

END FUNCTION TAN_perso
FUNCTION ATAN_perso(x) RESULT(f)
  USE QDUtil_m
  IMPLICIT NONE
  real (kind=Rkind) :: f
  real (kind=Rkind), intent(in) :: x

  f = ATAN(x)

END FUNCTION ATAN_perso
FUNCTION SINH_perso(x) RESULT(f)
  USE QDUtil_m
  IMPLICIT NONE
  real (kind=Rkind) :: f
  real (kind=Rkind), intent(in) :: x

  f = SINH(x)

END FUNCTION SINH_perso
FUNCTION ASINH_perso(x) RESULT(f)
  USE QDUtil_m
  IMPLICIT NONE
  real (kind=Rkind) :: f
  real (kind=Rkind), intent(in) :: x

    f = asinh(x)

END FUNCTION ASINH_perso
FUNCTION COSH_perso(x) RESULT(f)
  USE QDUtil_m
  IMPLICIT NONE
  real (kind=Rkind) :: f
  real (kind=Rkind), intent(in) :: x

  f = COSH(x)

END FUNCTION COSH_perso
FUNCTION ACOSH_perso(x) RESULT(f)
  USE QDUtil_m
  IMPLICIT NONE
  real (kind=Rkind) :: f
  real (kind=Rkind), intent(in) :: x

    f = acosh(x)

END FUNCTION ACOSH_perso
FUNCTION TANH_perso(x) RESULT(f)
  USE QDUtil_m
  IMPLICIT NONE
  real (kind=Rkind) :: f
  real (kind=Rkind), intent(in) :: x

  f = TANH(x)

END FUNCTION TANH_perso
FUNCTION ATANH_perso(x) RESULT(f)
  USE QDUtil_m
  IMPLICIT NONE
  real (kind=Rkind) :: f
  real (kind=Rkind), intent(in) :: x

    f = atanh(x)

END FUNCTION ATANH_perso
END PROGRAM TEST_dnS


!===============================================================================
!===============================================================================
!This file is part of AD_dnCSVM.
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
PROGRAM TEST_dnCS
  USE QDUtil_m
  USE QDUtil_Test_m
  USE ADdnSVM_m
  IMPLICIT NONE

    TYPE (dnS_t)                      :: dnRX,dnIX,dnRXana,dnIXana,dnRDiff,RSana,RSnum

    TYPE (dnCS_t)                     :: dnX,dn2X,dnY,dnZ,Sana,Snum,dnXZ
    TYPE (dnCS_t)                     :: dnA,dnB,dnC,dnDiff
    TYPE (dnCS_t)                     :: dnMX,dnMXana
    TYPE (dnCS_t)                     :: dnF,dnFX,dnFY
    TYPE (dnCS_t)                     :: dnZERO

    TYPE (dnCS_t), allocatable        :: Vec_dnCS(:),Vres_dnCS(:),Vana_dnCS(:)
    TYPE (dnCS_t), allocatable        :: Mat_dnCS(:,:),MatA_dnCS(:,:),MatB_dnCS(:,:),Mana_dnCS(:,:)

    TYPE (test_t)                    :: test_var
    logical                          :: val_test,res_test

    complex (kind=Rkind)             :: x,y,z
    real (kind=Rkind)                :: r,th,err,maxdiff,maxdnCS
    real (kind=Rkind), allocatable   :: JacNewOld(:,:),JacNewOld_ana(:,:)
    real (kind=Rkind), allocatable   :: FlatdnCS(:),FlatdnCS_ref(:)

    integer                          :: nderiv,nio_test
    real (kind=Rkind)                :: dnCSerr_test = FIVE*ONETENTH**4
    real (kind=Rkind),   parameter   :: ZeroTresh    = TEN**2*epsilon(ONE)

    character (len=:), allocatable   :: string


    integer                          :: i,j

    ! Add the functions in the CONTAINS to avoid a compiler bug (?, gfortran 15)
    !real (kind=Rkind), external  :: faplusx,faminusx,fatimex,faoverx
    !real (kind=Rkind), external  :: SQRT_perso,ABS_perso,EXP_perso,LOG_perso,LOG10_perso
    !real (kind=Rkind), external  :: SIN_perso,ASIN_perso,COS_perso,ACOS_perso,TAN_perso,ATAN_perso
    !real (kind=Rkind), external  :: SINH_perso,ASINH_perso,COSH_perso,ACOSH_perso,TANH_perso,ATANH_perso

    character (len=*), parameter :: name_sub='TEST_dnCS'

  CALL read_arg()

  CALL Initialize_Test(test_var,test_name='dnCS')

  IF (Rkind == Rk4) THEN
    nderiv = 1
    CALL Set_step_dnCS(ONETENTH**3)
  ELSE
    nderiv = 3
  END IF
  CALL Append_Test(test_var,'== TESTING dnCS module with nderiv= ' // int_TO_char(nderiv))

  x       = cmplx(HALF,ONE)
  dnX     = variable(x  ,nVar=1,nderiv=nderiv,iVar=1) ! to set up the derivatives
  dn2X    = variable(x+x,nVar=1,nderiv=nderiv,iVar=1) ! to set up the derivatives
  dnZERO  = variable(CZERO,nVar=1,nderiv=nderiv,iVar=1)

  CALL Write_dnCS(dnX,  string=test_var%test_log,info='dnX')
  CALL Write_dnCS(dn2X, string=test_var%test_log,info='dn2X')
  CALL Write_dnCS(dnX,  nio=out_unit,info='dnX (file)')
  CALL Write_dnCS(dn2X, nio=out_unit,info='dn2X (file)')

  CALL Flush_Test(test_var)

  CALL Append_Test(test_var,'============================================')
  CALL Append_Test(test_var,'operators: == /= ')
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

  CALL Flush_Test(test_var)

  CALL Append_Test(test_var,'============================================')
  CALL Append_Test(test_var,'operators: .EQ. .NE.')
  CALL Append_Test(test_var,'============================================')

  res_test = (dnX .EQ. dnX)
  CALL Logical_Test(test_var,test1=res_test,                                    &
                  info='dnX .EQ. dnX:   T ? ' // TO_string(res_test) )
  res_test = (dnX .NE. dn2X)
  CALL Logical_Test(test_var,test1=res_test,                                    &
                  info='dnX .NE.dn2X:   T ? ' // TO_string(res_test) )

  CALL Flush_Test(test_var)

  CALL Append_Test(test_var,'============================================')
  CALL Append_Test(test_var,'operators: areal, aimag, conjg, complx')
  CALL Append_Test(test_var,'============================================')
  dnRX  = dnX
  RSana = dnRX - variable(real(x,kind=Rkind),nVar=1,nderiv=nderiv,iVar=1) ! to set up the derivatives
  res_test = AD_Check_dnS_IS_ZERO(RSana,dnCSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='real(dnX)-Sana)==0?')
  IF (print_level > 0) THEN
    CALL Write_dnS(RSana,string=test_var%test_log,info='real(dnX)-Sana)==0')
  END IF
  dnRX  = real(dnX)
  RSana = dnRX - variable(real(x,kind=Rkind),nVar=1,nderiv=nderiv,iVar=1) ! to set up the derivatives
  res_test = AD_Check_dnS_IS_ZERO(RSana,dnCSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='real(dnX)-Sana)==0?')
  IF (print_level > 0) THEN 
   CALL Write_dnS(RSana,string=test_var%test_log,info='real(dnX)-Sana)==0')
  END IF

  dnIX  = aimag(dnX)
  CALL set_dnS(dnIXana,d0= ONE,d1=[ZERO],               &
                       d2=reshape([ZERO],shape=[1,1]),  &
                       d3=reshape([ZERO],shape=[1,1,1]))
  dnRDiff = dnIX - dnIXana ! to set up the derivatives
  res_test = AD_Check_dnS_IS_ZERO(dnRDiff,dnCSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='aimag(dnX)-dnIXana)==0?')
  IF (print_level > 0) THEN 
    CALL Write_dnCS(dnX,   string=test_var%test_log,info='dnX')
    CALL Write_dnS(dnIX,   string=test_var%test_log,info='dnIX')
    CALL Write_dnS(dnIXana,string=test_var%test_log,info='dnIXana')
    CALL Write_dnS(dnRDiff,string=test_var%test_log,info='aimag(dnX)-dnIXana)==0')
  END IF

  dnX = variable(x,nVar=1,nderiv=nderiv,iVar=1)
  Sana  = conjg(dnX)
  !Snum  = real(dnX) + (-EYE) * aimag(dnX)
  Snum = cmplx(dnRX,-dnIX)

  res_test = AD_Check_dnCS_IS_ZERO(Sana-Snum,dnCSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='conjg(dnX)-Snum)==0?')
  IF (print_level > 0) THEN 
    CALL Write_dnCS(dnX, string=test_var%test_log,info='dnX')
    CALL Write_dnCS(Sana,string=test_var%test_log,info='conj(dnX)')
    CALL Write_dnCS(Snum,string=test_var%test_log,info='cmplx(dnRX,-dnIX)')
    CALL Write_dnS(RSana,string=test_var%test_log,info='conjg(dnX)-Snum)==0')
  END IF

  dnX  = variable(x,nVar=1,nderiv=nderiv,iVar=1)
  dnRX = real(dnX)
  dnIX = aimag(dnX)
  Sana = cmplx(dnRX,dnIX)
  res_test = AD_Check_dnCS_IS_ZERO(Sana-dnX,dnCSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='cmplx(dnRX,dnIR)-Sana)==0?')
  IF (print_level > 0) THEN 
    CALL Write_dnCS(dnX, string=test_var%test_log,info='dnX')
    CALL Write_dnCS(Sana,string=test_var%test_log,info='cmplx(dnRX,dnIR)-Sana')
  END IF
 
  CALL Append_Test(test_var,'============================================')
  CALL Append_Test(test_var,'operators: + - * / **')
  CALL Append_Test(test_var,'============================================')

  Sana = 0.5_Rkind
  Sana = Sana + dnX
  Snum = AD_get_Num_dnCS_FROM_f_x(x,faplusx,nderiv=nderiv)
  res_test = AD_Check_dnCS_IS_ZERO(Sana-Snum,dnCSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='a+dnX:         (Sana-Snum)==0?')
  IF (print_level > 0) THEN 
    CALL Write_dnCS(Sana,string=test_var%test_log,info='Sana: 0.5 + dnX')
  END IF

  Sana = cmplx(0.5_Rkind,ZERO,kind=Rkind)
  Sana = Sana + dnX
  Snum = AD_get_Num_dnCS_FROM_f_x(x,faplusx,nderiv=nderiv)
  res_test = AD_Check_dnCS_IS_ZERO(Sana-Snum,dnCSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='a+dnX:         (Sana-Snum)==0?')
  IF (print_level > 0) THEN 
    CALL Write_dnCS(Sana,string=test_var%test_log,info='Sana: 0.5 + dnX')
  END IF

  Sana = dnX + 0.5_Rkind
  Snum = AD_get_Num_dnCS_FROM_f_x(x,faplusx,nderiv=nderiv)
  res_test = AD_Check_dnCS_IS_ZERO(Sana-Snum,dnCSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='dnX+a:         (Sana-Snum)==0?')
  IF (print_level > 0) CALL Write_dnCS(Sana,string=test_var%test_log,info='dnX + 0.5')

  Sana = dnX + cmplx(0.5_Rkind,ZERO,kind=Rkind)
  Snum = AD_get_Num_dnCS_FROM_f_x(x,faplusx,nderiv=nderiv)
  res_test = AD_Check_dnCS_IS_ZERO(Sana-Snum,dnCSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='dnX+a:         (Sana-Snum)==0?')
  IF (print_level > 0) CALL Write_dnCS(Sana,string=test_var%test_log,info='dnX + 0.5')

  Sana = dnX + dnX
  Snum = AD_get_Num_dnCS_FROM_f_x(x,fatimex,nderiv=nderiv)
  res_test = AD_Check_dnCS_IS_ZERO(Sana-Snum,dnCSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='dnX+dnX=2*dnX: (Sana-Snum)==0?')
  IF (print_level > 0) CALL Write_dnCS(Sana,string=test_var%test_log,info='dnX + dnX')

  Sana = +(0.5_Rkind - dnX)
  Snum = AD_get_Num_dnCS_FROM_f_x(x,faminusx,nderiv=nderiv)
  res_test = AD_Check_dnCS_IS_ZERO(Sana-Snum,dnCSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='+(a-dnX):      (Sana-Snum)==0?')
  IF (print_level > 0) CALL Write_dnCS(Sana,string=test_var%test_log,info='+(0.5 - dnX)')

  Sana = +(CHALF - dnX)
  Snum = AD_get_Num_dnCS_FROM_f_x(x,faminusx,nderiv=nderiv)
  res_test = AD_Check_dnCS_IS_ZERO(Sana-Snum,dnCSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='+(a-dnX):      (Sana-Snum)==0?')
  IF (print_level > 0) CALL Write_dnCS(Sana,string=test_var%test_log,info='+(0.5 - dnX)')

  Sana = -(dnX - 0.5_Rkind)
  Snum = AD_get_Num_dnCS_FROM_f_x(x,faminusx,nderiv=nderiv)
  res_test = AD_Check_dnCS_IS_ZERO(Sana-Snum,dnCSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='-(dnX-a):      (Sana-Snum)==0?')
  IF (print_level > 0) CALL Write_dnCS(Sana,string=test_var%test_log,info='-(dnX - 0.5)')

  Sana = -(dnX - CHALF)
  Snum = AD_get_Num_dnCS_FROM_f_x(x,faminusx,nderiv=nderiv)
  res_test = AD_Check_dnCS_IS_ZERO(Sana-Snum,dnCSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='-(dnX-a):      (Sana-Snum)==0?')
  IF (print_level > 0) CALL Write_dnCS(Sana,string=test_var%test_log,info='-(dnX - 0.5)')

  Sana = dnX - dnX
  res_test = AD_Check_dnCS_IS_ZERO(Sana,dnCSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='dnX-dnX                   ==0?')
  IF (print_level > 0) CALL Write_dnCS(Sana,string=test_var%test_log,info='dnX - dnX')

  Sana = 2._Rkind * dnX
  Snum = AD_get_Num_dnCS_FROM_f_x(x,fatimex,nderiv=nderiv)
  res_test = AD_Check_dnCS_IS_ZERO(Sana-Snum,dnCSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='a*dnX:         (Sana-Snum)==0?')
  IF (print_level > 0) CALL Write_dnCS(Sana,string=test_var%test_log,info='2. * dnX')

  Sana = CTWO * dnX
  Snum = AD_get_Num_dnCS_FROM_f_x(x,fatimex,nderiv=nderiv)
  res_test = AD_Check_dnCS_IS_ZERO(Sana-Snum,dnCSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='a*dnX:         (Sana-Snum)==0?')
  IF (print_level > 0) CALL Write_dnCS(Sana,string=test_var%test_log,info='2. * dnX')

  Sana =  dnX * 2._Rkind
  Snum = AD_get_Num_dnCS_FROM_f_x(x,fatimex,nderiv=nderiv)
  res_test = AD_Check_dnCS_IS_ZERO(Sana-Snum,dnCSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='dnX*a:         (Sana-Snum)==0?')
  IF (print_level > 0) CALL Write_dnCS(Sana,string=test_var%test_log,info='dnX * 2.')

  Sana =  dnX * CTWO
  Snum = AD_get_Num_dnCS_FROM_f_x(x,fatimex,nderiv=nderiv)
  res_test = AD_Check_dnCS_IS_ZERO(Sana-Snum,dnCSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='dnX*a:         (Sana-Snum)==0?')
  IF (print_level > 0) CALL Write_dnCS(Sana,string=test_var%test_log,info='dnX * 2.')

  Sana =  dnX * dnX - dnX**(2._Rkind)
  res_test = AD_Check_dnCS_IS_ZERO(Sana,dnCSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='dnX*dnX-dnX**2.           ==0?')
  IF (print_level > 0) CALL Write_dnCS(Sana,string=test_var%test_log,info='dnX * dnX - dnX**(2.)')

  Sana =  dnX / 0.5_Rkind -dnX*2._Rkind
  res_test = AD_Check_dnCS_IS_ZERO(Sana,dnCSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='dnX/0.5-dnX*2             ==0?')
  IF (print_level > 0) CALL Write_dnCS(Sana,string=test_var%test_log,info='dnX/0.5 -dnX*2.')

  Sana =  dnX / CHALF -dnX*CTWO
  res_test = AD_Check_dnCS_IS_ZERO(Sana,dnCSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='dnX/0.5-dnX*2             ==0?')
  IF (print_level > 0) CALL Write_dnCS(Sana,string=test_var%test_log,info='dnX/0.5 -dnX*2.')

  Sana =  0.5_Rkind / dnX - 0.5_Rkind*dnX**(-1)
  res_test = AD_Check_dnCS_IS_ZERO(Sana,dnCSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='0.5/dnX-0.5.dnX*^-1       ==0?')
  IF (print_level > 0) CALL Write_dnCS(Sana,string=test_var%test_log,info='0.5 / dnX - 0.5*dnX**(-1)')

  Sana =  CHALF / dnX - CHALF*dnX**(-1)
  res_test = AD_Check_dnCS_IS_ZERO(Sana,dnCSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='0.5/dnX-0.5.dnX*^-1       ==0?')
  IF (print_level > 0) CALL Write_dnCS(Sana,string=test_var%test_log,info='0.5 / dnX - 0.5*dnX**(-1)')

  Sana =  dnX / dnX - 1._Rkind
  res_test = AD_Check_dnCS_IS_ZERO(Sana,dnCSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='dnX/dnX-1.                ==0?')
  IF (print_level > 0) CALL Write_dnCS(Sana,string=test_var%test_log,info='dnX / dnX - 1.')

  Sana =  dnX / dnX - CONE
  res_test = AD_Check_dnCS_IS_ZERO(Sana,dnCSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='dnX/dnX-1.                ==0?')
  IF (print_level > 0) CALL Write_dnCS(Sana,string=test_var%test_log,info='dnX / dnX - 1.')

  Sana =  dnX**0.5_Rkind - sqrt(dnX)
  res_test = AD_Check_dnCS_IS_ZERO(Sana,dnCSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='dnX**0.5-sqrt(dnX)        ==0?')
  IF (print_level > 0) CALL Write_dnCS(Sana,string=test_var%test_log,info='dnX**0.5-sqrt(dnX)')

  Sana =  dnX**3 - dnX*dnX*dnX
  res_test = AD_Check_dnCS_IS_ZERO(Sana,dnCSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='dnX**3-dnX*dnX*dnX        ==0?')
  IF (print_level > 0) CALL Write_dnCS(Sana,string=test_var%test_log,info='dnX**3 - dnX*dnX*dnX')

  Sana = dnZERO**3 - dnZERO*dnZERO*dnZERO
  res_test = AD_Check_dnCS_IS_ZERO(Sana,dnCSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='dnX**3-dnX*dnX*dnX (x=0.) ==0?')
  IF (print_level > 0) CALL Write_dnCS(Sana,string=test_var%test_log,info='dnX**3 - dnX*dnX*dnX (x=0.)')

  CALL Flush_Test(test_var)


  CALL Append_Test(test_var,'============================================')
  CALL Append_Test(test_var,'functions: sqrt, exp, log, ... sin, asin, ... acosh ...')
  CALL Append_Test(test_var,'============================================')

  Sana = sqrt(dnX)
  Snum = AD_get_Num_dnCS_FROM_f_x(x,SQRT_perso,nderiv=nderiv)
  res_test = AD_Check_dnCS_IS_ZERO(Sana-Snum,dnCSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='sqrt: (Sana-Snum)==0?')
  IF (print_level > 0) THEN
    CALL Write_dnCS(Sana,string=test_var%test_log,info='Sana: sqrt(dnX)')
    CALL Write_dnCS(Snum,string=test_var%test_log,info='Snum: sqrt(dnX)')
  END IF


  Sana = abs(dnX)
  Snum = AD_get_Num_dnCS_FROM_f_x(x,ABS_perso,nderiv=nderiv)
  res_test = AD_Check_dnCS_IS_ZERO(Sana-Snum,dnCSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='abs:  (Sana-Snum)==0?')
  IF (print_level > 0) CALL Write_dnCS(Sana,string=test_var%test_log,info='abs(dnX)')

  Sana = exp(dnX)
  Snum = AD_get_Num_dnCS_FROM_f_x(x,EXP_perso,nderiv=nderiv)
  res_test = AD_Check_dnCS_IS_ZERO(Sana-Snum,dnCSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='exp:  (Sana-Snum)==0?')
  IF (print_level > 0) CALL Write_dnCS(Sana,string=test_var%test_log,info='exp(dnX)')

  Sana = log(dnX)
  Snum = AD_get_Num_dnCS_FROM_f_x(x,log_perso,nderiv=nderiv)
  res_test = AD_Check_dnCS_IS_ZERO(Sana-Snum,dnCSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='log:  (Sana-Snum)==0?')
  IF (print_level > 0) CALL Write_dnCS(Sana,string=test_var%test_log,info='log(dnX)')

  !Sana = log10(dnX)
  !Snum = AD_get_Num_dnCS_FROM_f_x(x,log10_perso,nderiv=nderiv)
  !res_test = AD_Check_dnCS_IS_ZERO(Sana-Snum,dnCSerr_test)
  !CALL Logical_Test(test_var,test1=res_test,info='log10:(Sana-Snum)==0?')
  !IF (print_level > 0) CALL Write_dnCS(Sana,string=test_var%test_log,info='log10(dnX)')

  Sana = sin(dnX)
  Snum = AD_get_Num_dnCS_FROM_f_x(x,sin_perso,nderiv=nderiv)
  res_test = AD_Check_dnCS_IS_ZERO(Sana-Snum,dnCSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='sin:  (Sana-Snum)==0?')
  IF (print_level > 0) CALL Write_dnCS(Sana,string=test_var%test_log,info='sin(dnX)')

  Sana = asin(dnX)
  Snum = AD_get_Num_dnCS_FROM_f_x(x,asin_perso,nderiv=nderiv)
  res_test = AD_Check_dnCS_IS_ZERO(Sana-Snum,dnCSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='asin: (Sana-Snum)==0?')
  IF (print_level > 0) CALL Write_dnCS(Sana,string=test_var%test_log,info='asin(dnX)')

  Sana = cos(dnX)
  Snum = AD_get_Num_dnCS_FROM_f_x(x,cos_perso,nderiv=nderiv)
  res_test = AD_Check_dnCS_IS_ZERO(Sana-Snum,dnCSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='cos:  (Sana-Snum)==0?')
  IF (print_level > 0) CALL Write_dnCS(Sana,string=test_var%test_log,info='cos(dnX)')

  Sana = acos(dnX)
  Snum = -EYE*log(dnX+EYE*sqrt(ONE-dnX*dnX))
  res_test = AD_Check_dnCS_IS_ZERO(Sana-Snum,dnCSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='acos: (Sana-Snum)==0?')
  IF (print_level > 0 .OR. .NOT. res_test) THEN
    CALL Write_dnCS(dnX,string=test_var%test_log,info='dnX')
    CALL Write_dnCS(Sana,string=test_var%test_log,info='acos(dnX)')
    CALL Write_dnCS(Snum,string=test_var%test_log,info='Snum: acos(dnX)')
  END IF

  Sana = tan(dnX)
  Snum = AD_get_Num_dnCS_FROM_f_x(x,tan_perso,nderiv=nderiv)
  res_test = AD_Check_dnCS_IS_ZERO(Sana-Snum,dnCSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='tan:  (Sana-Snum)==0?')
  IF (print_level > 0) CALL Write_dnCS(Sana,string=test_var%test_log,info='tan(dnX)')

  Sana = atan(dnX)
  Snum = AD_get_Num_dnCS_FROM_f_x(x,atan_perso,nderiv=nderiv)
  res_test = AD_Check_dnCS_IS_ZERO(Sana-Snum,dnCSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='atan: (Sana-Snum)==0?')
  IF (print_level > 0) CALL Write_dnCS(Sana,string=test_var%test_log,info='atan(dnX)')

  Sana = sinh(dnX)
  Snum = AD_get_Num_dnCS_FROM_f_x(x,sinh_perso,nderiv=nderiv)
  res_test = AD_Check_dnCS_IS_ZERO(Sana-Snum,dnCSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='sinh: (Sana-Snum)==0?')
  IF (print_level > 0) CALL Write_dnCS(Sana,string=test_var%test_log,info='sinh(dnX)')

  Sana = asinh(dnX)
  Snum = AD_get_Num_dnCS_FROM_f_x(x,asinh_perso,nderiv=nderiv)
  res_test = AD_Check_dnCS_IS_ZERO(Sana-Snum,dnCSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='asinh:(Sana-Snum)==0?')
  IF (print_level > 0) CALL Write_dnCS(Sana,string=test_var%test_log,info='asinh(dnX)')

  Sana = cosh(dnX)
  Snum = AD_get_Num_dnCS_FROM_f_x(x,cosh_perso,nderiv=nderiv)
  res_test = AD_Check_dnCS_IS_ZERO(Sana-Snum,dnCSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='cosh: (Sana-Snum)==0?')
  IF (print_level > 0) CALL Write_dnCS(Sana,string=test_var%test_log,info='cosh(dnX)')

  dnY = Variable(FOUR*x  ,nVar=1,nderiv=nderiv,iVar=1) ! to set up the derivatives
  Sana = acosh(dnY)
  Snum = AD_get_Num_dnCS_FROM_f_x(FOUR*x,acosh_perso,nderiv=nderiv)
  res_test = AD_Check_dnCS_IS_ZERO(Sana-Snum,dnCSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='acosh:(Sana-Snum)==0?')
  IF (print_level > 0) CALL Write_dnCS(Sana,string=test_var%test_log,info='acosh(dnY)')

  Sana = tanh(dnX)
  Snum = sinh(dnX)/cosh(dnX)
  res_test = AD_Check_dnCS_IS_ZERO(Sana-Snum,dnCSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='tanh: (Sana-Snum)==0?')
  IF (print_level > 0) CALL Write_dnCS(Sana,string=test_var%test_log,info='tanh(dnX)')

  Sana = atanh(dnX)
  Snum = AD_get_Num_dnCS_FROM_f_x(x,atanh_perso,nderiv=nderiv)
  res_test = AD_Check_dnCS_IS_ZERO(Sana-Snum,dnCSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='atanh:(Sana-Snum)==0?')
  IF (print_level > 0) CALL Write_dnCS(Sana,string=test_var%test_log,info='atanh(dnX)')

  CALL Flush_Test(test_var)


  CALL Append_Test(test_var,'============================================')
  CALL Append_Test(test_var,'new tests: **, composition')
  CALL Append_Test(test_var,'============================================')

  Sana =  dnX**0 - ONE
  res_test = AD_Check_dnCS_IS_ZERO(Sana,dnCSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='dnX**0-ONE                ==0?')
  IF (print_level > 0) CALL Write_dnCS(Sana,string=test_var%test_log,info='dnX**0-ONE')

  Sana =  dnX**1 - dnX
  res_test = AD_Check_dnCS_IS_ZERO(Sana,dnCSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='dnX**1-dnX                ==0?')
  IF (print_level > 0) CALL Write_dnCS(Sana,string=test_var%test_log,info='dnX**1-dnX')

  Sana =  dnX**2 - dnX*dnX
  res_test = AD_Check_dnCS_IS_ZERO(Sana,dnCSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='dnX**2-dnX*dnX            ==0?')
  IF (print_level > 0) CALL Write_dnCS(Sana,string=test_var%test_log,info='dnX**2-dnX*dnX')

  Sana =  dnX**3 - dnX*dnX*dnX
  res_test = AD_Check_dnCS_IS_ZERO(Sana,dnCSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='dnX**3-dnX*dnX*dnX        ==0?')
  IF (print_level > 0) CALL Write_dnCS(Sana,string=test_var%test_log,info='dnX**3-dnX*dnX*dnX')

  Sana =  sqrt(dnX**2) - dnX
  res_test = AD_Check_dnCS_IS_ZERO(Sana,dnCSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='sqrt(dnX**2) - dnX        ==0?')
  IF (print_level > 0) CALL Write_dnCS(Sana,string=test_var%test_log,info='sqrt(dnX**2) - dnX')

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
  CALL set_dnCS(dnXZ,d0=     x*z,                             &
                    d1=        [z,    CZERO,x],               &
                    d2=reshape([CZERO,CZERO,CONE,             &
                                CZERO,CZERO,CZERO,            &
                                CONE, CZERO,CZERO],shape=[3,3]))

  res_test = AD_Check_dnCS_IS_ZERO(Sana-dnXZ,dnCSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='dnX*dnZ -dnCS_Result       ==0?')
  IF (print_level > 0) THEN
    CALL Write_dnCS(dnX,string=test_var%test_log,info='dnX')
    CALL Write_dnCS(dnZ,string=test_var%test_log,info='dnZ')
    CALL Write_dnCS(Sana,string=test_var%test_log,info='dnX*dnZ (3D)')
  END IF

  nderiv = 2
  x = 0.5_Rkind
  z = 2.0_Rkind
  Vec_dnCS = Variable([x,CZERO,z],nderiv=nderiv)

  Sana    = Vec_dnCS(1)*Vec_dnCS(3) ! It is equivalent to a 3D function f(x,y,z) = x*z
  CALL set_dnCS(dnXZ,d0=     x*z,                             &
                     d1=        [z,    CZERO,x],              &
                     d2=reshape([CZERO,CZERO,CONE,            &
                                 CZERO,CZERO,CZERO,           &
                                 CONE, CZERO,CZERO],shape=[3,3]))

  res_test = AD_Check_dnCS_IS_ZERO(Sana-dnXZ,dnCSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='Vec_dnCS(1)*Vec_dnCS(3)-dnCS_Result==0?')
  IF (print_level > 0) THEN
    CALL Write_dnCS(Vec_dnCS(1),string=test_var%test_log,info='Vec_dnCS(1)')
    CALL Write_dnCS(Vec_dnCS(3),string=test_var%test_log,info='Vec_dnCS(3)')
    CALL Write_dnCS(Sana,string=test_var%test_log,info='dnX*dnZ (3D)')
  END IF
  CALL Flush_Test(test_var)

  Vec_dnCS = Variable([x,CZERO],nVar=3,iVar=[1,2],nderiv=nderiv)
  dnZ =     Variable(z,nVar=3,iVar=3,nderiv=nderiv)

  Sana    = Vec_dnCS(1)*dnZ ! It is equivalent to a 3D function f(x,y,z) = x*z
  CALL set_dnCS(dnXZ,d0=     x*z,                             &
                     d1=       [z,     CZERO,x],              &
                     d2=reshape([CZERO,CZERO,CONE,            &
                                 CZERO,CZERO,CZERO,           &
                                 CONE, CZERO,CZERO],shape=[3,3]))

  res_test = AD_Check_dnCS_IS_ZERO(Sana-dnXZ,dnCSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='Vec_dnCS(1)*Vec_dnCS(3)-dnCS_Result==0?')
  IF (print_level > 0) THEN
    CALL Write_dnCS(Vec_dnCS(1),string=test_var%test_log,info='Vec_dnCS(1)')
    CALL Write_dnCS(dnZ,string=test_var%test_log,info='dnZ')
    CALL Write_dnCS(Sana,string=test_var%test_log,info='dnX*dnZ (3D)')
  END IF


  Vec_dnCS = Variable([x,CZERO],nVar=3,nderiv=nderiv)
  dnZ =     Variable(z,nVar=3,iVar=3,nderiv=nderiv)

  Sana    = Vec_dnCS(1)*dnZ ! It is equivalent to a 3D function f(x,y,z) = x*z
  CALL set_dnCS(dnXZ,d0=     x*z,                             &
                     d1=        [z,    CZERO,x],              &
                     d2=reshape([CZERO,CZERO,CONE,            &
                                 CZERO,CZERO,CZERO,           &
                                 CONE, CZERO,CZERO],shape=[3,3]))

  res_test = AD_Check_dnCS_IS_ZERO(Sana-dnXZ,dnCSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='Vec_dnCS(1)*Vec_dnCS(3)-dnCS_Result==0?')
  IF (print_level > 0) THEN
    CALL Write_dnCS(Vec_dnCS(1),string=test_var%test_log,info='Vec_dnCS(1)')
    CALL Write_dnCS(dnZ,string=test_var%test_log,info='dnZ')
    CALL Write_dnCS(Sana,string=test_var%test_log,info='dnX*dnZ (3D)')
  END IF
  CALL Flush_Test(test_var)




  CALL Append_Test(test_var,'============================================')
  CALL Append_Test(test_var,'Jacobian(inew,iold): 2D, nderiv=2')
  CALL Append_Test(test_var,'Polar transformation')
  CALL Append_Test(test_var,'============================================')
  CALL Flush_Test(test_var)

  r  = TWO
  th = Pi/3
  Vec_dnCS = Variable([r,th], nderiv=2 ) ! Vec(1) : r, Vec(2) : th

  JacNewOld     = get_Jacobian( Vec_dnCS(1)*[cos(Vec_dnCS(2)),sin(Vec_dnCS(2))] ) ! Jacobian of a polar transformation
  JacNewOld_ana = reshape([cos(th),sin(th),-r*sin(th),r*cos(th)],shape(JacNewOld))

  res_test = ( maxval(abs(JacNewOld-JacNewOld_ana)) < dnCSerr_test )
  CALL Logical_Test(test_var,test1=res_test,info='Jacobian-Jacobian_ana     ==0?')

  IF (print_level > 0) THEN
    CALL Append_Test(test_var,'Jac(inew,iold)=[ dQinew/dQiold ]:',Print_res=.FALSE.)
    CALL Write_Mat(JacNewOld,string=test_var%test_log,nbcol=5)
    CALL Append_Test(test_var,'analytical: [dx/dr, dx/dth]:   [0.5,     -1.732...]',Print_res=.FALSE.)
    CALL Append_Test(test_var,'analytical: [dy/dr, dy/dth]:   [0.866..,  1.      ]',Print_res=.FALSE.)
  END IF
  CALL Flush_Test(test_var)


  !================================================================================
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
      STOP 'Wrong TEST_dnCS argument number'
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
        STOP 'Wrong TEST_dnCS argument(s)'
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
      CALL Append_Test(test_var,'== TESTING EXCEPTION dnCS module with nderiv= ' // int_TO_char(nderiv))
      CALL Append_Test(test_var,'============================================')

      x   = -ONE
      dnX = Variable(x  ,nVar=1,nderiv=nderiv,iVar=1) ! to set up the derivatives
      IF (print_level > 0) CALL Write_dnCS(dnX,string=test_var%test_log,info='dnX')

      CALL ieee_set_halting_mode(ieee_invalid,.FALSE._Ik4)
      CALL ieee_set_halting_mode(IEEE_DIVIDE_BY_ZERO,.FALSE._Ik4) ! this is added because nagfor (7.1.29) need that!!! Why?

      dnY = log(dnX)
      CALL  IEEE_GET_FLAG(ieee_invalid, flagik4_NAN) ; flag_NAN = flagik4_NAN
      CALL Logical_Test(test_var,test1=flag_NAN,info='log(-1.)            =>    NAN')
      IF (print_level > 0) CALL Write_dnCS(dnY,string=test_var%test_log,info='log(-1.)')

      ! reset the NAN exception
      CALL  IEEE_SET_FLAG(ieee_invalid,.FALSE._Ik4)

      dnY = cos(dnX)
      CALL  IEEE_GET_FLAG(ieee_invalid, flagik4_NAN)  ; flag_NAN = flagik4_NAN
      CALL Logical_Test(test_var,test1=flag_NAN,test2=.FALSE.,info='cos(-1.)            => no NAN')
      IF (print_level > 0) CALL Write_dnCS(dnY,string=test_var%test_log,info='cos(-1.)')

      ! reset the NAN exception
      CALL  IEEE_SET_FLAG(ieee_invalid,.FALSE._Ik4)

      dnY = log(dnX)
      dnY = cos(dnX)
      CALL  IEEE_GET_FLAG(ieee_invalid, flagik4_NAN) ; flag_NAN = flagik4_NAN
      CALL Logical_Test(test_var,test1=flag_NAN,              info='cos(-1.) ; log(-1.) =>    NAN')

      ! reset the NAN exception
      CALL  IEEE_SET_FLAG(ieee_invalid,.FALSE._Ik4)
      dnX = Variable(CZERO  ,nVar=1,nderiv=nderiv,iVar=1) ! to set up the derivatives
      !IF (print_level > 0) CALL Write_dnCS(dnX,string=test_var%test_log,info='dnX')
      dnY = sqrt(dnX)
      CALL  IEEE_GET_FLAG(ieee_invalid, flagik4_NAN) ; flag_NAN = flagik4_NAN
      CALL Logical_Test(test_var,test1=flag_NAN,              info='sqrt(0.0)           =>    NAN')
      IF (print_level > 0) CALL Write_dnCS(dnY,string=test_var%test_log,info='sqrt(0.0)')

      CALL Flush_Test(test_var)


  END SUBROUTINE TEST_EXCEPTION

FUNCTION SQRT_perso(x) RESULT(f)
  USE QDUtil_m
  IMPLICIT NONE
  complex (kind=Rkind) :: f
  complex (kind=Rkind), intent(in) :: x

  f = sqrt(x)

END FUNCTION SQRT_perso


FUNCTION faplusx(x)
  USE QDUtil_m
  IMPLICIT NONE

  complex (kind=Rkind) :: faplusx
  complex (kind=Rkind), intent(in) :: x

  faplusx = 0.5_Rkind + x

END FUNCTION faplusx
FUNCTION faminusx(x)
  USE QDUtil_m
  IMPLICIT NONE

  complex (kind=Rkind) :: faminusx
  complex (kind=Rkind), intent(in) :: x

  faminusx = 0.5_Rkind - x

END FUNCTION faminusx
FUNCTION fatimex(x)
  USE QDUtil_m
  IMPLICIT NONE

  complex (kind=Rkind) :: fatimex
  complex (kind=Rkind), intent(in) :: x

  fatimex = 2._Rkind * x

END FUNCTION fatimex
FUNCTION faoverx(x)
  USE QDUtil_m
  IMPLICIT NONE

  complex (kind=Rkind) :: faoverx
  complex (kind=Rkind), intent(in) :: x

  faoverx = 0.5_Rkind / x

END FUNCTION faoverx

FUNCTION ABS_perso(x) RESULT(f)
  USE QDUtil_m
  IMPLICIT NONE
  complex (kind=Rkind) :: f
  complex (kind=Rkind), intent(in) :: x

  f = ABS(x)

END FUNCTION ABS_perso
FUNCTION EXP_perso(x) RESULT(f)
  USE QDUtil_m
  IMPLICIT NONE
  complex (kind=Rkind) :: f
  complex (kind=Rkind), intent(in) :: x

  f = EXP(x)

END FUNCTION EXP_perso
FUNCTION LOG_perso(x) RESULT(f)
  USE QDUtil_m
  IMPLICIT NONE
  complex (kind=Rkind) :: f
  complex (kind=Rkind), intent(in) :: x

  f = LOG(x)

END FUNCTION LOG_perso
FUNCTION LOG10_perso(x) RESULT(f)
  USE QDUtil_m
  IMPLICIT NONE
  complex (kind=Rkind) :: f
  complex (kind=Rkind), intent(in) :: x

  f = log(x)/log(TEN)
  !f = LOG10(x)

END FUNCTION LOG10_perso
FUNCTION SIN_perso(x) RESULT(f)
  USE QDUtil_m
  IMPLICIT NONE
  complex (kind=Rkind) :: f
  complex (kind=Rkind), intent(in) :: x

  f = SIN(x)

END FUNCTION SIN_perso
FUNCTION ASIN_perso(x) RESULT(f)
  USE QDUtil_m
  IMPLICIT NONE
  complex (kind=Rkind) :: f
  complex (kind=Rkind), intent(in) :: x

  f = ASIN(x)

END FUNCTION ASIN_perso
FUNCTION COS_perso(x) RESULT(f)
  USE QDUtil_m
  IMPLICIT NONE
  complex (kind=Rkind) :: f
  complex (kind=Rkind), intent(in) :: x

  f = COS(x)

END FUNCTION COS_perso
FUNCTION ACOS_perso(x) RESULT(f)
  USE QDUtil_m
  IMPLICIT NONE
  complex (kind=Rkind) :: f
  complex (kind=Rkind), intent(in) :: x

  f = ACOS(x)

END FUNCTION ACOS_perso
FUNCTION TAN_perso(x) RESULT(f)
  USE QDUtil_m
  IMPLICIT NONE
  complex (kind=Rkind) :: f
  complex (kind=Rkind), intent(in) :: x

  f = TAN(x)

END FUNCTION TAN_perso
FUNCTION ATAN_perso(x) RESULT(f)
  USE QDUtil_m
  IMPLICIT NONE
  complex (kind=Rkind) :: f
  complex (kind=Rkind), intent(in) :: x

  f = ATAN(x)

END FUNCTION ATAN_perso
FUNCTION SINH_perso(x) RESULT(f)
  USE QDUtil_m
  IMPLICIT NONE
  complex (kind=Rkind) :: f
  complex (kind=Rkind), intent(in) :: x

  f = SINH(x)

END FUNCTION SINH_perso
FUNCTION ASINH_perso(x) RESULT(f)
  USE QDUtil_m
  IMPLICIT NONE
  complex (kind=Rkind) :: f
  complex (kind=Rkind), intent(in) :: x

    f = asinh(x)

END FUNCTION ASINH_perso
FUNCTION COSH_perso(x) RESULT(f)
  USE QDUtil_m
  IMPLICIT NONE
  complex (kind=Rkind) :: f
  complex (kind=Rkind), intent(in) :: x

  f = COSH(x)

END FUNCTION COSH_perso
FUNCTION ACOSH_perso(x) RESULT(f)
  USE QDUtil_m
  IMPLICIT NONE
  complex (kind=Rkind) :: f
  complex (kind=Rkind), intent(in) :: x

    f = acosh(x)

END FUNCTION ACOSH_perso
FUNCTION TANH_perso(x) RESULT(f)
  USE QDUtil_m
  IMPLICIT NONE
  complex (kind=Rkind) :: f
  complex (kind=Rkind), intent(in) :: x

  f = TANH(x)

END FUNCTION TANH_perso
FUNCTION ATANH_perso(x) RESULT(f)
  USE QDUtil_m
  IMPLICIT NONE
  complex (kind=Rkind) :: f
  complex (kind=Rkind), intent(in) :: x

    f = atanh(x)

END FUNCTION ATANH_perso
END PROGRAM TEST_dnCS


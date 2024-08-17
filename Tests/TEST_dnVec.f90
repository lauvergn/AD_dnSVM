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

    TYPE (dnVec_t)                   :: dnV1,dnV2

    TYPE (dnS_t), allocatable        :: Vec_dnS(:)
    TYPE (dnS_t)                     :: dnS,dnS_ana

    TYPE (test_t)                    :: test_var
    logical                          :: val_test,res_test

    real (kind=Rkind)                :: x,y,z,r,th,err,maxdiff,maxdnS
    real (kind=Rkind), allocatable   :: JacNewOld(:,:),JacNewOld_ana(:,:)
    real (kind=Rkind), allocatable   :: FlatdnS(:),FlatdnS_ref(:),Mat(:,:)

    integer                          :: nderiv,nio_test
    real (kind=Rkind)                :: dnSerr_test = FIVE*ONETENTH**4
    real (kind=Rkind)                :: ZeroTresh   = ONETENTH**10

    integer                          :: i,j


    character (len=*), parameter :: name_sub='TEST_dnVec'

  CALL read_arg()

  CALL Initialize_Test(test_var,test_name='dnVec')

  nderiv = 2
  CALL Append_Test(test_var,'== TESTING dnVec module with nderiv= ' // int_TO_char(nderiv))

  x = 0.5_Rkind
  z = 2.0_Rkind
  Vec_dnS = Variable([x,ZERO,z],nderiv=nderiv)

  dnV1 = Vec_dnS
  Vec_dnS = dnV1
  dnV1 = Vec_dnS

  CALL set_dnVec(dnV2,d0=[x,ZERO,z],                           &
                      d1=reshape([ONE ,ZERO,ZERO,              &
                                  ZERO,ONE ,ZERO,              &
                                  ZERO, ZERO,ONE],shape=[3,3]),&
                      d2=reshape([(ZERO,i=1,27)],shape=[3,3,3]))

  res_test = Check_dnVec_IS_ZERO(dnV2-dnV1,dnSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='dnV1-dnV2==0?')
  res_test = Check_dnVec_IS_ZERO(dnV2-dnV1,dnSerr_test)
  CALL Append_Test(test_var,'test: dnVec = VecOFdnS, VecOFdnS = dnVec, dnVec2 - dnVec1, set_dnVec, Write_dnVec')

  IF (print_level > 0) THEN
    CALL Write_dnVec(dnV1,info='dnV1')
    CALL Write_dnVec(dnV2,info='dnV2')
  END IF
  CALL Flush_Test(test_var)

  x = 0.5_Rkind
  z = 2.0_Rkind
  dnV1 = Variable_dnVec([x,ZERO,z],nderiv=nderiv)
  CALL set_dnVec(dnV2,d0=[x,ZERO,z],                           &
                      d1=reshape([ONE ,ZERO,ZERO,              &
                                  ZERO,ONE ,ZERO,              &
                                  ZERO, ZERO,ONE],shape=[3,3]),&
                      d2=reshape([(ZERO,i=1,27)],shape=[3,3,3]))

  res_test = Check_dnVec_IS_ZERO(dnV2-dnV1,dnSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='dnV1-dnV2==0?')
  res_test = Check_dnVec_IS_ZERO(dnV2-dnV1,dnSerr_test)
  CALL Append_Test(test_var,'test: dnVec = Variable_dnVec, dnVec2 - dnVec1, set_dnVec, Write_dnVec')

  IF (print_level > 0) THEN
    CALL Write_dnVec(dnV1,info='dnV1')
    CALL Write_dnVec(dnV2,info='dnV2')
  END IF
  CALL Flush_Test(test_var)

  dnV1     = ZERO
  FlatdnS  = get_Flatten(dnV1)
  res_test = maxval(abs(FlatdnS-[(ZERO,i=1,size(FlatdnS))])) < ZeroTresh
  CALL Logical_Test(test_var,test1=res_test,info='dnV1-[ZERO...]==0?')
  CALL Append_Test(test_var,'test: dnVec = ZERO, get_Flatten')
  IF (print_level > 0) THEN
    CALL Write_dnVec(dnV1,info='dnV1')
  END IF
  CALL Flush_Test(test_var)

  dnV1    = Vec_dnS
  dnS     = dot_product(dnV1,dnV1)
  dnS_ana = dot_product(Vec_dnS,Vec_dnS) 
  res_test = AD_Check_dnS_IS_ZERO(dnS_ana-dnS,dnSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='dnS_ana-dnS==0?')
  IF (print_level > 0) THEN
    CALL Write_dnS(dnS    ,string=test_var%test_log,info='dot_product from dnVec')
    CALL Write_dnS(dnS_ana,string=test_var%test_log,info='dot_product from dnS(:)')
  END IF
  CALL Flush_Test(test_var)

  dnV2    = dnV1*TWO
  dnS     = dot_product(dnV1,dnV2)
  dnS_ana = TWO*dot_product(Vec_dnS,Vec_dnS) 
  res_test = AD_Check_dnS_IS_ZERO(dnS_ana-dnS,dnSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='dnS_ana-dnS==0?')
  IF (print_level > 0) THEN
    CALL Write_dnS(dnS    ,string=test_var%test_log,info='dot_product from dnVec')
    CALL Write_dnS(dnS_ana,string=test_var%test_log,info='dot_product from dnS(:)')
  END IF
  CALL Flush_Test(test_var)

  dnV2    = -TWO*dnV1
  dnS     = dot_product(dnV1,dnV2)
  dnS_ana = -TWO*dot_product(Vec_dnS,Vec_dnS) 
  res_test = AD_Check_dnS_IS_ZERO(dnS_ana-dnS,dnSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='dnS_ana-dnS==0?')
  IF (print_level > 0) THEN
    CALL Write_dnS(dnS    ,string=test_var%test_log,info='dot_product from dnVec')
    CALL Write_dnS(dnS_ana,string=test_var%test_log,info='dot_product from dnS(:)')
  END IF
  CALL Flush_Test(test_var)

  CALL dnVec_TO_dnS(dnV1,dnS,1)
  res_test = AD_Check_dnS_IS_ZERO(Vec_dnS(1)-dnS,dnSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='(dnVec_TO_dnS) Vec_dnS(1)-dnS==0?')
  IF (print_level > 0) THEN
    CALL Write_dnS(dnS    ,string=test_var%test_log,info='Vec_dnS(1)')
    CALL Write_dnS(dnS_ana,string=test_var%test_log,info='dnS')
  END IF
  CALL Flush_Test(test_var)

  CALL dnS_TO_dnVec(-Vec_dnS(1)*TWO,dnV1,2)
  CALL set_dnVec(dnV2,d0=[x,-2*x,z],                           &
                      d1=reshape([ONE ,-TWO,ZERO,              &
                                  ZERO,ZERO,ZERO,              &
                                  ZERO, ZERO,ONE],shape=[3,3]),&
                      d2=reshape([(ZERO,i=1,27)],shape=[3,3,3]))

  res_test = Check_dnVec_IS_ZERO(dnV2-dnV1,dnSerr_test)
  CALL Logical_Test(test_var,test1=res_test,info='(dnS_TO_dnVec) dnV1-dnV2==0?')
  IF (print_level > 0) THEN
    CALL Write_dnVec(dnV1,info='dnV1')
    CALL Write_dnVec(dnV2,info='dnV2')
  END IF
  CALL Flush_Test(test_var)



  ! matmul
  dnV1 = Variable_dnVec([HALF,TWO],nderiv=1)
  Mat = reshape([ONE,TWO,  THREE,FOUR,  FIVE,SIX],shape=[3,2])
  dnV2 = matmul(Mat,dnV1)

  FlatdnS = get_Flatten(dnV2)
  FlatdnS_ref = [real(kind=Rkind) :: 8.5,11.,13.5, 1.,2.,3., 4.,5.,6. ]
  res_test = maxval(abs(FlatdnS-FlatdnS_ref)) < ZeroTresh
  CALL Logical_Test(test_var,test1=res_test,info='matmul(mat,dnVec) dnV1-dnV2==0?')
  IF (print_level > 0) THEN
    CALL Write_dnVec(dnV1,info='dnV1')
    CALL Write_mat(Mat,nio=out_unit,info='Mat',nbcol=5)
    CALL Write_dnVec(dnV2,info='dnV2 (from matmul)')
  END IF
  CALL Flush_Test(test_var)

  Mat = transpose(Mat)
  dnV2 = matmul(dnV1,Mat)
  FlatdnS = get_Flatten(dnV2)
  FlatdnS_ref = [real(kind=Rkind) :: 8.5,11.,13.5, 1.,2.,3., 4.,5.,6. ]
  res_test = maxval(abs(FlatdnS-FlatdnS_ref)) < ZeroTresh
  CALL Logical_Test(test_var,test1=res_test,info='matmul(dnVec,mat) dnV1-dnV2==0?')
  IF (print_level > 0) THEN
    CALL Write_dnVec(dnV1,info='dnV1')
    CALL Write_mat(Mat,nio=out_unit,info='Mat',nbcol=5)
    CALL Write_dnVec(dnV2,info='dnV2 (from matmul)')
  END IF
  CALL Flush_Test(test_var)


  CALL Append_Test(test_var,'------------------------------------------------------',Print_res=.FALSE.)
  CALL Append_Test(test_var,'------------------------------------------------------',Print_res=.FALSE.)
  CALL Append_Test(test_var,'------------------------------------------------------',Print_res=.FALSE.)



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

END PROGRAM TEST_dnS

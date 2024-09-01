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
PROGRAM TEST_dnMat
  USE QDUtil_m
  USE QDUtil_Test_m
  USE ADdnSVM_m
  IMPLICIT NONE

    TYPE (dnMat_t)                   :: dnM1,dnM2
    TYPE (dnMat_t)                   :: dnV,dnM,dnDiag

    TYPE (dnS_t), allocatable        :: Vec_dnS(:),Mat_dnS(:,:)
    TYPE (dnS_t)                     :: dnS,dnS_ana

    TYPE (test_t)                    :: test_var
    logical                          :: val_test,res_test

    real (kind=Rkind)                :: x,y,z,r,th,err,maxdiff,maxdnS
    real (kind=Rkind), allocatable   :: JacNewOld(:,:),JacNewOld_ana(:,:)
    real (kind=Rkind), allocatable   :: FlatdnM(:),FlatdnM_ref(:),Mat(:,:)

    integer                          :: nderiv,nio_test,nderiv_Mat,err_dnMat
    real (kind=Rkind)                :: dnSerr_test = FIVE*ONETENTH**4
    real (kind=Rkind)                :: ZeroTresh   = ONETENTH**10

    integer                          :: i,j


    character (len=*), parameter :: name_sub='TEST_dnMat'

  CALL read_arg()

  CALL Initialize_Test(test_var,test_name='dnMat')

  nderiv = 1
  CALL Append_Test(test_var,'== TESTING dnMat module with nderiv= ' // TO_String(nderiv))

  x = 0.5_Rkind
  y = 2.0_Rkind
  Vec_dnS = Variable([x,y],nderiv=nderiv)


  allocate(Mat_dnS(2,3))
  DO j=1,size(Mat_dnS,dim=2)
    DO i=1,size(Mat_dnS,dim=1)
      Mat_dnS(i,j) = Vec_dnS(1)**i + Vec_dnS(2)**j
    END DO
  END DO
  dnM1 = Mat_dnS

  FlatdnM = get_Flatten(dnM1)
  FlatdnM_ref = [real(kind=Rkind) :: 2.5,2.25,4.5,4.25,8.5,8.25,   1.,1.,1.,1.,1.,1.,   1.,1.,4.,4.,12.,12.]
  res_test = all(FlatdnM_ref == FlatdnM)
  CALL Logical_Test(test_var,test1=res_test,info='init dnMat (rect)')

  !CALL Write_dnMat(dnM1, info='dnM1')

  dnM2 = transpose(dnM1)

  !CALL Write_dnMat(dnM2, info='transpose(dnM1)')
  FlatdnM = get_Flatten(dnM2)
  FlatdnM_ref = [real(kind=Rkind) :: 2.5,4.5,8.5,2.25,4.25,8.25,   1.,1.,1.,1.,1.,1.,   1.,4.,12.,1.,4.,12.]
  res_test = all(FlatdnM_ref == FlatdnM)
  CALL Logical_Test(test_var,test1=res_test,info='transpose dnMat (rect)')

  CALL Flush_Test(test_var)

   ! test the allocate / deallocate
  CALL dealloc_dnMat(dnM1,err_dnMat)
  res_test = (err_dnMat == 0)
  CALL Logical_Test(test_var,test1=res_test,info='dealloc_dnMat')

  CALL alloc_dnMat(dnM1,nsurf=2,nVar=2,nderiv=1,err_dnMat=err_dnMat)
  res_test = (err_dnMat == 0)
  CALL Logical_Test(test_var,test1=res_test,info='alloc_dnMat (square)')

  CALL dealloc_dnMat(dnM1,err_dnMat)
  CALL alloc_dnMat(dnM1,sizeL=2,sizeC=3,nVar=2,nderiv=1,err_dnMat=err_dnMat)
  res_test = (err_dnMat == 0)
  CALL Logical_Test(test_var,test1=res_test,info='alloc_dnMat (rectangle)')

  CALL dealloc_dnMat(dnM1,err_dnMat)
  CALL alloc_dnMat(dnM1,sizeL=2,nVar=2,nderiv=1,err_dnMat=err_dnMat)
  res_test = (err_dnMat /= 0)
  CALL Logical_Test(test_var,test1=res_test,info='alloc_dnMat (error)')

  CALL dealloc_dnMat(dnM1,err_dnMat)
  CALL alloc_dnMat(dnM1,sizeL=2,nsurf=2,nVar=2,nderiv=1,err_dnMat=err_dnMat)
  res_test = (err_dnMat /= 0)
  CALL Logical_Test(test_var,test1=res_test,info='alloc_dnMat (error)')

  CALL Flush_Test(test_var)

   ! test diago
  deallocate(Mat_dnS)
  allocate(Mat_dnS(2,2))
  DO j=1,size(Mat_dnS,dim=2)
  DO i=1,size(Mat_dnS,dim=1)
    Mat_dnS(i,j) = (Vec_dnS(1)+Vec_dnS(2))**(i+j-2)
  END DO
  END DO
  dnM=Mat_dnS
  CALL Write_dnMat(dnM, info='M')
  CALL DIAG_dnMat(dnMat=dnM,dnMatDiag=dnDiag,dnVec=dnV)
  CALL Write_dnMat(dnDiag, info='Diag')
  CALL Write_dnMat(dnV, info='V')
  dnM1 = matmul(dnM,dnV)
  dnM2 = matmul(transpose(dnV),dnM1) - dnDiag
  CALL Write_dnMat(dnM2, info='Vt.M.V - Diag')
  
  res_test = Check_dnMat_IS_ZERO(dnM2)
  CALL Logical_Test(test_var,test1=res_test,info='Diago')


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

END PROGRAM TEST_dnMat

#!/bin/bash

here=`pwd`

cd ..
  make cleanall
cd $here

rm -f ALL_Tests.log

for FC in gfortran
do

  for OPT in 0 1
  do
  for OMP in 0 1
  do
  for LAPACK in 0 1
  do
  for INT in 4 8
  do
  echo xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx                  >> ALL_Tests.log
  echo xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx                  >> ALL_Tests.log
  echo $FC  OPT $OPT OpenMP $OMP LAPACK $LAPACK INT $INT  >> ALL_Tests.log
  echo xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx                  >> ALL_Tests.log
  echo xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx                  >> ALL_Tests.log

  cd ..
     ext=$F90"_Opt"$OPT"_OMP"$OMP"_LAPACK"$LAPACK"_INT"$INT
     RES="res_QDLib_"$ext
     LOG="comp_"$ext".log"
     #make clean > $here/$LOG 2>&1
     make FC=$FC OPT=$OPT OMP=$OMP LAPACK=$LAPACK INT=$INT > $here/$LOG 2>&1
     ./Test_QDLib.x > $here/$RES
  cd  $here
  grep "Number of error(s)" $RES >> ALL_Tests.log
  done
  done
  done
  done
done

#!/bin/bash
here=`pwd`

./clean


cd ..
  make cleanall &> comp.log
cd $here

rm -f ALL_Tests.log
num=0

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
for RKIND in real32 real64 real128
do
  num=$(($num + 1))
  echo test number: $num
  echo xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx            >> ALL_Tests.log
  echo xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx            >> ALL_Tests.log
  echo test number: $num                                              >> ALL_Tests.log
  echo $FC  OPT $OPT OpenMP $OMP LAPACK $LAPACK INT $INT RKIND $RKIND >> ALL_Tests.log
  echo ---------------------------------------------------            >> ALL_Tests.log
  cd ..
    ext=$FC"_Opt"$OPT"_OMP"$OMP"_LAPACK"$LAPACK"_INT"$INT"_RKIND"$RKIND
    RES="res_dnSV_dnPoly_"$ext
    LOG="comp_"$ext".log"
    make FC=$FC OPT=$OPT OMP=$OMP LAPACK=$LAPACK INT=$INT RKIND=$RKIND ut > $here/$LOG 2>&1
    rm -f lib*a Test*.x
    mv Test.log $here/$RES
  cd  $here
  #grep TESTING $RES >> ALL_Tests.log
  grep ERROR $here/$LOG >> ALL_Tests.log
  awk  -F: 'BEGIN{test=0} /Number of tests/ {test+=$2} END {print "Number of tests: " test}'                 $RES >> ALL_Tests.log
	awk  -F: 'BEGIN{err=0}  /Number of error/ {err+=$2}  END {print "Number of error(s) for all tests: " err}' $RES >> ALL_Tests.log
done
done
done
done
done
done
echo xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx            >> ALL_Tests.log
echo xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx            >> ALL_Tests.log
echo "Number of options: " $num
awk  -F: 'BEGIN{test=0} /Number of tests/ {test+=$2} END {print "Number of tests for all options: " test}'   ALL_Tests.log
awk  -F: 'BEGIN{err=0}  /Number of error/ {err+=$2}  END {print "Number of error(s) for all test options: " err}' ALL_Tests.log

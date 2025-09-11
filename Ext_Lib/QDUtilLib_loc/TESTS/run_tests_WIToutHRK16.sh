#!/bin/bash

here=`pwd`

QD_DIR=$here/.. 
TEST_DIR=$here
OUT_DIR=$here/output
TEST_LOG=$TEST_DIR/ALL_Tests.log

cd $QD_DIR
  make cleanall &> comp.log
cd $TEST_DIR

rm -f $TEST_LOG
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
  echo xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx                       >> $TEST_LOG
  echo xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx                       >> $TEST_LOG
  echo test number: $num                                                         >> $TEST_LOG
  echo $FC  OPT $OPT OpenMP $OMP LAPACK $LAPACK INT $INT RKIND $RKIND WITHRK16=0 >> $TEST_LOG
  echo ---------------------------------------------------                       >> $TEST_LOG

  ext=$FC"_Opt"$OPT"_OMP"$OMP"_LAPACK"$LAPACK"_INT"$INT"_RKIND"$RKIND
  RES="res_QDLib_"$ext
  LOG="comp_"$ext".log"
  echo RES $RES >> $TEST_LOG
  echo LOG $LOG >> $TEST_LOG

  cd $QD_DIR
    make Test_QDLib.x FC=$FC OPT=$OPT OMP=$OMP LAPACK=$LAPACK INT=$INT RKIND=$RKIND WITHRK16=0 > $TEST_DIR/$LOG 2>&1
  cd $TEST_DIR
  if [ -f $QD_DIR/Test_QDLib.x ]
  then
    echo "File Test_QDLib.x exists." >> $TEST_LOG
    mkdir -p $OUT_DIR
    cd $OUT_DIR
    $QD_DIR/Test_QDLib.x > $TEST_DIR/$RES
    cd $TEST_DIR
    rm -rf output
  else
    echo "File Test_QDLib.x does not exist." >> $TEST_LOG
  fi
  cd $QD_DIR
    make clean        FC=$FC OPT=$OPT OMP=$OMP LAPACK=$LAPACK INT=$INT RKIND=$RKIND >> $TEST_DIR/$LOG 2>&1
    rm -f lib*.a Test_QDLib.x
  cd $TEST_DIR
  grep ERROR $TEST_DIR/$LOG >> $TEST_LOG
  if [ -f $RES ]
  then
    awk  -F: 'BEGIN{test=0} /Number of tests/ {test+=$2} END {print "Number of tests: " test}'                 $RES >> $TEST_LOG
	  awk  -F: 'BEGIN{err=0}  /Number of error/ {err+=$2}  END {print "Number of error(s) for all tests: " err}' $RES >> $TEST_LOG
  fi

done
done
done
done
done
done
echo xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx            >> $TEST_LOG
echo xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx            >> $TEST_LOG
echo "Number of options: " $num
awk  -F: 'BEGIN{test=0} /Number of tests/ {test+=$2} END {print "Number of tests for all options: " test}'        $TEST_LOG
awk  -F: 'BEGIN{err=0}  /Number of error/ {err+=$2}  END {print "Number of error(s) for all test options: " err}' $TEST_LOG

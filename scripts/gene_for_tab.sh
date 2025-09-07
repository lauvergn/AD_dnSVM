#!/bin/bash

DefType()
{
    x=$1
  #echo $x
  case $x in

  logical)
    echo "logical"
    ;;

  int32)
    echo "integer (kind=Ik4)"
    ;;

  int64)
    echo "integer (kind=Ik8)"
    ;;

  Rk4|Rk8|Rk16)
    echo "real (kind=$x)"
    ;;

  Ck4|Ck8|Ck16)
    r=`echo ${x/C/R}`
    echo "complex (kind=$r)"
    ;;

  *)
    echo "unknown"
    ;;
esac
}


rm -f QDUtil_Dim1XXX_TO_string.f90

for XXX in logical int32 int64 Rk4 Rk8 Rk16 Ck4 Ck8 Ck16
do
  YYY=`DefType $XXX `
  echo $XXX $YYY
  sed "s/XXX/$XXX/" QDUtil_Dim1XXX_TO_string.fgen > 1
  sed "s/YYY/$YYY/" 1 > 2

  cat 2 >> QDUtil_Dim1XXX_TO_string.f90
done
rm -f 1 2


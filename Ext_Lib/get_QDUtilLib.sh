#!/bin/bash

echo "In get_QDUtilLib.sh"
pwd

FC=$1
OPT=$2
OMP=$3
LAPACK=$4
ExtLibDIR=$5

ext_obj="_"$FC"_opt"$OPT"_omp"$OMP

SAVE_version=Save_QDUtilLib-0.4
LOC_version=QDUtilLib

test -f $ExtLibDIR/$LOC_version/"libQD"$ext_obj.a && exit 0

rm -rf QDUtilLib* #always remove the link


#latest release
 version=https://github.com/lauvergn/QDUtilLib/archive/refs/tags/v0.4.zip
#latest HEAD version
#version=https://github.com/lauvergn/QDUtilLib/archive/refs/heads/main.zip

curl -LJ $version --output $LOC_version.zip
test -e $LOC_version.zip && echo $LOC_version.zip file exist || cp $SAVE_version.zip $LOC_version.zip

unzip $LOC_version.zip
rm -f $LOC_version.zip


LIBDIR=`ls -d QDUtilLib*`
#echo $QMLDIR

ln -s $LIBDIR $LOC_version

cd $LIBDIR
  make lib FC=$FC OPT=$OPT OMP=$OMP LAPACK=$LAPACK ExtLibDIR=$ExtLibDIR

echo "End get_QDUtilLib.sh"
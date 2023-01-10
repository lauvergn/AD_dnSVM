#!/bin/bash

EXTLIB_TYPE=$1

echo "In get_QDUtilLib.sh"
pwd

SAVE_version=Save_QDUtilLib-0.5
LOC_version=QDUtilLib

rm -rf QDUtilLib* #always remove the link


#latest release
 version=https://github.com/lauvergn/QDUtilLib/archive/refs/tags/v0.5.zip
#latest HEAD version
#version=https://github.com/lauvergn/QDUtilLib/archive/refs/heads/main.zip

test -z $EXTLIB_TYPE       &&    curl -LJ $version --output $LOC_version.zip
test $EXTLIB_TYPE != 'loc' &&    curl -LJ $version --output $LOC_version.zip

test -e $LOC_version.zip && echo $LOC_version.zip file exist || cp $SAVE_version.zip $LOC_version.zip

unzip $LOC_version.zip
rm -f $LOC_version.zip


LIBDIR=`ls -d QDUtilLib*`
#echo $QMLDIR

ln -s $LIBDIR $LOC_version


echo "End get_QDUtilLib.sh"
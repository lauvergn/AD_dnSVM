rm -r QDUtilLib*

#latest release
 version=https://github.com/lauvergn/QDUtilLib/archive/refs/tags/v0.1.zip
#latest HEAD version
#version=https://github.com/lauvergn/QDUtilLib/archive/refs/heads/main.zip


curl -LJ $version --output QDUtilLib.zip
test -e QDUtilLib.zip && echo QDUtilLib.zip file exist || cp Save_QDUtilLib-0.1.zip QDUtilLib.zip

unzip QDUtilLib.zip
rm -f QDUtilLib.zip

LIBDIR=`ls -d QDUtilLib*`
#echo $QMLDIR

ln -s $LIBDIR QDUtilLib

cd $LIBDIR
  pwd
  make lib

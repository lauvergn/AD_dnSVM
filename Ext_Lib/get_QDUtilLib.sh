rm -r QDUtilLib*

SAVELIB=Save_QDUtilLib-0.2.zip

#latest release
 version=https://github.com/lauvergn/QDUtilLib/archive/refs/tags/v0.2.zip
#latest HEAD version
#version=https://github.com/lauvergn/QDUtilLib/archive/refs/heads/main.zip


curl -LJ $version --output QDUtilLib.zip
test -e QDUtilLib.zip && echo QDUtilLib.zip file exist || cp $SAVELIB QDUtilLib.zip

unzip QDUtilLib.zip
rm -f QDUtilLib.zip

LIBDIR=`ls -d QDUtilLib*`
#echo $QMLDIR

ln -s $LIBDIR QDUtilLib

cd $LIBDIR
  pwd
  make lib

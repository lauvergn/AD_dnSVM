#!/bin/bash

name_dep=scripts/dependencies.mk

mkdeps_dir=Ext_Lib/mk-fdeps-dev
mkdeps_exe=$mkdeps_dir/mk-fdeps
#echo $mkdeps_exe
#pwd

LibName=mk-deps
BRANCH=dev
echo $LibName $BRANCH

LibVersion=https://github.com/lauvergn/mk-fdeps/archive/refs/heads/dev.zip
echo $LibVersion

mkdir -p Ext_Lib

if [ ! -d "$mkdeps_dir" ] ; then
  echo "get the source file"
  curl -LJ $LibVersion --output Ext_lib/$LibName.zip
  unzip -d Ext_lib Ext_lib/$LibName.zip
  rm Ext_lib/$LibName.zip
fi

if [ ! -x "$mkdeps_exe" ] ; then
  echo "The executable "$mkdeps_exe" does not exist => compilation"
  make -C $mkdeps_dir all
  mv $mkdeps_dir/build/mk-fdeps $mkdeps_exe
  make -C $mkdeps_dir clean
fi

if [ -x "$mkdeps_exe" ] ; then
  echo "Create dependencies ..."
  ./$mkdeps_exe SRC/**/*.f90 --with-parent '$(OBJ_DIR)/'  --strip-parents 3 --output $name_dep
  echo "Create dependencies: done"
else
  echo "The executable "$mkdeps_exe" does not exist"
  exit
fi
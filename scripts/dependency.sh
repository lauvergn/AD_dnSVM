#!/bin/bash

name_dep=scripts/dependencies.mk
SRCFile=scripts/fortranlist.mk

list=`ls SRC/*.f90 SRC/*/*.f90`
ExcludeList=''

echo "#===============================================" > $name_dep
echo "#===============================================" > $SRCFile
echo "SRCFILES= \\" >> $SRCFile

for ff90 in $list
do
   ff=`awk '{name=$1 ; n=split(name,tab,"/") ; if (n > 0) {l=length(tab[n]) ; print tab[n]}}' <<< $ff90`
   #echo $ff
   if grep -vq $ff <<< $ExcludeList;  then
     echo $ff " \\" >> $SRCFile
     awk -f scripts/mod2file.awk $ff90 >> $name_dep
   fi

done
echo "#===============================================" >> $name_dep
for ff90 in $list
do
   ff=`awk '{name=$1 ; n=split(name,tab,"/") ; if (n > 0) {l=length(tab[n]) ; print tab[n]}}' <<< $ff90`
   #echo '# '$ff >> $name_dep
   if grep -vq $ff <<< $ExcludeList;  then
     mname=`awk -f scripts/get_modname.awk $ff90`
     #echo "#mname: " $mname >> $name_dep
     awk -v mod_name=$mname -f scripts/dep2.awk $ff90 >> $name_dep
   fi
done
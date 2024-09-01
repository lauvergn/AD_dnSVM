#!/bin/bash

BaseName=$1
Branch_in=$2

DIR=$3

if (test -z "$Branch_in" ) then
  Branch=main
else
  Branch=$Branch_in
fi


#github versions
version="https://github.com/lauvergn/"$BaseName"/archive/refs/heads/"$Branch".zip"

#https://github.com/lauvergn/QDUtilLib/archive/refs/heads/dev.zip
#https://github.com/lauvergn/QDUtilLib/archive/refs/heads/main.zip

echo In get_Lib.sh $BaseName


ExtLibSAVEDIR=/Users/lauvergn/git/Ext_Lib

LOC_version=$BaseName


if (! test -z "$DIR" ) then
if (test -d "$DIR" ) then
  rm -f $LOC_version
  echo the DIR variable is not empty and $DIR directory exist. Adding the link.
  ln -s $DIR $LOC_version
  if (test -L $LOC_version -a -d $LOC_version) then
    echo $LOC_version directory exist and is a symbolic link. 
    ls -la $LOC_version
    echo End get_Lib.sh $BaseName
    exit 0
  fi
fi
fi

if (test -L $LOC_version -a -d $LOC_version) then
  echo $LOC_version directory exist and is a symbolic link. 
  echo The symbolic link is unchanged.
  ls -la $LOC_version
  echo End get_Lib.sh $BaseName
  exit 0
else
  echo $LOC_version directory does not exist.
  if (test -d $LOC_version"_loc") then
    echo $LOC_version"_loc" directory exist. Adding the link.
    rm -rf $LOC_version"_loc"/.git $LOC_version"_loc"/Ext_Lib/*_loc
    ln -s $LOC_version"_loc" $LOC_version
    if (test -L $LOC_version -a -d $LOC_version) then
      echo $LOC_version directory exist and is a symbolic link.
      ls -la $LOC_version
      echo End get_Lib.sh $BaseName
      exit 0
    fi
  fi
fi


#1) try to get from github
#latest HEAD version (defined at the beginning of the script)
echo "GitHub version:" $version
curl -LJ $version --output $LOC_version.zip
zipfile=$LOC_version.zip
if (test -f $zipfile) then
  echo $zipfile file exist
  DIRName=`unzip -Z -1 $zipfile | head -1 `
  unzip $zipfile
  mv $DIRName $LOC_version"_loc"
  if (test -d $LOC_version"_loc") then
    rm -rf $LOC_version"_loc"/.git $LOC_version"_loc"/Ext_Lib/*_loc
    ln -s $LOC_version"_loc" $LOC_version
  fi
  rm -f $zipfile
else
  echo $LOC_version.zip from github does not exist.
fi

if (test -L $LOC_version -a -d $LOC_version) then
  echo $LOC_version "directory exist (from github) and is a symbolic link."
  ls -la $LOC_version
  echo End get_Lib.sh $BaseName
  exit 0
fi



#2) try to get from a local directory $ExtLibSAVEDIR
echo Try to get a zip file from: $ExtLibSAVEDIR
test -d $ExtLibSAVEDIR || (echo $ExtLibSAVEDIR directory des not exist! ; exit 1 )
zipfile=$ExtLibSAVEDIR"/Save_"$BaseName"_devloc.zip"
if (test -f $zipfile) then
  echo $zipfile file exist
  DIRName=`unzip -Z -1 $zipfile | head -1 `
  unzip $zipfile
  mv $DIRName $LOC_version"_loc"
  rm -rf $LOC_version"_loc"/.git $LOC_version"_loc"/Ext_Lib/*_loc
  ln -s $LOC_version"_loc" $LOC_version
fi

#3) last test
if (test -L $LOC_version -a -d $LOC_version) then
  echo $LOC_version file exist from $ExtLibSAVEDIR
  ls -la $LOC_version
  echo End get_Lib.sh $BaseName
  exit 0
else
  echo $LOC_version file does not exist from
  echo End get_Lib.sh $BaseName
  exit 1
fi


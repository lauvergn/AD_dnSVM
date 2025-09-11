#!/bin/bash

ExtLibSAVEDIR=/Users/lauvergn/git/Ext_Lib
BaseName=QDUtilLib
test -d $ExtLibSAVEDIR || (echo $ExtLibDIR "does not exist" ; exit 1)
$ExtLibSAVEDIR/makezip.sh $BaseName
#cd $ExtLibSAVEDIR ; ./cp_QDUtil.sh
echo "  done zip"


#!/bin/bash

iFileXYZ=$1   # XYZ or RXYZ file
iFileOutputADF=$2   # ADF output file
alpha=$3
layout=$4

[ -z "$alpha" ] && alpha="1.1"
[ -z "$layout" ] && layout="neato"

if [ -z "$iFileXYZ" -o -z "$iFileOutputADF" ]
then
	echo "Usage: viewADFGraph.sh iFileXYZ iFileOutputADF [layout]"
	echo "                                                 neato"
	exit 0
fi

if [ -n "$iFileOutputADF" ]
then
	spinDesities=` \
		gawk '
		BEGIN{
			loc=0
			blk=0
		}
		{
			if($1=="Populations") loc=0
			if(loc==1 && blk==3 && $1~/[[:digit:]]+/){
				if($5=="A:") printf $4" "; else printf "0.0000 "
			}
			if($0~/M U L L I/){ loc=1; blk+=1 }
		}' $iFileOutputADF \
		| gawk '
		{
			for(i=1;i<=NF;i++)
				if(i!=NF) printf "\\\n"$i","
				else print "\\\n"$i
		}'`

	molecule.graph $iFileXYZ $alpha ${iFileXYZ%.*}.dot $spinDesities > /dev/null
else
	molecule.graph $iFileXYZ $alpha ${iFileXYZ%.*}.dot > /dev/null
fi

dot -K$layout -Tpng -O -Goverlap=scale -Gsep=0.5 -Nfontsize=25 -Efontsize=25 -Gepsilon=0.001 -Gmaxiter=1000 -Gmargin=0.0,0.0 ${iFileXYZ%.*}.dot &> /dev/null
convert -page 500x500 ${iFileXYZ%.*}.dot.png ${iFileXYZ%.*}.eps &> /dev/null
rm ${iFileXYZ%.*}.dot ${iFileXYZ%.*}.dot.png &> /dev/null


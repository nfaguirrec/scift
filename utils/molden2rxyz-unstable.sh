#!/bin/bash

for f in `ls *.molden`
do
	echo "$f --> ${f%.*}.rxyz"
# 	mv $f ${f%.*}.molden
	molecule.rotate -i $f > ${f%.*}.rxyz
	energy=`awk '($1=="_ENERGY="){print $2}' $f`
	sed -i 's/'$f'/Energy = '$energy'/g' ${f%.*}.rxyz
	echo "" >> ${f%.*}.rxyz
	awk 'BEGIN{loc=0}{ if($1~/^\[/) loc=0; if(loc==1) print $0; if($1=="[FREQ]"){ loc=1; print "FREQUENCIES @NFREQ"} }' $f >> ${f%.*}.rxyz
	nfreq=`molecule.fv $f`
	sed -i 's/@NFREQ/'`echo $nfreq`'/g' ${f%.*}.rxyz
	echo "" >> ${f%.*}.rxyz
done
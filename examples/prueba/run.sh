#!/bin/bash

cp ../caffeine.xyz molecule.xyz

for i in `seq 1 100`
do
# 	molecule.random -i molecule.xyz > molecule1.xyz
	cp molecule.xyz molecule1.xyz
	molecule.vrandom -i molecule1.xyz -E 10.0 -o salida.xyz -v salida.vxyz
	molecule.viewVXYZ salida.xyz salida.vxyz
	
	id=`printf "%05d\n" $i`
	echo $id
	mv salida.png mol$id.png
	
	cp molecule1.xyz molecule.xyz
done

mencoder -mc 0 -noskip -skiplimit 0 -ovc lavc -lavcopts vcodec=msmpeg4v2:vhq "mf://mol*.png" -mf type=png:fps=5 -o output.avi
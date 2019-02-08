#!/bin/bash

format=$1 # xyz or rxyz
targetDir=$2
referenceDir=$3
filter=$4 # i.e. H--C--H
bondScale=$5

[ -z "$bondScale" ] && bondScale=1.1

cat /dev/null > output-$$

for f in `ls $targetDir | grep -E ".$format$"`
do
	molecule.angles $referenceDir/$f $bondScale | grep "$filter" > fileRef-$$
	molecule.angles $targetDir/$f $bondScale | grep "$filter" > file-$$
	
	nAngles1=`cat fileRef-$$ | wc -l`
	nAngles2=`cat file-$$ | wc -l`
	
	if [ "$nAngles1" -ne "$nAngles2" ]
	then
		echo "### ERROR ### Number of angles are not the same between target and reference molecules" > /dev/stderr
		echo "              Consider to increase the bondScale" > /dev/stderr
		echo "              Molecule $f is not going to be included in the analysis" > /dev/stderr
	else
		paste fileRef-$$ file-$$ >> output-$$
	fi
done

rm fileRef-$$ file-$$

# Check consistency
gawk '( $2==$7 && $3==$8 && $4==$9 ){ print $0 }' output-$$ > output2-$$

minValueRef=`sort -k5 -n output2-$$ | head -n1 | awk '{print $5}'`
maxValueRef=`sort -k5 -n output2-$$ | tail -n1 | awk '{print $5}'`

minValue=`sort -k10 -n output2-$$ | head -n1 | awk '{print $10}'`
maxValue=`sort -k10 -n output2-$$ | tail -n1 | awk '{print $10}'`

minEff=`echo $minValueRef $minValue | gawk '{ if( $1<$2 ) print $1; else print $2 }'`
maxEff=`echo $maxValueRef $maxValue | gawk '{ if( $1>$2 ) print $1; else print $2 }'`


R2=`gawk '
	BEGIN{
		res=0.0
		sum+=0.0
		i=0
	}
	{$0!~/[[:blank:]]*$/}{
		y[i] = $5
		f[i] = $10
		i+=1
	}
	END{
		n = i
		
		avery = 0.0
		for( i=0; i<n; i++ ) avery += y[i]
		avery = avery/n
		
		SStot = 0.0; SSres = 0.0
		for( i=0; i<n; i++ ){ SStot += (y[i]-avery)^2 ; SSres += (y[i]-f[i])^2 }
		
		if( SStot < 1e-15 )
			print 1.0
		else
			print 1.0-SSres/SStot
	}
	' output2-$$`
	
RMSE=`gawk '
	BEGIN{
		i=0
	}
	{$0!~/[[:blank:]]*$/}{
		y[i] = $5
		f[i] = $10
		i+=1
	}
	END{
		n = i
		
		aver = 0.0
		for( i=0; i<n; i++ ) aver += (y[i]-f[i])^2
		
		print sqrt(aver/n)
	}
	' output2-$$`
	
SDR=`gawk '
	BEGIN{
		i=0
	}
	{$0!~/[[:blank:]]*$/}{
		y[i] = $5
		f[i] = $10
		i+=1
	}
	END{
		n = i
		
		aver = 0.0
		for( i=0; i<n; i++ ) aver += y[i]-f[i]
		aver = aver/n
		
		stdev = 0.0
		for( i=0; i<n; i++ ) stdev += ( y[i]-f[i]-aver )^2
		
		print sqrt(stdev/n)
	}
	' output2-$$`

cat > plot-$$.gnuplot << EOF
set terminal qt size 1024,768 font ",18" enhanced

set size square
set key inside left top

set xlabel "Angle REFERENCE `echo $filter | sed 's/--/-/g'` (deg.)"
set ylabel "Angle `echo $filter | sed 's/--/-/g'` (deg.)"

R2=$R2
SDR=$SDR
RMSE=$RMSE

margin = 0.05*($maxEff-($minEff))

if( "labels-$labels" eq "labels-TRUE" ){
	plot [$minEff-margin:$maxEff+margin] [$minEff-margin:$maxEff+margin] \\
		x w l t sprintf(" R^2 = %.5f",R2) lc rgb "white", \\
		x w l t sprintf("RMSE = %.5f",RMSE) lc rgb "white", \\
		x w l t sprintf(" SDR = %.5f",SDR) lc rgb "white", \\
		"output2-$$" u 5:10 notitle w p ps 1.0 pt 7 lc rgb "red", \\
		"output2-$$" u 5:10:1 notitle w labels left font ",9", \\
		for [i=1:10] x+i*0.1*abs($maxValueRef-($minValueRef)) notitle w l lt 0 lw 0.8 lc rgb "black", \\
		for [i=1:10] x-i*0.1*abs($maxValueRef-($minValueRef)) notitle w l lt 0 lw 0.8 lc rgb "black", \\
		x notitle w l lw 1.5 lc rgb "blue"
}else{
	plot [$minEff-margin:$maxEff+margin] [$minEff-margin:$maxEff+margin] \\
		x w l t sprintf("R^2 = %.5f",R2) lc rgb "white", \\
		x w l t sprintf("RMSE = %.5f",RMSE) lc rgb "white", \\
		x w l t sprintf("SDR = %.5f",SDR) lc rgb "white", \\
		"output2-$$" u 5:10 notitle w p ps 1.0 pt 7 lc rgb "red", \\
		for [i=1:10] x+i*0.1*abs($maxValueRef-($minValueRef)) notitle w l lt 0 lw 0.8 lc rgb "black", \\
		for [i=1:10] x-i*0.1*abs($maxValueRef-($minValueRef)) notitle w l lt 0 lw 0.8 lc rgb "black", \\
		x notitle w l lw 1.5 lc rgb "blue"
}

pause -1
EOF

cat plot-$$.gnuplot
gnuplot plot-$$.gnuplot

gawk '{ print ($10-$5) }' output2-$$ > output3-$$
histogram.sh -f output3-$$ -c 1 -p \
"
set size 1.0,0.5
set encoding iso_8859_1
set xlabel 'Angle Error (deg.)'
set linetype 1 lc rgb 'red'
"

rm plot-$$.gnuplot
rm output-$$
rm output2-$$
rm output3-$$

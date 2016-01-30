#!/bin/bash
##################################################################
#
#  This file is part of scift (Scientific Fortran Tools).
#  Copyright (C) by authors (2010-2014)
#  
#  Authors (alphabetic order):
#    * Aguirre N.F. (nfaguirrec@gmail.com)  (2012-2014)
#  
#  Contributors (alphabetic order):
#  
#  Redistribution and use in source and binary forms, with or
#  without modification, are permitted provided that the
#  following conditions are met:
#  
#   * Redistributions of binary or source code must retain
#     the above copyright notice and this list of conditions
#     and/or other materials provided with the distribution.
#   * All advertising materials mentioning features or use of
#     this software must display the following acknowledgement:
#     
#     This product includes software from scift
#     (Scientific Fortran Tools) project and its contributors.
#
##################################################################

function usage(){
        echo "Usage:"
        echo "   $ histogram.sh -f file -c columns [-n nbars] [-t type] [-d dimensions] [-h]"
        echo ""
        echo "   -f file"
        echo "      Input file name"
        echo "   -c columns"
        echo "      Columns to consider in the histogram"
        echo "      ( i.e. 2 for one dimension or 3,4 for tow dimensions )"
        echo "   -n nbars"
        echo "      Number of bars to include in the plot"
        echo "      (default: 30 if n=1 or 30,30 if n=2)"
        echo "   -t type"
        echo "      Type of histogram (freq or counts)"
        echo "      (default: freq)"
        echo "   -d dimensions"
        echo "      Number of dimensions"
        echo "      (default: 1)"
        echo "   -h"
        echo "      This will print this same message"
        echo ""
}

#############################################
#
#############################################
function hist1D()
{
	local iFile=$1
	local cols=$2
	local nbars=$3
	local type=$4
	
	col=`echo $cols | gawk 'BEGIN{FS=","}{print $1}'` # por si pasan como parametro algo como 3,3
	nbars=`echo $nbars | gawk 'BEGIN{FS=","}{print $1}'` # por si pasan como parametro algo como 30,30
	
	awk '( $0!~/^[[:blank:]]*$/ && $0!~/^[[:blank:]]*#/ ){ printf "%20.10f\n", $'`echo $col`' }' $iFile > .tmpFile$$
	min=`sort -n .tmpFile$$ | head -n1`
	max=`sort -n .tmpFile$$ | tail -n1`
	h=`echo "($max-($min))/($nbars)" | bc -l`
	
	gawk '
	function floor(x) {
		y = int(x)
		return y > x ? y - 1 : y
	}
	
	function ceil(x) {
		y = int(x)
		return y < x ? y + 1 : y
	}
	
	BEGIN{
		x0='`echo $min`'
		h='`echo $h`'
	}

	{
		n=ceil(($1-x0)/h)
		
		if( n in mymap ){
			mymap[n]+=1
		}else{
			mymap[n]=1
		}
	}

	END{
		sum=0.0
		
		for( n in mymap ){
			sum+=mymap[n]
		}
		
		for( n in mymap ){
			if( "'`echo $type`'" == "freq" ){
				printf "%20.10f%20.10f\n", x0+(n-0.5)*h, mymap[n]/(h*sum)
			}else if( "'`echo $type`'" == "counts" ){
				printf "%20.10f%20.10f\n", x0+(n-0.5)*h, mymap[n]
			}
		}
	}
	' .tmpFile$$ | sort -n -k 1 > .data$$
	
	case $type in
		"freq")
			freqLabel="Frequency"
			formatY="%.3f"
			;;
		"counts")
			freqLabel="Counts"
			formatY="%.0f"
			;;
	esac
	
	cat >> .plot$$ << EOF
		tripdf(x,a)= abs(x)<=a ? (1.0-abs(x)/a)/a : 0.0
		
		set size square
		unset key
		set xtics out
		set ytics out
# 		set format x "%.1f"
		set format y "$formatY"
		set xlabel "Value"
		set ylabel "$freqLabel"
		set style fill solid 1.00 border -1
		bw=($max-$min)/$nbars
		set boxwidth bw absolute
		set y2range [0:]
		plot [$min-2*bw:$max+2*bw] [0:] ".data$$" i 0 u 1:2 w boxes  #, ".data$$" w p
		pause -1
EOF
	
	gnuplot -p .plot$$
	
	if [ -n "$oDataFile" ]
	then
		cat .data$$ > $oDataFile
	fi
	
# 	rm .data$$ .plot$$ .tmpFile1$$ .tmpFile$$
	rm .data$$ .plot$$ .tmpFile$$
}

#############################################
#
#############################################
function hist2D()
{
	local iFile=$1
	local cols=$2
	local nbars=$3
	local type=$4
	
	# Se eliminan los espacios en blanco y los comentarios
	# por lo tanto, todos los experimentos entran en el
	# mismo análisis
# 	sed '/^[[:blank:]]*$/d;/#/d' $iFile > .tmpFile$$
	
	col1=`echo $cols | gawk 'BEGIN{FS=","}{print $1}'`
	col2=`echo $cols | gawk 'BEGIN{FS=","}{print $2}'`
	
	gawk '($0!~/^[[:blank:]]*$/ && $0!~/^#/){printf("%20.10f%20.10f\n",$'$col1',$'$col2')}' $iFile > .tmpFile$$
	
	# Se capturan los parámetros para el histograma
	nbars1=`echo $nbars | gawk 'BEGIN{FS=","}{print $1}'`
	min1=`cat .tmpFile$$ | sort -k 1 -n | head -n1 | gawk '{printf("%20.10f\n",$1)}'`
	max1=`cat .tmpFile$$ | sort -k 1 -n | tail -n1 | gawk '{printf("%20.10f\n",$1)}'`
	h1=`echo "($max1-($min1))/$nbars1" | bc -l`
	
	nbars2=`echo $nbars | gawk 'BEGIN{FS=","}{print $2}'`
	min2=`cat .tmpFile$$ | sort -k 2 -n | head -n1 | gawk '{printf("%20.10f\n",$2)}'`
	max2=`cat .tmpFile$$ | sort -k 2 -n | tail -n1 | gawk '{printf("%20.10f\n",$2)}'`
	h2=`echo "($max2-($min2))/$nbars2" | bc -l`
	
	gawk '
	function floor(x) {
		y = int(x)
		return y > x ? y - 1 : y
	}
	
	function ceil(x) {
		y = int(x)
		return y < x ? y + 1 : y
	}
	
	BEGIN{
		xb='`echo $min1`'
		xe='`echo $max1`'
		hx='`echo $h1`'
		
		yb='`echo $min2`'
		ye='`echo $max2`'
		hy='`echo $h2`'
	}

	{
		nx=ceil(($1-xb)/hx)
		ny=ceil(($2-yb)/hy)
		
		if( (nx,ny) in mymap ){
			mymap[nx,ny]+=1
		}else{
			mymap[nx,ny]=1
		}
	}

	END{
		sum=0.0
		
		x=xb
		for( nx=0; x<=xe; nx++ ){
			x = xb + nx*hx
			
			y=yb
			for( ny=0; y<=ye; ny++ ){
				y = yb + ny*hy
				
				if( (nx,ny) in mymap )
					sum += mymap[nx,ny]
			}
		}
		
		x=xb
		for( nx=0; x<=xe; nx++ ){
			x = xb + nx*hx
			
			y=yb
			for( ny=0; y<=ye; ny++ ){
				y = yb + ny*hy
				
				if( "'`echo $type`'" == "freq" ){
				
					if( (nx,ny) in mymap )
						printf "%20.10f%20.10f%20.10f\n", x, y, mymap[nx,ny]/(hx*hy*sum)
					else
						printf "%20.10f%20.10f%20.10f\n", x, y, 0.0
						
				}else if( "'`echo $type`'" == "counts" ){
				
					if( (nx,ny) in mymap )
						printf "%20.10f%20.10f%20.10f\n", x, y, mymap[nx,ny]
					else
						printf "%20.10f%20.10f%20.10f\n", x, y, 0.0
						
				}
			}
			
			print ""
		}
	}
	' .tmpFile$$ > .data$$
	
	minZ=`cat .data$$ | sort -k 3 -n | head -n1 | gawk '{printf("%20.10f\n",$3)}'`
	maxZ=`cat .data$$ | sort -k 3 -n | tail -n1 | gawk '{printf("%20.10f\n",$3)}'`
	
	cat >> .plot$$ << EOF
set termopt enhanced
set encoding iso_8859_1
set size square
set format y "%.1f"
# set format x "%.1f"
# set format cb "10^{%T}"

# set logscale cb

unset key
set palette defined ( 0 "white", 1 "blue", 2 "green", 3 "orange", 4 "yellow", 5 "red" )

# set pm3d implicit at s
# set xtics rotate by -45 font "Serif,8"
set ytics font "Serif,8"
set cbtics font "Serif,8"
# set pointsize 0.3
# set view map

set mxtics 10

margin=0.05

range1=abs($max1-$min1)
set xrange [$min1-margin*range1:$max1+margin*range1]
print "X = [", $min1, ", ", $max1, "]"

range2=abs($max2-$min2)
set yrange [$min2-margin*range2:$max2+margin*range2]
print "Y = [", $min2, ", ", $max2, "]"

# set cbrange [$minZ+0.001:$maxZ]
set cbrange [$minZ:0.6*$maxZ]
print "Z = [", $minZ, ", ", $maxZ, "]"

set xlabel 'X' font 'Serif,9'
set ylabel 'Y' font 'Serif,9'
# splot '.data$$' u 1:((\$2>0)?\$2:10.0):3 w p ps 0
# splot '.data$$' u 1:2:3 w p ps 0

# set dgrid3d 20,20,1
# set cntrparam levels incremental 0.2,0.05,1.2
# set contours
# 
# set table 'val.cont'
# splot ".data$$" u 1:2:3 w l
# unset table

# set multiplot

plot \
'.data$$' u 1:2:3 notitle with points pt 5 ps 2.0 palette

# , \
# for [n=1:20] "val.cont" i n u 1:2 w l lt -1 lw 0.5 

# unset multiplot

pause -1
EOF

	gnuplot -p .plot$$
	
	if [ -n "$oDataFile" ]
	then
		cat .data$$ > $oDataFile
	fi

	rm .data$$ .plot$$ .tmpFile$$
}

function main()
{
	local iFile=""
	local columns=""
	local nbars="30,30"
	local type="freq"
	local dimensions="1"
	local oDataFile=""
	
	while getopts "f:c:n:t:d:hs:" OPTNAME
	do
		case $OPTNAME in
			f)
				iFile=$OPTARG
				;;
			c)
				columns=$OPTARG
				;;
			n)
				nbars=$OPTARG
				;;
			t)
				if [ "$OPTARG" = "freq" -o "$OPTARG" = "counts" ]
				then
					type=$OPTARG
				else
					usage
					exit 0
				fi
				;;
			d)
				dimensions=$OPTARG
				;;
			s)
				oDataFile=$OPTARG
				;;
			h)
				usage
				exit 0
				;;
		esac
	done
	
	if [ -z "$iFile" -o -z "$columns" ]
	then
		usage
		exit 0
	fi
	
	case $dimensions in
		1)
			hist1D $iFile $columns $nbars $type
			;;
		2)
			hist2D $iFile $columns $nbars $type
			;;
		*)
			echo "### ERROR ### dimensions = $dimensions is not implemented"
			exit 0
			;;
	esac
}

main $*
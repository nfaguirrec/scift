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
        echo "   $ histogram.sh -f file -c columns [-n nbars] [-t type] [-d dimensions] [-h] [-s file]"
        echo ""
        echo "   -f file"
        echo "      Input file name"
        echo "   -b block"
        echo "      It allows you to select the specific block. Blocks are separated by "
        echo "   -l blockLocator"
        echo "      Blocks are considered that start with a line that match with blockLocator"
        echo "      (default = \"^[[:blank:]]*$\")"
        echo "   -c columns"
        echo "      Columns to consider in the histogram"
        echo "      ( i.e. 2 for one dimension or 3,4 for two dimensions )"
        echo "   -n nbars"
        echo "      Number of bars to include in the plot"
        echo "      (default: 30 if n=1 or 30,30 if n=2)"
        echo "   -t type"
        echo "      Type of histogram (freq or counts)"
        echo "      (default: freq)"
        echo "   -d dimensions"
        echo "      Number of dimensions"
        echo "      (default: 1)"
        echo "   -s file"
        echo "      Saves histrogram in file"
        echo "   -x "
        echo "      No plot"
        echo "   -p "
        echo "      Additional gnuplot commands. i.e. -p \"set size square; set title \"Test\"\""
        echo "   -h"
        echo "      This will print this same message"
        echo ""
}

##
# @brief
##
function getBlock()
{
	local iFile=$1
	local locator=$2
	local nSkipBlocks=$3
	local nSkipBlanks=$4
# 	local mergeLocators=$5   # it is already TRUE
	
	gawk '
		BEGIN{
			loc=0
			nBlanks=0
			nBlocks=0
			seq = 0
			firstTime = 1
		}
		
		{
# 			print "0*", $0
			
			if( loc == 1 && $0~/^[[:blank:]]*$/ ){
				nBlanks += 1
# 				print "1*", $0, loc, nBlanks, nBlocks, seq
				next
			}
			
			if( nBlanks == '$(($nSkipBlanks+1))' ){
# 				print "2**", $0, loc, nBlanks, nBlocks, seq
				exit
			}
			
			if( nBlanks == '$nSkipBlanks' && loc == 1 ){
# 				print "3***", $0, loc, nBlanks, nBlocks, seq
				print $0
			}
			
			if( $0~/'"$locator"'/ ){
				if( nBlocks-1 == '$nSkipBlocks' ){
					loc = 1
# 					print $0   # print header ?
				}
				
				if( seq != 1 )
					nBlocks+=1
					
				seq = 1
				firstTime = 0
				
# 				print "4****", loc, nBlanks, nBlocks, seq
			}else{
				seq = 0
			}
		}
	' $iFile
}

##
# @brief
##
function averFromHistogram()
{
	local iFile=$1
	
	gawk '
	BEGIN{
		sum = 0.0
		sumW = 0.0
	}
	{
		sum += $2*$1
		sumW += $2
	}
	END{
		print sum/sumW
	}
	' $iFile
}

##
# @brief
##
function stdevFromHistogram()
{
	local iFile=$1
	
	aver=`averFromHistogram $iFile`
	
	gawk '
	BEGIN{
		sum = 0.0
		sumW = 0.0
		n = 0
	}
	{
		sum += $2*($1-'$aver')**2
		sumW += $2
		
		if( $2 > 1e-8 )  n += 1
	}
	END{
		print sqrt(sum/((n-1)*sumW)/n)
	}
	' $iFile
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
		n=ceil(($1-x0)/h)+0.5  # Original equation is +1, but with 0.5 point is located in the middel of the interval
		
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
				printf "%20.10f%20.10f\n", x0+(n-0.5)*h, mymap[n]/sum
			}else if( "'`echo $type`'" == "counts" ){
				printf "%20.10f%20.10f\n", x0+(n-0.5)*h, mymap[n]
			}
		}
	}
	' .tmpFile$$ | sort -n -k 1 > .data$$
	
	case $type in
		"freq")
			freqLabel="Frequency"
			formatY="%.2f"
			;;
		"counts")
			freqLabel="Counts"
			formatY="%.0f"
			;;
	esac
	
	cat >> .plot$$ << EOF
tripdf(x,a)= abs(x)<=a ? (1.0-abs(x)/a)/a : 0.0

unset key
set grid front
unset grid
set format y "$formatY"
set xlabel "Value"
set ylabel "$freqLabel"
set style fill solid 1.00 border -1
bw=($max-$min)/$nbars
set boxwidth bw absolute
set yrange [0:] 

margin=0.05

range=abs($max-$min)
set xrange [$min-margin*range:$max+margin*range]

$GNUPLOT_PRECOMMANDS

plot "-" i 0 u 1:2 w boxes lt 1  #, ".data$$" w p
`cat .data$$`
	EOF
	
# X = [ $min, $max ]
# aver  = `averFromHistogram .data$$`
# stdev = `stdevFromHistogram .data$$`
EOF

	if [ "$SHOW_PLOT" = "TRUE" ]
	then
		echo "pause -1" >> .plot$$
	fi
	
	cat .plot$$
	gnuplot .plot$$
	
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
						printf "%20.10f%20.10f%20.10f\n", x, y, mymap[nx,ny]/sum
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

# set format cb "10^{%T}"
# set logscale cb

unset key
set palette defined ( 0 "white", 1 "blue", 2 "green", 3 "orange", 4 "yellow", 5 "red" )

margin=0.05

range1=abs($max1-$min1)
set xrange [$min1-margin*range1:$max1+margin*range1]
print "X = [", $min1, ", ", $max1, "]"

range2=abs($max2-$min2)
set yrange [$min2-margin*range2:$max2+margin*range2]
print "Y = [", $min2, ", ", $max2, "]"

# set cbrange [$minZ:$maxZ]
print "Z = [", $minZ, ", ", $maxZ, "]"

$GNUPLOT_PRECOMMANDS

set grid front
unset grid

plot \
'-' u 1:2:3 notitle with points pt 5 ps 2.0 palette
`cat .data$$`
	EOF

EOF

	if [ "$SHOW_PLOT" = "TRUE" ]
	then
		echo "pause -1" >> .plot$$
	fi
	
	cat .plot$$
	gnuplot .plot$$
	
	if [ -n "$oDataFile" ]
	then
		cat .data$$ > $oDataFile
	fi

	rm .data$$ .plot$$ .tmpFile$$
}

function main()
{
	local iFile=""
	local targetBlock="0"
	local blockLocator=""
	local columns=""
	local nbars="30,30"
	local type="freq"
	local dimensions="1"
	local oDataFile=""
	
	SHOW_PLOT="TRUE"
	GNUPLOT_PRECOMMANDS=""
	
	while getopts "f:b:l:c:n:t:d:hs:xp:" OPTNAME
	do
		case $OPTNAME in
			f)
				iFile="$OPTARG"
				;;
			b)
				targetBlock="$OPTARG"
				;;
			l)
				blockLocator="$OPTARG"
				;;
			c)
				columns="$OPTARG"
				;;
			n)
				nbars="$OPTARG"
				;;
			t)
				if [ ""$OPTARG"" = "freq" -o ""$OPTARG"" = "counts" ]
				then
					type="$OPTARG"
				else
					usage
					exit 0
				fi
				;;
			d)
				dimensions="$OPTARG"
				;;
			s)
				oDataFile="$OPTARG"
				;;
			x)
				SHOW_PLOT="FALSE"
				;;
			p)
				GNUPLOT_PRECOMMANDS="$OPTARG"
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
	
	if [ -n "$blockLocator" ]
	then
		getBlock $iFile $blockLocator $targetBlock 0 > .tmp$$
		iFile=.tmp$$
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
	
	if [ -n "$blockLocator" ]
	then
		rm .tmp$$
	fi
}

main "$@"

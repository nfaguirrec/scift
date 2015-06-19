#!/bin/bash

iFile=$1
columns=$2        # 1,2,3
centered=$3       # true | false
logScale=$4       # true | false
viewSpectrum=$5   # NONE | COS1 | COS2 | COS3 | GAUSS3 | GAUSS4 | ERFTOPHAT1 | ERF_TOPHAT2
oFile=$6

pid=$$

###################################
# 
###################################
plotAllSpectra(){

	local commentLog="#"
	if [ "$logScale" = "true" ]
	then
		commentLog=""
	elif [ "$logScale" = "false" ]
	then
		commentLog="#"
	fi
	
	cat >> .plot-$pid << EOF
# set termopt enhanced
set size square

$commentLog set logscale y; set format y "%.1e"
set key out

plot [:] [:] \
".outputNONE-$pid" t "NONE" w l lt 0, \
".outputCOS1-$pid" t "COS1" w l, \
".outputCOS2-$pid" t "COS2" w l, \
".outputCOS3-$pid" t "COS3" w l, \
".outputGAUSS3-$pid" t "GAUSS3" w l, \
".outputGAUSS4-$pid" t "GAUSS4" w l, \
".outputERFTOPHAT1-$pid" t "ERFTOPHAT1" w l, \
".outputERFTOPHAT2-$pid" t "ERFTOPHAT2" w l

pause -1
EOF

	gnuplot .plot-$pid

}

###################################
# 
###################################
plotSpectrum(){
	local type=$1
	
	winFile=.win$type-$pid
	spectrumFile=.output$type-$pid
	
	local commentLog="#"
	if [ "$logScale" = "true" ]
	then
		commentLog=""
	elif [ "$logScale" = "false" ]
	then
		commentLog="#"
	fi
	
	cat >> .plot-$pid << EOF
# set termopt enhanced
set size square

# set ytics nomirror
# set xtics rotate by -45
unset key

$commentLog set logscale y
# set y2range [-0.1:1.1]
# set y2tics 0.1 nomirror

set multiplot layout 1,2

	plot [:] [:] \
	"$winFile" axis x1y2 w l, \
	"$iFile" w l
	
	unset y2tics
	$commentLog set logscale y
	
	plot [:] [:] \
	"$spectrumFile" w l
	
unset multiplot

pause -1
EOF
	
	gnuplot .plot-$pid

}

main(){
	# ~/Dropbox/UAM/scift/examples/n1df.fft -i $iFile -c $columns -t BACKWARD -tw COS -pw XXX -cw XXX -ow XXXX > .output
	~/Dropbox/UAM/scift/examples/n1df.fft -i $iFile -c $columns -t BACKWARD -tw NONE -cw $centered -ow .win-$pid > .outputNONE-$pid
	~/Dropbox/UAM/scift/examples/n1df.fft -i $iFile -c $columns -t BACKWARD -tw COS -cw $centered -pw 1 -ow .winCOS1-$pid > .outputCOS1-$pid
	~/Dropbox/UAM/scift/examples/n1df.fft -i $iFile -c $columns -t BACKWARD -tw COS -cw $centered -pw 2 -ow .winCOS2-$pid > .outputCOS2-$pid
	~/Dropbox/UAM/scift/examples/n1df.fft -i $iFile -c $columns -t BACKWARD -tw COS -cw $centered -pw 3 -ow .winCOS3-$pid > .outputCOS3-$pid
	~/Dropbox/UAM/scift/examples/n1df.fft -i $iFile -c $columns -t BACKWARD -tw GAUSS -cw $centered -pw 3 -ow .winGAUSS3-$pid > .outputGAUSS3-$pid
	~/Dropbox/UAM/scift/examples/n1df.fft -i $iFile -c $columns -t BACKWARD -tw GAUSS -cw $centered -pw 4 -ow .winGAUSS4-$pid > .outputGAUSS4-$pid
	~/Dropbox/UAM/scift/examples/n1df.fft -i $iFile -c $columns -t BACKWARD -tw ERF_TOPHAT -cw $centered -pw 0.1 -ow .winERFTOPHAT1-$pid > .outputERFTOPHAT1-$pid
	~/Dropbox/UAM/scift/examples/n1df.fft -i $iFile -c $columns -t BACKWARD -tw ERF_TOPHAT -cw $centered -pw 0.2 -ow .winERFTOPHAT2-$pid > .outputERFTOPHAT2-$pid
	
	if [ -n "$viewSpectrum" ]
	then
		plotSpectrum $viewSpectrum
		
		if [ -n "$oFile" ]
		then
			cp .output$viewSpectrum-$pid $oFile
		fi
	else
		plotAllSpectra
	fi
	
	rm .plot-$pid
	rm .win-$pid .winCOS1-$pid .winCOS2-$pid .winCOS3-$pid .winGAUSS3-$pid .winGAUSS4-$pid .winERFTOPHAT1-$pid .winERFTOPHAT2-$pid
	rm .outputNONE-$pid .outputCOS1-$pid .outputCOS2-$pid .outputCOS3-$pid .outputGAUSS3-$pid .outputGAUSS4-$pid .outputERFTOPHAT1-$pid .outputERFTOPHAT2-$pid
}

main $*

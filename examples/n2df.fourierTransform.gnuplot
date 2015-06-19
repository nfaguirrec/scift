#------------------------------------------------------------------------
# Transformada de la función rectangular desplazada
#------------------------------------------------------------------------
# sinc(x) = sin(pi*x)/pi/x
# xMax=10.0; xMin=-xMax; nPointsX = 1000
# pMax=30.0; pMin=-pMax; nPointsW = 1000
# a = 1.0; t0 = 3.0
# fR(t) = ubox(a*(t-t0))
# fI(t) = 0.0
# FR(w) = cos(t0*w)*sinc(w/2.0/pi/a)/sqrt(2.0*pi*a**2)
# FI(w) = -sin(t0*w)*sinc(w/2.0/pi/a)/sqrt(2.0*pi*a**2)

##########################################################
# Generating exact values
##########################################################
set table

set format x "%.10f"
set format y "%.10f"

set sample nPointsX

set output "fx-R.dat"
plot [xMin:xMax] fR(x)

set output "fx-I.dat"
plot [xMin:xMax] fI(x)

unset table

##########################################################
# Generating FORWARD Fourier tranforms
##########################################################

! echo -n "FFT FORWARD  ... "
! join -j 1 fx-R.dat fx-I.dat > fx.dat
! rm fx-R.dat fx-I.dat
! ./n1df.fourier -i fx.dat -c 1,2,4 -s FORWARD -t REALPART -m FFT -tw ERF_TOPHAT -cw T -pw 0.2 -ow .window > nFfx-R.dat
! ./n1df.fourier -i fx.dat -c 1,2,4 -s FORWARD -t IMAGPART -m FFT -tw ERF_TOPHAT -cw T -pw 0.2 -ow .window > nFfx-I.dat
# ! ./n1df.fourier -i fx.dat -c 1,2,4 -s FORWARD -t REALPART -m NUMERICAL -tw ERF_TOPHAT -cw T -pw 0.2 -ow .window > nFfx-R.dat
# ! ./n1df.fourier -i fx.dat -c 1,2,4 -s FORWARD -t IMAGPART -m NUMERICAL -tw ERF_TOPHAT -cw T -pw 0.2 -ow .window > nFfx-I.dat
! join -j 1 nFfx-R.dat nFfx-I.dat > nFfx.dat
! rm nFfx-R.dat nFfx-I.dat
! echo "OK"

reset
unset key

##########################################################
# Plotting results
##########################################################

set format y "%5.1f"
set sample nPointsX
set multiplot layout 2,1 title "FORWARD"
	plot [xMin:xMax] \
	fR(x) w l lt 3, \
	fI(x) w l lt 1, \
	"fx.dat" u 1:2 w l lw 0.2 lt 3, \
	"fx.dat" u 1:2 w p pt 7 ps 0.2 lt 3, \
	"fx.dat" u 1:4 w l lw 0.2 lt 1, \
	"fx.dat" u 1:4 w p pt 7 ps 0.2 lt 1, \
	".window" axis x1y2 w l lt 0 lw 2
	
	plot [pMin:pMax] \
	FR(x) w l lt 3, \
	FI(x) w l lt 1, \
	"nFfx.dat" u 1:2 w l lw 0.2 lt 3, \
	"nFfx.dat" u 1:2 w p pt 7 ps 0.2 lt 3, \
	"nFfx.dat" u 1:3 w l lw 0.2 lt 1, \
	"nFfx.dat" u 1:3 w p pt 7 ps 0.2 lt 1
unset multiplot
pause -1

##########################################################
# Generating exact values
##########################################################
set table

set format x "%.10f"
set format y "%.10f"

set sample nPointsW

set output "Ffx-R.dat"
plot [pMin:pMax] FR(x)

set output "Ffx-I.dat"
plot [pMin:pMax] FI(x)

unset table

##########################################################
# Generating BACKWARD Fourier tranforms
##########################################################
unset format

! echo -n "FFT BACKWARD ... "
! join -j 1 Ffx-R.dat Ffx-I.dat > Ffx.dat
! rm Ffx-R.dat Ffx-I.dat
! ./n1df.fourier -i Ffx.dat -c 1,2,4 -s BACKWARD -t REALPART -m FFT -tw ERF_TOPHAT -cw T -pw 0.2 -ow .window > nfx-R.dat
! ./n1df.fourier -i Ffx.dat -c 1,2,4 -s BACKWARD -t IMAGPART -m FFT -tw ERF_TOPHAT -cw T -pw 0.2 -ow .window > nfx-I.dat
# ! ./n1df.fourier -i Ffx.dat -c 1,2,4 -s BACKWARD -t REALPART -m NUMERICAL -tw ERF_TOPHAT -cw T -pw 0.2 -ow .window > nfx-R.dat
# ! ./n1df.fourier -i Ffx.dat -c 1,2,4 -s BACKWARD -t IMAGPART -m NUMERICAL -tw ERF_TOPHAT -cw T -pw 0.2 -ow .window > nfx-I.dat
! join -j 1 nfx-R.dat nfx-I.dat > nfx.dat
! rm nfx-R.dat nfx-I.dat
! echo "OK"

##########################################################
# Plotting results
##########################################################

set format y "%5.1f"
set sample nPointsX
set multiplot layout 2,1 title "FORWARD"
	plot [pMin:pMax] \
	FR(x) w l lt 3, \
	FI(x) w l lt 1, \
	"Ffx.dat" u 1:2 w l lw 0.2 lt 3, \
	"Ffx.dat" u 1:2 w p pt 7 ps 0.2 lt 3, \
	"Ffx.dat" u 1:4 w l lw 0.2 lt 1, \
	"Ffx.dat" u 1:4 w p pt 7 ps 0.2 lt 1, \
	".window" axis x1y2 w l lt 0 lw 2
	
	plot [xMin:xMax] \
	fR(x) w l lt 3, \
	fI(x) w l lt 1, \
	"nfx.dat" u 1:2 w l lw 0.2 lt 3, \
	"nfx.dat" u 1:2 w p pt 7 ps 0.2 lt 3, \
	"nfx.dat" u 1:3 w l lw 0.2 lt 1, \
	"nfx.dat" u 1:3 w p pt 7 ps 0.2 lt 1
unset multiplot
pause -1

! rm fx.dat nFfx.dat .window Ffx.dat nfx.dat

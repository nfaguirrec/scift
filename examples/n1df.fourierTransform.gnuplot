#------------------------------------------------------------------------
# Transformada de una función gausiana
#------------------------------------------------------------------------
# xMax=10.0; xMin=-xMax; nPointsX = 1000
# pMax=10.0; pMin=-pMax; nPointsW = 1000
# a = 0.5
# fR(t) = exp(-a*t**2)
# fI(t) = 0.0
# FR(w) = (1.0/sqrt(2.0*a))*exp(-w**2/4.0/a)
# FI(w) = 0.0

#------------------------------------------------------------------------
# Transformada de la función cos(a*t**2)
#------------------------------------------------------------------------
# xMax=20.0; xMin=-xMax; nPointsX = 2000
# pMax=20.0; pMin=-pMax; nPointsW = 3000
# a = 0.5
# fR(t) = cos(a*t**2)
# fI(t) = 0.0
# FR(w) = (1.0/sqrt(2.0*a))*cos(w**2/4.0/a-pi/4.0)
# FI(w) = 0.0

#------------------------------------------------------------------------
# Transformada de la función sin(a*t**2)
#------------------------------------------------------------------------
# xMax=20.0; xMin=-xMax; nPointsX = 2000
# pMax=20.0; pMin=-pMax; nPointsW = 3000
# a = 0.5
# fR(t) = sin(a*t**2)
# fI(t) = 0.0
# FR(w) = -(1.0/sqrt(2.0*a))*sin(w**2/4.0/a-pi/4.0)
# FI(w) = 0.0

#------------------------------------------------------------------------
# Transformada de la función exp(-a*abs(t))
#------------------------------------------------------------------------
# xMax=10.0; xMin=-xMax; nPointsX = 2000
# pMax=30.0; pMin=-pMax; nPointsW = 1000
# a = 1.0
# fR(t) = exp(-a*abs(t))
# fI(t) = 0.0
# FR(w) = sqrt(2.0/pi)*a/(a**2+w**2)
# FI(w) = 0.0

#------------------------------------------------------------------------
# Transformada de la función sin(a*t)
#------------------------------------------------------------------------
# delta(x)= exp(-10000.0*x**2)*sqrt(10000.0/pi)
# 
# xMax=100.0; xMin=-xMax; nPointsX = 4000
# pMax=10.0; pMin=-pMax; nPointsW = 4000
# a = 0.5
# fR(t) = sin(a*t)
# fI(t) = 0.0
# FR(w) = 0.0
# FI(w) = sqrt(2.0*pi)*( -delta(w-a)+delta(w+a) )/2.0

#------------------------------------------------------------------------
# Transformada de la función besj0(a*t)
#------------------------------------------------------------------------
# xMax=100.0; xMin=-xMax; nPointsX = 500
# pMax=5.0; pMin=-pMax; nPointsW = 10000
# a = 1.0
# fR(t) = besj0(a*t)
# fI(t) = 0.0
# FR(w) = sqrt(2.0/pi)*rect(w/2.0)/sqrt(1.0-w**2)
# FI(w) = 0.0

#------------------------------------------------------------------------
# Transformada de la función rectangular desplazada
#------------------------------------------------------------------------
sinc(x) = sin(pi*x)/pi/x
xMax=10.0; xMin=-xMax; nPointsX = 1000
pMax=30.0; pMin=-pMax; nPointsW = 1000
a = 1.0; t0 = 3.0
fR(t) = ubox(a*(t-t0))
fI(t) = 0.0
FR(w) = cos(t0*w)*sinc(w/2.0/pi/a)/sqrt(2.0*pi*a**2)
FI(w) = -sin(t0*w)*sinc(w/2.0/pi/a)/sqrt(2.0*pi*a**2)

#------------------------------------------------------------------------
# Transformada de prueba exacta
#------------------------------------------------------------------------
# xMax=10.0; xMin=-xMax; nPointsX = 2000
# pMax=10.0; pMin=-pMax; nPointsW = 2000
# A = 1.0; a = 1.0; s = +1
# fR(t) = A*cos(a**2*t**2)
# fI(t) = A*s*sin(a**2*t**2)
# FR(w) =   0.5*(A/a)*( cos(w**2/4.0/a**2)+sin(w**2/4.0/a**2) )
# FI(w) = s*0.5*(A/a)*( cos(w**2/4.0/a**2)-sin(w**2/4.0/a**2) )

#------------------------------------------------------------------------
# Transformada de dos funciones gausianas desplazadas por 20.0 respecto
# al origen con un momento k0 cada una en dirección contraria
# f(x) = exp(-i*k0*x)*fleft(x)+exp(i*k0*x)*fright(x)
# No funciona
#------------------------------------------------------------------------
# xMax=50.0; xMin=-xMax; nPointsX = 501
# pMax=2.0; pMin=-pMax; nPointsW = 501
# a = 0.1; t0 = 10.0; k0=0.1
# g(t)  = exp(-a*t**2)
# fR(t) = cos(k0*t)*(  g(t+t0) + g(t-t0) )
# fI(t) = sin(k0*t)*( -g(t+t0) + g(t-t0) )
# FR(w) = (exp(-w**2/a/4.0E+0-k0*w/a/2.0-k0**2/a/4.0)*sin(t0*w)*(sin(2*t0*w+k0*t0)+sin(k0*t0)*exp(k0*w/a)) \
# 	+exp(-w**2/a/4.0-k0*w/a/2.0-k0**2/a/4.0)*cos(t0*w)*(cos(2*t0*w+k0*t0)+cos(k0*t0)*exp(k0*w/a)))/(sqrt(2)*sqrt(a))
# FI(w) = (exp(-w**2/a/4.0-k0*w/a/2.0-k0**2/a/4.0)*cos(t0*w)*(sin(2*t0*w+k0*t0)+sin(k0*t0)*exp(k0*w/a)) \
# 	-exp(-w**2/a/4.0-k0*w/a/2.0-k0**2/a/4.0)*sin(t0*w)*(cos(2*t0*w+k0*t0)+cos(k0*t0)*exp(k0*w/a)))/(sqrt(2)*sqrt(a))


#------------------------------------------------------------------------
dx = (xMax-xMin)/(nPointsX-1)
pMax0 = 2.0*pi*floor(nPointsX/2)/nPointsX/dx
pMin0 = -pMax0

print "x = (", xMin, ",", xMax, "), dx = ", dx
print "p = (", pMin0, ",", pMax0, ")

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
! ./n1df.fourier -i fx.dat -o nFfx-R.dat -c 1,2,4 -s FORWARD -t REALPART -m FFT -tw ERF_TOPHAT -cw T -pw 0.2 -ow .window
! ./n1df.fourier -i fx.dat -o nFfx-I.dat -c 1,2,4 -s FORWARD -t IMAGPART -m FFT -tw ERF_TOPHAT -cw T -pw 0.2 -ow .window
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
! ./n1df.fourier -i Ffx.dat -o nfx-R.dat -c 1,2,4 -s BACKWARD -t REALPART -m FFT -tw ERF_TOPHAT -cw T -pw 0.2 -ow .window
! ./n1df.fourier -i Ffx.dat -o nfx-I.dat -c 1,2,4 -s BACKWARD -t IMAGPART -m FFT -tw ERF_TOPHAT -cw T -pw 0.2 -ow .window
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

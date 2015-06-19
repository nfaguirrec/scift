# xMax=10.0; xMin=-xMax; nPointsX = 1000
# pMax=10.0; pMin=-pMax; nPointsW = 1000
# a = 0.5
# f(t) = exp(-a*t**2)
# F(omega) = (1.0/sqrt(2.0*a))*exp(-omega**2/4.0/a)

# xMax=20.0; xMin=-xMax; nPointsX = 2000
# pMax=20.0; pMin=-pMax; nPointsW = 3000
# a = 0.5
# f(t) = cos(a*t**2)
# F(omega) = (1.0/sqrt(2.0*a))*cos(omega**2/4.0/a-pi/4.0)

# xMax=10.0; xMin=-xMax; nPointsX = 2000
# pMax=30.0; pMin=-pMax; nPointsW = 1000
# a = 1.0
# f(t) = exp(-a*abs(t))
# F(omega) = sqrt(2.0/pi)*a/(a**2+omega**2)

# Transformada de dos funciones gausianas desplazadas por 20.0 respecto al origen
# con un momento k0 cada una en dirección contraria
# f(t) = exp(-i*k0*x)*fleft(x)+exp(i*k0*x)*fright(x)
# nPoints = 1001; xMax=200.0; xMin=-xMax
# a = 0.5
# k0=3.0
# fleft(t)  = exp(-a*(t+20.0)**2)
# fright(t) = exp(-a*(t-20.0)**2)
# f(t)  = fleft(t) + fright(t)
# fR(t) = cos(k0*t)*(  fleft(t) + fright(t) )
# fI(t) = sin(k0*t)*( -fleft(t) + fright(t) )
# F(w) = 2.0*sqrt(pi/a)*exp(-w**2/4.0/a)*cos(w*20.0)

# nPoints = 100; xMax=10.0; xMin=-xMax
# a = 0.5
# f(t) = sin(a*t)
# F(w) = sqrt(2.0*pi)*( delta(w-a)-delta(w+a) )/2.0

xMax=100.0; xMin=-xMax; nPointsX = 500
pMax=5.0; pMin=-pMax; nPointsW = 10000
a = 1.0
f(t) = besj0(a*t)
fR(t) = f(t)
fI(t) = 0.0
F(w) = sqrt(2.0/pi)*rect(w/2.0)/sqrt(1.0-w**2)
FR(w) = F(w)
FI(w) = 0.0

dx = (xMax-xMin)/(nPointsX-1)
pMax0 = 2.0*pi*floor(nPointsX/2)/nPointsX/dx
pMin0 = -pMax0

print "x = (", xMin, ",", xMax, "), dx = ", dx
print "p = (", pMin0, ",", pMax0, "), dp = "#, dp

##########################################################
# Generating exact values
##########################################################
set table

set format x "%.10f"
set format y "%.10f"

set sample nPointsX
set output "valuesFFT1D.dat"
plot [xMin:xMax] f(x)

set output "valuesFFT1D-R.dat"
plot [xMin:xMax] fR(x)

set output "valuesFFT1D-I.dat"
plot [xMin:xMax] fI(x)

set sample nPointsW
set output "exactFvaluesFFT1D.dat"
plot [pMin:pMax] F(x)

set sample nPointsW
set output "exactFvaluesFFT1D-R.dat"
plot [pMin:pMax] FR(x)

set sample nPointsW
set output "exactFvaluesFFT1D-I.dat"
plot [pMin:pMax] FI(x)

unset table

##########################################################
# Generating Fourier tranforms
##########################################################

! echo -n "FFT FORWARD  ... "
! join -j 1 valuesFFT1D-R.dat valuesFFT1D-I.dat > valuesFFT1D.dat
! rm valuesFFT1D-R.dat valuesFFT1D-I.dat
! ./n1df.fft -i valuesFFT1D.dat -c 1,2,4 -s FORWARD -t REALPART -tw ERF_TOPHAT -cw T -pw 0.2 -ow .window > FvaluesFFT1D-R.dat
! ./n1df.fft -i valuesFFT1D.dat -c 1,2,4 -s FORWARD -t IMAGPART -tw ERF_TOPHAT -cw T -pw 0.2 -ow .window > FvaluesFFT1D-I.dat
! join -j 1 FvaluesFFT1D-R.dat FvaluesFFT1D-I.dat > FvaluesFFT1D.dat
! rm FvaluesFFT1D-R.dat FvaluesFFT1D-I.dat
! echo "OK"

reset
unset key

set sample nPointsX
set multiplot layout 2,1 title "FORWARD"
	plot [xMin:xMax] \
	fR(x) w l lt 3, \
	fI(x) w l lt 2, \
	"valuesFFT1D.dat" u 1:2 w p pt 7 ps 0.2 lt 3, \
	"valuesFFT1D.dat" u 1:4 w p pt 7 ps 0.2 lt 2, \
	".window" w l lt 5 lw 2
	
	plot [pMin:pMax] \
	FR(x) w l lt 3, \
	FI(x) w l lt 2, \
	"FvaluesFFT1D.dat" u 1:2 w p pt 7 ps 0.2 lt 3, \
	"FvaluesFFT1D.dat" u 1:3 w p pt 7 ps 0.2 lt 2
	
# 	"FvaluesFFT1D.dat" u 1:2 w lp lw 0.4 pt 7 ps 0.3 lt 3

# 	plot [pMin:pMax] \
# 	abs(F(x)) w l lt 1, \
# 	"FvaluesFFT1D.dat" u 1:(sqrt($2**2+$3**2)) w lp pt 7 ps 0.2 lt 3
	
# 	plot [pMin:pMax] \
# 	F(x) w l lt 1, \
# 	"FvaluesFFT1D.dat" u 1:2 w lp pt 7 ps 0.2 lt 3, \
# 	"FvaluesFFT1D.dat" u 1:3 w lp pt 7 ps 0.2 lt 2
unset multiplot
pause -1

# ! echo -n "FFT BACKWARD ... "
# # ! ./n1df.fft -i exactFvaluesFFT1D.dat -c 1,2 -t BACKWARD -x1 -30.0 -tw ERF_TOPHAT -cw T -pw 0.2 -ow .window > exactValuesFFT1D.dat
# ! ./n1df.fft -i exactFvaluesFFT1D.dat -c 1,2 -s BACKWARD -t REALPART -tw ERF_TOPHAT -cw T -pw 0.2 -ow .window > exactValuesFFT1D.dat
# # ! ./n1df.fft -i exactFvaluesFFT1D.dat -c 1,2 -t BACKWARD > exactValuesFFT1D.dat
# ! echo "OK"
# 
# set sample nPointsW
# set multiplot layout 2,1 title "BACKWARD"
# 	plot [pMin:pMax] \
# 	F(x) w l lt 1, \
# 	"exactFvaluesFFT1D.dat" w p pt 7 ps 0.2 lt 3, \
# 	".window" w lp pt 0.5 lt 5 lw 2
# 
# 	plot [xMin:xMax] \
# 	f(x) w l lt 1, \
# 	"exactValuesFFT1D.dat" u 1:2 w lp lw 0.4 pt 7 ps 0.4 lt 3
# 
# # 	plot [xMin:xMax] \
# # 	f(x) w l lt 1, \
# # 	"exactValuesFFT1D.dat" u 1:2 w lp pt 7 ps 0.2 lt 3, \
# # 	"exactValuesFFT1D.dat" u 1:3 w lp pt 7 ps 0.2 lt 2
# unset multiplot
# pause -1
# 
# 
# ! rm valuesFFT1D.dat FvaluesFFT1D.dat
# ! rm exactFvaluesFFT1D.dat exactValuesFFT1D.dat
# ! rm .window
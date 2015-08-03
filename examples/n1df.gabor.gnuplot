! E0=0.05; omega=0.20279; t0=500.0; w=800.0; dw=100.0; tMax=1000.0; echo "n=2**11; x0=0.0; h=$tMax/(n-1); do for [i=1:n] { x=x0+(i-1)*h; print sprintf( '%10.5f%10.5f', x, $E0*cos($omega*x)*erfhat(x,$t0,$w,$dw) ); }" | gnuplot 2> signalErfHat.dat
! n1df.fourier -i signalErfHat.dat -c 1,2 -o fftErfHat.dat

plot "signalErfHat.dat" w lp pt 7 ps 0.5 lw 2
plot [0:0.4] "fftErfHat.dat" w lp pt 7 ps 0.5 lw 2
pause -1

! E0=0.05; omega=0.20279; t0=500.0; w=800.0; dw=100.0; tMax=1000.0; echo "n=2**11; x0=0.0; h=$tMax/(n-1); do for [i=1:n] { x=x0+(i-1)*h; print sprintf( '%10.5f%10.5f', x, $E0*cos($omega*x)*coshat(x,$t0,$w,$dw) ); }" | gnuplot 2> signalCosHat.dat
! n1df.fourier -i signalCosHat.dat -c 1,2 -o fftCosHat.dat
plot "signalCosHat.dat" w lp pt 7 ps 0.5 lw 2
plot [0:0.4] "fftCosHat.dat" w lp pt 7 ps 0.5 lw 2
pause -1

plot "signalErfHat.dat" w l lw 2, "signalCosHat.dat" w l lw 2 lt 3
set size square
plot [0:0.4] "fftErfHat.dat" w lp pt 7 ps 0.5 lw 2, "fftCosHat.dat" w lp pt 7 ps 0.5 lw 2 lt 3

pause -1
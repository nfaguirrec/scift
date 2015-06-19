! ./n1df.peakFinder -i n1df.peakFinder1.dat -c 1,2 -m 0 -w 12 -t 1.0 > .output

unset key

plot \
"n1df.peakFinder1.dat" w l, \
".output" w p pt 7 ps 1.0 lt 3
pause -1

! ./n1df.peakFinder -i n1df.peakFinder2.dat -c 1,2 -m 3 -w 10 -t 0.2 -bw 5 > .output

plot \
"n1df.peakFinder2.dat" w l, \
".output" w p pt 7 ps 1.0 lt 3
pause -1

! ./n1df.peakFinder -test true -m 0 -w 12 -t 1.0 > .output

plot \
".output" i 0 w l, \
".output" i 1 w p pt 7 ps 1.0 lt 3
pause -1

# ! ./n1df.peakFinder -i sAt2.dat -c 1,2 -m 2 -w 5 -t 1.0 > .output
# ! cat .output
# 
# plot \
# "sAt2.dat" w l, \
# ".output" w p pt 7 ps 1.0 lt 3
# pause -1

! rm .output
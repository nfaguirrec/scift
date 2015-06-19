set parametric

r=1.00
set xrange [-r:r]
set yrange [-r:r]
set zrange [-r:r]

set urange [0:2*pi]
set vrange [-pi:pi]

# Parametric functions for the sphere
fx(v,u) = r*cos(v)*cos(u)
fy(v,u) = r*cos(v)*sin(u)
fz(v)   = r*sin(v)

unset border
unset key
set origin 0.1,-0.2
set size 0.75,1.32
set ticslevel 0.0
set isosamples 40, 20
unset tics

set view 60,120

splot \
fx(v,u) w l lw 0.1, \
fy(v,u) w l lw 0.1, \
  fz(v) w l lw 0.1, \
"-" w vec lw 2 lc rgb "red", \
"-" w vec lw 2 lc rgb "green", \
"-" w vec lw 2 lc rgb "blue", \
"-" w vec lw 3 lt 0 lc rgb "red", \
"-" w vec lw 3 lt 0 lc rgb "green", \
"-" w vec lw 3 lt 0 lc rgb "blue"
0.00  0.00  0.00    0.70711  -0.70711   0.00000
end
0.00  0.00  0.00    0.70711   0.70711   0.00000
end
0.00  0.00  0.00    0.00000   0.00000   1.00000
end
0.00  0.00  0.00   1.0  0.0  0.0
end
0.00  0.00  0.00   0.0  1.0  0.0
end
0.00  0.00  0.00   0.0  0.0  1.0
end

pause -1

# Rotacion alpha=beta=gamma=45*deg
#0.00  0.00  0.00   -0.14645  -0.85355   0.50000
#end
#0.00  0.00  0.00    0.85355   0.14645   0.50000
#end
#0.00  0.00  0.00   -0.50000   0.50000   0.70711

# Rotacion alpha=beta=0*deg y gamma=45*deg
#0.00  0.00  0.00    0.70711  -0.70711   0.00000
#end
#0.00  0.00  0.00    0.70711   0.70711   0.00000
#end
#0.00  0.00  0.00    0.00000   0.00000   1.00000

# Rotacion alpha=beta=45*deg y gamma=0*deg
#0.00  0.00  0.00   0.5000  -0.7071  0.5000
#end
#0.00  0.00  0.00   0.5000   0.7071  0.5000
#end
#0.00  0.00  0.00  -0.7071   0.0000  0.7071

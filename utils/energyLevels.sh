#!/bin/bash

viewEnergyLevels()
{
	local iFile=$1
	local cols=$2
	local save=$3
	
	col1=`echo $cols | gawk 'BEGIN{FS=","}{print $1}'`
	col2=`echo $cols | gawk 'BEGIN{FS=","}{print $2}'`
	
	awk '
	BEGIN{
		min=10000000000000.0
		max=-10000000000000.0
	}
	( $1 !~ /^#/ ){
		map[$'$col2'] = $'$col1'
		
		if( $'$col2' < min ) min = $'$col2'
		if( $'$col2' > max ) max = $'$col2'
	}
	END{
		n = asorti(map,smap,"@val_num_asc")
		
# 		print "# max = ", smap[n]
# 		print "# min = ", smap[1]
# 		range = smap[1]-smap[n]

		print "# max = ", max
		print "# min = ", min
		range = max-min
		
		for( i=1; i<=n; i++ ){
			print "# i = ", i
			i0 = i
			degenerated = 0
			for( j=1; j<=10; j++ ){
				if( i > j && sqrt((smap[i]-smap[i-j])**2) < 0.05*range ){
					print map[smap[i]], j+0.2, smap[i]
					print map[smap[i]], j+0.7, smap[i]
					print ""
					i++
				}
			}
			
			if( degenerated == 0 && i0 == i ){
				print map[smap[i]], 0.2, smap[i]
				print map[smap[i]], 0.7, smap[i]
				print ""
			}else{
				i--
			}
		}
		
		########################################################
		# Esto es exactamente el mismo bloque anterior
		# y es necesario para gnuplot y2tics
# 		print "e"
# 		for( i=1; i<=n; i++ ){
# 			i0 = i
# 			degenerated = 0
# 			for( j=1; j<=10; j++ ){
# 				if( i > j && sqrt((smap[i]-smap[i-j])**2) < 0.05*range ){
# 					print map[smap[i]], j+0.2, smap[i]
# 					print map[smap[i]], j+0.7, smap[i]
# 					print ""
# 					i++
# 				}
# 			}
# 			
# 			if( degenerated == 0 && i0 == i ){
# 				print map[smap[i]], 0.2, smap[i]
# 				print map[smap[i]], 0.7, smap[i]
# 				print ""
# 			}else{
# 				i--
# 			}
# 		}
		########################################################
		
		print "e"
		for( i=1; i<=n; i++ ){
			i0 = i
			degenerated = 0
			for( j=1; j<=10; j++ ){
				if( i > j && sqrt((smap[i]-smap[i-j])**2) < 0.05*range ){
					print map[smap[i]], j+0.5, smap[i]
					degenerated = 1
					i++
				}
			}
			
			if( degenerated == 0 && i0 == i ){
				print map[smap[i]], 0.5, smap[i]
			}else{
				i--
			}
		}
		
		print "e"
	}
	' $iFile > .data-$$
	
cat > .plot-$$ << EOF

set termopt font "Serif,10"
set border 2
unset key
# set size square
set size 0.8,1.0
# set xtics 1
unset xtics
set format x "  "
# set ylabel "Energy (eV)"
# set y2label "Relative Energy (eV)"
set ylabel "Energy" font "Serif,13"
# set y2label "Relative Energy"
set ytics font "Serif,13"

max = `awk '(\$0~/# max/){print \$4}' .data-$$`
min = `awk '(\$0~/# min/){print \$4}' .data-$$`

range = max-min

set yrange [min-0.08*range:max+0.08*range]
# set y2range [min-0.08*range-min:max+0.08*range-min]

set ytics nomirror
# set y2tics nomirror

plot [-1:] \
"-" u 2:3 w l lw 2 lc rgb "blue", \
"-" u (\$2+0.2):3:1 with labels left notitle
`cat .data-$$`

# "-" u 2:(\$3-min) axis x1y2 w l lw 0, \
# "-" u 2:3:1 with labels center offset 0,1 notitle

pause -1
	
EOF

	gnuplot .plot-$$
	
	if [ "$save" = "save" ]
	then
		cat .plot-$$
	fi

	rm .data-$$
	rm .plot-$$
}

main(){
	local iFile=$1
	local cols=$2
	local save=$3
	
	if [ -z "$iFile" ]
	then
		echo "### ERROR ###: iFile is required"
		exit
	fi
	
	if [ -z "$cols" ]
	then
		cols="1,2"
	fi
	
	if [ -z "$save" ]
	then
		save="xxxx"
	fi
	
	viewEnergyLevels $iFile $cols $save
}

main $*


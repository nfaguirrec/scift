#!/usr/bin/awk -f
BEGIN{
	FS="="
}
( $0!~/^[[:blank:]]*$/){
	AtomicNumber = -1
	if($1~/^Atomic Number/){ AtomicNumber=$NF; getline }
	if($1~/^Atomic Symbol/){ AtomicSymbol=$NF; getline }
	if($1~/^Mass Number/){ MassNumber=$NF; getline }
	if($1~/^Relative Atomic Mass/){ RelativeAtomicMass=$NF; gsub("[(].*[#)]","",RelativeAtomicMass); getline }
	if($1~/^Isotopic Composition/){ IsotopiComposition=$NF; gsub("[(].*[#)]","",IsotopiComposition); getline }
	if($1~/^Standard Atomic Weight/){ StandardAtomicWeight=$NF; gsub("[(].*[#)]","",StandardAtomicWeight); getline }
		
	if( AtomicNumber !~ -1 ){
# 		if( 1.0*IsotopiComposition > 0.35 ){
# 			print AtomicNumber, AtomicSymbol, MassNumber, RelativeAtomicMass, ">>>", IsotopiComposition, StandardAtomicWeight
# 		if( IsotopiComposition ~ /^[[:blank:]]+$/ ){
# 		if( 1.0*IsotopiComposition == 0.0 ){
			printf "%10d%10.4f%10.4f%10d\n", AtomicNumber, RelativeAtomicMass, IsotopiComposition, MassNumber
# 		}
		
# 		print "elem.atomicNumber = ", AtomicNumber
# 		print "elem.symbol = ", AtomicSymbol
# 		print "elem.name = ", "Hydrogen"
# 		print "elem.atomicWeight = ", StandardAtomicWeight
# 		print "elem.mostStableIsotopeMassNumber = ", 1
# 		print "allocate( isot(1:7) )"
# 			print "isot(1).massNumber = ", MassNumber
# 			print "isot(1).abundance  = ", IsotopiComposition
# 			print "isot(1).atomicMass = ", RelativeAtomicMass
# 			
# 			print "isot(2).massNumber = ", 2
# 			print "isot(2).abundance  = ", 0.000115
# 			print "isot(2).atomicMass = ", 2.0141017778
# 		print "deallocate( isot )"
	}
}

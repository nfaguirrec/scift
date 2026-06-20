program test_StringRealHistogramMap
    use StringRealHistogramMap_
    use TestUtils_
    use GOptions_
    use IOStream_
    use String_
    use RealHistogram_
    use StringRealHistogramPair_
    use StringRealHistogramPairList_, StringRealHistogramMapIterator => StringRealHistogramPairListIterator
    implicit none
		type(String) :: str
		type(RealHistogram) :: hist, histAt
		type(StringRealHistogramPair) :: pair
		type(StringRealHistogramMap) :: mymap
		class(StringRealHistogramMapIterator), pointer :: ptr
		
		mymap = StringRealHistogramMap()
		call assert_equal( mymap%size(), 0, "StringRealHistogramMap_test: size empty" )
		
		str = "Ademas"
		hist = RealHistogram( Histogram_STURGES )
		call hist.add( [24.15162_8, 19.56235_8, 27.82564_8, 23.38200_8, 25.19829_8, 25.26511_8, 23.81071_8, 22.70389_8] )
		call mymap.insert( str, hist )
		call assert_equal( mymap%size(), 1, "StringRealHistogramMap_test: size 1" )
		
		histAt = mymap%at( str )
		call assert_equal( histAt%size(), 8, "StringRealHistogramMap_test: histAt size" )
		
		str = "Amor"
		hist = RealHistogram( Histogram_STURGES )
		call hist.add( [24.15162_8, 19.56235_8, 27.82564_8, 23.38200_8, 25.19829_8, 25.26511_8, 23.81071_8] )
		call mymap.insert( str, hist )
		call assert_equal( mymap%size(), 2, "StringRealHistogramMap_test: size 2" )
		
		str = "Amor"
		hist = RealHistogram( Histogram_STURGES )
		call hist.add( [24.15162_8] )
		call mymap.insert( str, hist )
		call assert_equal( mymap%size(), 2, "StringRealHistogramMap_test: size unchanged after duplicate insert" )
		histAt = mymap%at( str )
		call assert_equal( histAt%size(), 1, "StringRealHistogramMap_test: updated size" )
		
		str = "Entonces"
		hist = RealHistogram( Histogram_STURGES )
		call hist.add( [24.15162_8, 19.56235_8, 27.82564_8, 23.38200_8, 25.19829_8, 25.26511_8] )
		call mymap.insert( str, hist )
		
		str = "Corazon"
		hist = RealHistogram( Histogram_STURGES )
		call hist.add( [24.15162_8, 19.56235_8, 27.82564_8, 23.38200_8, 25.19829_8] )
		call mymap.insert( str, hist )
		
		str = "Hola"
		hist = RealHistogram( Histogram_STURGES )
		call hist.add( [24.15162_8, 19.56235_8, 27.82564_8, 23.38200_8] )
		call mymap.insert( str, hist )
		
		str = "Conejo"
		hist = RealHistogram( Histogram_STURGES )
		call hist.add( [24.15162_8, 19.56235_8, 27.82564_8] )
		call mymap.insert( str, hist )
		
		call assert_equal( mymap%size(), 6, "StringRealHistogramMap_test: size 6" )
		
		str = "Corazon"
		call assert_true( mymap.find( str, ptr ), "StringRealHistogramMap_test: find Corazon" )
		pair = mymap.pair( ptr )
		call assert_equal( pair%first%fstr, "Corazon", "StringRealHistogramMap_test: find key Corazon" )
		call assert_equal( pair%second%size(), 5, "StringRealHistogramMap_test: find value size" )
		
		str = "Corazon "
		call assert_true( .not. mymap.find( str, ptr ), "StringRealHistogramMap_test: find Corazon with trailing space" )
		
		str = "Conejo"
		call mymap.erase( str )
		call assert_equal( mymap%size(), 5, "StringRealHistogramMap_test: size after erase Conejo" )
		
		str = "Entonces"
		call mymap.erase( str )
		
		str = "Amor"
		call mymap.erase( str )
		call assert_equal( mymap%size(), 3, "StringRealHistogramMap_test: size after erase others" )
		
		call mymap.clear()
		call assert_equal( mymap%size(), 0, "StringRealHistogramMap_test: size after clear" )
		
		str = "Ademas"
		hist = RealHistogram( Histogram_STURGES )
		call hist.add( [24.15162_8] )
		call mymap.insert( str, hist )
		
		str = "Amor"
		hist = RealHistogram( Histogram_STURGES )
		call hist.add( [24.15162_8, 19.56235_8, 27.82564_8] )
		call mymap.insert( str, hist )
		
		str = "Amor"
		hist = RealHistogram( Histogram_STURGES )
		call hist.add( [24.15162_8, 19.56235_8] )
		call mymap.set( str, hist )
		histAt = mymap%at( str )
		call assert_equal( histAt%size(), 2, "StringRealHistogramMap_test: size after set Amor" )
		
		str = "Hola"
		call mymap.set( str, hist )
		call assert_equal( mymap%size(), 3, "StringRealHistogramMap_test: size after set non-existent Hola" )
		histAt = mymap%at( str )
		call assert_equal( histAt%size(), 2, "StringRealHistogramMap_test: size of Hola" )
		
		call mymap.clear()
		
		str = "Hola"
		call mymap.set( str, hist )
		histAt = mymap%at( str )
		call histAt.add( [0.456_8, 5.9423_8] )
		call mymap.set( str, histAt )
		histAt = mymap%at( str )
		call assert_equal( histAt%size(), 4, "StringRealHistogramMap_test: size after add elements manually" )
		
		str = "Hola"
		call mymap.add( str, [0.456_8, 5.9423_8] )
		histAt = mymap%at( str )
		call assert_equal( histAt%size(), 6, "StringRealHistogramMap_test: size after map.add array" )
		
		str = "Entonces"
		call mymap.add( str, [0.456_8, 5.9423_8] )
		call assert_equal( mymap%size(), 2, "StringRealHistogramMap_test: map size after map.add array to new key" )
		histAt = mymap%at( str )
		call assert_equal( histAt%size(), 2, "StringRealHistogramMap_test: value size after map.add array to new key" )
		
end program test_StringRealHistogramMap

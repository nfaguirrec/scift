program test_StringRealHistogramPair
    use StringRealHistogramPair_
    use TestUtils_
    use GOptions_
    use String_
    use RealHistogram_
    implicit none
		type(String) :: str
		type(RealHistogram) :: hist
		type(StringRealHistogramPair) :: mypair1
		type(StringRealHistogramPair) :: mypair2
		
		hist = RealHistogram( Histogram_STURGES )
		call hist.add( [24.15162_8, 19.56235_8, 27.82564_8, 23.38200_8, 25.19829_8, 25.26511_8, 23.81071_8, 22.70389_8] )
		
		str = "Hola"
		mypair1 = StringRealHistogramPair( str, hist )
		call assert_equal( mypair1%first%fstr, "Hola", "StringRealHistogramPair_test: mypair1 key" )
		call assert_equal( mypair1%second%size(), 8, "StringRealHistogramPair_test: mypair1 hist size" )
		
		str = "Entonces"
		call hist.add( [23.21883_8, 25.35600_8, 28.41117_8, 22.08219_8, 19.55053_8] )
		mypair2 = StringRealHistogramPair( str, hist )
		call assert_equal( mypair2%first%fstr, "Entonces", "StringRealHistogramPair_test: mypair2 key" )
		call assert_equal( mypair2%second%size(), 13, "StringRealHistogramPair_test: mypair2 hist size" )
		
		mypair1 = mypair2
		call assert_equal( mypair1%first%fstr, "Entonces", "StringRealHistogramPair_test: copy key" )
		call assert_equal( mypair1%second%size(), 13, "StringRealHistogramPair_test: copy hist size" )
end program test_StringRealHistogramPair

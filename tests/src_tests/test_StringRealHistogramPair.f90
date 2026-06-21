program test_StringRealHistogramPair
    use StringRealHistogramPair_
    use TestUtils_
    use GOptions_
    use String_
    use RealHistogram_
    use iso_c_binding
    implicit none
    
    interface
        subroutine memset(dest, c, n) bind(c, name="memset")
            import
            type(c_ptr), value :: dest
            integer(c_int), value :: c
            integer(c_size_t), value :: n
        end subroutine memset
    end interface

		type(String) :: str
		type(RealHistogram) :: hist
		type(StringRealHistogramPair), target :: mypair1
		type(StringRealHistogramPair), target :: mypair2
		
		call memset(c_loc(mypair1), 0, sizeof(mypair1))
		call memset(c_loc(mypair2), 0, sizeof(mypair2))
		
		hist = RealHistogram( Histogram_STURGES )
		call hist%add( [24.15162_8, 19.56235_8, 27.82564_8, 23.38200_8, 25.19829_8, 25.26511_8, 23.81071_8, 22.70389_8] )
		
		str = "Hola"
		call mypair1%init( str, hist )
		call assert_equal( mypair1%first%fstr, "Hola", "StringRealHistogramPair_test: mypair1 key" )
		call assert_equal( mypair1%second%size(), 8, "StringRealHistogramPair_test: mypair1 hist size" )
		
		str = "Entonces"
		call hist%add( [23.21883_8, 25.35600_8, 28.41117_8, 22.08219_8, 19.55053_8] )
		call mypair2%init( str, hist )
		call assert_equal( mypair2%first%fstr, "Entonces", "StringRealHistogramPair_test: mypair2 key" )
		call assert_equal( mypair2%second%size(), 13, "StringRealHistogramPair_test: mypair2 hist size" )
		
		mypair1 = mypair2
		call assert_equal( mypair1%first%fstr, "Entonces", "StringRealHistogramPair_test: copy key" )
		call assert_equal( mypair1%second%size(), 13, "StringRealHistogramPair_test: copy hist size" )
end program test_StringRealHistogramPair

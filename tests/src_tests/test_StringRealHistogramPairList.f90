program test_StringRealHistogramPairList
    use StringRealHistogramPairList_
    use TestUtils_
    use GOptions_
    use IOStream_
    use String_
    use RealHistogram_
    use StringRealHistogramPair_
    implicit none
		type(String) :: str
		type(RealHistogram) :: hist
		type(StringRealHistogramPair) :: mypair
		type(StringRealHistogramPairList) :: mylist
		class(StringRealHistogramPairListIterator), pointer :: iter
		class(StringRealHistogramPairListIterator), pointer :: iterPos
		
		mylist = StringRealHistogramPairList()
		call assert_equal( mylist%size(), 0, "StringRealHistogramPairList_test: empty size" )
		
		str = "Hello"
		hist = RealHistogram( Histogram_STURGES )
		call hist%add( [24.15162_8, 19.56235_8, 27.82564_8, 23.38200_8, 25.19829_8, 25.26511_8, 23.81071_8, 22.70389_8] )
		call mypair%init( str, hist )
		call mylist%append( mypair )
		
		str = "class"
		hist = RealHistogram( Histogram_STURGES )
		call hist%add( [24.15162_8, 19.56235_8, 27.82564_8] )
		call mypair%init( str, hist )
		call mylist%append( mypair )
		
		str = "string"
		hist = RealHistogram( Histogram_STURGES )
		call hist%add( [27.82564_8] )
		call mypair%init( str, hist )
		call mylist%append( mypair )
		
		str = "list"
		hist = RealHistogram( Histogram_STURGES )
		call hist%add( [23.81071_8, 22.70389_8] )
		call mypair%init( str, hist )
		call mylist%append( mypair )
		
		call assert_equal( mylist%size(), 4, "StringRealHistogramPairList_test: size after append" )
		
		mypair = mylist%at( mylist%begin )
		call assert_equal( mypair%first%fstr, "Hello", "StringRealHistogramPairList_test: at begin key" )
		call assert_equal( mypair%second%size(), 8, "StringRealHistogramPairList_test: at begin val size" )
		
		mypair = mylist%at( 1 )
		call assert_equal( mypair%first%fstr, "Hello", "StringRealHistogramPairList_test: at 1 key" )
		
		iter => mylist%begin
		iter => iter%next
		iter => iter%next
		iterPos => iter ! Should point to "string", size 1
		
		mypair = mylist%at( iterPos )
		call assert_equal( mypair%first%fstr, "string", "StringRealHistogramPairList_test: at iterPos key" )
		call assert_equal( mypair%second%size(), 1, "StringRealHistogramPairList_test: at iterPos val size" )
		
		mypair = mylist%at( mylist%end )
		call assert_equal( mypair%first%fstr, "list", "StringRealHistogramPairList_test: at end key" )
		call assert_equal( mypair%second%size(), 2, "StringRealHistogramPairList_test: at end val size" )
		
		mypair = mylist%at( mylist%size() )
		call assert_equal( mypair%first%fstr, "list", "StringRealHistogramPairList_test: at size key" )
		
		str = "Prueba"
		hist = RealHistogram( Histogram_STURGES )
		call hist%add( [23.81071_8, 22.70389_8, 22.70389_8, 22.70389_8] )
		call mypair%init( str, hist )
		call mylist%insert( iterPos, mypair ) ! Inserts after iterPos (string)
		call assert_equal( mylist%size(), 5, "StringRealHistogramPairList_test: size after insert" )
		
		mypair = mylist%at( 4 )
		call assert_equal( mypair%first%fstr, "Prueba", "StringRealHistogramPairList_test: inserted key" )
		call assert_equal( mypair%second%size(), 4, "StringRealHistogramPairList_test: inserted val size" )
		
		! Reset iterPos to point to index 4 (Prueba)
		iter => mylist%begin
		iter => iter%next
		iter => iter%next
		iter => iter%next
		iterPos => iter
		
		call mylist%erase( iterPos )
		call assert_equal( mylist%size(), 4, "StringRealHistogramPairList_test: size after erase" )
		
		mypair = mylist%at( 4 )
		call assert_equal( mypair%first%fstr, "list", "StringRealHistogramPairList_test: key after erase" )
		
		call mylist%clear()
		call assert_equal( mylist%size(), 0, "StringRealHistogramPairList_test: size after clear" )
		
end program test_StringRealHistogramPairList

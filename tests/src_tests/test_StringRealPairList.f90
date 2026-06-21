program test_StringRealPairList
    use StringRealPairList_
    use TestUtils_
    use IOStream_
    use String_
    use StringRealPair_
    implicit none
		type(String) :: str
		type(StringRealPair) :: mypair
		type(StringRealPairList) :: mylist
		class(StringRealPairListIterator), pointer :: iter
		class(StringRealPairListIterator), pointer :: iterPos
		
		mylist = StringRealPairList()
		call assert_equal( mylist%size(), 0, "StringRealPairList_test: empty size" )
		
		str = "Hello"
		call mypair%init( str, 3.0_8 )
		call mylist%append( mypair )
		
		str = "class"
		call mypair%init( str, 2.0_8 )
		call mylist%append( mypair )
		
		str = "string"
		call mypair%init( str, 6.0_8 )
		call mylist%append( mypair )
		
		str = "list"
		call mypair%init( str, 9.0_8 )
		call mylist%append( mypair )
		
		call assert_equal( mylist%size(), 4, "StringRealPairList_test: size after append" )
		
		str = "day"
		call mypair%init( str, 3.0_8 )
		call mylist%prepend( mypair )
		
		str = "control"
		call mypair%init( str, 2.0_8 )
		call mylist%prepend( mypair )
		
		call assert_equal( mylist%size(), 6, "StringRealPairList_test: size after prepend" )
		
		! List: control, day, Hello, class, string, list
		mypair = mylist%at( mylist%begin )
		call assert_equal( mypair%first%fstr, "control", "StringRealPairList_test: at begin key" )
		call assert_true( abs(mypair%second - 2.0_8) < 1e-12_8, "StringRealPairList_test: at begin val" )
		
		mypair = mylist%at( 1 )
		call assert_equal( mypair%first%fstr, "control", "StringRealPairList_test: at 1 key" )
		
		iter => mylist%begin
		iter => iter%next
		iter => iter%next
		iterPos => iter ! points to "Hello", 3.0
		
		mypair = mylist%at( iterPos )
		call assert_equal( mypair%first%fstr, "Hello", "StringRealPairList_test: at iterPos key" )
		call assert_true( abs(mypair%second - 3.0_8) < 1e-12_8, "StringRealPairList_test: at iterPos val" )
		
		mypair = mylist%at( mylist%end )
		call assert_equal( mypair%first%fstr, "list", "StringRealPairList_test: at end key" )
		call assert_true( abs(mypair%second - 9.0_8) < 1e-12_8, "StringRealPairList_test: at end val" )
		
		mypair = mylist%at( mylist%size() )
		call assert_equal( mypair%first%fstr, "list", "StringRealPairList_test: at size key" )
		
		str = "Prueba"
		call mypair%init( str, 15.0_8 )
		call mylist%insert( iterPos, mypair ) ! Inserts after iterPos (Hello)
		call assert_equal( mylist%size(), 7, "StringRealPairList_test: size after insert" )
		
		mypair = mylist%at( 4 )
		call assert_equal( mypair%first%fstr, "Prueba", "StringRealPairList_test: inserted key" )
		call assert_true( abs(mypair%second - 15.0_8) < 1e-12_8, "StringRealPairList_test: inserted val" )
		
		! Reset iterPos to point to index 4 (Prueba)
		iter => mylist%begin
		iter => iter%next
		iter => iter%next
		iter => iter%next
		iterPos => iter
		
		call mylist%erase( iterPos )
		call assert_equal( mylist%size(), 6, "StringRealPairList_test: size after erase" )
		
		mypair = mylist%at( 4 )
		call assert_equal( mypair%first%fstr, "class", "StringRealPairList_test: key after erase" )
		
		call mylist%clear()
		call assert_equal( mylist%size(), 0, "StringRealPairList_test: size after clear" )
		
		str = "Hello1 aaaaaa"
		call mypair%init( str, 21.0_8 )
		call mylist%append( mypair )
		
		str = "Hello2 bbbbb ccccc"
		call mypair%init( str, 31.0_8 )
		call mylist%append( mypair )
		
		call assert_equal( mylist%size(), 2, "StringRealPairList_test: size after second append" )
		
		call mylist%clear()
		call assert_equal( mylist%size(), 0, "StringRealPairList_test: size after second clear" )
		
end program test_StringRealPairList

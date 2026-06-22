program test_StringIntegerPairList
    use StringIntegerPairList_
    use TestUtils_
    use IOStream_
    use String_
    use StringIntegerPair_
    implicit none
		type(String) :: str
		type(StringIntegerPair) :: mypair
		type(StringIntegerPairList) :: mylist
		type(StringIntegerPairListIterator), pointer :: iter
		type(StringIntegerPairListIterator), pointer :: iterPos
		
		mylist = StringIntegerPairList()
		call assert_equal( mylist%size(), 0, "StringIntegerPairList_test: empty size" )
		
		str = "Hello"
		mypair = StringIntegerPair( str, 3 )
		call mylist%append( mypair )
		
		str = "class"
		mypair = StringIntegerPair( str, 2 )
		call mylist%append( mypair )
		
		str = "string"
		mypair = StringIntegerPair( str, 6 )
		call mylist%append( mypair )
		
		str = "list"
		mypair = StringIntegerPair( str, 9 )
		call mylist%append( mypair )
		
		call assert_equal( mylist%size(), 4, "StringIntegerPairList_test: size after append" )
		
		str = "day"
		mypair = StringIntegerPair( str, 3 )
		call mylist%prepend( mypair )
		
		str = "control"
		mypair = StringIntegerPair( str, 2 )
		call mylist%prepend( mypair )
		
		call assert_equal( mylist%size(), 6, "StringIntegerPairList_test: size after prepend" )
		
		! List should be: control, day, Hello, class, string, list
		mypair = mylist%at( mylist%begin )
		call assert_equal( mypair%first%fstr, "control", "StringIntegerPairList_test: at begin key" )
		call assert_equal( mypair%second, 2, "StringIntegerPairList_test: at begin val" )
		
		mypair = mylist%at( 1 )
		call assert_equal( mypair%first%fstr, "control", "StringIntegerPairList_test: at 1 key" )
		call assert_equal( mypair%second, 2, "StringIntegerPairList_test: at 1 val" )
		
		iter => mylist%begin
		iter => iter%next
		iter => iter%next
		iterPos => iter ! Should point to "Hello", 3
		
		mypair = mylist%at( iterPos )
		call assert_equal( mypair%first%fstr, "Hello", "StringIntegerPairList_test: at iterPos key" )
		call assert_equal( mypair%second, 3, "StringIntegerPairList_test: at iterPos val" )
		
		mypair = mylist%at( mylist%end )
		call assert_equal( mypair%first%fstr, "list", "StringIntegerPairList_test: at end key" )
		call assert_equal( mypair%second, 9, "StringIntegerPairList_test: at end val" )
		
		mypair = mylist%at( mylist%size() )
		call assert_equal( mypair%first%fstr, "list", "StringIntegerPairList_test: at size key" )
		
		str = "Prueba"
		mypair = StringIntegerPair( str, 15 )
		call mylist%insert( iterPos, mypair ) ! Inserts at iterPos (before Hello)
		call assert_equal( mylist%size(), 7, "StringIntegerPairList_test: size after insert" )
		
		mypair = mylist%at( 4 )
		call assert_equal( mypair%first%fstr, "Prueba", "StringIntegerPairList_test: inserted key" )
		call assert_equal( mypair%second, 15, "StringIntegerPairList_test: inserted val" )
		
		! iterPos still points to "Hello", which is now at index 4
		call mylist%erase( iterPos )
		call assert_equal( mylist%size(), 6, "StringIntegerPairList_test: size after erase" )
		
		mypair = mylist%at( 4 )
		call assert_equal( mypair%first%fstr, "class", "StringIntegerPairList_test: key after erase" )
		
		call mylist%clear()
		call assert_equal( mylist%size(), 0, "StringIntegerPairList_test: size after clear" )
		
		str = "Hello1 aaaaaa"
		mypair = StringIntegerPair( str, 21 )
		call mylist%append( mypair )
		
		str = "Hello2 bbbbb ccccc"
		mypair = StringIntegerPair( str, 31 )
		call mylist%append( mypair )
		
		call assert_equal( mylist%size(), 2, "StringIntegerPairList_test: size after second append" )
		
		call mylist%clear()
		call assert_equal( mylist%size(), 0, "StringIntegerPairList_test: size after second clear" )
		
end program test_StringIntegerPairList

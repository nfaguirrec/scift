program test_StringIntegerPair
    use StringIntegerPair_
    use TestUtils_
    use String_
    implicit none
		type(String) :: str
		type(StringIntegerPair) :: mypair1
		type(StringIntegerPair) :: mypair2
		
		str = "Hola"
		mypair1 = StringIntegerPair( str, 3 )
		call assert_equal( mypair1%first%fstr, "Hola", "StringIntegerPair_test: mypair1 key" )
		call assert_equal( mypair1%second, 3, "StringIntegerPair_test: mypair1 value" )
		
		str = "Entonces"
		mypair2 = StringIntegerPair( str, 4 )
		call assert_equal( mypair2%first%fstr, "Entonces", "StringIntegerPair_test: mypair2 key" )
		call assert_equal( mypair2%second, 4, "StringIntegerPair_test: mypair2 value" )
		
		mypair1 = mypair2
		call assert_equal( mypair1%first%fstr, "Entonces", "StringIntegerPair_test: copy key" )
		call assert_equal( mypair1%second, 4, "StringIntegerPair_test: copy value" )
end program test_StringIntegerPair

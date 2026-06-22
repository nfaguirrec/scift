program test_StringRealPair
    use StringRealPair_
    use TestUtils_
    use String_
    implicit none
		type(String) :: str
		type(StringRealPair) :: mypair1
		type(StringRealPair) :: mypair2
		
		str = "Hola"
		mypair1 = StringRealPair( str, 3.0_8 )
		call assert_equal( mypair1%first%fstr, "Hola", "StringRealPair_test: mypair1 key" )
		call assert_true( abs(mypair1%second - 3.0_8) < 1e-12_8, "StringRealPair_test: mypair1 value" )
		
		str = "Entonces"
		mypair2 = StringRealPair( str, 4.0_8 )
		call assert_equal( mypair2%first%fstr, "Entonces", "StringRealPair_test: mypair2 key" )
		call assert_true( abs(mypair2%second - 4.0_8) < 1e-12_8, "StringRealPair_test: mypair2 value" )
		
		mypair1 = mypair2
		call assert_equal( mypair1%first%fstr, "Entonces", "StringRealPair_test: copy key" )
		call assert_true( abs(mypair1%second - 4.0_8) < 1e-12_8, "StringRealPair_test: copy value" )
end program test_StringRealPair

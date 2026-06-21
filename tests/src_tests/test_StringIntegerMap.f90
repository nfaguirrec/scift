program test_StringIntegerMap
    use StringIntegerMap_
    use TestUtils_
    use IOStream_
    use String_
    use StringIntegerPair_
    use StringIntegerPairList_, StringIntegerMapIterator => StringIntegerPairListIterator
    implicit none
		type(String) :: str
		type(StringIntegerPair) :: pair
		type(StringIntegerMap) :: mymap
		type(StringIntegerMapIterator), pointer :: ptr
		
		mymap = StringIntegerMap()
		call assert_equal( mymap%size(), 0, "StringIntegerMap_test: size empty" )
		
		str = "Ademas"
		call mymap%insert( str, 45 )
		call assert_equal( mymap%size(), 1, "StringIntegerMap_test: size after 1 insert" )
		call assert_equal( mymap%at( str ), 45, "StringIntegerMap_test: value of Ademas" )
		
		str = "Amor"
		call mymap%insert( str, 3 )
		call assert_equal( mymap%size(), 2, "StringIntegerMap_test: size after 2 inserts" )
		call assert_equal( mymap%at( str ), 3, "StringIntegerMap_test: value of Amor" )
		
		str = "Entonces"
		call mymap%insert( str, 8 )
		
		str = "Corazon"
		call mymap%insert( str, 9 )
		
		str = "Hola"
		call mymap%insert( str, 4 )
		
		str = "Conejo"
		call mymap%insert( str, 24 )
		
		call assert_equal( mymap%size(), 6, "StringIntegerMap_test: size after 6 inserts" )
		
		str = "Corazon"
		call assert_true( mymap%find( str, ptr ), "StringIntegerMap_test: find Corazon" )
		pair = mymap%pair( ptr )
		call assert_equal( pair%first%fstr, "Corazon", "StringIntegerMap_test: key of Corazon" )
		call assert_equal( pair%second, 9, "StringIntegerMap_test: value of Corazon" )
		
		str = "Corazon "
		call assert_true( .not. mymap%find( str, ptr ), "StringIntegerMap_test: find Corazon with trailing space" )
		
		str = "Conejo"
		call mymap%erase( str )
		call assert_equal( mymap%size(), 5, "StringIntegerMap_test: size after erase Conejo" )
		
		str = "Entonces"
		call mymap%erase( str )
		
		str = "Amor"
		call mymap%erase( str )
		call assert_equal( mymap%size(), 3, "StringIntegerMap_test: size after erase and 3 left" )
		
		call mymap%clear()
		call assert_equal( mymap%size(), 0, "StringIntegerMap_test: size after clear" )
		
		str = "Ademas"
		call mymap%insert( str, 45 )
		
		str = "Amor"
		call mymap%insert( str, 3 )
		
		str = "Amor"
		call mymap%set( str, 8 )
		call assert_equal( mymap%at( str ), 8, "StringIntegerMap_test: value of Amor after set" )
		
		str = "Hola"
		call mymap%set( str, 56 )
		call assert_equal( mymap%at( str ), 56, "StringIntegerMap_test: value of Hola after set non-existent" )
		call assert_equal( mymap%size(), 3, "StringIntegerMap_test: size after set non-existent" )
		
		call mymap%clear()
		
		str = "HHHHHHH"
		call mymap%set( str, 56 )
		call assert_equal( mymap%at( str ), 56, "StringIntegerMap_test: set HHHHHHH" )
		
		str = "HHHHHHH"
		call mymap%insert( str, 60 )
		call assert_equal( mymap%at( str ), 60, "StringIntegerMap_test: insert HHHHHHH duplicates key but updates" )
		
end program test_StringIntegerMap

program test_StringRealMap
    use StringRealMap_
    use TestUtils_
    use IOStream_
    use String_
    use StringRealPair_
    use StringRealPairList_, StringRealMapIterator => StringRealPairListIterator
    implicit none
		type(String) :: str
		type(StringRealPair) :: pair
		type(StringRealMap) :: mymap
		class(StringRealMapIterator), pointer :: ptr
		
		mymap = StringRealMap()
		call assert_equal( mymap%size(), 0, "StringRealMap_test: size empty" )
		
		str = "Ademas"
		call mymap.insert( str, 45.0_8 )
		call assert_equal( mymap%size(), 1, "StringRealMap_test: size after 1 insert" )
		call assert_true( abs(mymap%at( str ) - 45.0_8) < 1e-12_8, "StringRealMap_test: value of Ademas" )
		
		str = "Amor"
		call mymap.insert( str, 3.0_8 )
		call assert_equal( mymap%size(), 2, "StringRealMap_test: size after 2 inserts" )
		call assert_true( abs(mymap%at( str ) - 3.0_8) < 1e-12_8, "StringRealMap_test: value of Amor" )
		
		str = "Entonces"
		call mymap.insert( str, 8.0_8 )
		
		str = "Corazon"
		call mymap.insert( str, 9.0_8 )
		
		str = "Hola"
		call mymap.insert( str, 4.0_8 )
		
		str = "Conejo"
		call mymap.insert( str, 24.0_8 )
		
		call assert_equal( mymap%size(), 6, "StringRealMap_test: size after 6 inserts" )
		
		str = "Corazon"
		call assert_true( mymap.find( str, ptr ), "StringRealMap_test: find Corazon" )
		pair = mymap.pair( ptr )
		call assert_equal( pair%first%fstr, "Corazon", "StringRealMap_test: key of Corazon" )
		call assert_true( abs(pair%second - 9.0_8) < 1e-12_8, "StringRealMap_test: value of Corazon" )
		
		str = "Corazon "
		call assert_true( .not. mymap.find( str, ptr ), "StringRealMap_test: find Corazon with trailing space" )
		
		str = "Conejo"
		call mymap.erase( str )
		call assert_equal( mymap%size(), 5, "StringRealMap_test: size after erase Conejo" )
		
		str = "Entonces"
		call mymap.erase( str )
		
		str = "Amor"
		call mymap.erase( str )
		call assert_equal( mymap%size(), 3, "StringRealMap_test: size after erase and 3 left" )
		
		call mymap.clear()
		call assert_equal( mymap%size(), 0, "StringRealMap_test: size after clear" )
		
		str = "Ademas"
		call mymap.insert( str, 45.0_8 )
		
		str = "Amor"
		call mymap.insert( str, 3.0_8 )
		
		str = "Amor"
		call mymap.set( str, 8.0_8 )
		call assert_true( abs(mymap%at( str ) - 8.0_8) < 1e-12_8, "StringRealMap_test: value of Amor after set" )
		
		str = "Hola"
		call mymap.set( str, 56.0_8 )
		call assert_true( abs(mymap%at( str ) - 56.0_8) < 1e-12_8, "StringRealMap_test: value of Hola after set non-existent" )
		call assert_equal( mymap%size(), 3, "StringRealMap_test: size after set non-existent" )
		
		call mymap.clear()
		
		str = "HHHHHHH"
		call mymap.set( str, 56.0_8 )
		call assert_true( abs(mymap%at( str ) - 56.0_8) < 1e-12_8, "StringRealMap_test: set HHHHHHH" )
		
		str = "HHHHHHH"
		call mymap.insert( str, 60.0_8 )
		call assert_true( abs(mymap%at( str ) - 60.0_8) < 1e-12_8, "StringRealMap_test: insert HHHHHHH duplicates key but updates" )
		
end program test_StringRealMap

program test_StringList
    use StringList_
    use TestUtils_
    use String_
    use IOStream_
    implicit none
		type(String) :: str
		type(StringList) :: mylist
		type(StringListIterator), pointer :: iter
		type(StringListIterator), pointer :: iterPos
		
		mylist = StringList()
		call assert_equal( mylist%size(), 0, "StringList_test: empty size" )
		
		str = "Hello"
		call mylist%append( str )
		str = "class"
		call mylist%append( str )
		str = "string"
		call mylist%append( str )
		str = "list"
		call mylist%append( str )
		
		call assert_equal( mylist%size(), 4, "StringList_test: size after append" )
		
		str = "day"
		call mylist%prepend( str )
		str = "control"
		call mylist%prepend( str )
		
		call assert_equal( mylist%size(), 6, "StringList_test: size after prepend" )
		
		! List: control, day, Hello, class, string, list
		str = mylist%at( mylist%begin )
		call assert_equal( str%fstr, "control", "StringList_test: at begin" )
		
		str = mylist%at( 1 )
		call assert_equal( str%fstr, "control", "StringList_test: at 1" )
		
		iter => mylist%begin
		iter => iter%next
		iter => iter%next
		iterPos => iter ! Should point to "Hello" (index 3)
		
		str = mylist%at( iterPos )
		call assert_equal( str%fstr, "Hello", "StringList_test: at iterPos" )
		
		str = mylist%at( mylist%end )
		call assert_equal( str%fstr, "list", "StringList_test: at end" )
		
		str = mylist%at( mylist%size() )
		call assert_equal( str%fstr, "list", "StringList_test: at size" )
		
		str = "Prueba"
		call mylist%insert( iterPos, str ) ! Inserts after Hello
		call assert_equal( mylist%size(), 7, "StringList_test: size after insert Prueba" )
		
		str = mylist%at( 4 )
		call assert_equal( str%fstr, "Prueba", "StringList_test: at 4" )
		
		str = "Prueba2"
		call mylist%insert( mylist%end, str ) ! Inserts after list
		call assert_equal( mylist%size(), 8, "StringList_test: size after insert Prueba2" )
		
		str = mylist%at( 8 )
		call assert_equal( str%fstr, "Prueba2", "StringList_test: at 8" )
		
		str = "Corazon"
		call mylist%insert( mylist%begin, str ) ! Inserts after control
		call assert_equal( mylist%size(), 9, "StringList_test: size after insert Corazon" )
		
		str = mylist%at( 2 )
		call assert_equal( str%fstr, "Corazon", "StringList_test: at 2" )
		
		! Reset iterPos to index 4 (which is Hello in: control, Corazon, day, Hello, Prueba, ...)
		iter => mylist%begin
		iter => iter%next
		iter => iter%next
		iter => iter%next
		iterPos => iter
		
		str = mylist%at( iterPos )
		call assert_equal( str%fstr, "Hello", "StringList_test: Hello position check" )
		
		str = "PruebaRep"
		call mylist%replace( iterPos, str )
		
		str = mylist%at( iterPos )
		call assert_equal( str%fstr, "PruebaRep", "StringList_test: after replace" )
		
		call mylist%erase( iterPos )
		call assert_equal( mylist%size(), 8, "StringList_test: size after erase" )
		
		str = mylist%at( 4 )
		call assert_equal( str%fstr, "Prueba", "StringList_test: index 4 after erase" )
		
		call mylist%erase( mylist%begin )
		call assert_equal( mylist%size(), 7, "StringList_test: size after erase begin" )
		str = mylist%at( 1 )
		call assert_equal( str%fstr, "Corazon", "StringList_test: begin element after erase" )
		
		call mylist%erase( mylist%end )
		call assert_equal( mylist%size(), 6, "StringList_test: size after erase end" )
		str = mylist%at( mylist%end )
		call assert_equal( str%fstr, "list", "StringList_test: end element after erase" )
		
		call mylist%clear()
		call assert_equal( mylist%size(), 0, "StringList_test: size after clear" )
		
		str = "Hello1 aaaaaa"
		call mylist%append( str )
		str = "Hello2 bbbbb ccccc"
		call mylist%append( str )
		call assert_equal( mylist%size(), 2, "StringList_test: size after second append" )
		
		call mylist%clear()
		call assert_equal( mylist%size(), 0, "StringList_test: size after second clear" )
		
end program test_StringList

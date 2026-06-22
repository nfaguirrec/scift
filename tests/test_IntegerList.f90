program test_IntegerList
    use IntegerList_
    use TestUtils_
    use IOStream_
    implicit none
		type(IntegerList) :: mylist
		type(IntegerListIterator), pointer :: iter
		
		mylist = IntegerList()
		
		! Testing append
		call mylist%append( 8 )
		call mylist%append( 5 )
		call mylist%append( 1 )
		call assert_equal( mylist%size(), 3, "IntegerList_test: size after append" )
		
		! Testing erase
		iter => mylist%begin
		iter => iter%next
		call mylist%erase( iter )
		call assert_equal( mylist%size(), 2, "IntegerList_test: size after erase iter" )
		
		call mylist%erase( mylist%begin )
		call assert_equal( mylist%size(), 1, "IntegerList_test: size after erase begin" )
		
		call mylist%erase( mylist%end )
		call assert_equal( mylist%size(), 0, "IntegerList_test: size after erase end" )
		
		! Testing clear
		call mylist%clear()
		call assert_equal( mylist%size(), 0, "IntegerList_test: size after clear" )
		
		call mylist%append( 1 )
		call mylist%append( 2 )
		call mylist%append( 3 )
		call assert_equal( mylist%size(), 3, "IntegerList_test: size after final append" )
end program test_IntegerList

program test_IntegerHyperList
    use IntegerHyperList_
    use TestUtils_
    use Math_
    use IOStream_
    use IntegerList_
    use Matrix_
    implicit none
		type(IntegerHyperList) :: hilist
		class(IntegerHyperListIterator), pointer :: iter
		type(IntegerList) :: ilist
		
		hilist = IntegerHyperList()
		
		! Testing append
		ilist = IntegerList( 3, value=1 )
		call hilist%append( ilist )
		ilist = IntegerList( 4, value=2 )
		call hilist%append( ilist )
		ilist = IntegerList( 2, value=1 )
		call hilist%append( ilist )
		call assert_equal( hilist%size(), 3, "IntegerHyperList_test: size after append" )
		
		! Testing prepend
		ilist = IntegerList( 2, value=4 )
		call hilist%prepend( ilist )
		ilist = IntegerList( 4, value=5 )
		call hilist%prepend( ilist )
		ilist = IntegerList( 3, value=8 )
		call hilist%prepend( ilist )
		call assert_equal( hilist%size(), 6, "IntegerHyperList_test: size after prepend" )
		
		! Testing insert
		iter => hilist%begin
		iter => iter%next
		iter => iter%next
		ilist = IntegerList( 3, value=9 )
		call hilist%insert( iter, ilist )
		call assert_equal( hilist%size(), 7, "IntegerHyperList_test: size after insert 1" )
		
		ilist = IntegerList( 4, value=8 )
		call hilist%insert( iter, ilist )
		call assert_equal( hilist%size(), 8, "IntegerHyperList_test: size after insert 2" )
		
		ilist = IntegerList( 2, value=7 )
		call hilist%insert( hilist%end, ilist )
		call assert_equal( hilist%size(), 9, "IntegerHyperList_test: size after insert 3" )
		
		! Testing erase
		call hilist%erase( 2 )
		call assert_equal( hilist%size(), 8, "IntegerHyperList_test: size after erase index 2" )
		
		iter => hilist%begin
		iter => iter%next
		call hilist%erase( iter )
		call assert_equal( hilist%size(), 7, "IntegerHyperList_test: size after erase iter" )
		
		call hilist%erase( hilist%begin )
		call assert_equal( hilist%size(), 6, "IntegerHyperList_test: size after erase begin" )
		
		call hilist%erase( hilist%end )
		call assert_equal( hilist%size(), 5, "IntegerHyperList_test: size after erase end" )
		
		! Testing clear
		call hilist%clear()
		call assert_equal( hilist%size(), 0, "IntegerHyperList_test: size after clear" )
end program test_IntegerHyperList

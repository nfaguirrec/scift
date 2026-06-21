program test_RealList
    use RealList_
    use TestUtils_
    use IOStream_
    implicit none
		type(RealList) :: mylist, mylist2
		class(RealListIterator), pointer :: iter
		
		mylist = RealList()
		
		write(*,*) "-------------------------"
		write(*,*) "Testing for append method"
		write(*,*) "-------------------------"
		
		write(*,*) "call mylist.append( 8.0 )"
		write(*,*) "call mylist.append( 5.0 )"
		write(*,*) "call mylist.append( 1.0 )"
		write(*,*)
		
		call mylist%append( 8.0_8 )
		call mylist%append( 5.0_8 )
		call mylist%append( 1.0_8 )
		
		call showMyListForward( mylist )
		call assert_equal( mylist%size(), 3, "List size after append" )
		call assert_equal_real( mylist%at(1), 8.0_8, 1e-10_8, "List at(1) after append" )
		call assert_equal_real( mylist%at(3), 1.0_8, 1e-10_8, "List at(3) after append" )

		write(*,*) "-------------------------"
		write(*,*) "Testing copy method"
		write(*,*) "-------------------------"
		mylist2 = mylist
		call showMyListForward( mylist2 )
		call assert_equal( mylist2%size(), 3, "Copied list size" )
		call assert_equal_real( mylist2%at(1), 8.0_8, 1e-10_8, "Copied list at(1)" )
		
		write(*,*) "-------------------------"
		write(*,*) "Testing for prepend method"
		write(*,*) "-------------------------"
		
		write(*,*) "call mylist.prepend( 2.0 )"
		write(*,*) "call mylist.prepend( 7.0 )"
		write(*,*) "call mylist.prepend( 0.0 )"
		write(*,*)
		
		call mylist%prepend( 2.0_8 )
		call mylist%prepend( 7.0_8 )
		call mylist%prepend( 0.0_8 )
		
		call showMyListForward( mylist )
		call assert_equal( mylist%size(), 6, "List size after prepend" )
		call assert_equal_real( mylist%at(1), 0.0_8, 1e-10_8, "List at(1) after prepend" )
		
		write(*,*) "-------------------------"
		write(*,*) "Testing for insert method"
		write(*,*) "-------------------------"
		
		write(*,*) "iter => mylist.begin"
		write(*,*) "iter => iter.next"
		write(*,*) "iter => iter.next"
		write(*,*) "call mylist.insert( iter, 1.0 )"
		write(*,*)
		
		iter => mylist%begin
		iter => iter%next
		iter => iter%next
		
		call mylist%insert( iter, 1.0_8 )
		call showMyListForward( mylist )
		call assert_equal( mylist%size(), 7, "List size after insert" )
		call assert_equal_real( mylist%at(4), 1.0_8, 1e-10_8, "List at(4) after insert" )
		
		write(*,*)
		write(*,*) "call mylist.insert( iter, 2.0 )"
		write(*,*)
		
		call mylist%insert( iter, 2.0_8 )
		call showMyListForward( mylist )
		
		write(*,*)
		write(*,*) "call mylist.insert( mylist.end, 9.0 )"
		write(*,*)
				
		call mylist%insert( mylist%end, 9.0_8 )
		call showMyListForward( mylist )

		write(*,*) "------------------------"
		write(*,*) "Testing for erase method"
		write(*,*) "------------------------"
		
		write(*,*) "iter => mylist.begin"
		write(*,*) "iter => iter.next"
		write(*,*) "call mylist.erase( iter )"
		write(*,*)
		
		iter => mylist%begin
		iter => iter%next
		
		call mylist%erase( iter )
		call showMyListForward( mylist )
		
		write(*,*)
		write(*,*) "call mylist.erase( mylist.begin )"
		write(*,*)
		
		call mylist%erase( mylist%begin )
		call showMyListForward( mylist )
		
		write(*,*)
		write(*,*) "call mylist.erase( mylist.end )"
		write(*,*)
		call mylist%erase( mylist%end )
		call showMyListForward( mylist )
		
		write(*,*) "------------------------"
		write(*,*) "Testing for clear method"
		write(*,*) "------------------------"
		
		write(*,*) "call mylist.clear()"
		write(*,*)
		call mylist%clear()
		call showMyListForward( mylist )
		call assert_equal( mylist%size(), 0, "List size after clear" )
		
		write(*,*) "call mylist.clear()"
		write(*,"(A)") "call mylist.append( ( [ 1.0_8, 2.0_8, 3.0_8, 4.0_8, 5.0_8, 6.0_8, 7.0_8, 8.0_8], 8 )"
		write(*,*)
		
		call mylist%clear()
		call mylist%append( [ 1.0_8, 2.0_8, 3.0_8, 4.0_8, 5.0_8, 6.0_8, 7.0_8, 8.0_8] )
		call showMyListForward( mylist )
		call showMyListBackward( mylist )
		call assert_equal( mylist%size(), 8, "List size after bulk append" )
		
		write(*,*)
		write(*,*) "call mylist.clear()"
		write(*,"(X,A)") "call mylist.append( ( [ 1.0_8, 2.0_8, 3.0_8, 4.0_8, 5.0_8, 6.0_8, 7.0_8, 8.0_8] )"
		call mylist%clear()
		call mylist%append( [ 1.0_8, 2.0_8, 3.0_8, 4.0_8, 5.0_8, 6.0_8, 7.0_8, 8.0_8] )
		write(*,*) "call mylist.eraseAllExcept( [1,3,5] )"
		call mylist%eraseAllExcept( [1,3,5] )
		call showMyListForward( mylist )
		call assert_equal( mylist%size(), 3, "List size after eraseAllExcept" )
		
		write(*,*)
		write(*,*) "call mylist.clear()"
		write(*,"(X,A)") "call mylist.append( ( [ 1.0_8, 2.0_8, 3.0_8, 4.0_8, 5.0_8, 6.0_8, 7.0_8, 8.0_8] )"
		call mylist%clear()
		call mylist%append( [ 1.0_8, 2.0_8, 3.0_8, 4.0_8, 5.0_8, 6.0_8, 7.0_8, 8.0_8] )
		write(*,*) "call mylist.eraseAllExceptFirst( 4 )"
		call mylist%eraseAllExceptFirst( 4 )
		call showMyListForward( mylist )
		call assert_equal( mylist%size(), 4, "List size after eraseAllExceptFirst" )
		
		write(*,*)
		write(*,*) "call mylist.clear()"
		write(*,"(X,A)") "call mylist.append( ( [ 1.0_8, 2.0_8, 3.0_8, 4.0_8, 5.0_8, 6.0_8, 7.0_8, 8.0_8] )"
		call mylist%clear()
		call mylist%append( [ 1.0_8, 2.0_8, 3.0_8, 4.0_8, 5.0_8, 6.0_8, 7.0_8, 8.0_8] )
		write(*,*) "call mylist.eraseAllExceptLast( 4 )"
		call mylist%eraseAllExceptLast( 4 )
		call showMyListForward( mylist )
		call assert_equal( mylist%size(), 4, "List size after eraseAllExceptLast" )
		
		write(*,*) "------------------------"
		write(*,*) "Testing for get methods"
		write(*,*) "------------------------"
		
		write(*,*) "call mylist.at(3) ==>", mylist%at(3)
		write(*,*) "call mylist.at(1) ==>", mylist%at(1)
		
		write(*,*) "All RealList tests PASSED"
end program test_RealList

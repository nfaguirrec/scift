program test_IntegerVector
    use IntegerVector_
    use TestUtils_
    use IOStream_
    use String_
    implicit none
		type(IntegerVector) :: myvector
		
		call myvector%init()
		call assert_equal( myvector%size(), 0, "IntegerVector_test: initial size" )
		
		call myvector%append( 8 )
		call myvector%append( 5 )
		call myvector%append( 1 )
		call assert_equal( myvector%size(), 3, "IntegerVector_test: size after append" )
		call assert_equal( myvector%at(1), 8, "IntegerVector_test: append elem 1" )
		call assert_equal( myvector%at(2), 5, "IntegerVector_test: append elem 2" )
		call assert_equal( myvector%at(3), 1, "IntegerVector_test: append elem 3" )
		
		call myvector%prepend( 8 )
		call myvector%prepend( 5 )
		call myvector%prepend( 1 )
		call assert_equal( myvector%size(), 6, "IntegerVector_test: size after prepend" )
		call assert_equal( myvector%at(1), 1, "IntegerVector_test: prepend elem 1" )
		call assert_equal( myvector%at(2), 5, "IntegerVector_test: prepend elem 2" )
		call assert_equal( myvector%at(3), 8, "IntegerVector_test: prepend elem 3" )
		call assert_equal( myvector%at(4), 8, "IntegerVector_test: prepend elem 4" )
		call assert_equal( myvector%at(5), 5, "IntegerVector_test: prepend elem 5" )
		call assert_equal( myvector%at(6), 1, "IntegerVector_test: prepend elem 6" )
		
		call myvector%erase( 1 )
		call assert_equal( myvector%size(), 5, "IntegerVector_test: size after erase 1" )
		call assert_equal( myvector%at(1), 5, "IntegerVector_test: elem 1 after erase 1" )
		
		call myvector%erase( 2 )
		call assert_equal( myvector%size(), 4, "IntegerVector_test: size after erase 2" )
		call assert_equal( myvector%at(2), 8, "IntegerVector_test: elem 2 after erase 2" )
		
		call myvector%erase( 3 )
		call assert_equal( myvector%size(), 3, "IntegerVector_test: size after erase 3" )
		call assert_equal( myvector%at(3), 1, "IntegerVector_test: elem 3 after erase 3" )
		
		call myvector%erase( 1 )
		call assert_equal( myvector%size(), 2, "IntegerVector_test: size after erase 4" )
		call assert_equal( myvector%at(1), 8, "IntegerVector_test: elem 1 after erase 4" )
		
		call myvector%erase( 1 )
		call assert_equal( myvector%size(), 1, "IntegerVector_test: size after erase 5" )
		call assert_equal( myvector%at(1), 1, "IntegerVector_test: elem 1 after erase 5" )
		
		call myvector%erase( 1 )
		call assert_equal( myvector%size(), 0, "IntegerVector_test: size after erase 6" )
		
		call myvector%erase( 1 )
		call assert_equal( myvector%size(), 0, "IntegerVector_test: size after erase 7" )
		
		call myvector%erase( 2 )
		call assert_equal( myvector%size(), 0, "IntegerVector_test: size after erase 8" )
		
		call myvector%clear()
		call assert_equal( myvector%size(), 0, "IntegerVector_test: size after clear" )
		
		call myvector%append( 1 )
		call myvector%append( 2 )
		call myvector%append( 3 )
		call assert_equal( myvector%size(), 3, "IntegerVector_test: size after final append" )
		call assert_equal( myvector%at(1), 1, "IntegerVector_test: final append elem 1" )
		call assert_equal( myvector%at(2), 2, "IntegerVector_test: final append elem 2" )
		call assert_equal( myvector%at(3), 3, "IntegerVector_test: final append elem 3" )
		
end program test_IntegerVector

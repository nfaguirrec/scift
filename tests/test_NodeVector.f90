program test_NodeVector
    use NodeVector_
    use TestUtils_
    use IOStream_
    use Node_
    implicit none
		type(NodeVector) :: myvector
		type(Node) :: nd1, nd2, nd3
		type(Node) :: tmpNode
		
		call nd1%init( id=8 )
		call nd2%init( id=5 )
		call nd3%init( id=1 )
		
		call myvector%init()
		call assert_equal( myvector%size(), 0, "NodeVector_test: initial size" )
		
		call myvector%append( nd1 )
		call myvector%append( nd2 )
		call myvector%append( nd3 )
		call assert_equal( myvector%size(), 3, "NodeVector_test: size after append" )
		tmpNode = myvector%at(1)
		call assert_equal( tmpNode%id, 8, "NodeVector_test: append elem 1" )
		tmpNode = myvector%at(2)
		call assert_equal( tmpNode%id, 5, "NodeVector_test: append elem 2" )
		tmpNode = myvector%at(3)
		call assert_equal( tmpNode%id, 1, "NodeVector_test: append elem 3" )
		
		call myvector%prepend( nd1 )
		call myvector%prepend( nd2 )
		call myvector%prepend( nd3 )
		call assert_equal( myvector%size(), 6, "NodeVector_test: size after prepend" )
		tmpNode = myvector%at(1)
		call assert_equal( tmpNode%id, 1, "NodeVector_test: prepend elem 1" )
		tmpNode = myvector%at(2)
		call assert_equal( tmpNode%id, 5, "NodeVector_test: prepend elem 2" )
		tmpNode = myvector%at(3)
		call assert_equal( tmpNode%id, 8, "NodeVector_test: prepend elem 3" )
		tmpNode = myvector%at(4)
		call assert_equal( tmpNode%id, 8, "NodeVector_test: prepend elem 4" )
		tmpNode = myvector%at(5)
		call assert_equal( tmpNode%id, 5, "NodeVector_test: prepend elem 5" )
		tmpNode = myvector%at(6)
		call assert_equal( tmpNode%id, 1, "NodeVector_test: prepend elem 6" )
		
		call myvector%erase( 1 )
		call assert_equal( myvector%size(), 5, "NodeVector_test: size after erase 1" )
		tmpNode = myvector%at(1)
		call assert_equal( tmpNode%id, 5, "NodeVector_test: elem 1 after erase 1" )
		
		call myvector%erase( 2 )
		call assert_equal( myvector%size(), 4, "NodeVector_test: size after erase 2" )
		tmpNode = myvector%at(2)
		call assert_equal( tmpNode%id, 8, "NodeVector_test: elem 2 after erase 2" )
		
		call myvector%erase( 3 )
		call assert_equal( myvector%size(), 3, "NodeVector_test: size after erase 3" )
		tmpNode = myvector%at(3)
		call assert_equal( tmpNode%id, 1, "NodeVector_test: elem 3 after erase 3" )
		
		call myvector%erase( 1 )
		call assert_equal( myvector%size(), 2, "NodeVector_test: size after erase 4" )
		tmpNode = myvector%at(1)
		call assert_equal( tmpNode%id, 8, "NodeVector_test: elem 1 after erase 4" )
		
		call myvector%erase( 1 )
		call assert_equal( myvector%size(), 1, "NodeVector_test: size after erase 5" )
		tmpNode = myvector%at(1)
		call assert_equal( tmpNode%id, 1, "NodeVector_test: elem 1 after erase 5" )
		
		call myvector%erase( 1 )
		call assert_equal( myvector%size(), 0, "NodeVector_test: size after erase 6" )
		
		call myvector%erase( 1 )
		call assert_equal( myvector%size(), 0, "NodeVector_test: size after erase 7" )
		
		call myvector%erase( 2 )
		call assert_equal( myvector%size(), 0, "NodeVector_test: size after erase 8" )
		
		call myvector%clear()
		call assert_equal( myvector%size(), 0, "NodeVector_test: size after clear" )
		
		call myvector%append( nd3 )
		call myvector%append( nd2 )
		call myvector%append( nd1 )
		call assert_equal( myvector%size(), 3, "NodeVector_test: size after final append" )
		tmpNode = myvector%at(1)
		call assert_equal( tmpNode%id, 1, "NodeVector_test: final append elem 1" )
		tmpNode = myvector%at(2)
		call assert_equal( tmpNode%id, 5, "NodeVector_test: final append elem 2" )
		tmpNode = myvector%at(3)
		call assert_equal( tmpNode%id, 8, "NodeVector_test: final append elem 3" )
end program test_NodeVector

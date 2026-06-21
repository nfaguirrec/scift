program test_EdgeVector
    use EdgeVector_
    use TestUtils_
    use IOStream_
    use Edge_
    implicit none
		type(EdgeVector) :: myvector
		type(Edge) :: edge1, edge2, edge3
		type(Edge) :: tempEdge
		
		call edge1%init( sNode=1, tNode=2, id=1, label="edge1", weight=1.5_8, directed=.true. )
		call edge2%init( sNode=2, tNode=3, id=2, label="edge2", weight=2.5_8, directed=.false. )
		call edge3%init( sNode=3, tNode=4, id=3, label="edge3", weight=3.5_8, directed=.true. )
		
		call myvector%init()
		call assert_equal( myvector%size(), 0, "Initial size is 0" )
		call assert_true( myvector%isEmpty(), "Vector is initially empty" )
		
		! Test append
		call myvector%append( edge1 )
		call myvector%append( myvector%last() )  ! dummy self-append just to test
		call myvector%replace( 2, edge2 )        ! replace with edge2
		call assert_equal( myvector%size(), 2, "Size after 2 elements" )
		call assert_true( .not. myvector%isEmpty(), "Vector is not empty" )
		
		! Test at
		tempEdge = myvector%at(1)
		call assert_equal( tempEdge%sNode, 1, "Element 1 sNode" )
		call assert_equal_real( tempEdge%weight, 1.5_8, 1e-10_8, "Element 1 weight" )
		
		tempEdge = myvector%at(2)
		call assert_equal( tempEdge%sNode, 2, "Element 2 sNode" )
		call assert_equal_real( tempEdge%weight, 2.5_8, 1e-10_8, "Element 2 weight" )
		
		! Test prepend
		call myvector%prepend( edge3 )
		call assert_equal( myvector%size(), 3, "Size after prepend" )
		
		tempEdge = myvector%at(1)
		call assert_equal( tempEdge%sNode, 3, "Element 1 after prepend sNode" )
		
		tempEdge = myvector%at(2)
		call assert_equal( tempEdge%sNode, 1, "Element 2 after prepend sNode" )
		
		tempEdge = myvector%at(3)
		call assert_equal( tempEdge%sNode, 2, "Element 3 after prepend sNode" )
		
		! Test erase
		call myvector%erase( 2 ) ! Erases second element
		call assert_equal( myvector%size(), 2, "Size after erase" )
		
		tempEdge = myvector%at(1)
		call assert_equal( tempEdge%sNode, 3, "Element 1 after erase sNode" )
		
		tempEdge = myvector%at(2)
		call assert_equal( tempEdge%sNode, 2, "Element 2 after erase sNode" )
		
		! Test clear
		call myvector%clear()
		call assert_equal( myvector%size(), 0, "Size after clear" )
		call assert_true( myvector%isEmpty(), "Vector is empty after clear" )
		
		write(*,*) "All EdgeVector tests PASSED"
end program test_EdgeVector

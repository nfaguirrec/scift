program test_IVector
    use IVector_
    use TestUtils_
    implicit none
		type(IVector) :: A, B, C
		integer :: i
		
		call A%init( 12, 1 )
		call assert_equal( A%nElems, 12, "IVector_test: A size" )
		call assert_equal( A%type, ROW_VECTOR, "IVector_test: A type default" )
		do i = 1, 12
			call assert_equal( A%get(i), 1, "IVector_test: A elem" )
		end do
		
		call B%random( 12, type=COLUMN_VECTOR )
		call assert_equal( B%nElems, 12, "IVector_test: B size" )
		call assert_equal( B%type, COLUMN_VECTOR, "IVector_test: B type" )
		
		A = A*2
		do i = 1, 12
			call assert_equal( A%get(i), 2, "IVector_test: A*2 elem" )
		end do
		
		call B%random( 12, type=ROW_VECTOR )
		call assert_equal( B%nElems, 12, "IVector_test: B row size" )
		call assert_equal( B%type, ROW_VECTOR, "IVector_test: B row type" )
end program test_IVector

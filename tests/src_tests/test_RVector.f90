program test_RVector
    use RVector_
    use TestUtils_
    implicit none
		type(RVector) :: A, B
		integer :: i
		
		call A.init( 12, 1.0_8 )
		call assert_equal( A%nElems, 12, "RVector_test: A size" )
		call assert_equal( A%type, ROW_VECTOR, "RVector_test: A type row" )
		do i = 1, A%nElems
			call assert_true( abs(A%get(i) - 1.0_8) < 1e-12_8, "RVector_test: A element init" )
		end do
		
		call B.random( 12, type=COLUMN_VECTOR )
		call assert_equal( B%nElems, 12, "RVector_test: B size" )
		call assert_equal( B%type, COLUMN_VECTOR, "RVector_test: B type column" )
		
		A = A*2.0_8
		do i = 1, A%nElems
			call assert_true( abs(A%get(i) - 2.0_8) < 1e-12_8, "RVector_test: A element multiplied" )
		end do
		
		call B.random( 12, type=ROW_VECTOR )
		call assert_equal( B%nElems, 12, "RVector_test: B size after random row" )
		call assert_equal( B%type, ROW_VECTOR, "RVector_test: B type after random row" )
end program test_RVector

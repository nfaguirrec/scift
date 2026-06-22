program test_IntegerHyperVector
    use IntegerHyperVector_
    use TestUtils_
    use Math_
    use IOStream_
    use IntegerVector_
    use Matrix_
    implicit none
		type(IntegerHyperVector) :: hvec
		type(IntegerVector) :: ivec
		
		call hvec%init()
		
		! Testing append
		call ivec%init( 3, value=1 )
		call hvec%append( ivec )
		call ivec%init( 4, value=2 )
		call hvec%append( ivec )
		call ivec%init( 2, value=1 )
		call hvec%append( ivec )
		call assert_equal( hvec%size(), 3, "IntegerHyperVector_test: size after append" )
		
		! Testing prepend
		call ivec%init( 2, value=4 )
		call hvec%prepend( ivec )
		call ivec%init( 4, value=5 )
		call hvec%prepend( ivec )
		call ivec%init( 3, value=8 )
		call hvec%prepend( ivec )
		call assert_equal( hvec%size(), 6, "IntegerHyperVector_test: size after prepend" )
		
		! Testing clear
		call hvec%clear()
		call assert_equal( hvec%size(), 0, "IntegerHyperVector_test: size after clear" )
end program test_IntegerHyperVector

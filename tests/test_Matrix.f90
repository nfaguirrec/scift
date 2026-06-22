program test_Matrix
    use Matrix_
    use TestUtils_
    use ieee_arithmetic   ! http://fortranwiki%org/fortran/show/ieee_arithmetic
    use Math_
    use RandomUtils_
    implicit none
		type(Matrix) :: A, B, C, I
		real(8), allocatable :: lA(:)
		real(8) :: detB
		character(30) :: fmtStr
		
		write(*,*) ""
		write(*,*) "Testing contructors"
		write(*,*) "==================="
		write(*,*) "Initialization of a matrix 12x12 with all values equal to 1.0"
		write(*,*) ""
		call A%init( 12, 12, 1.0_8 )
		call A%show( 6, .true. )
		call assert_equal( A%nRows, 12, "Matrix A rows" )
		call assert_equal( A%nCols, 12, "Matrix A columns" )
		call assert_equal_real( A%data(1,1), 1.0_8, 1e-10_8, "Matrix A(1,1)" )
		
		write(*,*) ""
		write(*,*) "Initialization of a matrix 12x12 with random values"
		write(*,*) ""
		call B%random( 12, 12, symmetric=.true. )
		call B%show( 6, .true. )
		call assert_equal( B%nRows, 12, "Matrix B rows" )
		call assert_equal( B%nCols, 12, "Matrix B columns" )
		call assert_equal_real( B%data(1,2), B%data(2,1), 1e-10_8, "Matrix B symmetry" )
		
		write(*,*) ""
		write(*,*) "Initialization with elements Inf"
		write(*,*) ""
		call B%random( 4, 4, symmetric=.true. )
		B%data(1,1) = Math_Inf
		B%data(2,2) = Math_Inf
		call B%show( 6, .true. )
		
		write(*,*) ""
		write(*,*) "Initialization with elements NaN"
		write(*,*) ""
		call B%random( 4, 4, symmetric=.true. )
		B%data(1,1) = Math_Inf
		B%data(2,2) = Math_NAN
		call B%show( 6, .true. )
		
		write(*,*) ""
		write(*,*) "Testing inverse"
		write(*,*) "==============="
		write(*,*) ""
		write(*,*) "B ="
		call B%random( 12, 12, symmetric=.true. )
		call B%show( 6, .true. )
		C = B%inverse()
		write(*,*) ""
		write(*,*) "B^{-1} ="
		call C%show( 6, .true. )
		A = B*C
		write(*,*) ""
		write(*,*) "B*B^{-1} ="
		call A%show( 6, .true. )
		! Check if A is identity
		call assert_equal_real( A%data(1,1), 1.0_8, 1e-8_8, "B*B^-1 diagonal element" )
		call assert_equal_real( A%data(1,2), 0.0_8, 1e-8_8, "B*B^-1 off-diagonal element" )
		
		write(*,*) ""
		write(*,*) "Testing inverse for small matrices"
		write(*,*) "=================================="
		write(*,*) ""
		write(*,*) "B ="
		call B%init(3,3)
		B%data(1,:) = [ -0.76675, 0.00000, -0.64195 ]
		B%data(2,:) = [  0.00000, 1.00000,  0.00000 ]
		B%data(3,:) = [  0.64195, 0.00000, -0.76675 ]
		call B%show( 6, .true. )
		C = B%inverse()
		write(*,*) ""
		write(*,*) "B^{-1} ="
		call C%show( 6, .true. )
		A = B*C
		write(*,*) ""
		write(*,*) "B*B^{-1} ="
		call A%show( 6, .true. )
		call assert_equal_real( A%data(2,2), 1.0_8, 1e-10_8, "Identity diagonal element" )
		
		write(*,*) ""
		write(*,*) "B ="
		call B%init(2,2)
		B%data(1,:) = [ -0.76675, -0.84195 ]
		B%data(2,:) = [  0.64195, -2.76675 ]
		call B%show( 6, .true. )
		C = B%inverse()
		write(*,*) ""
		write(*,*) "B^{-1} ="
		call C%show( 6, .true. )
		A = B*C
		write(*,*) ""
		write(*,*) "B*B^{-1} ="
		call A%show( 6, .true. )
		call assert_equal_real( A%data(1,1), 1.0_8, 1e-10_8, "2x2 Identity diagonal element" )
		
		write(*,*) ""
		write(*,*) "Approximately singular matrix "
		write(*,*) "B ="
		call B%init(3,3)
		B%data(1,:) = [  63534.983702,     97467.773498,    -54790.146796 ]
		B%data(2,:) = [  97467.773498,    212028.341745,     27139.351856 ]
		B%data(3,:) = [ -54790.146796,     27139.351856,    245051.310679 ]
		write(*,*) ""
		detB = B%determinant3x3()
		write(*,*) "det(B) =", detB
		call assert_true( abs(detB) > 0.0_8, "Determinant is not exactly zero" )
		write(*,*) ""
		call B%show( 6, .true. )
		C = B%inverse()
		write(*,*) ""
		write(*,*) "B^{-1} ="
		call C%show( 6, .true. )
		A = B*C
		write(*,*) ""
		write(*,*) "B*B^{-1} ="
		call A%show( 6, .true. )
		
		write(*,*) ""
		write(*,*) "Testing operators"
		write(*,*) "================="
		write(*,*) ""
		A = A*2.0_8
		write(*,*) "A*2.0 ="
		call A%show( 6, .true. )
		
		call B%random( 12, 12, symmetric=.true. )
		write(*,*) ""
		write(*,*) "B ="
		call B%show( 6, .true. )
		A = B/B
		write(*,*) ""
		write(*,*) "B/B ="
		call A%show( 6, .true. )
		call assert_equal_real( A%data(2,2), 1.0_8, 1e-10_8, "B/B diagonal element" )
		
		write(*,*) ""
		write(*,*) "Testing linearization"
		write(*,*) "====================="
		write(*,*) ""
		call A%random( 3, 3, symmetric=.true. )
		write(*,*) ""
		write(*,*) "A ="
		call A%show( 6, .true. )
		call A%linearize( lA )
		write(*,*) ""
		write(*,*) "linearize(A) ="
		write(fmtStr, "(a, i0, a)") "(5X, ", size(lA), "F10.6)"
		write(*, fmtStr) lA
		call assert_equal( size(lA), 6, "Linearized size" )
		call assert_equal_real( lA(1), A%data(1,1), 1e-10_8, "Linearize first elem" )
		deallocate(lA)
		
		write(*,*) ""
		write(*,*) "Testing eigenvalues and eigenvectors"
		write(*,*) "===================================="
		
		call A%init(5,5)
		A%data(1,:) = [  1.96,  -6.49,  -0.47,  -7.20,  -0.65 ]
		A%data(2,:) = [ -6.49,   3.80,  -6.39,   1.50,  -6.34 ]
		A%data(3,:) = [ -0.47,  -6.39,   4.17,  -1.51,   2.67 ]
		A%data(4,:) = [ -7.20,   1.50,  -1.51,   5.70,   1.80 ]
		A%data(5,:) = [ -0.65,  -6.34,   2.67,   1.80,  -7.10 ]
		
		write(*,*) ""
		write(*,*) " Sorted"
		write(*,*) "--------"
		
		write(*,*) ""
		write(*,*) "A ="
		call A%show( 6, .true. )
		
		call A%eigen( eVecs=B, eVals=C )
		
		write(*,*) ""
		write(*,*) "eigenvectors ="
		call B%show( 6, .true. )
		
		write(*,*) ""
		write(*,*) "eigenvalues ="
		call C%show( 6, .true. )
		! Check sorted
		call assert_true( C%data(1,1) <= C%data(2,2), "Eigenvalues sorted" )
		
		write(*,*) ""
		write(*,*) " Not sorted"
		write(*,*) "------------"
		
		write(*,*) ""
		write(*,*) "A ="
		call A%show( 6, .true. )
		
		call A%eigenNotSorted( eVecs=B, eVals=C )
		
		write(*,*) ""
		write(*,*) "eigenvectors ="
		call B%show( 6, .true. )
		
		write(*,*) ""
		write(*,*) "eigenvalues ="
		call C%show( 6, .true. )
		
		write(*,*) ""
		write(*,*) "Testing projection for column vectors"
		write(*,*) "====================================="
		call A%columnVector( 3, values=[0.5_8,0.7071_8,0.5_8] )
		write(*,*) ""
		write(*,*) "A = "
		call A%show( formatted=.true. )
		call assert_equal_real( A%data(2,1), 0.7071_8, 1e-10_8, "Column vector set" )
		
		! These axis are equivalent to a rotation with alpha=45º and beta=45º
		call B%init(3,3)
		B%data(1,:) = [  0.5000, 0.5000, -0.7071 ]
		B%data(2,:) = [ -0.7071, 0.7071,  0.0000 ]
		B%data(3,:) = [  0.5000, 0.5000,  0.7071 ]
		
		write(*,*) ""
		write(*,*) "axes = "
		call B%show( formatted=.true. )
		
		C = A%projectionOntoNewAxes( B )
		
		write(*,*) ""
		write(*,*) "projection = "
		call C%show( formatted=.true. )
		
		write(*,*) ""
		write(*,*) "Testing info methods"
		write(*,*) "===================="
		call A%random( 3, 3, symmetric=.true. )
		write(*,*) ""
		write(*,*) "A = "
		call A%show( formatted=.true. )
		write(*,*) "A.isDiagonal() = ", A%isDiagonal()
		
		call A%init( 3, 3, val=1d-12 )
		write(*,*) ""
		write(*,*) "A = "
		call A%show( formatted=.true. )
		write(*,*) "A.isZero() = ", A%isZero()
		write(*,*) "A.isZero( tol=1d-12 ) = ", A%isZero( tol=1d-12 )
		
		call assert_true( .not. A%isZero(), "isZero with default tol" )
		call assert_true( A%isZero( tol=1d-11 ), "isZero with custom tol" )
		
		write(*,*) "All Matrix tests PASSED"
end program test_Matrix

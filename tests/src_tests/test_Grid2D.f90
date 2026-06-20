program test_Grid2D
    use Grid2D_
    use TestUtils_
    use GOptions_
    use IOStream_
    use Grid_
    implicit none
		type(Grid2D) :: xyzGrid, xyzGrid2
		
		real(8) :: rMin(2), rMax(2)
		integer :: gridSize(2)
		
		rMin = [-5.0_8,-5.0_8]
		rMax = [ 5.0_8, 5.0_8]
		gridSize = [10, 10]
		
		call xyzGrid%init( rMin, rMax, gridSize )
		call assert_equal( xyzGrid%nPoints(1), 10, "Grid2D_test: init dim 1" )
		call assert_equal( xyzGrid%nPoints(2), 10, "Grid2D_test: init dim 2" )
		call assert_equal_real( xyzGrid%min(1), -5.0_8, 1e-10_8, "Grid2D_test: min dim 1" )
		call assert_equal_real( xyzGrid%max(2), 5.0_8, 1e-10_8, "Grid2D_test: max dim 2" )
		
		xyzGrid2 = xyzGrid
		call assert_equal( xyzGrid2%nPoints(1), 10, "Grid2D_test: copy dim 1" )
		call assert_equal( xyzGrid2%nPoints(2), 10, "Grid2D_test: copy dim 2" )
		
		xyzGrid2 = xyzGrid * 2.0_8
		call assert_equal_real( xyzGrid2%min(1), -5.0_8, 1e-10_8, "Grid2D_test: operator * min" )
		call assert_equal_real( xyzGrid2%component(1)%data(1), -10.0_8, 1e-10_8, "Grid2D_test: operator * data(1)" )
		
		call xyzGrid%init( rMin, rMax, gridSize )
		call xyzGrid%resize( 5, 5, +1, -1 )
		call assert_equal( xyzGrid%nPoints(1), 15, "Grid2D_test: resize dim 1" )
		call assert_equal( xyzGrid%nPoints(2), 15, "Grid2D_test: resize dim 2" )
end program test_Grid2D

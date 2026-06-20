program test_Grid3D
    use Grid3D_
    use TestUtils_
    use IOStream_
    use Grid_
    implicit none
		type(Grid3D) :: xyzGrid, xyzGrid2
		
		real(8) :: rMin(3), rMax(3)
		integer :: gridSize(3)
		
		rMin = [-5.0_8,-5.0_8,-10.0_8]
		rMax = [ 5.0_8, 5.0_8, 10.0_8]
		gridSize = [10, 10, 20]
		
		call xyzGrid%init( rMin, rMax, gridSize )
		call assert_equal( xyzGrid%nPoints(1), 10, "Grid3D_test: init dim 1" )
		call assert_equal( xyzGrid%nPoints(2), 10, "Grid3D_test: init dim 2" )
		call assert_equal( xyzGrid%nPoints(3), 20, "Grid3D_test: init dim 3" )
		call assert_equal_real( xyzGrid%min(1), -5.0_8, 1e-10_8, "Grid3D_test: min dim 1" )
		call assert_equal_real( xyzGrid%max(3), 10.0_8, 1e-10_8, "Grid3D_test: max dim 3" )
		
		xyzGrid2 = xyzGrid
		call assert_equal( xyzGrid2%nPoints(1), 10, "Grid3D_test: copy dim 1" )
		call assert_equal( xyzGrid2%nPoints(3), 20, "Grid3D_test: copy dim 3" )
		
		call xyzGrid%resize( 5, 5, 5, +1, -1, +1 )
		call assert_equal( xyzGrid%nPoints(1), 15, "Grid3D_test: resize dim 1" )
		call assert_equal( xyzGrid%nPoints(2), 15, "Grid3D_test: resize dim 2" )
		call assert_equal( xyzGrid%nPoints(3), 25, "Grid3D_test: resize dim 3" )
end program test_Grid3D

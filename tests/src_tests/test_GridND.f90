program test_GridND
    use GridND_
    use TestUtils_
    use IOStream_
    use Grid_
    implicit none
		type(GridND) :: xyzGrid, xyzGrid2
		
		real(8) :: rMin(3), rMax(3)
		integer :: gridSize(3)
		
		rMin = [-5.0_8,-5.0_8,-10.0_8]
		rMax = [ 5.0_8, 5.0_8, 10.0_8]
		gridSize = [10, 10, 20]
		
		call xyzGrid%init( rMin, rMax, gridSize )
		call assert_equal( xyzGrid%component(1)%nPoints, 10, "GridND_test: init dim 1" )
		call assert_equal( xyzGrid%component(2)%nPoints, 10, "GridND_test: init dim 2" )
		call assert_equal( xyzGrid%component(3)%nPoints, 20, "GridND_test: init dim 3" )
		call assert_equal_real( xyzGrid%component(1)%min, -5.0_8, 1e-10_8, "GridND_test: min dim 1" )
		call assert_equal_real( xyzGrid%component(3)%max, 10.0_8, 1e-10_8, "GridND_test: max dim 3" )
		
		xyzGrid2 = xyzGrid
		call assert_equal( xyzGrid2%component(1)%nPoints, 10, "GridND_test: copy dim 1" )
		call assert_equal( xyzGrid2%component(3)%nPoints, 20, "GridND_test: copy dim 3" )
		
		xyzGrid2 = xyzGrid * 2.0_8
		call assert_equal_real( xyzGrid2%component(1)%min, -5.0_8, 1e-10_8, "GridND_test: operator * min" )
		call assert_equal_real( xyzGrid2%component(1)%data(1), -10.0_8, 1e-10_8, "GridND_test: operator * data(1)" )
end program test_GridND

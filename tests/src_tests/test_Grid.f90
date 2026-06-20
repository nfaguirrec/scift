program test_Grid
    use Grid_
    use TestUtils_
    use GOptions_
    use Math_
    use IOStream_
    use String_
    implicit none
		real(8) :: rMin, rMax
		integer :: gridSize
		type(Grid) :: rGrid
		type(Grid) :: rGrid2
		real(8) :: stepSize
			
		rMin=0.0_8
		rMax=5.0_8
		gridSize=10
		
		call rGrid%init( rMin, rMax, gridSize )
		stepSize = rGrid%stepSize
		
		call assert_equal( rGrid%nPoints, 10, "Grid_test: rGrid nPoints" )
		call assert_equal_real( rGrid%min, 0.0_8, 1e-10_8, "Grid_test: rGrid min" )
		call assert_equal_real( rGrid%max, 5.0_8, 1e-10_8, "Grid_test: rGrid max" )
		
		call rGrid2%copyGrid( rGrid )
		call assert_equal( rGrid2%nPoints, 10, "Grid_test: copyGrid nPoints" )
		call assert_equal_real( rGrid2%min, 0.0_8, 1e-10_8, "Grid_test: copyGrid min" )
		call assert_equal_real( rGrid2%max, 5.0_8, 1e-10_8, "Grid_test: copyGrid max" )
		
		! Testing resize dir = +1
		call rGrid2%resize( 5, +1 )
		call assert_equal( rGrid2%nPoints, 15, "Grid_test: resize +5 dir +1 nPoints" )
		call assert_equal_real( rGrid2%min, 0.0_8, 1e-10_8, "Grid_test: resize +5 dir +1 min" )
		call assert_equal_real( rGrid2%max, 0.0_8 + 14.0_8*stepSize, 1e-10_8, "Grid_test: resize +5 dir +1 max" )
		
		call rGrid2%resize( -5, +1 )
		call assert_equal( rGrid2%nPoints, 10, "Grid_test: resize -5 dir +1 nPoints" )
		call assert_equal_real( rGrid2%min, 0.0_8, 1e-10_8, "Grid_test: resize -5 dir +1 min" )
		call assert_equal_real( rGrid2%max, 5.0_8, 1e-10_8, "Grid_test: resize -5 dir +1 max" )
		
		! Testing resize dir = -1
		call rGrid2%resize( 5, -1 )
		call assert_equal( rGrid2%nPoints, 15, "Grid_test: resize +5 dir -1 nPoints" )
		call assert_equal_real( rGrid2%min, -5.0_8*stepSize, 1e-10_8, "Grid_test: resize +5 dir -1 min" )
		call assert_equal_real( rGrid2%max, 5.0_8, 1e-10_8, "Grid_test: resize +5 dir -1 max" )
		
		call rGrid2%resize( -5, -1 )
		call assert_equal( rGrid2%nPoints, 10, "Grid_test: resize -5 dir -1 nPoints" )
		call assert_equal_real( rGrid2%min, 0.0_8, 1e-10_8, "Grid_test: resize -5 dir -1 min" )
		call assert_equal_real( rGrid2%max, 5.0_8, 1e-10_8, "Grid_test: resize -5 dir -1 max" )
		
		! Testing resize dir = 0
		call rGrid2%resize( 5, 0 )
		call assert_equal( rGrid2%nPoints, 20, "Grid_test: resize +5 dir 0 nPoints" )
		call assert_equal_real( rGrid2%min, -5.0_8*stepSize, 1e-10_8, "Grid_test: resize +5 dir 0 min" )
		
		call rGrid2%resize( -5, 0 )
		call assert_equal( rGrid2%nPoints, 10, "Grid_test: resize -5 dir 0 nPoints" )
		call assert_equal_real( rGrid2%min, 0.0_8, 1e-10_8, "Grid_test: resize -5 dir 0 min" )
		call assert_equal_real( rGrid2%max, 5.0_8, 1e-10_8, "Grid_test: resize -5 dir 0 max" )
		
		! Testing even/odd points initialization from stepSize
		call rGrid2%init( rMin, rMax, nPoints=10 )
		call rGrid2%resize( 5, 0 )
		call rGrid2%init( rGrid2%min, rGrid2%max, stepSize=rGrid2%stepSize )
		call assert_equal( rGrid2%nPoints, 20, "Grid_test: even points init from stepSize" )
		
		call rGrid2%init( rMin, rMax, nPoints=11 )
		call rGrid2%resize( 5, 0 )
		call rGrid2%init( rGrid2%min, rGrid2%max, stepSize=rGrid2%stepSize )
		call assert_equal( rGrid2%nPoints, 21, "Grid_test: odd points init from stepSize" )
end program test_Grid

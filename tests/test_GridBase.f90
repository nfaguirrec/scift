program test_GridBase
    use GridBase_
    use TestUtils_
    use IOStream_
    use String_
    implicit none
		real(8) :: rMin, rMax
		integer :: gridSize
		type(GridBase) :: rGridBase
		type(GridBase) :: rGridBase2
			
		rMin=0.0_8
		rMax=5.0_8
		gridSize=10
		
		call rGridBase%init( rMin, rMax, gridSize )
		call assert_equal( rGridBase%size, 10, "GridBase_test: rGridBase size" )
		call assert_equal_real( rGridBase%min, 0.0_8, 1e-10_8, "GridBase_test: rGridBase min" )
		call assert_equal_real( rGridBase%max, 5.0_8, 1e-10_8, "GridBase_test: rGridBase max" )
		
		call rGridBase2%copyGridBase( rGridBase )
		call assert_equal( rGridBase2%size, 10, "GridBase_test: copy size" )
		call assert_equal_real( rGridBase2%min, 0.0_8, 1e-10_8, "GridBase_test: copy min" )
		call assert_equal_real( rGridBase2%max, 5.0_8, 1e-10_8, "GridBase_test: copy max" )
end program test_GridBase

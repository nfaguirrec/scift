program test_NPeakFinder
    use NPeakFinder_
    use TestUtils_
    use GOptions_
    use Math_
    use Grid_
    use RNFunction_
    use IntegerList_
    implicit none
		type(RNFunction) :: func
		type(RNFunction) :: peaks
		type(NPeakFinder) :: peakFinder
		real(8), allocatable :: x(:), y(:)
		integer :: i, nPoints
		real(8) :: t
		
		nPoints = 1001
		allocate(x(nPoints), y(nPoints))
		do i = 1, nPoints
			t = (i - 1) * 0.1_8
			x(i) = t
			! Three peaks at 25.0, 50.0, and 75.0
			y(i) = 10.0_8 * exp(-0.5_8 * ((t - 25.0_8)/1.0_8)**2) &
			     + 15.0_8 * exp(-0.5_8 * ((t - 50.0_8)/1.0_8)**2) &
			     + 8.0_8  * exp(-0.5_8 * ((t - 75.0_8)/1.0_8)**2)
		end do
		
		func = RNFunction(x, y)
		call peakFinder%init(func, method=NPeakFinder_MAX_DIST, windowSize=25, tolerance=1.0_8)
		peaks = peakFinder%execute()
		
		! Verify we found exactly 3 peaks
		call assert_equal(peaks%nPoints(), 3, "NPeakFinder_test: number of peaks")
		
		! The peaks should be close to 25.0, 50.0, and 75.0
		call assert_true(abs(peaks%xGrid%at(1) - 25.0_8) < 0.2_8, "NPeakFinder_test: peak 1 pos")
		call assert_true(abs(peaks%xGrid%at(2) - 50.0_8) < 0.2_8, "NPeakFinder_test: peak 2 pos")
		call assert_true(abs(peaks%xGrid%at(3) - 75.0_8) < 0.2_8, "NPeakFinder_test: peak 3 pos")
		
		! The intensities should be close to 10.0, 15.0, 8.0
		call assert_true(abs(peaks%at(1) - 10.0_8) < 0.1_8, "NPeakFinder_test: peak 1 intensity")
		call assert_true(abs(peaks%at(2) - 15.0_8) < 0.1_8, "NPeakFinder_test: peak 2 intensity")
		call assert_true(abs(peaks%at(3) - 8.0_8) < 0.1_8, "NPeakFinder_test: peak 3 intensity")
		
		deallocate(x, y)
end program test_NPeakFinder

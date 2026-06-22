program test_CNFunction2D
    use CNFunction2D_
    use TestUtils_
    use GOptions_
    use Math_
    use String_
    use IOStream_
    use RNFunction_
    use Grid2D_
    use RNFunction2D_
    implicit none
		type(CNFunction2D) :: func, func2
		
		real(8) :: xVec(3)
		real(8) :: yVec(2)
		complex(8) :: fArray(3,2)
		
		type(Grid2D) :: xyGrid
	
		xVec(:) = [ 1.0, 2.0, 3.0 ]
		yVec(:) = [-1.0, 0.0 ]
		
		fArray(1,:) = [ 6.0,-1.0 ]
		fArray(2,:) = [ 7.0, 0.0 ]
		fArray(3,:) = [ 0.0, 0.0 ]
		
		call func%init( xVec, yVec, fArray )
		call assert_equal( func%nPoints(1), 3, "func.nPoints(1)" )
		call assert_equal( func%nPoints(2), 2, "func.nPoints(2)" )
		call assert_equal_real( func%xyGrid%min(1), 1.0_8, 1e-10_8, "func min(1)" )
		call assert_equal_real( func%xyGrid%max(2), 0.0_8, 1e-10_8, "func max(2)" )
		
		call xyGrid%fromArray( xVec, yVec )
		call func%init( xyGrid, fArray )
		call assert_equal( func%nPoints(1), 3, "func.nPoints(1) xyGrid" )
		call assert_equal( func%nPoints(2), 2, "func.nPoints(2) xyGrid" )
		
		call xyGrid%init( min=[-5.0_8,-5.0_8], max=[5.0_8,5.0_8], size=[100,100] )
		call func%init( xyGrid, funcTest )
		call assert_equal( func%nPoints(1), 100, "func.nPoints(1) formula" )
		call assert_equal( func%nPoints(2), 100, "func.nPoints(2) formula" )
		
		call func%init( "data/formats/complex-N2DF", format=N2DF_FORMAT )
		call assert_equal( func%nPoints(1), 100, "func.nPoints(1) load N2DF" )
		call assert_equal( func%nPoints(2), 100, "func.nPoints(2) load N2DF" )
		
		func2 = func
		call assert_equal( func2%nPoints(1), 100, "func2.nPoints(1) copy" )
		call assert_equal( func2%nPoints(2), 100, "func2.nPoints(2) copy" )
		call assert_equal_real( dble(func2%at(10,10)), dble(func%at(10,10)), 1e-10_8, "copy at(10,10) real" )
		call assert_equal_real( imag(func2%at(10,10)), imag(func%at(10,10)), 1e-10_8, "copy at(10,10) imag" )
		
		call func%save( "salida.n2df", format=N2DF_FORMAT )
		call func%load( "salida.n2df", format=N2DF_FORMAT )
		call assert_equal( func%nPoints(1), 100, "func.nPoints(1) reload" )
		call assert_equal( func%nPoints(2), 100, "func.nPoints(2) reload" )
		
		call func%init( xVec, yVec, fArray )
		call func%resize( 3, 2, +1, +1 )
		call assert_equal( func%nPoints(1), 6, "resized nPoints(1)" )
		call assert_equal( func%nPoints(2), 4, "resized nPoints(2)" )
		call assert_equal_real( func%xyGrid%max(1), 6.0_8, 1e-10_8, "resized max(1)" )
		call assert_equal_real( func%xyGrid%max(2), 2.0_8, 1e-10_8, "resized max(2)" )
		
		write(*,*) "All CNFunction2D tests PASSED"

    contains

    
    	!>
    	!! This is neccesary only for NFunction_test()
    	!!
    	pure function funcTest( x, y ) result( output )
    		real(8), intent(in) :: x, y
    		complex(8) :: output
    		
    		output = sin( Math_PI*sqrt(x**2+y**2) ) /sqrt(x**2+y**2)  ! Mexican Hat function
    	end function funcTest

end program test_CNFunction2D

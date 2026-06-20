program test_FourierTransform2D
    use FourierTransform2D_
    use TestUtils_
    use FFTW3_
    use GOptions_
    use Math_
    use Grid2D_
    use CNFunction2D_
    use RNFunction2D_
    use RandomUtils_
    use FourierTransform_
    implicit none
		type(Grid2D) :: xyGrid
		
		xyGrid = FourierTransform2D_omegaGridFromData( [10,11], [0.1_8,0.2_8], FourierTransform_NORDER )
		call assert_equal( xyGrid%nPoints(1), 10, "FourierTransform2D_test: NORDER omegaGrid dim 1" )
		call assert_equal( xyGrid%nPoints(2), 11, "FourierTransform2D_test: NORDER omegaGrid dim 2" )
		
		xyGrid = FourierTransform2D_omegaGridFromData( [10,11], [0.1_8,0.2_8], FourierTransform_SORDER )
		call assert_equal( xyGrid%nPoints(1), 10, "FourierTransform2D_test: SORDER omegaGrid dim 1" )
		call assert_equal( xyGrid%nPoints(2), 11, "FourierTransform2D_test: SORDER omegaGrid dim 2" )
		
		xyGrid = FourierTransform2D_xyGridFromData( [10,11], [0.1_8,0.2_8], FourierTransform_NORDER )
		call assert_equal( xyGrid%nPoints(1), 10, "FourierTransform2D_test: NORDER xyGrid dim 1" )
		call assert_equal( xyGrid%nPoints(2), 11, "FourierTransform2D_test: NORDER xyGrid dim 2" )
		
		xyGrid = FourierTransform2D_xyGridFromData( [10,11], [0.1_8,0.2_8], FourierTransform_SORDER )
		call assert_equal( xyGrid%nPoints(1), 10, "FourierTransform2D_test: SORDER xyGrid dim 1" )
		call assert_equal( xyGrid%nPoints(2), 11, "FourierTransform2D_test: SORDER xyGrid dim 2" )

    contains

    	
    	!>
    	!! This is neccesary only for FourierTransform2D_test()
    	!!
    	function funcRectangular( x, y ) result( output )
    		real(8), intent(in) :: x, y
    		complex(8) :: output
    		
    		real(8) :: a1, a2
    		
    		a1 = 0.5_8
    		a2 = 1.0_8
    		
    		output = Math_ubox( a1*x )*Math_ubox( a2*y )
    	end function funcRectangular

    	
    	!>
    	!! This is neccesary only for FourierTransform2D_test()
    	!!
    	function FfuncRectangular( omega1, omega2 ) result( output )
    		real(8), intent(in) :: omega1, omega2
    		complex(8) :: output
    		
    		real(8) :: a1, a2
    		
    		a1 = 0.5_8
    		a2 = 1.0_8
    		
    		output = Math_nsinc( omega1/(2.0_8*Math_PI*a1) )*Math_nsinc( omega2/(2.0_8*Math_PI*a2) )/(2.0_8*Math_PI)/a1/a2
    	end function FfuncRectangular

end program test_FourierTransform2D

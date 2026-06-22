program test_FourierTransform3D
    use FourierTransform3D_
    use TestUtils_
    use FFTW3_
    use GOptions_
    use Math_
    use Grid3D_
    use CNFunction3D_
    use RNFunction3D_
    use RandomUtils_
    use FourierTransform_
    use FourierTransform2D_
    implicit none
		type(Grid3D) :: xyzGrid
		
		xyzGrid = FourierTransform3D_omegaGridFromData( [10,11,12], [0.1_8,0.2_8,0.3_8], FourierTransform_NORDER )
		call assert_equal( xyzGrid%nPoints(1), 10, "FourierTransform3D_test: NORDER omegaGrid dim 1" )
		call assert_equal( xyzGrid%nPoints(2), 11, "FourierTransform3D_test: NORDER omegaGrid dim 2" )
		call assert_equal( xyzGrid%nPoints(3), 12, "FourierTransform3D_test: NORDER omegaGrid dim 3" )
		
		xyzGrid = FourierTransform3D_omegaGridFromData( [10,11,12], [0.1_8,0.2_8,0.3_8], FourierTransform_SORDER )
		call assert_equal( xyzGrid%nPoints(1), 10, "FourierTransform3D_test: SORDER omegaGrid dim 1" )
		call assert_equal( xyzGrid%nPoints(2), 11, "FourierTransform3D_test: SORDER omegaGrid dim 2" )
		call assert_equal( xyzGrid%nPoints(3), 12, "FourierTransform3D_test: SORDER omegaGrid dim 3" )
		
		xyzGrid = FourierTransform3D_xyzGridFromData( [10,11,12], [0.1_8,0.2_8,0.3_8], FourierTransform_NORDER )
		call assert_equal( xyzGrid%nPoints(1), 10, "FourierTransform3D_test: NORDER xyzGrid dim 1" )
		call assert_equal( xyzGrid%nPoints(2), 11, "FourierTransform3D_test: NORDER xyzGrid dim 2" )
		call assert_equal( xyzGrid%nPoints(3), 12, "FourierTransform3D_test: NORDER xyzGrid dim 3" )
		
		xyzGrid = FourierTransform3D_xyzGridFromData( [10,11,12], [0.1_8,0.2_8,0.3_8], FourierTransform_SORDER )
		call assert_equal( xyzGrid%nPoints(1), 10, "FourierTransform3D_test: SORDER xyzGrid dim 1" )
		call assert_equal( xyzGrid%nPoints(2), 11, "FourierTransform3D_test: SORDER xyzGrid dim 2" )
		call assert_equal( xyzGrid%nPoints(3), 12, "FourierTransform3D_test: SORDER xyzGrid dim 3" )

    contains

    	
    	!>
    	!! This is neccesary only for FourierTransform3D_test()
    	!! Maxima:
    	!!   f(x) := exp(-0.5*x**2);
    	!!
    	function funcGaussian( x, y, z ) result( output )
    		real(8), intent(in) :: x, y, z
    		complex(8) :: output
    		
    		real(8) :: alpha
    		
    		alpha = 5.0_8
    		output = exp(-alpha*( x**2 + y**2 + z**2 ))
    	end function funcGaussian

    	
    	!>
    	!! This is neccesary only for FourierTransform3D_test()
    	!! Maxima:
    	!!   f(x) := exp(-0.5*x**2);
    	!!
    	!! La convencion de normalizacion de acuerdo con wikipedia es:
        !!   Fourier transform unitary, angular frequency
    	!!
    	function funcFGaussian( x ) result( output )
    		real(8), intent(in) :: x
    		complex(8) :: output
    		
    		real(8) :: alpha
    		
    		alpha = 0.5_8
    ! 		output = exp(-x**2/4.0_8/alpha)/sqrt(2.0_8*alpha)
    		output = exp(-(x-5.0_8)**2/4.0_8/alpha)/sqrt(2.0_8*alpha) + 0.4_8*exp(-(x+2.0_8)**2/10.0_8/alpha)/sqrt(2.0_8*alpha)*sin(2.0*x) &
    			+ 0.4_8*Math_I*exp(-(x-2.0_8)**2/10.0_8/alpha)/sqrt(2.0_8*alpha)*cos(2.0*x)
    	end function funcFGaussian

    	
    	!>
    	!! This is neccesary only for FourierTransform3D_test()
    	!!
    	function funcRectangular( x, y, z ) result( output )
    		real(8), intent(in) :: x, y, z
    		complex(8) :: output
    		
    		real(8) :: a1, a2, a3
    		
    		a1 = 0.5_8
    		a2 = 1.0_8
    		a3 = 1.5_8
    		
    		output = Math_ubox( a1*x )*Math_ubox( a2*y )*Math_ubox( a3*z )
    	end function funcRectangular

    	
    	!>
    	!! This is neccesary only for FourierTransform3D_test()
    	!!
    	function FfuncRectangular( w1, w2, w3 ) result( output )
    		real(8), intent(in) :: w1, w2, w3
    		complex(8) :: output
    		
    		real(8) :: a1, a2, a3
    		
    		a1 = 0.5_8
    		a2 = 1.0_8
    		a3 = 1.5_8
    		
    		output = Math_nsinc( w1/(2.0_8*Math_PI*a1) ) &
    					*Math_nsinc( w2/(2.0_8*Math_PI*a2) ) &
    					*Math_nsinc( w3/(2.0_8*Math_PI*a3) )&
    					/sqrt(2.0_8*Math_PI)**3/a1/a2/a3
    	end function FfuncRectangular

end program test_FourierTransform3D

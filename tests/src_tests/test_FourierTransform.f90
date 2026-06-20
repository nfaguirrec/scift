program test_FourierTransform
    use FourierTransform_
    use TestUtils_
    use GOptions_
    use String_
    use Math_
    use Grid_
    use CNFunction_
    use RNFunction_
    use RandomUtils_
    use FFTW3_
    implicit none
		type(Grid) :: xGrid, omegaGrid
		type(Grid) :: xGrid2, omegaGrid2
		
		call xGrid.init( -10.0_8, 10.0_8, 10 )
		omegaGrid = FourierTransform_omegaGrid( xGrid )
		
		xGrid2 = xGrid
		call xGrid2.resize( 5, 0 )
		omegaGrid2 = FourierTransform_omegaGrid( xGrid2 )
		
		call assert_equal( xGrid%nPoints, 10, "FourierTransform_test: xGrid nPoints" )
		call assert_equal( xGrid2%nPoints, 20, "FourierTransform_test: xGrid2 nPoints" )
		call assert_equal( omegaGrid%nPoints, 10, "FourierTransform_test: omegaGrid nPoints" )
		call assert_equal( omegaGrid2%nPoints, 20, "FourierTransform_test: omegaGrid2 nPoints" )

    contains

    	
    	!>
    	!! This is neccesary only for FourierTransform_test()
    	!! Maxima:
    	!!   f(x) := exp(-0.1*x**2)*(   0.5*cos(9.0*x) -  0.5*Math_I*sin(5.0*x) +  2.0*sin(2.5*x) );
    	!!
    	!!   fortran( diff( f(x), x, 1 ) );
    	!!   fortran( diff( f(x), x, 2 ) );
    	!!
    	function funcTest( x ) result( output )
    		real(8), intent(in) :: x
    		complex(8) :: output
    		
    		output = exp(-0.1_8*x**2)*( 0.5_8*cos(9.0_8*x) - 0.5_8*Math_I*sin(5.0_8*x) + 2.0_8*sin(2.5_8*x) )
    
    ! 		http://www.ee.nmt.edu/~wedeward/EE341/FA97/example9.html
    ! 		output = exp(-2.0_8*x)*Math_ustep(x)
    	end function funcTest

    	
    	!>
    	!! This is neccesary only for FourierTransform_test()
    	!! Maxima:
    	!!   f(x) := exp(-0.1*x**2)*(   0.5*cos(9.0*x) -  0.5*Math_I*sin(5.0*x) +  2.0*sin(2.5*x) );
    	!!
    	!!   fortran( diff( f(x), x, 1 ) );
    	!!   fortran( diff( f(x), x, 2 ) );
    	!!
    	function funcTestWithNoise( x ) result( output )
    		real(8), intent(in) :: x
    		complex(8) :: output
    		
    		output = exp(-0.1_8*x**2)*( 0.5_8*cos(9.0_8*x) - 0.5_8*Math_I*sin(5.0_8*x) + 2.0_8*sin(2.5_8*x) ) &
    					+ 0.3_8*sin(RandomUtils_uniform([7.0_8,10.0_8])*x)  !  uniform noise
    	end function funcTestWithNoise

    	
    	!>
    	!! This is neccesary only for FourierTransform_test()
    	!! Maxima:
    	!!   f(x) := exp(-0.1*x**2)*(   0.5*cos(9.0*x) -  0.5*Math_I*sin(5.0*x) +  2.0*sin(2.5*x) );
    	!!
    	!!   fortran( diff( f(x), x, 1 ) );
    	!!   fortran( diff( f(x), x, 2 ) );
    	!!
    	function dfuncTest( x ) result( output )
    		real(8), intent(in) :: x
    		complex(8) :: output
    		
    		output = exp(-1.0E-1*x**2)*(-4.5E+0*sin(9.0E+0*x)-2.5E+0*Math_I*cos(5.0E+0*x) &
    					+5.0E+0*cos(2.5E+0*x))-2.0E-1*x*exp(-1.0E-1*x**2)*(5.0E-1*cos(9.0E+0*x) &
    						-5.0E-1*Math_I*sin(5.0E+0*x)+2.0E+0*sin(2.5E+0*x))
    	end function dfuncTest

    	
    	!>
    	!! This is neccesary only for FourierTransform_test()
    	!! Maxima:
    	!!   f(x) := exp(-0.1*x**2)*(   0.5*cos(9.0*x) -  0.5*Math_I*sin(5.0*x) +  2.0*sin(2.5*x) );
    	!!
    	!!   fortran( diff( f(x), x, 1 ) );
    	!!   fortran( diff( f(x), x, 2 ) );
    	!!
    	function d2funcTest( x ) result( output )
    		real(8), intent(in) :: x
    		complex(8) :: output
    		
    		output = -4.0E-1*x*exp(-1.0E-1*x**2)*(-4.5E+0*sin(9.0E+0*x)-2.5E+0*Math_I*cos(5.0E+0*x) &
    					+5.0E+0*cos(2.5E+0*x))+4.000000000000001E-2*x**2*exp(-1.0E-1*x**2)*(5.0E-1*cos(9.0E+0*x) &
    						-5.0E-1*Math_I*sin(5.0E+0*x)+2.0E+0*sin(2.5E+0*x))-2.0E-1*exp(-1.0E-1*x**2)*(5.0E-1*cos(9.0E+0*x) &
    							-5.0E-1*Math_I*sin(5.0E+0*x)+2.0E+0*sin(2.5E+0*x))+exp(-1.0E-1*x**2)*(-4.05E+1*cos(9.0E+0*x) &
    								+1.25E+1*Math_I*sin(5.0E+0*x)-1.25E+1*sin(2.5E+0*x))
    	end function d2funcTest

    	
    	!>
    	!! This is neccesary only for FourierTransform_test()
    	!! Maxima:
    	!!   f(x) := exp(-0.5*x**2);
    	!!
    	function funcGaussian( x ) result( output )
    		real(8), intent(in) :: x
    		complex(8) :: output
    		
    		real(8) :: alpha
    		
    		alpha = 0.5_8
    		output = exp(-alpha*x**2)
    	end function funcGaussian

    	
    	!>
    	!! This is neccesary only for FourierTransform_test()
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
    	!! This is neccesary only for FourierTransform_test()
    	!!
    	function funcRectangular( x ) result( output )
    		real(8), intent(in) :: x
    		complex(8) :: output
    		
    		real(8) :: a
    		
    		a = 0.5_8
    		
    		output = Math_ubox( a*x )
    	end function funcRectangular

    	
    	!>
    	!! This is neccesary only for FourierTransform_test()
    	!!
    	function FfuncRectangular( omega ) result( output )
    		real(8), intent(in) :: omega
    		complex(8) :: output
    		
    		real(8) :: a
    		
    		a = 0.5_8
    		
    		output = Math_nsinc( omega/(2.0_8*Math_PI*a) )/sqrt(2.0_8*Math_PI)/a
    	end function FfuncRectangular

end program test_FourierTransform

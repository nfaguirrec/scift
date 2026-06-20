program test_NIntegrator
    use NIntegrator_
    use TestUtils_
    use GOptions_
    use Grid_
    use RNFunction_
    implicit none
		type(Grid) :: xGrid
		type(RNFunction) :: nFunc
		type(NIntegrator) :: integrator
		real(8) :: exactValue, val
		real(8) :: Math_PI
		
		Math_PI = acos(-1.0_8)
		
		call xGrid%init( -30.0_8, 30.0_8, 1000 )
		nFunc = RNFunction( xGrid, funcTest )
		
		exactValue = 2.16780136532979_8
		
		call integrator%init( nFunc, NIntegrator_SIMPSON )
		val = integrator%evaluate()
		call assert_equal_real( val, exactValue, 1e-6_8, "NIntegrator_test: SIMPSON full domain" )
		
		call integrator%init( nFunc, NIntegrator_BOOLE )
		val = integrator%evaluate()
		call assert_equal_real( val, exactValue, 1e-6_8, "NIntegrator_test: BOOLE full domain" )
		
		exactValue = 1.623685454371397_8
		
		call integrator%init( nFunc, NIntegrator_SIMPSON )
		val = integrator%evaluate( -Math_PI, Math_PI )
		call assert_equal_real( val, exactValue, 1e-4_8, "NIntegrator_test: SIMPSON sub-domain" )
		
		call integrator%init( nFunc, NIntegrator_BOOLE )
		val = integrator%evaluate( -Math_PI, Math_PI )
		call assert_equal_real( val, exactValue, 1e-4_8, "NIntegrator_test: BOOLE sub-domain" )
		

    contains

    	
    	!>
    	!! This is neccesary only for NFunction_test()
    	!!
    	function funcTest( x ) result( output )
    		real(8), intent(in) :: x
    		real(8) :: output
    		
    		output = exp(-0.44*abs(x))*sin(x)**2.0_8
    	end function funcTest

end program test_NIntegrator

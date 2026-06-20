program test_NDerivator
    use NDerivator_
    use TestUtils_
    use Math_
    use Grid_
    use RNFunction_
    implicit none
		type(Grid) :: xGrid
		type(RNFunction), target :: func
		type(NDerivator) :: derivator
		
		call xGrid%init( 0.0_8, 10.0_8, 101 )
		func = RNFunction( xGrid, funcTest )
		call derivator%init( func )
		
		call assert_equal_real( derivator%evaluate( 1.0_8, 1 ), dfuncTest(1.0_8), 1e-4_8, "NDerivator_test: 1st derivative at 1.0" )
		call assert_equal_real( derivator%evaluate( 5.0_8, 1 ), dfuncTest(5.0_8), 1e-4_8, "NDerivator_test: 1st derivative at 5.0" )
		
		call assert_equal_real( derivator%evaluate( 1.0_8, 2 ), d2funcTest(1.0_8), 1e-4_8, "NDerivator_test: 2nd derivative at 1.0" )
		call assert_equal_real( derivator%evaluate( 5.0_8, 2 ), d2funcTest(5.0_8), 1e-4_8, "NDerivator_test: 2nd derivative at 5.0" )
		

    contains

    	
    	!>
    	!! This is neccesary only for NFunction_test()
    	!!       f = exp(-0.44*x)*sin(x)**2
    	!!   df/dx = exp(-0.44*x)*(2.0*sin(x)*cos(x)-0.44*sin(x)**2)
    	!! d2f/dx2 = exp(-0.44*x)*(2.0*cos(x)**2 - 1.76*cos(x)*sin(x) - 2.0*sin(x)**2 + 0.1936*sin(x)**2)
    	!!
    	function funcTest( x ) result( output )
    		real(8), intent(in) :: x
    		real(8) :: output
    		
    		output = exp(-0.44*x)*sin(x)**2.0_8
    	end function funcTest

    	
    	!>
    	!! This is neccesary only for NFunction_test()
    	!!       f = exp(-0.44*x)*sin(x)**2
    	!!   df/dx = exp(-0.44*x)*(2.0*sin(x)*cos(x)-0.44*sin(x)**2)
    	!! d2f/dx2 = exp(-0.44*x)*(2.0*cos(x)**2 - 1.76*cos(x)*sin(x) - 2.0*sin(x)**2 + 0.1936*sin(x)**2)
    	!!
    	function dfuncTest( x ) result( output )
    		real(8), intent(in) :: x
    		real(8) :: output
    		
    		output = exp(-0.44*x)*(2.0*sin(x)*cos(x)-0.44*sin(x)**2)
    	end function dfuncTest

    	
    	!>
    	!! This is neccesary only for NFunction_test()
    	!!       f = exp(-0.44*x)*sin(x)**2
    	!!   df/dx = exp(-0.44*x)*(2.0*sin(x)*cos(x)-0.44*sin(x)**2)
    	!! d2f/dx2 = exp(-0.44*x)*(2.0*cos(x)**2 - 1.76*cos(x)*sin(x) - 2.0*sin(x)**2 + 0.1936*sin(x)**2)
    	!!
    	function d2funcTest( x ) result( output )
    		real(8), intent(in) :: x
    		real(8) :: output
    		
    		output = exp(-0.44*x)*(2.0*cos(x)**2 - 1.76*cos(x)*sin(x) - 2.0*sin(x)**2 + 0.1936*sin(x)**2)
    	end function d2funcTest

end program test_NDerivator

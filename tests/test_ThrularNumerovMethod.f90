program test_ThrularNumerovMethod
    use ThrularNumerovMethod_
    use TestUtils_
    use IOStream_
    use Grid_
    use RNFunction_
    implicit none
		type(Grid) :: rGrid
		type(RNFunction) :: potential
		type(ThrularNumerovMethod) :: solver
		
		call rGrid%init( 1.0_8, 30.0_8, 1000 )
		
		potential = RNFunction( rGrid, funcTest )
		
		call solver%init( potential, rMass=5.0_8 )
		call solver%run()
		
		call assert_equal( solver%nStates, 7, "ThrularNumerovMethod_test: nStates" )
		call assert_equal_real( solver%rMass, 5.0_8, 1e-12_8, "ThrularNumerovMethod_test: rMass" )
		
		call assert_equal_real( solver%eigenValues(1), -4.3178702854_8, 1e-8_8, "ThrularNumerovMethod_test: eigenValues(1)" )
		call assert_equal_real( solver%eigenValues(2), -3.1035619836_8, 1e-8_8, "ThrularNumerovMethod_test: eigenValues(2)" )
		call assert_equal_real( solver%eigenValues(3), -2.0891620216_8, 1e-8_8, "ThrularNumerovMethod_test: eigenValues(3)" )
		call assert_equal_real( solver%eigenValues(4), -1.2747349806_8, 1e-8_8, "ThrularNumerovMethod_test: eigenValues(4)" )
		call assert_equal_real( solver%eigenValues(5), -0.6604098819_8, 1e-8_8, "ThrularNumerovMethod_test: eigenValues(5)" )
		call assert_equal_real( solver%eigenValues(6), -0.2462924742_8, 1e-8_8, "ThrularNumerovMethod_test: eigenValues(6)" )
		call assert_equal_real( solver%eigenValues(7), -0.0323844666_8, 1e-8_8, "ThrularNumerovMethod_test: eigenValues(7)" )

    contains

    	
    	!>
    	! This is neccesary only for NFunction_test()
    	!!
    	function funcTest( x ) result( output )
    		real(8), intent(in) :: x
    		real(8) :: output
    		
    		output = 5.0_8*( exp(2.0_8*(2.0_8-x))-2.0_8*exp(2.0_8-x) )
    	end function funcTest

end program test_ThrularNumerovMethod

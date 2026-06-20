program test_FourierGridDiagonalization
    use FourierGridDiagonalization_
    use TestUtils_
    use GOptions_
    use Math_
    use Grid_
    use RNFunction_
    use CNFunction_
    use FourierTransform_
    use Matrix_
    implicit none
		type(Grid) :: rGrid
		type(RNFunction) :: potential
		type(FourierGridDiagonalization) :: solver
		real(8) :: exactEigenValues(17)
		integer :: i
		
		exactEigenValues = [&
			0.00986922_8, &
			0.02874535_8, &
			0.04647172_8, &
			0.06304833_8, &
			0.07847518_8, &
			0.09275227_8, &
			0.10587960_8, &
			0.11785717_8, &
			0.12868498_8, &
			0.13836303_8, &
			0.14689132_8, &
			0.15426985_8, &
			0.16049862_8, &
			0.16557763_8, &
			0.16950689_8, &
			0.17228638_8, &
			0.17391611_8  &
		]
		
		call rGrid.init( 0.0_8, 30.0_8, 219 )
		
		potential = RNFunction( rGrid, funcTest )
		
		call solver.init( potential, rMass=1836.15280477874_8/2.0_8 )  ! mass from NIST for proton mp = 1.672621898e-27*kg
		call solver.run( nStates=17 )
		
		call assert_equal( solver.nStates(), 17, "FourierGridDiagonalization_test: nStates == 17" )
		do i=1,solver.nStates()
			call assert_equal_real( solver.eigenValues(i), exactEigenValues(i), 1e-5_8, "FourierGridDiagonalization_test: eigenValues match" )
		end do

    contains

    	
    	!>
    	! This is neccesary only for NFunction_test()
    	!!
    	function funcTest( x ) result( output )
    		real(8), intent(in) :: x
    		real(8) :: output
    		
    		real(8) :: D, beta, xe
    		
    ! 		output = 5.0_8*( exp(2.0_8*(2.0_8-x))-2.0_8*exp(2.0_8-x) )
    
    		D = 0.1744_8
    		beta = 1.02734_8
    		xe = 1.40201_8
    		
    		output = D*( 1.0_8 - exp(-beta*(x-xe)) )**2
    	end function funcTest

end program test_FourierGridDiagonalization

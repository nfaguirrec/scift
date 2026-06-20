program test_Morse
    use Morse_
    use TestUtils_
    use IOStream_
    use UnitsConverter_
    use Grid_
    use RNFunction_
    use ThrularNumerovMethod_
    implicit none
		real(8) :: Re, we, wexe, rMass
		type(Morse) :: myMorse
		type(Grid) :: rGrid
		type(ThrularNumerovMethod) :: solver
		integer :: i
		
		Re = 1.9879_8*angs
		we = 559.72_8*cm1
		wexe = 2.675_8*cm1
		rMass = 0.5_8*35.4257_8*amu
		
		call rGrid%init( 0.5_8, 20.0_8, 10000 )
		call myMorse%fromExp( rGrid, Re, we, wexe, rMass )
		
		call assert_equal_real( myMorse%Re, Re, 1e-6_8, "Morse_test: Re" )
		
		call solver%init( myMorse%parent(), 10, rMass )
		call solver%run()
		
		do i=1,solver%nStates
			if ( solver%eigenValues(i) < 0.0_8 ) then
				call assert_equal_real( solver%eigenValues(i), myMorse%exactEigenValues(i-1, rMass), 1e-6_8, "Morse_test: eigenvalue" )
			end if
		end do
		
		call myMorse%destroy()
		call solver%destroy()
		
end program test_Morse

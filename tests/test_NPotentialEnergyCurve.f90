program test_NPotentialEnergyCurve
    use NPotentialEnergyCurve_
    use TestUtils_
    use UnitsConverter_
    use Grid_
    use RNFunction_
    use Spline_
    use Morse_
    use ThrularNumerovMethod_
    implicit none
		type(Grid) :: rGrid
		type(Grid) :: finalGrid
		type(RNFunction) :: rawCurve
		type(NPotentialEnergyCurve) :: pECurve
		type(ThrularNumerovMethod) :: solver
		
		real(8) :: rMass = 30.0_8*0.5_8*35.4257_8*amu
		
		call rGrid%fromFile( "data/formats/GRID2D" )
		
		rawCurve = RNFunction( rGrid, shortRangeDefault )
		
		call finalGrid%init( 1.0_8, 1000.0_8, nPoints=100000 )
		
		call pECurve%init( rawCurve )
		call pECurve%run( finalGrid, longRangeDefault, veryShortRangeDefault )
		call pECurve%setUnits( [angs,cm1] )
		
		call solver%init( pECurve, rMass=rMass )
		call solver%run()
		
		call assert_equal( solver%nStates, 3, "NPotentialEnergyCurve_test: number of bound states" )
		call assert_true( abs(solver%eigenValues(1)/cm1 - (-0.0820573553_8)) < 1e-6_8, "NPotentialEnergyCurve_test: state 1" )
		call assert_true( abs(solver%eigenValues(2)/cm1 - (-0.0165778739_8)) < 1e-6_8, "NPotentialEnergyCurve_test: state 2" )
		call assert_true( abs(solver%eigenValues(3)/cm1 - (-0.0000098428_8)) < 1e-6_8, "NPotentialEnergyCurve_test: state 3" )
		
		call solver%destroy()
end program test_NPotentialEnergyCurve

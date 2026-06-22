program test_AtomicElementsDB
    use AtomicElementsDB_
    use TestUtils_
    use GOptions_
    use UnitsConverter_
    use String_
    use SpecialAtomsPair_
    implicit none
		type(AtomicElementsDB) :: atomicDB
		character(20) :: upperSymb
		
		call upper( "   Ti ", upperSymb )
		call assert_equal( atomicDB%atomicNumber( upperSymb ), 22, "Ti upperSymb atomic number" )
		call assert_equal( atomicDB%atomicNumber( " Ti " ), 22, "Ti atomic number" )
		call assert_equal( atomicDB%atomicNumber( "TI" ), 22, "TI atomic number" )
		
		call upper( "   Ti ", upperSymb )
		call assert_equal_real( atomicDB%atomicMass( upperSymb )/amu, 47.9478988647461_8, 1e-6_8, "Ti upperSymb atomic mass" )
		call assert_equal_real( atomicDB%atomicMass( " He " )/amu, 4.00260019302368_8, 1e-6_8, "He atomic mass" )
		call assert_equal_real( atomicDB%atomicMass( " Ti " )/amu, 47.9478988647461_8, 1e-6_8, "Ti atomic mass" )
		call assert_equal_real( atomicDB%atomicMass( "AU" )/amu, 196.966598510742_8, 1e-6_8, "Au atomic mass" )
		call assert_equal_real( atomicDB%atomicMass( "AR" )/amu, 39.9623985290527_8, 1e-6_8, "Ar atomic mass" )
		
		call assert_equal( atomicDB%symbol( 22 ), "Ti", "symbol(22)" )
		call assert_equal( atomicDB%symbol( 22, .true. ), "TI", "symbol(22, .true.)" )
		call assert_equal( atomicDB%symbol( 12 ), "Mg", "symbol(12)" )
		call assert_equal( atomicDB%symbol( 79 ), "Au", "symbol(79)" )
		
		call assert_equal_real( atomicDB%covalentRadius( " Ti " )/angs, 1.36000001430511_8, 1e-6_8, "Ti covalent radius" )
		call assert_equal_real( atomicDB%covalentRadius( " O " )/angs, 0.740000009536743_8, 1e-6_8, "O covalent radius" )
		call assert_equal_real( atomicDB%covalentRadius( " Re" )/angs, 0.0_8, 1e-6_8, "Re covalent radius" )
		call assert_equal_real( atomicDB%covalentRadius( "Au" )/angs, 0.0_8, 1e-6_8, "Au covalent radius" )
		
		call assert_equal( AtomicElementsDB_instance%symbol(22), "Ti", "singleton symbol(22)" )
		
		write(*,*) "All AtomicElementsDB tests PASSED"
end program test_AtomicElementsDB

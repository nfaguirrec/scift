program test_ElementsDB
    use ElementsDB_
    use TestUtils_
    use String_
    use UnitsConverter_
    implicit none
		type(ElementsDB) :: db
		
		call db%init()
		call assert_equal( db%nElements(), 1, "db.nElements" )
		call assert_equal_real( db%atomicMass( 1 )/amu, 1.00782503207_8, 1e-10_8, "Hydrogen atomic mass" )
		call assert_equal_real( db%atomicMass( "H" )/amu, 1.00782503207_8, 1e-10_8, "Hydrogen atomic mass by symbol" )
		
		write(*,*) "All ElementsDB tests PASSED"
end program test_ElementsDB

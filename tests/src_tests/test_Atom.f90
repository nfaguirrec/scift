program test_Atom
    use Atom_
    use TestUtils_
    use AtomicElementsDB_
    use UnitsConverter_
    implicit none
		type(Atom) :: atom0, atom1
		character(10) :: buffer
		
		call atom0.init()
		call assert_equal( atom0.symbol, "X", "Default constructor symbol" )
		
		call atom0.init( " He", 0.156_8, 1.456_8, 0.725_8 )
		call assert_equal( atom0.symbol, "He", "Constructor symbol" )
		call assert_equal_real( atom0.r(1), 0.156_8, 1e-10_8, "r(1)" )
		call assert_equal_real( atom0.r(2), 1.456_8, 1e-10_8, "r(2)" )
		call assert_equal_real( atom0.r(3), 0.725_8, 1e-10_8, "r(3)" )
		call assert_equal_real( atom0.x, 0.156_8, 1e-10_8, "x pointer" )
		call assert_equal_real( atom0.y, 1.456_8, 1e-10_8, "y pointer" )
		call assert_equal_real( atom0.z, 0.725_8, 1e-10_8, "z pointer" )
		
		buffer = "  He  "
		call atom0.init( trim(buffer), 0.156_8, 1.456_8, 0.725_8 )
		call assert_equal( atom0.symbol, "He", "Constructor symbol with buffer" )
		call assert_equal_real( atom0.x, 0.156_8, 1e-10_8, "x pointer after buffer init" )
		
		atom1 = atom0
		call assert_equal( atom1.symbol, "He", "Copy constructor symbol" )
		call assert_equal_real( atom1.r(1), atom0.r(1), 1e-10_8, "Copy constructor r(1)" )
		call assert_equal_real( atom1.x, atom0.x, 1e-10_8, "Copy constructor x" )
		
		atom1.r = [ 2.000_8, 1.000_8, 0.000_8 ]
		call assert_equal_real( atom1.r(1), 2.000_8, 1e-10_8, "Modified r(1)" )
		call assert_equal_real( atom1.x, 2.000_8, 1e-10_8, "x reflects r(1) change" )
		call assert_equal_real( atom1.y, 1.000_8, 1e-10_8, "y reflects r(2) change" )
		call assert_equal_real( atom1.z, 0.000_8, 1e-10_8, "z reflects r(3) change" )
		
		! Verify atom0 is not affected
		call assert_equal_real( atom0.r(1), 0.156_8, 1e-10_8, "atom0.r(1) unchanged" )
		call assert_equal_real( atom0.x, 0.156_8, 1e-10_8, "atom0.x unchanged" )
		
		atom1.x = -1.000_8
		call assert_equal_real( atom1.r(1), -1.000_8, 1e-10_8, "r(1) reflects x pointer change" )
		call assert_equal_real( atom1.x, -1.000_8, 1e-10_8, "x pointer updated value" )
		
		! Verify atom0 is not affected
		call assert_equal_real( atom0.r(1), 0.156_8, 1e-10_8, "atom0.r(1) remains unchanged" )
		
		write(*,*) "All Atom tests PASSED"
end program test_Atom

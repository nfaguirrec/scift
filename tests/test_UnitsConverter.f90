program test_UnitsConverter
    use UnitsConverter_
    use TestUtils_
    use GOptions_
    use String_
    implicit none
		real(8) :: val
		
		val = sUnit("eV")
		call assert_equal_real( val, eV, 1e-12_8, "UnitsConverter_test: sUnit eV" )
		
		val = sUnit("angs")
		call assert_equal_real( val, angs, 1e-12_8, "UnitsConverter_test: sUnit angs" )
		
		val = sUnit("amu")
		call assert_equal_real( val, amu, 1e-12_8, "UnitsConverter_test: sUnit amu" )
		
		val = sUnit("Eh")
		call assert_equal_real( val, Eh, 1e-12_8, "UnitsConverter_test: sUnit Eh" )
		
		val = sUnit("kelvin")
		call assert_equal_real( val, kelvin, 1e-12_8, "UnitsConverter_test: sUnit kelvin" )
		
		val = sUnit("fs")
		call assert_equal_real( val, fs, 1e-12_8, "UnitsConverter_test: sUnit fs" )
		
		val = sUnit("non-existent")
		call assert_equal_real( val, 1.0_8, 1e-12_8, "UnitsConverter_test: sUnit non-existent" )
		
		call assert_equal_real( 27.21_8 * eV, 0.999948659468975_8, 1e-6_8, "UnitsConverter_test: Energy eV" )
		call assert_equal_real( 9.10938188e-31_8 * kg, 5.485798935660015e-4_8, 1e-9_8, "UnitsConverter_test: Mass kg" )
end program test_UnitsConverter

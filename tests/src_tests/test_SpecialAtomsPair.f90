program test_SpecialAtomsPair
    use SpecialAtomsPair_
    use TestUtils_
    implicit none
		type(SpecialAtomsPair) :: pair1, pair2
		
		call pair1%init( "C  ", "H  ", 1.08_8, 1.20_8 )
		call assert_equal( pair1%symbol1, "C  ", "SpecialAtomsPair_test: symbol1" )
		call assert_equal( pair1%symbol2, "H  ", "SpecialAtomsPair_test: symbol2" )
		call assert_true( abs(pair1%bondCutoff - 1.08_8) < 1e-12_8, "SpecialAtomsPair_test: bondCutoff" )
		call assert_true( abs(pair1%doubleBondCutoff - 1.20_8) < 1e-12_8, "SpecialAtomsPair_test: doubleBondCutoff" )
		
		pair2 = pair1
		call assert_equal( pair2%symbol1, "C  ", "SpecialAtomsPair_test: copy symbol1" )
		call assert_equal( pair2%symbol2, "H  ", "SpecialAtomsPair_test: copy symbol2" )
		call assert_true( abs(pair2%bondCutoff - 1.08_8) < 1e-12_8, "SpecialAtomsPair_test: copy bondCutoff" )
		call assert_true( abs(pair2%doubleBondCutoff - 1.20_8) < 1e-12_8, "SpecialAtomsPair_test: copy doubleBondCutoff" )
end program test_SpecialAtomsPair

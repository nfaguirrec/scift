program test_MathFormula
    use MathFormula_
    use TestUtils_
    use String_
    use MathParser_
    implicit none
		type(MathFormula) :: formula
		character(100) :: cVars(3)
		real(8) :: cVals(3)
		real(8) :: val
		
		cVars = [ character(len=100) :: "pi", "c", "a" ]
		cVals = [ 3.141592_8, 137.0_8, 1.0_8 ]
		
		call formula%init( "0.5*x**2+y**2+a**2+c*pi", variables="x,y", constants=cVars, constantsValues=cVals )
		
		val = formula%evaluate( [0.5_8,0.6_8] )
		call assert_equal_real( val, 431.883104_8, 1e-6_8, "MathFormula_test: evaluate" )
end program test_MathFormula

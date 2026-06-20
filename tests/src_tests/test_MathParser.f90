program test_MathParser
    use MathParser_
    use TestUtils_
    implicit none
		type(MathParser) :: parser
		character(255) :: func
		character(6) :: var(5)
		real(8) :: val(5)
		
		character(10), allocatable :: cVar(:)
		real(8), allocatable :: cVal(:)
		
		allocate( cVar(2) )
		cVar = [ 'r', 's ' ]
		
		allocate( cVal(2) )
		cVal = [ 2.0_8, 3.0_8 ]
		
		call parser%init()
		
		func = "-0.5*12*cos(0.5)*(a**2+2)*sin(b*0.5+0.6)*1e-2+0.5*x+r+s"
		var  = [ 'x', 'a', 'b', cVar ]
		val  = [ 2.0_8, 3.0_8, 4.0_8, cVal ]
		
		call parser%parseFunction( func, var )
		call assert_equal_real( parser%evaluateFunction( val ), 5.7014166_8, 1e-5_8, "MathParser_test: evaluate complex function" )
		
		call parser%parseFunction( "0.5*10.8", ["x"] )
		call assert_equal_real( parser%evaluateFunction( [0.0_8] ), 5.4_8, 1e-6_8, "MathParser_test: evaluate simple multiplication" )
		
		func = "sgn(-1.456)"
		call parser%parseFunction( func, var )
		call assert_equal_real( parser%evaluateFunction( [0.0_8] ), -1.0_8, 1e-6_8, "MathParser_test: sgn" )
		
		func = "ustep(1.456)"
		call parser%parseFunction( func, var )
		call assert_equal_real( parser%evaluateFunction( [0.0_8] ), 1.0_8, 1e-6_8, "MathParser_test: ustep positive" )
		
		func = "ustep(-1.456)"
		call parser%parseFunction( func, var )
		call assert_equal_real( parser%evaluateFunction( [0.0_8] ), 0.0_8, 1e-6_8, "MathParser_test: ustep negative" )
		
		func = "ustep(0.0)"
		call parser%parseFunction( func, var )
		call assert_equal_real( parser%evaluateFunction( [0.0_8] ), 0.5_8, 1e-6_8, "MathParser_test: ustep zero" )
		
		func = "ubox(-0.6)"
		call parser%parseFunction( func, var )
		call assert_equal_real( parser%evaluateFunction( [0.0_8] ), 0.0_8, 1e-6_8, "MathParser_test: ubox -0.6" )
		
		func = "ubox(-0.5)"
		call parser%parseFunction( func, var )
		call assert_equal_real( parser%evaluateFunction( [0.0_8] ), 0.5_8, 1e-6_8, "MathParser_test: ubox -0.5" )
		
		func = "ubox(0.0)"
		call parser%parseFunction( func, var )
		call assert_equal_real( parser%evaluateFunction( [0.0_8] ), 1.0_8, 1e-6_8, "MathParser_test: ubox 0.0" )
		
		func = "ubox(0.3)"
		call parser%parseFunction( func, var )
		call assert_equal_real( parser%evaluateFunction( [0.0_8] ), 1.0_8, 1e-6_8, "MathParser_test: ubox 0.3" )
		
		func = "ubox(0.5)"
		call parser%parseFunction( func, var )
		call assert_equal_real( parser%evaluateFunction( [0.0_8] ), 0.5_8, 1e-6_8, "MathParser_test: ubox 0.5" )
		
		func = "ubox(0.6)"
		call parser%parseFunction( func, var )
		call assert_equal_real( parser%evaluateFunction( [0.0_8] ), 0.0_8, 1e-6_8, "MathParser_test: ubox 0.6" )
		
		deallocate( cVar )
		deallocate( cVal )
		
end program test_MathParser

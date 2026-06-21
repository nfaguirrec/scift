program test_RNFunction2D
    use RNFunction2D_
    use TestUtils_
    use GOptions_
    use Math_
    use String_
    use IOStream_
    use RNFunction_
    use Grid2D_
    implicit none
		type(RNFunction2D) :: func, func2
		
		real(8) :: xVec(3)
		real(8) :: yVec(2)
		real(8) :: fArray(3,2)
		
		type(Grid2D) :: xyGrid
	
		xVec(:) = [ 1.0, 2.0, 3.0 ]
		yVec(:) = [-1.0, 0.0 ]
		
		fArray(1,:) = [ 0.0, 4.0 ]
		fArray(2,:) = [ 2.0, 2.0 ]
		fArray(3,:) = [ 6.0,-1.0 ]
		
		call func%init( xVec, yVec, fArray )
		call assert_equal( func%nPoints(1), 3, "RNFunction2D_test: nPoints(1)" )
		call assert_equal( func%nPoints(2), 2, "RNFunction2D_test: nPoints(2)" )
		call assert_true( abs(func%at(1,1) - 0.0_8) < 1e-12_8, "RNFunction2D_test: at(1,1)" )
		call assert_true( abs(func%at(1,2) - 4.0_8) < 1e-12_8, "RNFunction2D_test: at(1,2)" )
		call assert_true( abs(func%at(2,1) - 2.0_8) < 1e-12_8, "RNFunction2D_test: at(2,1)" )
		call assert_true( abs(func%at(2,2) - 2.0_8) < 1e-12_8, "RNFunction2D_test: at(2,2)" )
		call assert_true( abs(func%at(3,1) - 6.0_8) < 1e-12_8, "RNFunction2D_test: at(3,1)" )
		call assert_true( abs(func%at(3,2) - (-1.0_8)) < 1e-12_8, "RNFunction2D_test: at(3,2)" )
		
		call xyGrid%fromArray( xVec, yVec )
		call func%init( xyGrid, fArray )
		call assert_equal( func%nPoints(1), 3, "RNFunction2D_test: nPoints(1) xyGrid" )
		call assert_equal( func%nPoints(2), 2, "RNFunction2D_test: nPoints(2) xyGrid" )
		call assert_true( abs(func%at(3,1) - 6.0_8) < 1e-12_8, "RNFunction2D_test: at(3,1) xyGrid" )
		
		call xyGrid%init( min=[-5.0_8,-5.0_8], max=[5.0_8,5.0_8], size=[100,100] )
		call func%init( xyGrid, funcTest )
		call assert_equal( func%nPoints(1), 100, "RNFunction2D_test: nPoints(1) funcTest" )
		call assert_equal( func%nPoints(2), 100, "RNFunction2D_test: nPoints(2) funcTest" )
		call assert_true( abs(func%at(50,50) - funcTest(func%x(50), func%y(50))) < 1e-12_8, "RNFunction2D_test: at(50,50) funcTest" )
		
		call func%init( "data/formats/real-N2DF", format=N2DF_FORMAT )
		call assert_equal( func%nPoints(1), 100, "RNFunction2D_test: load real-N2DF nPoints(1)" )
		call assert_equal( func%nPoints(2), 100, "RNFunction2D_test: load real-N2DF nPoints(2)" )
		
		func2 = func
		call assert_equal( func2%nPoints(1), 100, "RNFunction2D_test: copy nPoints(1)" )
		call assert_equal( func2%nPoints(2), 100, "RNFunction2D_test: copy nPoints(2)" )
		call assert_true( abs(func2%at(1,1) - func%at(1,1)) < 1e-12_8, "RNFunction2D_test: copy at(1,1)" )
		
		call func%save( "salida.n2df", format=N2DF_FORMAT )
		call func2%load( "salida.n2df", format=N2DF_FORMAT )
		call assert_equal( func2%nPoints(1), 100, "RNFunction2D_test: load saved nPoints(1)" )
		call assert_equal( func2%nPoints(2), 100, "RNFunction2D_test: load saved nPoints(2)" )
		call assert_true( abs(func2%at(10,10) - func%at(10,10)) < 1e-12_8, "RNFunction2D_test: load saved values" )
		
		call func%init( xVec, yVec, fArray )
		call func%resize( 3, 2, +1, +1 )
		call assert_equal( func%nPoints(1), 6, "RNFunction2D_test: resize nPoints(1)" )
		call assert_equal( func%nPoints(2), 4, "RNFunction2D_test: resize nPoints(2)" )
		call assert_true( abs(func%at(1,1) - 0.0_8) < 1e-12_8, "RNFunction2D_test: resize at(1,1)" )
		call assert_true( abs(func%at(3,2) - (-1.0_8)) < 1e-12_8, "RNFunction2D_test: resize at(3,2)" )
		

    contains

    	
    	!>
    	!! This is neccesary only for NFunction_test()
    	!!
    	pure function funcTest( x, y ) result( output )
    		real(8), intent(in) :: x, y
    		real(8) :: output
    		
    		output = sin( Math_PI*sqrt(x**2+y**2) ) /sqrt(x**2+y**2)  ! Mexican Hat function
    	end function funcTest

end program test_RNFunction2D

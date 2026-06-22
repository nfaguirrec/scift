program test_RNFunction3D
    use RNFunction3D_
    use TestUtils_
    use GOptions_
    use Math_
    use String_
    use IOStream_
    use RNFunction_
    use Grid3D_
    implicit none
		type(RNFunction3D) :: func, func2
		
		real(8) :: xVec(3)
		real(8) :: yVec(2)
		real(8) :: zVec(2)
		real(8) :: fArray(3,2,2)
		
		type(Grid3D) :: xyzGrid
	
		xVec(:) = [ 1.0, 2.0, 3.0 ]
		yVec(:) = [-1.0, 0.0 ]
		zVec(:) = [ 1.0, 2.0 ]
		
		fArray(1,:,1) = [ 0.0, 4.0 ]
		fArray(2,:,1) = [ 2.0, 2.0 ]
		fArray(3,:,1) = [ 0.0, 4.0 ]
		
		fArray(1,:,2) = [ 6.0,-1.0 ]
		fArray(2,:,2) = [ 7.0, 0.0 ]
		fArray(3,:,2) = [ 2.0, 1.0 ]
		
		call func%init( xVec, yVec, zVec, fArray )
		call assert_equal( func%nPoints(1), 3, "RNFunction3D_test: nPoints(1)" )
		call assert_equal( func%nPoints(2), 2, "RNFunction3D_test: nPoints(2)" )
		call assert_equal( func%nPoints(3), 2, "RNFunction3D_test: nPoints(3)" )
		call assert_true( abs(func%at(1,1,1) - 0.0_8) < 1e-12_8, "RNFunction3D_test: at(1,1,1)" )
		call assert_true( abs(func%at(3,2,2) - 1.0_8) < 1e-12_8, "RNFunction3D_test: at(3,2,2)" )
		
		call xyzGrid%init( xVec, yVec, zVec )
		call func%init( xyzGrid, fArray )
		call assert_equal( func%nPoints(1), 3, "RNFunction3D_test: Grid3D nPoints(1)" )
		call assert_true( abs(func%at(3,2,2) - 1.0_8) < 1e-12_8, "RNFunction3D_test: Grid3D at(3,2,2)" )
		
		call xyzGrid%init( min=[-3.0_8,-3.0_8,-3.0_8], max=[3.0_8,3.0_8,3.0_8], size=[50,50,50] )
		call func%init( xyzGrid, funcTest )
		call assert_equal( func%nPoints(1), 50, "RNFunction3D_test: funcTest nPoints(1)" )
		call assert_true( abs(func%at(25,25,25) - funcTest(func%x(25), func%y(25), func%z(25))) < 1e-12_8, "RNFunction3D_test: funcTest value" )
		
		call func%init( "data/formats/real-N3DF", format=N3DF_FORMAT )
		call assert_equal( func%nPoints(1), 50, "RNFunction3D_test: N3DF load nPoints(1)" )
		
		call func%init( "data/formats/CUBE", format=CUBE_FORMAT )
		call assert_equal( func%nPoints(1), 50, "RNFunction3D_test: CUBE load nPoints(1)" )
		
		func2 = func
		call assert_equal( func2%nPoints(1), 50, "RNFunction3D_test: copy nPoints(1)" )
		call assert_true( abs(func2%at(1,1,1) - func%at(1,1,1)) < 1e-12_8, "RNFunction3D_test: copy at(1,1,1)" )
		
		call func%save( "salida.cube", format=CUBE_FORMAT )
		call func%save( "salida.n3df", format=N3DF_FORMAT )
		
		call func%load( "salida.cube", format=CUBE_FORMAT )
		call func%save( "salida2.cube", format=CUBE_FORMAT )
		call assert_equal( func%nPoints(1), 50, "RNFunction3D_test: load CUBE nPoints(1)" )
		
		call func%load( "salida.n3df", format=N3DF_FORMAT )
		call func%save( "salida3.cube", format=CUBE_FORMAT )
		call assert_equal( func%nPoints(1), 50, "RNFunction3D_test: load N3DF nPoints(1)" )
		
		call func%init( xVec, yVec, zVec, fArray )
		call func%resize( 3, 2, 1, +1, +1, +1 )
		call assert_equal( func%nPoints(1), 6, "RNFunction3D_test: resize nPoints(1)" )
		call assert_equal( func%nPoints(2), 4, "RNFunction3D_test: resize nPoints(2)" )
		call assert_equal( func%nPoints(3), 3, "RNFunction3D_test: resize nPoints(3)" )
		

    contains

    	
    	!>
    	!! This is neccesary only for NFunction_test()
    	!!       f = exp(-0.44*x)*sin(x)**2
    	!!   df/dx = exp(-0.44*x)*(2.0*sin(x)*cos(x)-0.44*sin(x)**2)
    	!! d2f/dx2 = exp(-0.44*x)*(2.0*cos(x)**2 - 1.76*cos(x)*sin(x) - 2.0*sin(x)**2 + 0.1936*sin(x)**2)
    	!!
    	pure function funcTest( x, y, z ) result( output )
    		real(8), intent(in) :: x, y, z
    		real(8) :: output
    		
    ! 		output = sqrt( x**2 + y**2 + z**2 )
    ! 		output = (x**2+y**2-1.0_8)**3-x**2*y**3+z**2
    		output = (x**2+2.25_8*y**2+z**2-1.0_8)**3-x**2*z**3-0.1125_8*y**2*z**3  ! http://mathworld%wolfram%com/HeartSurface%html
    	end function funcTest

    	
    	!>
    	!! This is neccesary only for NFunction_test()
    	!!       f = exp(-0.44*x)*sin(x)**2
    	!!   df/dx = exp(-0.44*x)*(2.0*sin(x)*cos(x)-0.44*sin(x)**2)
    	!! d2f/dx2 = exp(-0.44*x)*(2.0*cos(x)**2 - 1.76*cos(x)*sin(x) - 2.0*sin(x)**2 + 0.1936*sin(x)**2)
    	!!
    	function dfuncTest( x ) result( output )
    		real(8), intent(in) :: x
    		real(8) :: output
    		
    		output = exp(-0.44*x)*(2.0*sin(x)*cos(x)-0.44*sin(x)**2)
    	end function dfuncTest

    	
    	!>
    	!! This is neccesary only for NFunction_test()
    	!!       f = exp(-0.44*x)*sin(x)**2
    	!!   df/dx = exp(-0.44*x)*(2.0*sin(x)*cos(x)-0.44*sin(x)**2)
    	!! d2f/dx2 = exp(-0.44*x)*(2.0*cos(x)**2 - 1.76*cos(x)*sin(x) - 2.0*sin(x)**2 + 0.1936*sin(x)**2)
    	!!
    	function d2funcTest( x ) result( output )
    		real(8), intent(in) :: x
    		real(8) :: output
    		
    		output = exp(-0.44*x)*(2.0*cos(x)**2 - 1.76*cos(x)*sin(x) - 2.0*sin(x)**2 + 0.1936*sin(x)**2)
    	end function d2funcTest

    	
    	!>
    	!! This is neccesary only for NFunction_test()
    	!!
    	function funcTest2( x ) result( output )
    		real(8), intent(in) :: x
    		real(8) :: output
    		
    		output = sin(x)
    	end function funcTest2

end program test_RNFunction3D

program test_CNFunction3D
    use CNFunction3D_
    use TestUtils_
    use Grid3D_
    use GOptions_
    use Math_
    use String_
    use IOStream_
    use RNFunction_
    use RNFunction3D_
    implicit none
		type(CNFunction3D) :: func, func2
		
		real(8) :: xVec(3)
		real(8) :: yVec(2)
		real(8) :: zVec(2)
		complex(8) :: fArray(3,2,2)
		
		type(Grid3D) :: xyzGrid
	
		xVec(:) = [ 1.0, 2.0, 3.0 ]
		yVec(:) = [-1.0, 0.0 ]
		zVec(:) = [ 2.0, 1.0 ]
		
		fArray(1,:,1) = [ 0.0, 4.0 ]
		fArray(2,:,1) = [ 2.0, 2.0 ]
		fArray(3,:,1) = [ 0.0, 4.0 ]
		
		fArray(1,:,2) = [ 6.0,-1.0 ]
		fArray(2,:,2) = [ 7.0, 0.0 ]
		fArray(3,:,2) = [ 2.0, 1.0 ]
		
		call func.init( xVec, yVec, zVec, fArray )
		call assert_equal( func.size(), 12, "func size init" )
		call assert_equal_real( func.xyzGrid.min(1), 1.0_8, 1e-10_8, "func min(1) init" )
		call assert_equal_real( func.xyzGrid.max(2), 0.0_8, 1e-10_8, "func max(2) init" )
		
		call xyzGrid.init( xVec, yVec, zVec )
		call func.init( xyzGrid, fArray )
		call assert_equal( func.size(), 12, "func size init Grid" )
		
		call func.init( "data/formats/complex-N3DF", format=N3DF_FORMAT )
		call assert_equal( func.size(), 125000, "complex-N3DF size" )
		
		call func.init( "data/formats/CUBE", format=CUBE_FORMAT )
		call assert_equal( func.size(), 125000, "CUBE format size" )
		
		call xyzGrid.init( min=[-3.0_8,-3.0_8,-3.0_8], max=[3.0_8,3.0_8,3.0_8], size=[200,200,200] )
		call func.init( xyzGrid, funcTest )
		call assert_equal( func.size(), 8000000, "funcTest formula size" )
		
		func2 = func
		call assert_equal( func2.size(), 8000000, "copy constructor size" )
		
		call func.save( "salida.cube", format=CUBE_FORMAT )
		call func.save( "salida.rcube", format=RCUBE_FORMAT )
		call func.save( "salida.icube", format=ICUBE_FORMAT )
		call func.save( "salida.n3df", format=N3DF_FORMAT )
		
		write(*,*) "All CNFunction3D tests PASSED"

    contains

    	
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

end program test_CNFunction3D

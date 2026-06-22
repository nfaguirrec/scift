program test_RNFunction
    use RNFunction_
    use TestUtils_
    use GOptions_
    use Grid_
    use String_
    use IOStream_
    implicit none
		type(Grid) :: xGrid
		type(Grid) :: xGrid2
		type(RNFunction) :: nFunc
		type(RNFunction) :: nFunc2
		real(8), allocatable :: data(:)
		integer :: i
		
		call xGrid%init( 1.0_8, 10.0_8, 100 )
		
		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		! Test from function
		nFunc = RNFunction( xGrid, func=funcTest )
		call assert_equal( nFunc%nPoints(), 100, "RNFunction_test: nPoints from function" )
		call assert_true( abs(nFunc%at(1) - funcTest(1.0_8)) < 1e-12_8, "RNFunction_test: at(1) from function" )
		call assert_true( abs(nFunc%at(100) - funcTest(10.0_8)) < 1e-12_8, "RNFunction_test: at(100) from function" )
		
		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		! Test for copy constructor
		call nFunc2%copy( nFunc )
		call assert_equal( nFunc2%nPoints(), 100, "RNFunction_test: nPoints copy" )
		call assert_true( all(nFunc2%fArray == nFunc%fArray), "RNFunction_test: fArray copy" )
		
		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		! Test from array
		allocate( data(xGrid%nPoints) )
		do i=1,xGrid%nPoints
			data(i) = funcTest( xGrid%data(i) )
		end do
		
		nFunc = RNFunction( xGrid, fArray=data )
		call assert_equal( nFunc%nPoints(), 100, "RNFunction_test: nPoints from array" )
		call assert_true( all(nFunc%fArray == data), "RNFunction_test: fArray from array" )
		deallocate( data )

		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		! Testing resize
		call xGrid%init( 1.0_8, 10.0_8, 10 )
		nFunc2 = RNFunction( xGrid, func=funcTest )
		
		! resize grid +10, dir = +1
		nFunc = nFunc2
		call nFunc%resize(+10,+1)
		call assert_equal( nFunc%nPoints(), 20, "RNFunction_test: resize +10, dir=+1 size" )
		call assert_true( abs(nFunc%x(1) - 1.0_8) < 1e-12_8, "RNFunction_test: resize +10, dir=+1 min" )
		call assert_true( abs(nFunc%x(20) - 20.0_8) < 1e-12_8, "RNFunction_test: resize +10, dir=+1 max" )
		
		call nFunc%resize(-10,+1)
		call assert_equal( nFunc%nPoints(), 10, "RNFunction_test: resize -10, dir=+1 size" )
		call assert_true( abs(nFunc%x(1) - 1.0_8) < 1e-12_8, "RNFunction_test: resize -10, dir=+1 min" )
		call assert_true( abs(nFunc%x(10) - 10.0_8) < 1e-12_8, "RNFunction_test: resize -10, dir=+1 max" )
		
		! resize grid +10, dir = -1
		nFunc = nFunc2
		call nFunc%resize(+10,-1)
		call assert_equal( nFunc%nPoints(), 20, "RNFunction_test: resize +10, dir=-1 size" )
		call assert_true( abs(nFunc%x(1) - (-9.0_8)) < 1e-12_8, "RNFunction_test: resize +10, dir=-1 min" )
		call assert_true( abs(nFunc%x(20) - 10.0_8) < 1e-12_8, "RNFunction_test: resize +10, dir=-1 max" )
		
		call nFunc%resize(-10,-1)
		call assert_equal( nFunc%nPoints(), 10, "RNFunction_test: resize -10, dir=-1 size" )
		call assert_true( abs(nFunc%x(1) - 1.0_8) < 1e-12_8, "RNFunction_test: resize -10, dir=-1 min" )
		
		! resize grid +10, dir = 0
		nFunc = nFunc2
		call nFunc%resize(+10,0)
		call assert_equal( nFunc%nPoints(), 30, "RNFunction_test: resize +10, dir=0 size" )
		call assert_true( abs(nFunc%x(1) - (-9.0_8)) < 1e-12_8, "RNFunction_test: resize +10, dir=0 min" )
		call assert_true( abs(nFunc%x(30) - 20.0_8) < 1e-12_8, "RNFunction_test: resize +10, dir=0 max" )
		
		call nFunc%resize(-10,0)
		call assert_equal( nFunc%nPoints(), 10, "RNFunction_test: resize -10, dir=0 size" )
		call assert_true( abs(nFunc%x(1) - 1.0_8) < 1e-12_8, "RNFunction_test: resize -10, dir=0 min" )
		
		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		! Testing interpolation
		call xGrid%init( 1.0_8, 10.0_8, 21 )
		nFunc = RNFunction( xGrid, func=funcTest )
		
		call xGrid2%init( -2.0_8, 13.0_8, 41 )
		nFunc2 = nFunc%interpolate( xGrid2 )
		call assert_equal( nFunc2%nPoints(), 41, "RNFunction_test: interpolate size" )
		call assert_true( abs(nFunc2%interpolate(5.0_8) - funcTest(5.0_8)) < 1e-3_8, "RNFunction_test: interpolate accuracy" )

    contains

    	
    ! 	!>
    ! 	!! Save the data in two column format in a
    ! 	!! selected unit
    ! 	!!
    ! 	subroutine toFStream( this, ofile, units, resolution, xrange, ixrange, beforeLine )
    ! 		class(RNFunction) :: this
    ! 		type(OFStream), optional, intent(in) :: ofile
    ! 		real(8), optional, intent(in) :: units(2)
    ! 		real(8), optional, intent(in) :: resolution
    ! 		real(8), optional, intent(in) :: xrange(2)
    ! 		integer, optional, intent(in) :: ixrange(2)
    ! 		character(*), optional, intent(in) :: beforeLine
    ! 		
    ! 		integer :: effUnit
    ! 		real(8) :: effUnits(2)
    ! 		integer :: effIXRange(2)
    ! 		character(100) :: effBeforeLine
    ! 		integer :: invResolution
    ! 
    ! 		integer :: i
    ! 		character(255) :: date
    ! 		
    ! 		effUnit = IO_STDOUT
    ! 		if( present(ofile) ) effUnit = ofile%unit
    ! 		
    ! 		effUnits = [1.0_8, 1.0_8]
    ! 		if( present(units) ) effUnits = units
    ! 		
    ! 		effIXRange = [1,this%nPoints()]
    ! 		if( present(xrange) ) then
    ! 			effIXRange = [ &
    ! 				floor( 1.0000001*(xrange(1)-this%xGrid%min)/this%xGrid%stepSize+1.0 ), &
    ! 				floor( 1.0000001*(xrange(2)-this%xGrid%min)/this%xGrid%stepSize+1.0 ) ]
    ! 		else if( present(ixrange) ) then
    ! 			effIXRange = ixrange
    ! 		end if
    ! 		
    ! 		effBeforeLine = ""
    ! 		if( present(beforeLine) ) effBeforeLine = beforeLine
    ! 		
    ! 		invResolution = 1
    ! 		if( present(resolution) ) invResolution = ceiling( 1.0_8/resolution )
    ! 		
    ! 		call fdate(date)
    ! 		
    ! 		write(effUnit,"(A)") "# Real Numerical Function"
    ! 		write(effUnit,"(A)") "# "//trim(date)
    ! 		
    ! 		do i=effIXRange(1),effIXRange(2),invResolution
    ! 			if( abs(this%fArray( i )) > 1d-98 ) then
    ! 				write(effUnit,"(A,E15.7,E15.7)") trim(effBeforeLine), this%xGrid%data(i)/effUnits(1), &
    ! 					this%fArray( i )/effUnits(2)
    ! 			else
    ! 				write(effUnit,"(A,E15.7,E15.7)") trim(effBeforeLine), this%xGrid%data(i)/effUnits(1), 0.0_8
    ! 			end if
    ! 		end do
    ! 		
    ! 		write(effUnit,"(a)") ""
    ! 		write(effUnit,"(a)") ""
    ! 	end subroutine toFStream
    	
    	!>
    	!! This is neccesary only for RNFunction_test()
    	!!       f = exp(-0.44*x)*sin(x)**2
    	!!   df/dx = exp(-0.44*x)*(2.0*sin(x)*cos(x)-0.44*sin(x)**2)
    	!! d2f/dx2 = exp(-0.44*x)*(2.0*cos(x)**2 - 1.76*cos(x)*sin(x) - 2.0*sin(x)**2 + 0.1936*sin(x)**2)
    	!!
    	function funcTest( x ) result( output )
    		real(8), intent(in) :: x
    		real(8) :: output
    		
    		output = exp(-0.44*x)*sin(x)**2.0_8
    	end function funcTest

    	
    	!>
    	!! This is neccesary only for RNFunction_test()
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
    	!! This is neccesary only for RNFunction_test()
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
    	!! This is neccesary only for RNFunction_test()
    	!!
    	function funcTest2( x ) result( output )
    		real(8), intent(in) :: x
    		real(8) :: output
    		
    		output = sin(x)
    	end function funcTest2

end program test_RNFunction

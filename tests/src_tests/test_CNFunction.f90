program test_CNFunction
    use CNFunction_
    use TestUtils_
    use GOptions_
    use Grid_
    use String_
    use IOStream_
    implicit none
		type(Grid) :: xGrid
		type(Grid) :: xGrid2
		type(IFStream) :: ifile
		type(CNFunction) :: nFunc
		type(CNFunction) :: nFunc2
		complex(8), allocatable :: data(:)
		integer :: i
		
		call xGrid%init( 1.0_8, 10.0_8, 100 )
		
		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		! Test from function
		nFunc = CNFunction( xGrid, func=funcTest )
		call assert_equal( nFunc%nPoints(), 100, "nFunc from func nPoints" )
		call assert_equal_real( nFunc%x(1), 1.0_8, 1e-10_8, "nFunc from func x(1)" )
		call assert_equal_real( nFunc%x(100), 10.0_8, 1e-10_8, "nFunc from func x(100)" )
		call assert_equal_real( real(nFunc%at(1)), real(funcTest(1.0_8)), 1e-10_8, "nFunc from func at(1) real" )
		call assert_equal_real( aimag(nFunc%at(1)), aimag(funcTest(1.0_8)), 1e-10_8, "nFunc from func at(1) imag" )
		call nFunc%save( "salida1", format=BLKS_FORMAT )
		
		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		! Test for copy constructor
		call nFunc2%copy( nFunc )
		call assert_equal( nFunc2%nPoints(), 100, "nFunc2 copy nPoints" )
		call assert_equal_real( real(nFunc2%at(50)), real(nFunc%at(50)), 1e-10_8, "nFunc2 copy at(50) real" )
		call assert_equal_real( aimag(nFunc2%at(50)), aimag(nFunc%at(50)), 1e-10_8, "nFunc2 copy at(50) imag" )
		
		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		! Test from array
		allocate( data(xGrid%nPoints) )
		do i=1,xGrid%nPoints
			data(i) = funcTest( xGrid%data(i) )
		end do
		
		nFunc = CNFunction( xGrid, fArray=data )
		call assert_equal( nFunc%nPoints(), 100, "nFunc from array nPoints" )
		call assert_equal_real( real(nFunc%at(30)), real(data(30)), 1e-10_8, "nFunc from array at(30) real" )
		call nFunc%save( "salida3", format=BLKS_FORMAT )
		deallocate( data )
		
		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		! Test from IFStream
		call ifile%init( "data/formats/TWO_COLUMNS" )
		nFunc = CNFunction( ifile, columns=[1,2] )
		call assert_equal( nFunc%nPoints(), 190, "nFunc from IFStream nPoints" )
		call assert_equal_real( nFunc%x(1), 1.0_8, 1e-10_8, "nFunc from IFStream x(1)" )
		call ifile%close()
		call nFunc%save( "salidaF0", format=BLKS_FORMAT )
		
		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		! TESTING RESIZE
		call xGrid%init( 1.0_8, 10.0_8, 10 )
		nFunc = CNFunction( xGrid, func=funcTest )
		
		! Testing resize grid 5, dir = -1
		call nFunc%resize( 5, -1 )
		call assert_equal( nFunc%nPoints(), 15, "nFunc resize(5,-1) nPoints" )
		call assert_equal_real( nFunc%x(1), -4.0_8, 1e-10_8, "nFunc resize(5,-1) x(1)" )
		call assert_equal_real( real(nFunc%at(6)), 0.45603_8, 1e-5_8, "nFunc resize(5,-1) at(6) real" )
		call assert_equal_real( aimag(nFunc%at(6)), 0.54030_8, 1e-5_8, "nFunc resize(5,-1) at(6) imag" )
		
		! Testing resize grid -5, dir = -1
		call nFunc%resize( -5, -1 )
		call assert_equal( nFunc%nPoints(), 10, "nFunc resize(-5,-1) nPoints" )
		call assert_equal_real( nFunc%x(1), 1.0_8, 1e-10_8, "nFunc resize(-5,-1) x(1)" )
		
		call xGrid%init( 1.0_8, 10.0_8, 10 )
		nFunc = CNFunction( xGrid, func=funcTest )
		
		! Testing resize grid 5, dir = 0
		call nFunc%resize( 5, 0 )
		call assert_equal( nFunc%nPoints(), 20, "nFunc resize(5,0) nPoints" )
		call assert_equal_real( nFunc%x(1), -4.0_8, 1e-10_8, "nFunc resize(5,0) x(1)" )
		call assert_equal_real( nFunc%x(20), 15.0_8, 1e-10_8, "nFunc resize(5,0) x(20)" )
		
		! Testing resize grid -5, dir = 0
		call nFunc%resize( -5, 0 )
		call assert_equal( nFunc%nPoints(), 10, "nFunc resize(-5,0) nPoints" )
		call assert_equal_real( nFunc%x(1), 1.0_8, 1e-10_8, "nFunc resize(-5,0) x(1)" )
		
		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		! Testing interpolation
		call xGrid%init( 1.0_8, 10.0_8, 1000 )
		nFunc = CNFunction( xGrid, func=funcTest )
		call nFunc%save( "salidaFuncExact.dat" )
		
		call xGrid%init( 1.0_8, 10.0_8, 21 )
		nFunc = CNFunction( xGrid, func=funcTest )
		call nFunc%save( "salidaFunc.dat" )
		
		call xGrid2%init( -2.0_8, 13.0_8, 41 )
		nFunc2 = nFunc%interpolate( xGrid2 )
		call assert_equal( nFunc2%nPoints(), 41, "Interpolation nPoints" )
		call assert_equal_real( nFunc2%x(1), -2.0_8, 1e-10_8, "Interpolation x(1)" )
		call assert_equal_real( nFunc2%x(41), 13.0_8, 1e-10_8, "Interpolation x(41)" )
		call nFunc2%save( "salidaFunc2.dat" )
		
		write(*,*) "All CNFunction tests PASSED"

    contains

    	
    ! 	!>
    ! 	!! Save the data in two column format in a
    ! 	!! selected unit
    ! 	!!
    ! 	subroutine toFStream( this, ofile, units, resolution, xrange, ixrange, beforeLine )
    ! 		class(CNFunction) :: this
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
    ! 		write(effUnit,"(A)") "# Complex Numerical Function"
    ! 		write(effUnit,"(A)") "# "//trim(date)
    ! 		
    ! 		do i=effIXRange(1),effIXRange(2),invResolution
    ! 			if( abs(this%fArray( i )) > 1d-98 ) then
    ! 				write(effUnit,"(A,E15.7,2E15.7)") trim(effBeforeLine), this%xGrid%data(i)/effUnits(1), &
    ! 					real(this%fArray( i ))/effUnits(2), &
    ! 					aimag(this%fArray( i ))/effUnits(2)
    ! 			else
    ! 				write(effUnit,"(A,E15.7,2E15.7)") trim(effBeforeLine), this%xGrid%data(i)/effUnits(1), 0.0_8, 0.0_8
    ! 			end if
    ! 		end do
    ! 		
    ! 		write(effUnit,"(a)") ""
    ! 		write(effUnit,"(a)") ""
    ! 	end subroutine toFStream
    	
    	!>
    	!! This is neccesary only for CNFunction_test()
    	!!       f = exp(-0.44*x)*sin(x)**2
    	!!   df/dx = exp(-0.44*x)*(2.0*sin(x)*cos(x)-0.44*sin(x)**2)
    	!! d2f/dx2 = exp(-0.44*x)*(2.0*cos(x)**2 - 1.76*cos(x)*sin(x) - 2.0*sin(x)**2 + 0.1936*sin(x)**2)
    	!!
    	function funcTest( x ) result( output )
    		real(8), intent(in) :: x
    		complex(8) :: output
    		
    		output = exp(-0.44*x)*sin(x)**2.0_8 + cmplx(0.0_8,1.0_8)*cos(x)
    	end function funcTest

    	
    	!>
    	!! This is neccesary only for CNFunction_test()
    	!!       f = exp(-0.44*x)*sin(x)**2
    	!!   df/dx = exp(-0.44*x)*(2.0*sin(x)*cos(x)-0.44*sin(x)**2)
    	!! d2f/dx2 = exp(-0.44*x)*(2.0*cos(x)**2 - 1.76*cos(x)*sin(x) - 2.0*sin(x)**2 + 0.1936*sin(x)**2)
    	!!
    	function dfuncTest( x ) result( output )
    		real(8), intent(in) :: x
    		complex(8) :: output
    		
    		output = exp(-0.44*x)*(2.0*sin(x)*cos(x)-0.44*sin(x)**2) - cmplx(0.0_8,1.0_8)*sin(x)
    	end function dfuncTest

    	
    	!>
    	!! This is neccesary only for CNFunction_test()
    	!!       f = exp(-0.44*x)*sin(x)**2
    	!!   df/dx = exp(-0.44*x)*(2.0*sin(x)*cos(x)-0.44*sin(x)**2)
    	!! d2f/dx2 = exp(-0.44*x)*(2.0*cos(x)**2 - 1.76*cos(x)*sin(x) - 2.0*sin(x)**2 + 0.1936*sin(x)**2)
    	!!
    	function d2funcTest( x ) result( output )
    		real(8), intent(in) :: x
    		complex(8) :: output
    		
    		output = exp(-0.44*x)*(2.0*cos(x)**2 - 1.76*cos(x)*sin(x) - 2.0*sin(x)**2 + 0.1936*sin(x)**2) - cmplx(0.0_8,1.0_8)*cos(x)
    	end function d2funcTest

    	
    	!>
    	!! This is neccesary only for CNFunction_test()
    	!!
    	function funcTest2( x ) result( output )
    		real(8), intent(in) :: x
    		complex(8) :: output
    		
    		output = sin(x) + cmplx(0.0_8,1.0_8)*cos(x)
    	end function funcTest2

end program test_CNFunction

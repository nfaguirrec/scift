!>
!! @brief Test program
!!
program main
	use GOptions_
	use Math_
	use IOStream_
	use String_
	use CommandLineParser_
	use RNFunction_
	use CNFunction_
	use NIntegrator_
	implicit none
	
	type(String) :: iFileName
	type(String) :: oFileName
	type(IFStream) :: ifile
	type(RNFunction) :: nFunc, oNFunc, nFunc2
	type(String) :: strBuffer
	type(CommandLineParser) :: parser
	
	character(5), allocatable :: tokens(:)
	integer, allocatable :: columns(:)
	integer :: nPoints
	integer :: i
	
	type(RNFunction) :: nFuncWindow
	type(NIntegrator) :: integrator
	real(8) :: t, sigma
	integer :: freq
	real(8) :: resolution
	logical :: isXRange
	real(8) :: xrange(2)
	
	integer :: n
	
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	! General parameters
	! -i
	! -o
	! -c
	! -n
	!-----------------------------------------------------------------
	iFileName = parser.getString( "-i" )
! 	oFileName = parser.getString( "-o" )
	
	strBuffer = parser.getString( "-c", def="1,2" )
	call strBuffer.split( tokens, "," )
	
	if( size(tokens) == 3 ) then
		allocate( columns(3) )
! 		columns = [ FString_toInteger(tokens(1)), FString_toInteger(tokens(2)), FString_toInteger(tokens(3)) ]
		write(*,*) "### ERROR ### Complex functions are not implemeneted yet"
		stop
	else if( size(tokens) == 2 ) then
		allocate( columns(2) )
		columns = [ FString_toInteger(tokens(1)), FString_toInteger(tokens(2)) ]
	else
		write(*,*) "### ERROR ### Bad value for parameter -c"
		stop
	end if
	
	nPoints = parser.getInteger( "-n", def=-1 )
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	! Window parameters
	! -m
	!-----------------------------------------------------------------
	sigma = parser.getReal( "-sigma", def=0.1_8 )
	freq = parser.getInteger( "-freq", def=1 )
	resolution = parser.getReal( "-resol", def=1.0_8 )
	
	isXRange = .false.
	strBuffer = parser.getString( "-x", def="" )
	call strBuffer.split( tokens, "," )
	if( size(tokens) == 2 ) then
		isXRange = .true.
		xrange = [ FString_toReal(tokens(1)), FString_toReal(tokens(2)) ]
	end if
	!----------------------------------------------------------------------	
	
	call ifile.init( iFileName.fstr )
	call nFunc.fromFStream( ifile, columns=columns )
	call ifile.close()
	
	if( nPoints == -1 ) nPoints = nFunc.nPoints()
	
! 	oNFunc.init(  )
	call integrator.init( nFunc2, NIntegrator_SIMPSON )
	
	n = 1
	t = nFunc.xGrid.min!+3.0_8*sigma
	do while( t <= nFunc.xGrid.max )!-3.0_8*sigma )
! 		write(*,"(A,F20.6,A)", advance="no") "Generating ", t, " ... "
		
		call nFuncWindow.fromFunction( nFunc.xGrid, window )
		
		nFunc2 = nFuncWindow*nFunc
		
		write(*,*) t, integrator.evaluate()
		
		t = t + freq*nFunc.xGrid.stepSize
		n = n + 1
	end do

	deallocate( columns )
	
	contains
	
	function window( tprime ) result( output )
		real(8) :: tprime
		real(8) :: output
		
		output = exp(-(tprime-t)**2/2.0_8/sigma**2)/sigma/sqrt(2.0_8*Math_PI)
	end function window
end program main

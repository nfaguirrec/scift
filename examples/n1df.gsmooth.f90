!>
!! @brief Test program
!!
program main
	use GOptions_
	use Math_
	use IOStream_
	use String_
	use CommandLineParser_
	use Grid_
	use RNFunction_
	use NIntegrator_
	implicit none
	
	type(String) :: iFileName
	type(String) :: oFileName
	real(8) :: sigma
	
	real(8) :: t, ta, tb
	
	type(Grid) :: tgrid
	type(IFStream) :: ifile
	type(RNFunction) :: nFunc, nFunc2, nFuncWindow, oFunc
	type(NIntegrator) :: integrator
	type(String) :: strBuffer
	type(CommandLineParser) :: parser
	
	character(5), allocatable :: tokens(:)
	integer :: columns(2)
	integer :: i
	
	iFileName = parser.getString( "-i" )
	oFileName = parser.getString( "-o" )
	sigma = parser.getReal( "-s" )
	
	strBuffer = parser.getString( "-c", def="1,2" )
	call strBuffer.split( tokens, "," )
	columns = [ FString_toInteger(tokens(1)), FString_toInteger(tokens(2)) ]
	
	call ifile.init( iFileName.fstr )
	call nFunc.fromFStream( ifile, columns=columns )
	call ifile.destroy()
	
	call nFuncWindow.init( nFunc.xGrid )
	
	call integrator.init( nFunc2, NIntegrator_SIMPSON )
! 	call integrator.init( nFunc, SIMPSON )
			
	
	write(*,*) "### ERROR ### Esto no funciona"
	call oFunc.init( nFunc.xGrid )
	do i=1,nFunc.nPoints()
		t = nFunc.x(i)
		write(*,*) "t = ", t
		
		ta = max( t-4.0_8*sigma, nFunc.x( 0 ) )
		tb = min( t+4.0_8*sigma, nFunc.x( nFunc.nPoints() ) )
		
		call nFunc2.init( ta, tb, stepSize=nFunc.xGrid.stepSize )
		call nFuncWindow.fromFunction( nFunc2.xGrid, window )
		
		write(*,*) ta, tb
		write(*,*) nFunc.xGrid.pos(ta), nFunc.xGrid.pos(tb)
		nFunc2.fArray = nFuncWindow.fArray*nFunc.fArray(nFunc.xGrid.pos(ta):nFunc.xGrid.pos(tb))
		
		call oFunc.set( i, integrator.evaluate( ta, tb ) )
	end do
	call oFunc.save( oFileName.fstr )
	
	contains
	
	function window( tprime ) result( output )
		real(8) :: tprime
		real(8) :: output
		
		output = exp(-(tprime-t)**2/2.0_8/sigma**2)/sigma/sqrt(2.0_8*Math_PI)
	end function window
	
end program main

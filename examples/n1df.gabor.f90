!>
!! @brief Test program
!!
!! E0=0.05; omega=0.20279; t0=500.0; w=800.0; dw=100.0; tMax=1000.0
!! echo "n=2**11; x0=0.0; h=$tMax/(n-1); do for [i=1:n] { x=x0+(i-1)*h; print sprintf( '%10.5f%10.5f', x, $E0*cos($omega*x)*erfhat(x,$t0,$w,$dw) ); }" | gnuplot 2> signalErfHat.dat
!! ./fft1D -i signalErfHat.dat > fftErfHat.dat
!! plot "signalErfHat.dat" w lp pt 7 ps 0.5 lw 2
!! plot [0:0.4] "fftErfHat.dat" w lp pt 7 ps 0.5 lw 2
!!
!! echo "n=2**11; x0=0.0; h=$tMax/(n-1); do for [i=1:n] { x=x0+(i-1)*h; print sprintf( '%10.5f%10.5f', x, $E0*cos($omega*x)*coshat(x,$t0,$w,$dw) ); }" | gnuplot 2> signalCosHat.dat
!! ./fft1D -i signalCosHat.dat > fftCosHat.dat
!! plot "signalCosHat.dat" w lp pt 7 ps 0.5 lw 2
!! plot [0:0.4] "fftCosHat.dat" w lp pt 7 ps 0.5 lw 2
!!
!! plot "signalErfHat.dat" w l lw 2, "signalCosHat.dat" w l lw 2 lt 3
!! set size square
!! plot [0:0.4] "fftErfHat.dat" w lp pt 7 ps 0.5 lw 2, "fftCosHat.dat" w lp pt 7 ps 0.5 lw 2 lt 3
!!
program main
	use GOptions_
	use Math_
	use IOStream_
	use String_
	use CommandLineParser_
	use RNFunction_
	use CNFunction_
	use FourierTransform_
	implicit none
	
	type(String) :: fileName
	type(IFStream) :: ifile
	type(CNFunction) :: nFunc, nFuncBuff, nFuncWindow
	type(RNFunction) :: spectrum
	type(String) :: strBuffer
	type(CommandLineParser) :: parser
	integer :: fftType
	
	character(5), allocatable :: tokens(:)
	integer, allocatable :: columns(:)
	integer :: nPoints
	
	real(8) :: t, sigma
	integer :: freq
	real(8) :: resolution
	logical :: isXRange
	real(8) :: xrange(2)
	
	!----------------------------------------------------------------------
	! Command line parameters
	!----------------------------------------------------------------------
	fileName = parser.getString( "-i" )
	
	strBuffer = parser.getString( "-t", def="FORWARD" )
	if( trim(strBuffer.fstr) == "FORWARD" ) then
		fftType = FourierTransform_FORWARD
	else if( trim(strBuffer.fstr) == "BACKWARD" ) then
		fftType = FourierTransform_BACKWARD
	else
		write(*,*) "### ERROR ### Bad value for parameter -t (FORWARD|BACKWARD)"
		stop
	end if
	
	strBuffer = parser.getString( "-c", def="1,2,3" )
	call strBuffer.split( tokens, "," )
	
	if( size(tokens) == 3 ) then
		allocate( columns(3) )
		columns = [ FString_toInteger(tokens(1)), FString_toInteger(tokens(2)), FString_toInteger(tokens(3)) ]
	else if( size(tokens) == 2 ) then
		allocate( columns(2) )
		columns = [ FString_toInteger(tokens(1)), FString_toInteger(tokens(2)) ]
	else
		write(*,*) "### ERROR ### Bad value for parameter -c"
		stop
	end if
	
	nPoints = parser.getInteger( "-n", def=-1 )
	
	call ifile.init( fileName.fstr )
	call nFunc.fromFStream( ifile, columns=columns )
	call ifile.destroy()
	deallocate( columns )
	
	if( nPoints == -1 ) nPoints = nFunc.nPoints()
	
	sigma = parser.getReal( "-s", def=0.1_8 )
	freq = parser.getInteger( "-f", def=1 )
	resolution = parser.getReal( "-r", def=1.0_8 )
	
	isXRange = .false.
	strBuffer = parser.getString( "-x", def="" )
	call strBuffer.split( tokens, "," )
	if( size(tokens) == 2 ) then
		isXRange = .true.
		xrange = [ FString_toReal(tokens(1)), FString_toReal(tokens(2)) ]
	end if
	!----------------------------------------------------------------------	
	
	t = nFunc.xGrid.min!+3.0_8*sigma
	do while( t <= nFunc.xGrid.max )!-3.0_8*sigma )
		call nFuncWindow.fromFunction( nFunc.xGrid, window )
		
		nFuncBuff = nFuncWindow*nFunc
		spectrum = FourierTransform_spectrum( nFuncBuff, sgn=fftType, ixrange=[1,nPoints] )
		
		write(*,*) "### Error ### Code is not available yet"
! 		if( isXRange ) then
! 			call spectrum.save( append=.true., xrange=xrange, beforeLine=FString_fromReal( t, "(F10.5)" ), resolution=resolution )
! 		else
! 			call spectrum.save( append=.true., beforeLine=FString_fromReal( t, "(F10.5)" ), resolution=resolution )
! 		end if
		
		t = t + freq*nFunc.xGrid.stepSize
	end do
	
	contains
	
	function window( tprime ) result( output )
		real(8) :: tprime
		complex(8) :: output
		
		output = exp(-(tprime-t)**2/2.0_8/sigma**2)/sigma/sqrt(2.0_8*Math_PI)
	end function window
end program main

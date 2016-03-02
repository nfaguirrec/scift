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
	use IOStream_
	use String_
	use CommandLineParser_
	use Spline_
	use RNFunction_
	use CNFunction_
	use FourierTransform_
	implicit none
	
	type(String) :: iFileName
	type(String) :: oFileName
	type(IFStream) :: ifile
	type(CNFunction) :: nFunc, FnFunc
	type(RNFunction) :: spectrum
	type(String) :: strBuffer
	type(CommandLineParser) :: parser
	integer :: fftSgn
	
	character(5), allocatable :: tokens(:)
	integer, allocatable :: columns(:)
	integer :: nPoints
	integer :: smoothFactor
	integer :: i
	
	type(String) :: typeOfSpectrum
	integer :: idTypeOfSpectrum
	
	type(String) :: typeOfMethod
	integer :: idTypeOfMethod
	
	type(String) :: typeWindow
	integer :: idTypeWindow
	real(8) :: paramWindow
	logical :: centeredWindow
	character(100) :: oFileWindow
	
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	! General parameters
	! -i
	! -o
	! -c
	! -n
	!-----------------------------------------------------------------
	iFileName = parser.getString( "-i" )
	oFileName = parser.getString( "-o" )
	
	strBuffer = parser.getString( "-s", def="FORWARD" )
	if( trim(strBuffer.fstr) == "FORWARD" ) then
		fftSgn = FourierTransform_FORWARD
	else if( trim(strBuffer.fstr) == "BACKWARD" ) then
		fftSgn = FourierTransform_BACKWARD
	else
		write(*,*) "### ERROR ### Bad value for parameter -s (FORWARD|BACKWARD)"
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
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	! Type of spectrum
	! -t
	!-----------------------------------------------------------------
	typeOfSpectrum = parser.getString( "-t", def="NORM" )

	select case( trim(typeOfSpectrum.fstr) )
		case( "NORM" )
			idTypeOfSpectrum = FourierTransform_NORM_SPECTRUM
		case( "REALPART" )
			idTypeOfSpectrum = FourierTransform_REALPART_SPECTRUM
		case( "IMAGPART" )
			idTypeOfSpectrum = FourierTransform_IMAGPART_SPECTRUM
		case( "PHASE" )
			idTypeOfSpectrum = FourierTransform_PHASE_SPECTRUM
		case( "POWER" )
			idTypeOfSpectrum = FourierTransform_POWER_SPECTRUM
		case( "FT" )
			idTypeOfSpectrum = -1
	end select
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	! Type of method
	! -m
	!-----------------------------------------------------------------
	typeOfMethod = parser.getString( "-m", def="FFT" )

	select case( trim(typeOfMethod.fstr) )
		case( "FFT" )
			idTypeOfMethod = FourierTransform_FFT_METHOD
		case( "NUMERICAL" )
			idTypeOfMethod = FourierTransform_NUMERICAL_METHOD
	end select
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	! Window parameters
	!-----------------------------------------------------------------
	typeWindow = parser.getString( "-tw", def="NONE" )
	
	select case( trim(typeWindow.fstr) )
		case( "NONE" )
			idTypeWindow = FourierTransform_WINDOW_NONE
			paramWindow = parser.getReal( "-pw", def=-1.0_8 )
		case( "COS" )
			idTypeWindow = FourierTransform_WINDOW_COS
			paramWindow = parser.getReal( "-pw", def=2.0_8 )
		case( "GAUSS" )
			idTypeWindow = FourierTransform_WINDOW_GAUSS
			paramWindow = parser.getReal( "-pw", def=4.0_8 )
		case( "ERF_TOPHAT" )
			idTypeWindow = FourierTransform_WINDOW_ERF_TOPHAT
			paramWindow = parser.getReal( "-pw", def=0.2_8 )
		case( "FLATTOP" )
			idTypeWindow = FourierTransform_WINDOW_FLATTOP
			paramWindow = parser.getReal( "-pw", def=0.0_8 )
	end select
	
	centeredWindow = parser.getLogical( "-cw", def=.false. )
	strBuffer = parser.getString( "-ow", def=FString_NULL )
	oFileWindow = strBuffer.fstr
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	
	call ifile.init( iFileName.fstr )
	call nFunc.fromFStream( ifile, columns=columns )
	call ifile.close()
	
	if( idTypeOfSpectrum /= -1 ) then
		
		if( nPoints == -1 ) nPoints = nFunc.nPoints()
		
		spectrum = FourierTransform_spectrum( nFunc, sgn=fftSgn, type=idTypeOfSpectrum, method=idTypeOfMethod, &
							window=FourierTransform_Window( idTypeWindow, paramWindow, centeredWindow, trim(oFileWindow) ) )
							
		call spectrum.save( oFileName.fstr )
	else
	
		FnFunc = FourierTransform_fft( nFunc, sgn=fftSgn )
		call FnFunc.save( oFileName.fstr )
		
	end if
	
	deallocate( columns )
	
end program main

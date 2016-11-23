!>
!! @brief Test program
!!
program main
	use GOptions_
	use IOStream_
	use String_
	use CommandLineParser_
	use CNFunction_
	use FourierTransform_
	implicit none
	
	type(String) :: iFileName
	type(String) :: oFileName
	type(IFStream) :: ifile
	type(CNFunction) :: nFunc, FnFunc
	type(String) :: strBuffer
	type(CommandLineParser) :: parser
	
	character(5), allocatable :: tokens(:)
	integer, allocatable :: columns(:)
	
	type(String) :: filterName
	real(8) :: frequencyCutoff, frequencyCutoffL, frequencyCutoffH
	
! 	type(String) :: typeOfMethod
! 	integer :: idTypeOfMethod
	
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	! General parameters
	! -i
	! -o
	! -c
	! -t
	!-----------------------------------------------------------------
	iFileName = parser.getString( "-i" )
	oFileName = parser.getString( "-o" )
	
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
	
	filterName = parser.getString( "-t", def="LOW_PASS" ) ! HIGH_PASS, BAND_PASS
	
	select case( trim(filterName.fstr) )
		case( "LOW_PASS" )
			frequencyCutoff = parser.getReal( "-f" )
		case( "HIGH_PASS" )
			frequencyCutoff = parser.getReal( "-f" )
		case( "BAND_PASS" )
			frequencyCutoffL = parser.getReal( "-fL" )
			frequencyCutoffH = parser.getReal( "-fH" )
		case( "BAND_STOP" )
			frequencyCutoffL = parser.getReal( "-fL" )
			frequencyCutoffH = parser.getReal( "-fH" )
		case default
			write(*,*) "### ERROR ### Bad value for parameter -t ( LOW_PASS | HIGH_PASS | BAND_PASS | BAND_STOP )"
			stop
	end select
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	
! 	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 	! Type of method
! 	! -m
! 	!-----------------------------------------------------------------
! 	typeOfMethod = parser.getString( "-m", def="FFT" )
! 
! 	select case( trim(typeOfMethod.fstr) )
! 		case( "FFT" )
! 			idTypeOfMethod = FourierTransform_FFT_METHOD
! 		case( "NUMERICAL" )
! 			idTypeOfMethod = FourierTransform_NUMERICAL_METHOD
! 	end select
! 	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	
	call ifile.init( iFileName.fstr )
	call nFunc.fromFStream( ifile, columns=columns )
	call ifile.close()
	
	FnFunc = FourierTransform_fft( nFunc, sgn=FourierTransform_FORWARD )
	
	select case( trim(filterName.fstr) )
		case( "LOW_PASS" )
			FnFunc.fArray = merge( FnFunc.fArray, FnFunc.fArray*0.0_8, FnFunc.xGrid.data > frequencyCutoff )
		case( "HIGH_PASS" )
			FnFunc.fArray = merge( FnFunc.fArray, FnFunc.fArray*0.0_8, FnFunc.xGrid.data < frequencyCutoff )
		case( "BAND_PASS" )
			FnFunc.fArray = merge( FnFunc.fArray, FnFunc.fArray*0.0_8, FnFunc.xGrid.data > frequencyCutoffL .and. FnFunc.xGrid.data < frequencyCutoffH )
		case( "BAND_STOP" )
			FnFunc.fArray = merge( FnFunc.fArray, FnFunc.fArray*0.0_8, FnFunc.xGrid.data < frequencyCutoffL .or. FnFunc.xGrid.data > frequencyCutoffH )
	end select
	
	nFunc = FourierTransform_fft( FnFunc, sgn=FourierTransform_FORWARD )
	
	call nFunc.save( oFileName.fstr )
	
	deallocate( columns )
	
end program main

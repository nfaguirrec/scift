!>
!! @brief Test program
!!
program main
	use GOptions_
	use IOStream_
	use String_
	use CommandLineParser_
	use Spline_
	use RNFunction_
	use NPeakFinder_
	implicit none
	
	type(String) :: iFileName
	type(String) :: oFileName
	type(IFStream) :: iFile
	type(RNFunction) :: nFunc, peaks
	type(NPeakFinder) :: pFinder
	type(String) :: strBuffer
	type(CommandLineParser) :: parser
	integer :: method
	integer :: windowSize
	real(8) :: tolerance
	integer :: bandwidth
	logical :: test
	
	character(5), allocatable :: tokens(:)
	integer :: columns(2)
	
	method = parser.getInteger( "-m", def=0 )
	windowSize = parser.getInteger( "-w", def=10 )
	tolerance = parser.getReal( "-t", def=0.2_8 )
	bandwidth = parser.getInteger( "-bw", def=5 )
	test = parser.getLogical( "-test", def=.false. )
	
	if( test ) then
		nFunc = NPeakFinder_generateSignal( 0.0_8, 100.0_8, 0.1_8, 10, 0.1_8, 50.0_8, 0.1_8, 1.0_8 )
		call nFunc.save()
	else
		iFileName = parser.getString( "-i" )
		oFileName = parser.getString( "-o" )
		
		strBuffer = parser.getString( "-c", def="1,2" )
		call strBuffer.split( tokens, "," )
		columns = [ FString_toInteger(tokens(1)), FString_toInteger(tokens(2)) ]
		
		call iFile.init( iFileName.fstr )
		call nFunc.fromFStream( iFile, columns=columns )
		call iFile.destroy()
	end if
	
	call pFinder.init( nFunc, method, windowSize, tolerance, bandwidth )
	peaks = pFinder.execute()
	call peaks.save( trim(oFileName.fstr) )
end program main

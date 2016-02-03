!>
!! @brief Test program
!!
program main
	use IOStream_
	use String_
	use CommandLineParser_
	use Spline_
	use RNFunction_
	use NDerivator_
	implicit none
	
	type(String) :: iFileName
	type(String) :: oFileName
	type(IFStream) :: ifile
	type(RNFunction) :: nFunc
	type(RNFunction) :: nFuncSmooth
	type(RNFunction) :: dnFunc
	type(Spline) :: nFuncSpline
	type(NDerivator) :: derivator
	type(String) :: strBuffer
	type(CommandLineParser) :: parser
	
	character(5), allocatable :: tokens(:)
	integer :: columns(2)
	integer :: order
	integer :: nPoints
	integer :: smoothFactor
	integer :: i
	
	iFileName = parser.getString( "-i" )
	oFileName = parser.getString( "-o" )
	
	strBuffer = parser.getString( "-c", def="1,2" )
	call strBuffer.split( tokens, "," )
	columns = [ FString_toInteger(tokens(1)), FString_toInteger(tokens(2)) ]
	
	order = parser.getInteger( "-n", def=1 )
	nPoints = parser.getInteger( "-p", def=5 )
	smoothFactor = parser.getInteger( "-s", def=1 )
	
	call ifile.init( iFileName.fstr )
	call nFunc.fromFStream( ifile, columns=columns )
	call ifile.close()
	
	if( nFunc.xGrid.isEquallyspaced ) then
		call derivator.init( nFunc, nPoints )
		dnFunc = derivator.evaluate( order )
	else
		call nFuncSpline.init( nFunc )
		nFuncSmooth = nFuncSpline.smooth( smoothFactor )
		
		call derivator.init( nFuncSmooth, nPoints )
		dnFunc = derivator.evaluate( order )
	end if
	
	call dnFunc.save( oFileName.fstr )
	
! 	if( .not. nFunc.xGrid.isEquallyspaced ) then
! 		write(*,*) ""
! 		write(*,*) ""
! 		call nFuncSmooth.save( oFileName.fstr )
! 	end if
	
end program main

!>
!! @brief Test program
!!
program main
	use CommandLineParser_
	use String_
	use RNFunction_
	use CNFunction_
	use Spline_

	implicit none
	
	type(CommandLineParser) :: parser
	type(String) :: iFileName
	type(String) :: oFileName
	integer :: smoothFactor
	
	integer :: argc
	integer :: fileType
	type(RNFunction) :: riFunc, riFuncSmooth
	type(CNFunction) :: ciFunc, ciFuncSmooth
	type(Spline) :: spl
	
	argc = command_argument_count()
	
	if( argc < 6 ) then
		write(*,*) "Usage:"
		write(*,*) "   n1df.interp -i ifile -o ofile -n smoothFactor"
		stop
	end if
	
	iFileName = parser.getString( "-i" )
	oFileName = parser.getString( "-o" )
	smoothFactor = parser.getReal( "-n" )
	
	fileType = RNFunction_checkTypeN1DF( iFileName.fstr )
	
	if( fileType == 0 ) then
		call riFunc.init( iFileName.fstr )
		
		call spl.init( riFunc )
		riFuncSmooth = spl.smooth( smoothFactor )
		
		call riFuncSmooth.save( oFileName.fstr )
	else if( fileType == 1 ) then
		write(0,*) "### ERROR ### for complex functions it is not implemented yet"
		stop
! 		call ciFunc.init( iFileName.fstr )
! 		call cbFunc.init( bFileName.fstr )
! 		coFunc = ciFunc.interpolate( cbFunc.xGrid )
! 		call coFunc.save( oFileName.fstr )
	else
		write(0,*) "### ERROR ### unknown type for "//trim(iFileName.fstr)
		stop
	end if
	
end program main

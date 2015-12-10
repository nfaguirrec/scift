!>
!! @brief Test program
!!
program main
	use CommandLineParser_
	use String_
	use Spline_
	use RNFunction_
	use CNFunction_

	implicit none
	
	type(CommandLineParser) :: parser
	type(String) :: iFileName
	type(String) :: bFileName
	type(String) :: oFileName
	
	integer :: argc
	integer :: fileType
	type(RNFunction) :: riFunc, rbFunc, roFunc
	type(CNFunction) :: ciFunc, cbFunc, coFunc
	type(Spline) :: spl
	
	argc = command_argument_count()
	
	if( argc < 6 ) then
		write(*,*) "Usage:"
		write(*,*) "   n1df.interp -i ifile -b bfile -o ofile"
		stop
	end if
	
	iFileName = parser.getString( "-i" )
	bFileName = parser.getString( "-b" )
	oFileName = parser.getString( "-o" )
	
	fileType = RNFunction_checkTypeN1DF( iFileName.fstr )
	
	if( fileType == 0 ) then
		call riFunc.init( iFileName.fstr )
		
		if( .not. riFunc.xGrid.isEquallyspaced ) then
			!! Homogeniza el grid
			call spl.init( riFunc )
			riFunc = spl.smooth( 1 )
		end if
		
		call rbFunc.init( bFileName.fstr )
		roFunc = riFunc.interpolate( rbFunc.xGrid )
		call roFunc.save( oFileName.fstr )
	else if( fileType == 1 ) then
		call ciFunc.init( iFileName.fstr )
		call cbFunc.init( bFileName.fstr )
		coFunc = ciFunc.interpolate( cbFunc.xGrid )
		call coFunc.save( oFileName.fstr )
	else
		write(0,*) "### ERROR ### unknown type for "//trim(iFileName.fstr)
		stop
	end if
	
end program main

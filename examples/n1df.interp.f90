!>
!! @brief Test program
!!
program main
	use CommandLineParser_
	use String_
	use Spline_
	use Grid_
	use RNFunction_
	use CNFunction_

	implicit none
	
	type(CommandLineParser) :: parser
	type(String) :: iFileName
	type(String) :: bFileName
	type(String) :: oFileName
	real(8) :: value
	integer :: stencil
	
	integer :: argc
	integer :: fileType
	type(RNFunction) :: riFunc, roFunc
	type(CNFunction) :: ciFunc, coFunc
	type(Grid) :: xGrid
	type(Spline) :: spl
	
	argc = command_argument_count()
	
	if( argc < 6 ) then
		write(*,*) "Usage:"
		write(*,*) "   n1df.interp -i ifile -b bfile -o ofile [-v value] [-s stencil]"
		stop
	end if
	
	iFileName = parser.getString( "-i" )
	bFileName = parser.getString( "-b" )
	oFileName = parser.getString( "-o" )
	value = parser.getReal( "-v", def=0.0_8 )
	stencil = parser.getInteger( "-s", def=3 )
	
	fileType = RNFunction_checkTypeN1DF( iFileName.fstr )
	
	if( fileType == 0 ) then
		call riFunc.init( iFileName.fstr )
		
		if( .not. riFunc.xGrid.isEquallyspaced ) then
			!! Homogeniza el grid
			call spl.init( riFunc )
			riFunc = spl.smooth( 1 )
		end if
		
		call xGrid.init( bFileName.fstr, column=1 )
		roFunc = riFunc.interpolate( xGrid, stencil=stencil, value=value )
		call roFunc.save( oFileName.fstr )
	else if( fileType == 1 ) then
		call ciFunc.init( iFileName.fstr )
		call xGrid.init( bFileName.fstr, column=1 )
		coFunc = ciFunc.interpolate( xGrid, stencil=stencil, value=dcmplx(value) )
		call coFunc.save( oFileName.fstr )
	else
		write(0,*) "### ERROR ### unknown type for "//trim(iFileName.fstr)
		stop
	end if
	
end program main

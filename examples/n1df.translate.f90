!>
!! @brief Test program
!!
program main
	use CommandLineParser_
	use String_
	use RNFunction_
	use CNFunction_

	implicit none
	
	type(CommandLineParser) :: parser
	type(String) :: iFileName
	type(String) :: oFileName
	real(8) :: dx
	real(8) :: dy
	
	integer :: argc
	integer :: fileType
	type(RNFunction) :: rFunc
	type(CNFunction) :: cFunc
	
	argc = command_argument_count()
	
	if( argc < 4 ) then
		write(*,*) "Usage:"
		write(*,*) "   n1df.translate -i ifile -o ofile -dx xstep -dy ystep"
		stop
	end if
	
	iFileName = parser.getString( "-i" )
	oFileName = parser.getString( "-o" )
	dx = parser.getReal( "-dx", def=0.0_8 )
	dy = parser.getReal( "-dy", def=0.0_8 )
	
	fileType = RNFunction_checkTypeN1DF( iFileName.fstr )
	
	if( fileType == 0 ) then
		call rFunc.init( iFileName.fstr )
		call rFunc.translate( dx=dx, dy=dy )
		call rFunc.save( oFileName.fstr )
	else if( fileType == 1 ) then
		call cFunc.init( iFileName.fstr )
		call cFunc.translate( dx=dx, dy=dy )
		call cFunc.save( oFileName.fstr )
	else
		write(0,*) "### ERROR ### unknown type for "//trim(iFileName.fstr)
		stop
	end if
	
end program main

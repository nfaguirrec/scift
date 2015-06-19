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
	integer :: addPoints
	integer :: dir
	
	integer :: argc
	integer :: fileType
	type(RNFunction) :: rFunc
	type(CNFunction) :: cFunc
	
	argc = command_argument_count()
	
	if( argc < 8 ) then
		write(*,*) "Usage:"
		write(*,*) "   n1df.resize -i ifile -o ofile -n newsize -d direction"
		stop
	end if
	
	iFileName = parser.getString( "-i" )
	oFileName = parser.getString( "-o" )
	addPoints = parser.getInteger( "-n" )
	dir = parser.getInteger( "-d" )
	
	fileType = RNFunction_checkTypeN1DF( iFileName.fstr )
	
	if( fileType == 0 ) then
		call rFunc.init( iFileName.fstr )
		call rFunc.resize( addPoints, dir )
		call rFunc.save( oFileName.fstr )
	else if( fileType == 1 ) then
		call cFunc.init( iFileName.fstr )
		call cFunc.resize( addPoints, dir )
		call cFunc.save( oFileName.fstr )
	else
		write(0,*) "### ERROR ### unknown type for "//trim(iFileName.fstr)
		stop
	end if
	
end program main

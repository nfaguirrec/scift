!>
!! @brief Test program
!!
program main
	use GOptions_
	use String_
	use CommandLineParser_
	use RNFunction_
	use CNFunction_
	implicit none
	
	type(String) :: iFileName
	type(String) :: oFileName
	type(RNFunction) :: rNFunc
	type(CNFunction) :: cNFunc
	type(CommandLineParser) :: parser
	integer :: fileType
	logical :: describe
	
	iFileName = parser.getString( "-i" )
	
! 	write(0,"(A)",advance="no") "Reading file "//trim(iFileName.fstr)//" ... "
	
	fileType = CNFunction_checkTypeN1DF( iFileName.fstr )
	
	if( fileType == 0 ) then
		call rNFunc.init( iFileName.fstr )
	else if( fileType == 1 ) then
		call cNFunc.init( iFileName.fstr )
	else
		write(0,*) "### ERROR ### unknown format for "//trim(iFileName.fstr)
		stop
	end if
		
! 	write(0,"(A)") "OK"
	
	describe = parser.getLogical( "-s", def=.false. )
	if( describe ) then
		if( fileType == 0 ) then
			write(*,*) "min  = ", rNFunc.min()
			write(*,*) "max  = ", rNFunc.max()
			write(*,*) "step = ", rNFunc.stepSize()
		else if( fileType == 1 ) then
			write(*,*) "min  = ", cNFunc.min()
			write(*,*) "max  = ", cNFunc.max()
			write(*,*) "step = ", cNFunc.stepSize()
		else
			write(0,*) "### ERROR ### unknown format for "//trim(iFileName.fstr)
			stop
		end if
		
		stop
	end if
	
	oFileName = parser.getString( "-o" )
	
	if( fileType == 0 ) then
	
! 		write(*,"(A)",advance="no") "Saving real part of the function to "//trim(oFileName.fstr)//".rcube ... "
		call rNFunc.save( oFileName.fstr, format=AUTO_FORMAT )
! 		write(*,"(A)") "OK"
		
	else if( fileType == 1 ) then
	
		call cNFunc.save( oFileName.fstr, format=AUTO_FORMAT )
		
	end if
end program main

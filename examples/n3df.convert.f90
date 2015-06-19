!>
!! @brief Test program
!!
program main
	use String_
	use CommandLineParser_
	use RNFunction3D_
	use CNFunction3D_
	implicit none
	
	type(String) :: iFileName
	type(String) :: oFileName
	type(RNFunction3D) :: rNFunc
	type(CNFunction3D) :: cNFunc
	type(CommandLineParser) :: parser
	integer :: fileType
	logical :: describe
	
	iFileName = parser.getString( "-i" )
	
! 	write(0,"(A)",advance="no") "Reading file "//trim(iFileName.fstr)//" ... "
	
	fileType = cNFunc.checkTypeN3DF( iFileName.fstr )
	
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
	
	oFileName = FString_replace( iFileName.fstr, ".n3df", "" )
	
	if( fileType == 0 ) then
	
		write(*,"(A)",advance="no") "Saving real part of the function to "//trim(oFileName.fstr)//".rcube ... "
		call rNFunc.save( trim(oFileName.fstr)//".rcube" )
		write(*,"(A)") "OK"
		
		write(*,"(A)",advance="no") "Saving imaginary part of the function to "//trim(oFileName.fstr)//".icube ... "
		call rNFunc.save( trim(oFileName.fstr)//".icube" )
		write(*,"(A)") "OK"
		
		write(*,"(A)",advance="no") "Saving module of the function to "//trim(oFileName.fstr)//".cube ... "
		call rNFunc.save( trim(oFileName.fstr)//".cube" )
		write(*,"(A)") "OK"
		
	else if( fileType == 1 ) then
	
		write(*,"(A)",advance="no") "Saving real part of the function to "//trim(oFileName.fstr)//".rcube ... "
		call cNFunc.save( trim(oFileName.fstr)//".rcube" )
		write(*,"(A)") "OK"
		
		write(*,"(A)",advance="no") "Saving imaginary part of the function to "//trim(oFileName.fstr)//".icube ... "
		call cNFunc.save( trim(oFileName.fstr)//".icube" )
		write(*,"(A)") "OK"
		
		write(*,"(A)",advance="no") "Saving module of the function to "//trim(oFileName.fstr)//".cube ... "
		call cNFunc.save( trim(oFileName.fstr)//".cube" )
		write(*,"(A)") "OK"
		
	end if
end program main

!>
!! @brief Test program
!!
program main
	use RNFunction_
	use CNFunction_

	implicit none
	character(255) :: fileName
	integer :: argc
	
	integer :: fileType
	type(RNFunction) :: rFunc
	type(CNFunction) :: cFunc
	
	argc = command_argument_count()
	
	if( argc < 1 ) then
		write(*,*) "usage:"
		write(*,*) "   n1df.norm fileA"
		stop
	end if
	
	call get_command_argument( 1, fileName )
	
	fileType = RNFunction_checkTypeN1DF( fileName )
	
	if( fileType == 0 ) then
		call rFunc.init( fileName )
		write(*,*) rFunc.norm()
	else if( fileType == 1 ) then
		call cFunc.init( fileName )
		write(*,*) cFunc.norm()
	else
		write(0,*) "### ERROR ### unknown type for "//trim(fileName)
		stop
	end if
	
end program main

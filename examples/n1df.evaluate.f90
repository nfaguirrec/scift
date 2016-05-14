!>
!! @brief Test program
!!
program main
	use String_
	use RNFunction_
	use CNFunction_

	implicit none
	integer :: argc
	character(255) :: fileName
	character(255) :: xreal_str, ximag_str
	
	integer :: fileType
	type(RNFunction) :: rFunc
	type(CNFunction) :: cFunc
	real(8) :: rx
	complex(8) :: cx
	
	argc = command_argument_count()
	
	if( argc < 1 ) then
		write(*,*) "usage:"
		write(*,*) "   n1df.evaluate fileA ( real(x) | real(x) imag(x) )"
		stop
	end if
	
	call get_command_argument( 1, fileName )
	call get_command_argument( 2, xreal_str )
	call get_command_argument( 3, ximag_str )
	
	fileType = RNFunction_checkTypeN1DF( fileName )
	
	if( argc == 2 ) then
		rx = FString_toReal( xreal_str )
	else if( argc == 3 ) then
		cx = cmplx( FString_toReal( xreal_str ), FString_toReal( ximag_str ) )
	end if
	
	if( fileType == 0 ) then
		call rFunc.init( fileName )
		write(*,*) rFunc.interpolate( rx )
	else if( fileType == 1 ) then
		call cFunc.init( fileName )
		write(0,*) "### ERROR ### Interpolation for complex functions is not implemented yet "//trim(fileName)
		stop
! 		write(*,*) cFunc.interpolate( cx )
	else
		write(0,*) "### ERROR ### unknown type for "//trim(fileName)
		stop
	end if
	
end program main

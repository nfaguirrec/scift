!>
!! @brief Test program
!!
program main
	use RNFunction_
	use CNFunction_

	implicit none
	character(1000) :: formula
	character(255) :: iFileRef
	character(255) :: oFileName
	real(8) :: rInitValue
	complex(8) :: cInitValue
	integer :: argc
	character(255) :: buffer
	
	integer :: fileTypeA
	type(RNFunction) :: rFunc, rA
	type(CNFunction) :: cFunc, cA
	
	argc = command_argument_count()
	
	if( argc < 2 ) then
		write(*,*) "usage:"
		write(*,*) "   n1df.func iFileRef oFileName [initValue]"
		write(*,*) ""
		stop
	end if
	
	call get_command_argument( 1, iFileRef )
	call get_command_argument( 2, oFileName )
	
	fileTypeA = CNFunction_checkTypeN1DF( iFileRef )
	if( fileTypeA == 0 ) then
		call rA.init( iFileRef )
	else if( fileTypeA == 1 ) then
		call cA.init( iFileRef )
	else
		write(0,*) "### ERROR ### unknown format for "//trim(iFileRef)
		stop
	end if
	
	if( fileTypeA == 0 ) then
		
		rInitValue = 0.0_8
		select case( argc )
			case( 3 )
				call get_command_argument( 3, buffer )
				read(buffer,*) rInitValue
		end select
		
		call rFunc.init( rA.xGrid, value=rInitValue )
		
	else if( fileTypeA == 1 ) then
		
		cInitValue = 0.0_8
		select case( argc )
			case( 3 )
				call get_command_argument( 3, buffer )
				read(buffer,*) cInitValue
		end select
		
		call cFunc.init( cA.xGrid, value=cInitValue )
		
	end if
	
	!---------------------------------------------
	! Saving AB
	!---------------------------------------------
	if( fileTypeA == 0 ) then
		call rFunc.save( oFileName )
	else if( fileTypeA == 1 ) then
		call cFunc.save( oFileName )
	end if
	
end program main

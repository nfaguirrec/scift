!>
!! @brief Test program
!!
program main
	use RNFunction3D_
	use CNFunction3D_

	implicit none
	character(255) :: fileNameA
	character(255) :: fileNameB
	character(255) :: fileNameAB
	character(1) :: oper
	real(8) :: prefactorA
	real(8) :: prefactorB
	integer :: argc
	character(255) :: buffer
	
	integer :: fileTypeA, fileTypeB
	type(RNFunction3D) :: rA, rB, rAB
	type(CNFunction3D) :: cA, cB, cAB
	
	argc = command_argument_count()
	
	if( argc < 2 ) then
		write(*,*) "usage:"
		write(*,*) "   n3df.oper fileA "//achar(39)//"oper"//achar(39)//" fileB fileAB [prefactA] [prefactB]"
		write(*,*) ""
		write(*,*) "Implemented operators (Element-by-element):"
		write(*,*) "   +   Addition"
		write(*,*) "   -   Subtraction"
		write(*,*) "   *   Multiplication"
		write(*,*) "   /   Division"
		stop
	end if
	
	call get_command_argument( 1, fileNameA )
	call get_command_argument( 2, oper )
	call get_command_argument( 3, fileNameB )
	call get_command_argument( 4, fileNameAB )
	
	prefactorA = 1.0_8
	prefactorB = 1.0_8
	
	select case( argc )
		case( 5 )
			call get_command_argument( 5, buffer )
			read(buffer,*) prefactorA
			prefactorB = 1.0_8
		case( 6 )
			call get_command_argument( 5, buffer )
			read(buffer,*) prefactorA
			call get_command_argument( 6, buffer )
			read(buffer,*) prefactorB
	end select
	
	!---------------------------------------------
	! Loading A
	!---------------------------------------------
	fileTypeA = cA.checkTypeN3DF( fileNameA )
	if( fileTypeA == 0 ) then
		call rA.init( fileNameA )
	else if( fileTypeA == 1 ) then
		call cA.init( fileNameA )
	else
		write(0,*) "### ERROR ### unknown format for "//trim(fileNameA)
		stop
	end if
	
	!---------------------------------------------
	! Loading B
	!---------------------------------------------
	fileTypeB = cA.checkTypeN3DF( fileNameB )
	if( fileTypeB == 0 ) then
		call rB.init( fileNameB )
	else if( fileTypeB == 1 ) then
		call cB.init( fileNameB )
	else
		write(0,*) "### ERROR ### unknown format for "//trim(fileNameB)
		stop
	end if
	
	!---------------------------------------------
	! Cheking
	!---------------------------------------------
	if( fileTypeA /= fileTypeB ) then
		write(*,*) "### ERROR ### Mixed operation (real-complex) are not implemented yet"
		stop
	end if
	
	if( fileTypeA == 0 .and. .not. rA.checkBox( rB ) ) then
		write(*,*) "### ERROR ### rFunctions have not the same box A != B"
		stop
	else if( fileTypeA == 1 .and. .not. cA.checkBox( cB ) ) then
		write(*,*) "### ERROR ### cFunctions have not the same box A != B"
		stop
	end if
	
	!---------------------------------------------
	! Operations
	!---------------------------------------------
	if( fileTypeA == 0 ) then
		rA = rA*prefactorA
		rB = rB*prefactorB
		
		select case( trim(oper) )
			case( '+' )
				rAB = rA + rB
			case( '-' )
				rAB = rA - rB
			case( '*' )
				rAB = rA * rB
			case( '/' )
				rAB = rA / rB
		end select

	else if( fileTypeA == 1 ) then
		cA = cA*prefactorA
		cB = cB*prefactorB
		
		select case( trim(oper) )
			case( '+' )
				cAB = cA + cB
			case( '-' )
				cAB = cA - cB
			case( '*' )
				cAB = cA * cB
			case( '/' )
				cAB = cA / cB
		end select
	end if
	
	!---------------------------------------------
	! Saving AB
	!---------------------------------------------
	if( fileTypeA == 0 ) then
		call rAB.save( fileNameAB )
	else if( fileTypeA == 1 ) then
		call cAB.save( fileNameAB )
	end if
	
end program main

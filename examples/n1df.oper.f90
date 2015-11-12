!>
!! @brief Test program
!!
program main
	use MathParser_
	use RNFunction_
	use CNFunction_

	implicit none
	character(255) :: fileNameA, fileNameB
	character(255) :: fileNameFuncAB
	character(1) :: oper
	real(8) :: prefactor
	integer :: argc
	character(255) :: buffer
	
	integer :: fileTypeA, fileTypeB
	type(RNFunction) :: rFuncA, rFuncB, rFuncAB
	type(CNFunction) :: cFuncA, cFuncB, CFuncAB
	
	argc = command_argument_count()
	
	if( argc < 4 ) then
		write(*,*) "usage:"
		write(*,*) "   n1df.oper fileA "//achar(39)//"oper"//achar(39)//" fileB ofilefAB [prefactor]"
		write(*,*) ""
		write(*,*) "   fAB = A .oper. a*B"
		stop
	end if
	
	call get_command_argument( 1, fileNameA )
	call get_command_argument( 2, oper )
	call get_command_argument( 3, fileNameB )
	call get_command_argument( 4, fileNameFuncAB )
	
	prefactor = 1.0_8
	select case( argc )
		case( 5 )
			call get_command_argument( 5, buffer )
			read(buffer,*) prefactor
	end select
	
	!---------------------------------------------
	! Loading A and B
	!---------------------------------------------
	fileTypeA = CNFunction_checkTypeN1DF( fileNameA )
	fileTypeB = CNFunction_checkTypeN1DF( fileNameB )
	
	if( fileTypeB == 0 .and. fileTypeA == 0 ) then
		call rFuncA.init( fileNameA )
		call rFuncB.init( fileNameB )
	else if( fileTypeA == 1 .and. fileTypeB == 0 ) then
		call cFuncA.init( fileNameA )
		call rFuncB.init( fileNameB )
		
		! @todo En el futuro esto debe estar internamente integrado en libscift. Convertir entre RNFunction y CNFunction, o cargar un CNFunction desde dos columnas
! 		call cFuncB.fromGridArray( cFuncB.xGrid, fArray=cmplx(rFuncB.fArray) )
		call cFuncB.init( rFuncB.xGrid )
		cFuncB.fArray = rFuncB.fArray
	else if( fileTypeA == 0 .and. fileTypeB == 1 ) then
		call rFuncA.init( fileNameA )
		call cFuncB.init( fileNameB )
		
! 		call cFuncA.fromGridArray( cFuncA.xGrid, fArray=rFuncA.fArray )
		call cFuncA.init( rFuncA.xGrid )
		cFuncA.fArray = rFuncA.fArray
	else if( fileTypeB == 1 .and. fileTypeA == 1 ) then
		call cFuncA.init( fileNameA )
		call cFuncB.init( fileNameB )
	else
		write(0,*) "### ERROR ### unknown format for "//trim(fileNameA)//" or "//trim(fileNameB)
		stop
	end if
	
	!---------------------------------------------
	! Operations
	!---------------------------------------------
	if( fileTypeB == 0 .and. fileTypeA == 0 ) then
		select case( trim(oper) )
			case( '+' )
				rFuncAB = rFuncA + rFuncB*prefactor
			case( '-' )
				rFuncAB = rFuncA - rFuncB*prefactor
			case( '*' )
				rFuncAB = rFuncA * rFuncB*prefactor
			case( '/' )
				rFuncAB = rFuncA / rFuncB*prefactor
		end select
	
	else if( ( fileTypeA == 1 .and. fileTypeB == 0 ) .or. &
			 ( fileTypeA == 0 .and. fileTypeB == 1 ) .or. &
			 ( fileTypeA == 1 .and. fileTypeB == 1 ) ) then
		select case( trim(oper) )
			case( '+' )
				cFuncAB = cFuncA + cFuncB*prefactor
			case( '-' )
				cFuncAB = cFuncA - cFuncB*prefactor
			case( '*' )
				cFuncAB = cFuncA * cFuncB*prefactor
			case( '/' )
				cFuncAB = cFuncA / cFuncB*prefactor
		end select
	end if
	
	!---------------------------------------------
	! Saving AB
	!---------------------------------------------
	if( fileTypeB == 0 .and. fileTypeA == 0 ) then
		
		call rFuncAB.save( fileNameFuncAB )
		
	else if( ( fileTypeA == 1 .and. fileTypeB == 0 ) .or. &
			 ( fileTypeA == 0 .and. fileTypeB == 1 ) .or. &
			 ( fileTypeA == 1 .and. fileTypeB == 1 ) ) then
		
		call cFuncAB.save( fileNameFuncAB )
	
	end if
	
end program main

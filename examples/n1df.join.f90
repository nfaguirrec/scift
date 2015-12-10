!>
!! @brief Test program
!!
program main
	use RNFunction_
	use CNFunction_

	implicit none
	character(255) :: fileNameA, fileNameB
	character(255) :: fileNameFuncAB
	character(1) :: oper
	real(8) :: prefactor
	integer :: argc
	character(255) :: buffer
	
	integer :: i, j, nFiles
	integer, allocatable :: fileType(:)
	integer :: gFileType
	type(RNFunction), allocatable :: rFunc(:)
	type(CNFunction), allocatable :: cFunc(:)
	type(RNFunction) :: rFuncOut
	type(CNFunction) :: cFuncOut
	
	argc = command_argument_count()
	
	if( argc < 2 ) then
		write(*,*) "usage:"
		write(*,*) "   n1df.oper fileA fileB [ fileC ... ]"
		stop
	end if
	
	nFiles = argc
	allocate( fileType(nFiles) )
	allocate( rFunc(nFiles) )
	allocate( cFunc(nFiles) )
	
	do i=1,nFiles
		call get_command_argument( i, buffer )
		fileType(i) = CNFunction_checkTypeN1DF( buffer )
	end do
	
	gFileType = min( sum( fileType ), 1 )
	
	if( gFileType == 0 ) then
		
		do i=1,nFiles
			call get_command_argument( i, buffer )
			call rFunc(i).init( buffer )
		end do
		
		call rFuncOut.fromGrid( rFunc(1).xGrid )
		
		do j=1,rFunc(1).nPoints()
			write(*,"(E20.8)",advance="no") rFunc(1).x(j)
			
			do i=1,nFiles
				write(*,"(E20.8)",advance="no") rFunc(i).at(j)
			end do
			
			write(*,"(A)") ""
		end do

	else if( gFileType == 1 ) then
		
		do i=1,nFiles
			call get_command_argument( i, buffer )
			call cFunc(i).init( buffer )
		end do
		
		call cFuncOut.fromGrid( cFunc(1).xGrid )
		
		do j=1,cFunc(1).nPoints()
			write(*,"(E20.8)",advance="no") cFunc(1).x(j)
			
			do i=1,nFiles
				write(*,"(E25.8)",advance="no") cFunc(i).at(j)
			end do
			
			write(*,"(A)") ""
		end do
		
	else
		write(0,*) "### ERROR ### unknown format"
		stop
	end if
	
! 	!---------------------------------------------
! 	! Operations
! 	!---------------------------------------------
! 	if( fileTypeB == 0 .and. fileTypeA == 0 ) then
! 		select case( trim(oper) )
! 			case( '+' )
! 				rFuncOut = rFunc + rFuncOut*prefactor
! 			case( '-' )
! 				rFuncOut = rFunc - rFuncOut*prefactor
! 			case( '*' )
! 				rFuncOut = rFunc * rFuncOut*prefactor
! 			case( '/' )
! 				rFuncOut = rFunc / rFuncOut*prefactor
! 		end select
! 	
! 	else if( ( fileTypeA == 1 .and. fileTypeB == 0 ) .or. &
! 			 ( fileTypeA == 0 .and. fileTypeB == 1 ) .or. &
! 			 ( fileTypeA == 1 .and. fileTypeB == 1 ) ) then
! 		select case( trim(oper) )
! 			case( '+' )
! 				cFuncB = cFunc + cFuncB*prefactor
! 			case( '-' )
! 				cFuncB = cFunc - cFuncB*prefactor
! 			case( '*' )
! 				cFuncB = cFunc * cFuncB*prefactor
! 			case( '/' )
! 				cFuncB = cFunc / cFuncB*prefactor
! 		end select
! 	end if
! 	
! 	!---------------------------------------------
! 	! Saving AB
! 	!---------------------------------------------
! 	if( fileTypeB == 0 .and. fileTypeA == 0 ) then
! 		
! 		call rFuncOut.save( fileNameFuncAB )
! 		
! 	else if( ( fileTypeA == 1 .and. fileTypeB == 0 ) .or. &
! 			 ( fileTypeA == 0 .and. fileTypeB == 1 ) .or. &
! 			 ( fileTypeA == 1 .and. fileTypeB == 1 ) ) then
! 		
! 		call cFuncB.save( fileNameFuncAB )
! 	
! 	end if
	
end program main

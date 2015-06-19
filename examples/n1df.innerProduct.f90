!>
!! @brief Test program
!!
program main
	use Grid_
	use RNFunction_
	use CNFunction_

	implicit none
	character(255) :: fileNameA
	character(255) :: fileNameB
	integer :: argc
	character(255) :: snorm
	logical :: norm
	
	integer :: fileTypeA, fileTypeB
	type(RNFunction) :: rFuncA, rFuncB
	type(CNFunction) :: cFuncA, cFuncB
	type(Grid) :: xGrid
	real(8) :: rOverlap
	complex(8) :: cOverlap
	
	argc = command_argument_count()
	
	if( argc < 2 ) then
		write(*,*) "usage:"
		write(*,*) "   n1df.innerProduct fileA fileB norm"
		stop
	end if
	
	call get_command_argument( 1, fileNameA )
	call get_command_argument( 2, fileNameB )
	call get_command_argument( 3, snorm )
	
	norm = .false.
	if( argc > 2 ) then
		read(snorm,*) norm
	end if
	
	fileTypeA = RNFunction_checkTypeN1DF( fileNameA )
	fileTypeB = RNFunction_checkTypeN1DF( fileNameB )
	
	if( fileTypeA == 0 .and. fileTypeB == 0 ) then
	
		call rFuncA.init( fileNameA )
		call rFuncB.init( fileNameB )
		
		rFuncB = rFuncB.interpolate( rFuncA.xGrid )
		
		rOverlap = rFuncA.innerProduct( rFuncB )
		
		write(*,*) rOverlap
		
	else if( fileTypeA == 1 .and. fileTypeB == 1 ) then

		call cFuncA.init( fileNameA )
		call cFuncB.init( fileNameB )
		
		cFuncB = cFuncB.interpolate( cFuncA.xGrid )
		
		cOverlap = cFuncA.innerProduct( cFuncB )
		
		if( norm ) then
			write(*,*) abs( cOverlap )
		else
			write(*,*) cOverlap
		end if

	else
		write(0,*) "### ERROR ### reading type of file "//trim(fileNameA)//" and "//trim(fileNameB)
		stop
	end if
	
end program main

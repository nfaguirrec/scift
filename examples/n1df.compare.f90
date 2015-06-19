!>
!! @brief Test program
!!
program main
	use GOptions_
	use RNFunction_
	use CNFunction_
	implicit none
	
	integer :: argc
	character(1000) :: fileNameA, fileNameB
	type(RNFunction) :: rNFuncA, rNFuncB
	type(CNFunction) :: cNFuncA, cNFuncB
	integer :: fileTypeA, fileTypeB
	real(8) :: diff
	
    argc = command_argument_count()
    
    if( argc < 2 ) then
            write(*,*) "usage:"
            write(*,*) "   n1df.compare fileA fileB"
            write(*,*) ""
            write(*,*) "   diff² = 1.0-|<fA|fB>|²/( |fA|² * |fB|² )"
            stop
    end if
    
    call get_command_argument( 1, fileNameA )
    call get_command_argument( 2, fileNameB )
    
    write(0,*) "### ERROR ### Command not implemented yet"//trim(fileNameB)

! 	fileTypeA = cNFunc.checkTypeN1DF( fileNameA )
! 	fileTypeB = cNFunc.checkTypeN1DF( fileNameB )
! 	
! 	if( fileTypeA == 0 ) then
! 		call rNFunc.init( fileNameA )
! 	else if( fileTypeA == 1 ) then
		call cNFuncA.init( fileNameA )
! 	else
! 		write(0,*) "### ERROR ### unknown format for "//trim(fileNameA)
! 		stop
! 	end if
! 	
! 	if( fileTypeB == 0 ) then
! 		call rNFunc.init( fileNameB )
! 	else if( fileTypeB == 1 ) then
		call cNFuncB.init( fileNameB )
! 	else
! 		write(0,*) "### ERROR ### unknown format for "//trim(fileNameB)
! 		stop
! 	end if
	
	diff = sqrt( 1.0_8-abs( cNFuncA.innerProduct( cNFuncB ) )**2/cNFuncA.norm()**2/cNFuncB.norm()**2 )
	write(*,*) diff
end program main

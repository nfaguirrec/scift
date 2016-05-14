!>
!! @brief Test program
!!
program main
	use String_
	use Molecule_
	implicit none
	
	character(1000) :: sBuffer
	type(String) :: iFileNameRef, iFileNameFrag
	type(Molecule) :: molRef, molFrag
	
	if( command_argument_count() < 2 ) then
		write(*,*) "usage: molecule.isFragmentOf fileFrag fileRef"
		stop
	end if
	
	call get_command_argument( 1, sBuffer )
	iFileNameFrag = sBuffer
	
	call get_command_argument( 2, sBuffer )
	iFileNameRef = sBuffer
	
	call molRef.init( iFileNameRef.fstr )
	call molFrag.init( iFileNameFrag.fstr )
	
	if( molFrag.isFragmentOf( molRef ) ) then
		write(*,"(A)") "OK"
	else
		write(*,"(A)") "Failed"
	end if
end program main

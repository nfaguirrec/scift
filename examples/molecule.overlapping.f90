!>
!! @brief Test program
!!
program main
	use String_
	use Molecule_
	implicit none
	
	character(1000) :: sBuffer
	type(String) :: iFileNameMol1, iFileNameMol2
	type(Molecule) :: mol1, mol2
	
	if( command_argument_count() < 2 ) then
		write(*,*) "usage: molecule.overlapping mol1 mol2"
		stop
	end if
	
	call get_command_argument( 1, sBuffer )
	iFileNameMol1 = sBuffer
	
	call get_command_argument( 2, sBuffer )
	iFileNameMol2 = sBuffer
	
	call mol1.init( iFileNameMol1.fstr )
	call mol2.init( iFileNameMol2.fstr )
	
	if( mol1.overlapping( mol2 ) ) then
		write(*,"(A)") "TRUE"
	else
		write(*,"(A)") "FALSE"
	end if
end program main

!>
!! @brief Test program
!!
program main
	use String_
	use Matrix_
	use Molecule_
	implicit none
	
	character(1000) :: sBuffer
	type(String) :: iFileName
	type(Molecule) :: mol
	integer :: charge
	
	if( command_argument_count() < 1 ) then
		write(*,*) "## ERROR ## Number of Parameter == 1"
		write(*,*) "Usage: molecule.minMult iFile [charge]"
		stop
	end if
	
	call get_command_argument( 1, sBuffer )
	iFileName = sBuffer
	
	if( command_argument_count() >= 2 ) then
		call get_command_argument( 2, sBuffer )
		charge = FString_toInteger( sBuffer )
	else
		charge = 0
	end if
	
	call mol.init( iFileName.fstr )
	
	write(*,"(I5)") mol.minSpinMultiplicity( charge )
end program main

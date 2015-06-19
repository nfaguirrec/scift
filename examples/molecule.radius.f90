!>
!! @brief Test program
!!
program main
	use UnitsConverter_
	use String_
	use Molecule_
	implicit none
	
	character(1000) :: sBuffer
	type(String) :: iFileName
	type(Molecule) :: mol
	
	if( command_argument_count() < 1 ) then
		write(*,*) "## ERROR ## Number of Parameter == 1"
		stop
	end if
	
	call get_command_argument( 1, sBuffer )
	iFileName = sBuffer
	
	call mol.init( iFileName.fstr )
	
	write(*,"(F20.5)") mol.radius()/angs
end program main

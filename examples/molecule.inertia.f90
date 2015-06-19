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
	type(Matrix) :: Im
	
	if( command_argument_count() < 1 ) then
		write(*,*) "## ERROR ## Number of Parameter == 1"
		stop
	end if
	
	call get_command_argument( 1, sBuffer )
	iFileName = sBuffer
	
	call mol.init( iFileName.fstr )
	call mol.orient()
	Im = mol.inertiaTensor()
	
	write(*,"(3F20.5)") Im.get(1,1), Im.get(2,2), Im.get(2,2)
end program main

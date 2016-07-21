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
	call mol.orient( debug=.false. )
	
	write(*,"(I20)") mol.fv()
end program main

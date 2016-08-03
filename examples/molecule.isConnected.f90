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
	real(8) :: alpha
	type(Molecule) :: mol
	
	if( command_argument_count() < 1 ) then
		write(*,*) "usage: molecule.isConnected geomFile [alpha]"
		write(*,*) "                                       1.0  "
		stop
	end if
	
	call get_command_argument( 1, sBuffer )
	iFileName = sBuffer
	
	alpha = 1.0_8
	if( command_argument_count() > 1 ) then
		call get_command_argument( 2, sBuffer )
		alpha = FString_toReal( sBuffer )
	end if
	
	call mol.init( iFileName.fstr )
	
	call mol.buildGraph( alpha=alpha )
	write(*,*) mol.molGraph.isConnected()
end program main

!>
!! @brief Test program
!!
program main
	use UnitsConverter_
	use String_
	use Matrix_
	use Molecule_
	implicit none
	
	character(1000) :: sBuffer
	type(String) :: iFileName1, iFileName2
	type(Molecule) :: mol1, mol2
	real(8) :: gamma
	type(Matrix) :: I1, I2, Idiff, Isum
	real(8) :: value
	
	real(8) :: R1, R2
	
	if( command_argument_count() < 2 ) then
		write(*,*) "usage: molecule.compare file1 file2 [ gamma ]"
		write(*,*) "                                       1.0   "
		stop
	end if
	
	call get_command_argument( 1, sBuffer )
	iFileName1 = sBuffer
	
	call get_command_argument( 2, sBuffer )
	iFileName2 = sBuffer
	
	gamma = 1.0_8
	if( command_argument_count() > 2 ) then
		call get_command_argument( 3, sBuffer )
		gamma = FString_toReal( sBuffer )
	end if
	
	call mol1.init( iFileName1.fstr )
	call mol2.init( iFileName2.fstr )
	
	write(*,"(A)", advance="no") "Chemical formula ... "
	if( mol1.compareFormula( mol2 ) ) then
		write(*,*) "OK"
	else
		write(*,*) "Failed"
	end if
	
	write(*,"(A)", advance="no") "Inertia moment ... "
	I1 = mol1.inertiaTensor()
	I2 = mol2.inertiaTensor()
	Idiff = I2-I1
	Isum = I2+I1
	
	value = 100.0*Idiff.trace()/Isum.trace()
	if( abs( value ) < gamma ) then
		write(*,*) "OK", abs(value)
	else
		write(*,*) "Failed", abs(value)
	end if
	
	write(*,"(A)", advance="no") "Radius ... "
	R1 = mol1.radius()
	R2 = mol2.radius()
	
	value = 100.0*( R2-R1 )/( R2+R1 )
	if( abs(value) < gamma ) then
		write(*,*) "OK", abs(value)
	else
		write(*,*) "Failed", abs(value)
	end if
end program main

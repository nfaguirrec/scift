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
	real(8) :: alpha
	type(Matrix) :: I1, I2, Idiff, Isum
	real(8) :: value
	
	real(8) :: R1, R2
! 	integer, allocatable :: conn1(:,:), conn2(:,:)
	
	if( command_argument_count() < 2 ) then
		write(*,*) "## ERROR ##"
	end if
	
	call get_command_argument( 1, sBuffer )
	iFileName1 = sBuffer
	
	call get_command_argument( 2, sBuffer )
	iFileName2 = sBuffer
	
	alpha = 1.0_8
	if( command_argument_count() > 2 ) then
		call get_command_argument( 3, sBuffer )
		alpha = FString_toReal( sBuffer )
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
	if( abs( value ) < 5.0_8 ) then
		write(*,*) "OK", value
	else
		write(*,*) "Failed", value
	end if
	
	write(*,"(A)", advance="no") "Radius ... "
	R1 = mol1.radius()
	R2 = mol2.radius()
	
	value = abs( R1-R2 )
	if( value < 0.5_8*angs ) then
		write(*,*) "OK", value
	else
		write(*,*) "Failed", value
	end if
	
! 	write(*,"(A)", advance="no") "Connectivity ... "
! 	
! 	if ( mol1.nAtoms() /= 1 .or. mol2.nAtoms() /= 1 ) then
! 		if( mol1.compareConnectivity( mol2, alpha ) ) then
! 			write(*,*) "OK"
! 		else
! 			write(*,*) "Failed"
! 		end if
! 	else
! 		write(*,*) "OK"
! 	end if
! 	
! 	call mol1.connectivity( conn1, alpha )
! 	call mol2.connectivity( conn2, alpha )
! 	
! 	write(*,"(A,<size(conn1,dim=1)>I5)") "conn1a = ", conn1(:,1)
! 	write(*,"(A,<size(conn1,dim=1)>I5)") "conn1b = ", conn1(:,2)
! 	write(*,"(A,<size(conn2,dim=1)>I5)") "conn2a = ", conn2(:,1)
! 	write(*,"(A,<size(conn2,dim=1)>I5)") "conn2b = ", conn2(:,2)
end program main

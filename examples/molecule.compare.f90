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
	type(Matrix) :: I1, I2, Idiff, Iaver
	real(8) :: value
	logical :: debug
	
	integer :: i
	real(8) :: averMass, linearR, maxI
	real(8) :: R1, R2
	real(8) :: Ivalue(3)
	
	if( command_argument_count() < 2 ) then
		write(*,*) "usage: molecule.compare file1 file2 [ gamma ] [ debug ]"
		write(*,*) "                                       1.0      false  "
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
	
	debug = .false.
	if( command_argument_count() > 3 ) then
		call get_command_argument( 4, sBuffer )
		debug = FString_toLogical( sBuffer )
	end if
	
	call mol1.init( iFileName1.fstr )
	call mol2.init( iFileName2.fstr )
	
	if( debug ) then
		write(*,*) "Formula1 = ", trim(mol1.chemicalFormula())
		write(*,*) "Formula2 = ", trim(mol2.chemicalFormula())
	end if
	
	write(*,"(A)", advance="no") "Chemical formula ... "
	if( mol1.compareFormula( mol2 ) ) then
		write(*,*) "OK"
	else
		write(*,*) "Failed"
	end if
	
	call mol1.orient()
	call mol2.orient()
	
	I1 = buildInertiaTensor( mol1 )
	I2 = buildInertiaTensor( mol2 )
	
! 	averMass = 0.0_8
	linearR = 0.0_8
	do i=1,mol1.nAtoms()
! 		averMass = averMass + mol1.atoms(i).mass()
		linearR = linearR + mol1.atoms(i).radius()
	end do
! 	averMass = averMass/real(mol1.nAtoms(),8)
	averMass = 1.0_8
	maxI = averMass*linearR/2.0_8
	
	Idiff = I2-I1
	Iaver = (I2+I1)/2.0_8
	
	if( debug ) then
		write(*,*) "Inertia Tensor 1 = "
		call I1.show( formatted = .true. )
		write(*,*) "Inertia Tensor 2 = "
		call I2.show( formatted = .true. )
		write(*,*) "Idiff = "
		call Idiff.show( formatted = .true. )
		write(*,*) "Iaver = "
		call Iaver.show( formatted = .true. )
		write(*,"(A,3F20.5)") "100*|Idiff|/Iaver", 100.0_8*abs( Idiff.get(1,1) )/Iaver.get(1,1), 100.0_8*abs( Idiff.get(2,2) )/Iaver.get(2,2), 100.0_8*abs( Idiff.get(3,3) )/Iaver.get(3,3)
	end if
	
	write(*,"(A)", advance="no") "Inertia moment ... "
	
	Ivalue = 0.0_8
	do i=3,4-mol1.fv(), -1
		Ivalue(i) = 100.0_8*abs( Idiff.get(i,i) )/Iaver.get(i,i)
	end do
	
	if( mol1.fv() == mol2.fv() .and. Ivalue(1) < gamma .and. Ivalue(2) < gamma .and. Ivalue(3) < gamma ) then
		write(*,"(A,2I3,3F20.5)") "OK", mol1.fv(), mol2.fv(), Ivalue
	else
		write(*,"(A,2I3,3F20.5)") "Failed", mol1.fv(), mol2.fv(), Ivalue
	end if
	
	R1 = mol1.radius()
	R2 = mol2.radius()
	
	if( debug ) then
		write(*,*) "Radius1 = ", R1
		write(*,*) "Radius2 = ", R2
	end if
	
	write(*,"(A)", advance="no") "Radius ... "
	value = 100.0*( R2-R1 )/( R2+R1 )/2.0_8
	if( abs(value) < gamma ) then
		write(*,*) "OK", abs(value)
	else
		write(*,*) "Failed", abs(value)
	end if
	
	contains
	
	!>
	!! @brief builds the inertia tensor
	!!
	function buildInertiaTensor( mol ) result( Im )
		class(Molecule), intent(in) :: mol
		type(Matrix) :: Im
		
		integer :: i
		real(8) :: centerOfMass(3)
		real(8), allocatable :: X(:), Y(:), Z(:), m(:)
		type(Matrix) :: diagInertiaTensor, Vm
		type(Matrix) :: r, u
		
		
		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		! 1) Calculate the center of mass
		centerOfMass = 0.0_8
		do i=1,mol.nAtoms()
			centerOfMass = centerOfMass + 1.0_8*mol.atoms(i).r  ! mass(i) = 1.0
		end do
		centerOfMass = centerOfMass/real(mol.nAtoms(),8)  ! Total mass = nAtoms
		
		allocate( X(mol.nAtoms()) )
		allocate( Y(mol.nAtoms()) )
		allocate( Z(mol.nAtoms()) )
		allocate( m(mol.nAtoms()) )
		
		do i=1,mol.nAtoms()
			X(i) = mol.atoms(i).x
			Y(i) = mol.atoms(i).y
			Z(i) = mol.atoms(i).z
			m(i) = 1.0_8
		end do
		
		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		! 2) Chooses the origin as the center of mass
		X = X - centerOfMass(1)
		Y = Y - centerOfMass(2)
		Z = Z - centerOfMass(3)
		
		call Im.init(3,3)
		
		call Im.set( 1, 1,  sum( m*(Y**2+Z**2) ) )
		call Im.set( 1, 2, -sum( m*X*Y ) )
		call Im.set( 1, 3, -sum( m*X*Z ) )
		call Im.set( 2, 1, -sum( m*Y*X ) )
		call Im.set( 2, 2,  sum( m*(X**2+Z**2) ) )
		call Im.set( 2, 3, -sum( m*Y*Z ) )
		call Im.set( 3, 1, -sum( m*Z*X ) )
		call Im.set( 3, 2, -sum( m*Z*Y ) )
		call Im.set( 3, 3,  sum( m*(X**2+Y**2) ) )
		
		call Im.eigen( eVecs=Vm, eVals=diagInertiaTensor )
		
		Im = diagInertiaTensor
		
		deallocate( X )
		deallocate( Y )
		deallocate( Z )
		deallocate( m )
	end function buildInertiaTensor

end program main

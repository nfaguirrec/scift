!>
!! @brief Test program
!!
program main
	use String_
	use Matrix_
	use CommandLineParser_
	use Atom_
	use Molecule_
	implicit none
	
	type(String) :: iFileName
	type(CommandLineParser) :: parser
	character(1000) :: sBuffer
	type(String) :: strFormula
	character(10), allocatable :: tokens(:)
	character(10), allocatable :: tokens2(:)
	type(Atom) :: atom1
	type(Molecule) :: mol
	character(3) :: symb
	integer :: mult
	integer :: i, j, nAtoms
	real(8) :: gamma
	
	if( command_argument_count() < 1 ) then
		write(*,*) "Usage:"
		write(*,*) "   molecule.random -i xyzfile"
		write(*,*) "   molecule.random 3H,C,S"
		stop
	end if
	
	iFileName = parser.getString( "-i", def=FString_NULL )
	if( iFileName /= FString_NULL ) then
		call mol.init( iFileName.fstr )
	else
		call get_command_argument( 1, sBuffer )
		strFormula = sBuffer
		
		nAtoms = 0
		call strFormula.split( tokens, "," )
		do i=1,size(tokens)
			call FString_split( tokens(i), tokens2, "_" )
			
			symb = trim(tokens2(1))
			
			if( size(tokens2) > 1 ) then
				mult = FString_toInteger( tokens2(2) )
			else
				mult = 1
			end if
			
			do j=1,mult
				nAtoms = nAtoms + 1
			end do

			deallocate( tokens2 )
		end do
		deallocate( tokens )
		
		call mol.init( nAtoms, trim(strFormula.fstr)//" ( Random geometry )" )
		
		nAtoms = 1
		call strFormula.split( tokens, "," )
		do i=1,size(tokens)
			call FString_split( tokens(i), tokens2, "_" )
			
			symb = trim(tokens2(1))
			
			if( size(tokens2) > 1 ) then
				mult = FString_toInteger( tokens2(2) )
			else
				mult = 1
			end if
			
			do j=1,mult
				call atom1.init( symb )
				mol.atoms(nAtoms) = atom1
				
				nAtoms = nAtoms + 1
			end do

			deallocate( tokens2 )
		end do
		deallocate( tokens )
	end if
	
	gamma = 0.7_8
	if( mol.nAtoms() == 2 ) gamma = 2.0_8
	if( mol.nAtoms() == 3 ) gamma = 1.0_8
	
	call mol.randomGeometry( gamma=gamma )
	
	call mol.save()
end program main

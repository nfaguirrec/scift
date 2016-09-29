!>
!! @brief Test program
!!
program main
	use String_
	use Matrix_
	use Molecule_
	use IntegerGraph_
	implicit none
	
	character(1000) :: sBuffer
	type(String) :: iFileName
	real(8) :: alpha
	type(Molecule) :: mol
	type(Matrix) :: D, L, A
	
	if( command_argument_count() < 1 ) then
		write(*,*) "Usage: molecule.graph file [ alpha ] "
		write(*,*) "                              1.0    "
		stop
	end if
	
	call get_command_argument( 1, sBuffer )
	iFileName = sBuffer
	
	alpha = 1.0
	call get_command_argument( 2, sBuffer )
	if( len_trim(sBuffer) /= 0 ) alpha = FString_toReal(sBuffer)
	
	call mol.init( iFileName.fstr )
	call mol.buildGraph( alpha=alpha )
	
	call mol.molGraph.show( formatted=.true. )
	
	L = mol.molGraph.laplacianMatrix()
	write(*,*) "Laplacian Matrix = "
	call L.show( formatted=.true. )
	
	write(*,*) ""
	write(*,*) "Randic = ", mol.molGraph.randicIndex()
	write(*,*) "Wiener = ", mol.molGraph.wienerIndex()
	write(*,*) "InverseWiener = ", mol.molGraph.inverseWienerIndex()
	write(*,*) "Balaban = ", mol.molGraph.balabanIndex()
	write(*,*) "MolecularTopological = ", mol.molGraph.molecularTopologicalIndex()
	write(*,*) "Kirchhoff = ", mol.molGraph.kirchhoffIndex()
	write(*,*) "KirchhoffSum = ", mol.molGraph.kirchhoffSumIndex()
	write(*,*) "WienerSum = ", mol.molGraph.wienerSumIndex()
	write(*,*) "JOmega = ", mol.molGraph.JOmegaIndex()
	
! 	write(*,*) "Wiener = ", mol.molGraph.wienerIndex( distanceMatrix=D )
! 	write(*,*) "InverseWiener = ", mol.molGraph.inverseWienerIndex( distanceMatrix=D )
! 	write(*,*) "Balaban = ", mol.molGraph.balabanIndex( distanceMatrix=D )
! 	write(*,*) "MolecularTopological = ", mol.molGraph.molecularTopologicalIndex( adjacencyMatrix=A, distanceMatrix=D )
! 	write(*,*) "Kirchhoff = ", mol.molGraph.kirchhoffIndex( resistanceDistanceMatrix=Omega )
! 	write(*,*) "KirchhoffSum = ", mol.molGraph.kirchhoffSumIndex( distanceMatrix=D, resistanceDistanceMatrix=Omega )
! 	write(*,*) "WienerSum = ", mol.molGraph.wienerSumIndex( distanceMatrix=D, resistanceDistanceMatrix=Omega )
! 	write(*,*) "JOmega = ", mol.molGraph.JOmegaIndex( distanceMatrix=D, resistanceDistanceMatrix=Omega )

end program main

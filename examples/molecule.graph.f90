!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                                                                                   !!
!!  This file is part of SciFT project                                               !!
!!  Copyright (c) 2011-2016 Nestor F. Aguirre (nfaguirrec@gmail.com)                 !!
!!                                                                                   !!
!!  Redistribution and use in source and binary forms, with or without               !!
!!  modification, are permitted provided that the following conditions are met:      !!
!!                                                                                   !!
!!  1. Redistributions of source code must retain the above copyright notice, this   !!
!!     list of conditions and the following disclaimer.                              !!
!!  2. Redistributions in binary form must reproduce the above copyright notice,     !!
!!     this list of conditions and the following disclaimer in the documentation     !!
!!     and/or other materials provided with the distribution.                        !!
!!  3. Neither the name of the copyright holders nor the names of its contributors   !!
!!     may be used to endorse or promote products derived from this software         !!
!!     without specific prior written permission.                                    !!
!!                                                                                   !!
!!  The copyright holders provide no reassurances that the source code provided      !!
!!  does not infringe any patent, copyright, or any other intellectual property      !!
!!  rights of third parties.  The copyright holders disclaim any liability to any    !!
!!  recipient for claims brought against recipient by any third party for            !!
!!  infringement of that parties intellectual property rights.                       !!
!!                                                                                   !!
!!  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND  !!
!!  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED    !!
!!  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE           !!
!!  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR  !!
!!  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES   !!
!!  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;     !!
!!  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND      !!
!!  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT       !!
!!  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS    !!
!!  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                     !!
!!                                                                                   !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!>
!! @brief Test program
!!
program main
	use UnitsConverter_
	use GOptions_
	use String_
	use Matrix_
	use Molecule_
	use IntegerGraph_
	use Edge_
	implicit none
	
	character(1000) :: sBuffer
	type(String) :: iFileName
	real(8) :: alpha
	type(Molecule) :: mol
	type(Matrix) :: D, L, A
	character(100), allocatable :: nodesSuffixes(:)
	character(100), allocatable :: effNodesLabels(:)
	integer :: i
	type(String) :: oFileDOT
	type(Edge) :: edgeProp
	
	if( command_argument_count() < 1 ) then
		write(*,*) "Usage: molecule.graph file [ alpha ] [ofileDOT] [nodesSuffixes]"
		write(*,*) "                              1.0                              "
		stop
	end if
	
	call get_command_argument( 1, sBuffer )
	iFileName = sBuffer
	
	alpha = 1.0
	call get_command_argument( 2, sBuffer )
	if( len_trim(sBuffer) /= 0 ) alpha = FString_toReal(sBuffer)
	
	call get_command_argument( 3, sBuffer )
	if( len_trim(sBuffer) /= 0 ) oFileDOT = trim(sBuffer)
	
	call get_command_argument( 4, sBuffer )
	if( len_trim(sBuffer) /= 0 ) call FString_split( trim(sBuffer), nodesSuffixes, "," )
	
	call mol.init( iFileName.fstr )
	call mol.buildGraph( alpha=alpha )
	
! 	do i=1,mol.molGraph.nEdges()
! 		edgeProp = mol.molGraph.getEdgeProperties(i)
! ! 		edgeProp.weight = anint(edgeProp.weight*10.0_8**bondPrec)/10.0_8**bondPrec
! 		edgeProp.weight = 1.0_8
! 		call mol.molGraph.setEdgeProperties( i, edgeProp )
! 	end do
	
	if( allocated(nodesSuffixes) ) then
		call mol.showGraph( nodesSuffixes=nodesSuffixes )
	else
		call mol.showGraph()
	end if
	
! 	L = mol.molGraph.laplacianMatrix()
! 	write(*,*) "Laplacian Matrix = "
! 	call L.show( formatted=.true. )

! 	D = mol.molGraph.distanceMatrix()
! 	write(*,*) "Distance Matrix = "
! 	call D.show( formatted=.true. )
	
! 	write(*,*) "nComponents = ", mol.molGraph.nComponents()
! 	write(*,*) "Randic = ", mol.molGraph.randicIndex()
! 	write(*,*) "Wiener = ", mol.molGraph.wienerIndex()
! 	write(*,*) "InverseWiener = ", mol.molGraph.inverseWienerIndex()
! 	write(*,*) "Balaban = ", mol.molGraph.balabanIndex()
! 	write(*,*) "MolecularTopological = ", mol.molGraph.molecularTopologicalIndex()
! 	write(*,*) "Kirchhoff = ", mol.molGraph.kirchhoffIndex()
! 	write(*,*) "KirchhoffSum = ", mol.molGraph.kirchhoffSumIndex()
! 	write(*,*) "WienerSum = ", mol.molGraph.wienerSumIndex()
! 	write(*,*) "JOmega = ", mol.molGraph.JOmegaIndex()
	
! 	write(*,*) "Wiener = ", mol.molGraph.wienerIndex( distanceMatrix=D )
! 	write(*,*) "InverseWiener = ", mol.molGraph.inverseWienerIndex( distanceMatrix=D )
! 	write(*,*) "Balaban = ", mol.molGraph.balabanIndex( distanceMatrix=D )
! 	write(*,*) "MolecularTopological = ", mol.molGraph.molecularTopologicalIndex( adjacencyMatrix=A, distanceMatrix=D )
! 	write(*,*) "Kirchhoff = ", mol.molGraph.kirchhoffIndex( resistanceDistanceMatrix=Omega )
! 	write(*,*) "KirchhoffSum = ", mol.molGraph.kirchhoffSumIndex( distanceMatrix=D, resistanceDistanceMatrix=Omega )
! 	write(*,*) "WienerSum = ", mol.molGraph.wienerSumIndex( distanceMatrix=D, resistanceDistanceMatrix=Omega )
! 	write(*,*) "JOmega = ", mol.molGraph.JOmegaIndex( distanceMatrix=D, resistanceDistanceMatrix=Omega )
	
	if( allocated(nodesSuffixes) ) then
		if( .not. oFileDOT.isEmpty() ) then
			call mol.saveDOT( oFileDOT.fstr, nodesSuffixes=nodesSuffixes )
		else
			call mol.saveDOT( nodesSuffixes=nodesSuffixes )
		end if
	else
		if( .not. oFileDOT.isEmpty() ) then
			call mol.saveDOT( oFileDOT.fstr )
		else
			call mol.saveDOT()
		end if

	end if
	
	if( allocated(nodesSuffixes) ) deallocate( nodesSuffixes )
	
end program main

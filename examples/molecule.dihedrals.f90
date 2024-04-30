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
	use String_
	use Molecule_
	use IntegerGraph_
	use IntegerVector_
	use Edge_
	implicit none
	
	character(1000) :: sBuffer
	type(String) :: iFileName
	real(8) :: alpha
	type(Molecule) :: mol
	type(String) :: oFileDOT
	
	type(IntegerGraph) :: molGraph
	type(IntegerVector) :: iNeighborsA, iNeighborsB, iNeighborsC
	integer :: i, j, k, l
	integer :: hashKey
	type(IntegerVector) :: hashKeys
	
	if( command_argument_count() < 1 ) then
		write(*,*) "Usage: molecule.dihedrals file [ alpha ]"
		write(*,*) "                                  1.0   "
		stop
	end if
	
	call get_command_argument( 1, sBuffer )
	iFileName = sBuffer
	
	alpha = 1.0
	call get_command_argument( 2, sBuffer )
	if( len_trim(sBuffer) /= 0 ) alpha = FString_toReal(sBuffer)
	
	call get_command_argument( 3, sBuffer )
	if( len_trim(sBuffer) /= 0 ) oFileDOT = trim(sBuffer)
	
	mol = Molecule( iFileName.fstr )
	call mol.buildGraph( alpha=alpha )
	
	molGraph = mol.molGraph
	call hashKeys.init()
	
	do i=1,molGraph.nNodes()
		iNeighborsA = molGraph.neighbors(i)
		
		do j=1,molGraph.nNodes()
			if( iNeighborsA.contains(j) ) then
				iNeighborsB = molGraph.neighbors(j)
				
				do k=1,molGraph.nNodes()
					if( iNeighborsB.contains(k) .and. k/=i ) then
						iNeighborsC = molGraph.neighbors(k)
				
						do l=1,molGraph.nNodes()
							if( iNeighborsC.contains(l) .and. l/=i .and. l/=j ) then
							
								hashKey = i*molGraph.nNodes()**0 + j*molGraph.nNodes()**1 + k*molGraph.nNodes()**2 + l*molGraph.nNodes()**3
								
								if( .not. hashKeys.contains(hashKey) ) then
									
									write(*,"(A,4I5,F10.2)") trim(mol.atoms(i).symbol)//"--"//trim(mol.atoms(j).symbol)//"--"//trim(mol.atoms(k).symbol)//"--"//trim(mol.atoms(l).symbol), &
																i, j, k, l, mol.dihedral( mol.atoms(i), mol.atoms(j), mol.atoms(k), mol.atoms(l) )/deg
									
									call hashKeys.append( hashKey )
									
									hashKey = l*molGraph.nNodes()**0 + k*molGraph.nNodes()**1 + j*molGraph.nNodes()**2 + i*molGraph.nNodes()**3
									call hashKeys.append( hashKey )
								end if
								
							end if
						end do
						
					end if
				end do
				
			end if
		end do
		
	end do
	
end program main

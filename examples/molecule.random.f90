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
	real(8) :: alpha
	
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
	
	alpha = 0.7_8
	if( mol.nAtoms() == 2 ) alpha = 2.0_8
	if( mol.nAtoms() == 3 ) alpha = 1.0_8
	
	call mol.randomGeometry( overlappingRadius=0.6_8, alpha=alpha )
	
	call mol.save()
end program main

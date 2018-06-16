!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                                                                                   !!
!!  This file is part of SciFT project                                               !!
!!  Copyright (c) 2016-2016 Nestor F. Aguirre (nfaguirrec@gmail.com)                 !!
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
	use GOptions_
	use UnitsConverter_
	use String_
	use Molecule_
	implicit none
	
	character(1000) :: sBuffer
	type(String) :: iFileName
	type(Molecule) :: mol
	real(8) :: radius
	integer :: method
	real(8) :: overlapping
	real(8) :: alpha
	real(8) :: symthr
	
	if( command_argument_count() < 1 ) then
		write(*,"(A)") "Usage: molecule.distort file [ radius ] [ overlapping ] [ alpha ] [ symthr ] [ FIXED_DISTORSION | RANDOM_DISTORSION ]"
		write(*,"(A)") "                                0.1           0.15         1.1       0.92               RANDOM_DISTORSION"
		stop
	end if
	
	call get_command_argument( 1, sBuffer )
	iFileName = sBuffer
	
	radius = 0.1_8*angs
	call get_command_argument( 2, sBuffer )
	if( len_trim(sBuffer) /= 0 ) radius = FString_toReal(sBuffer)*angs
	
	overlapping = 0.15_8*angs
	call get_command_argument( 3, sBuffer )
	if( len_trim(sBuffer) /= 0 ) overlapping = FString_toReal(sBuffer)*angs
	
	alpha = 1.1_8
	call get_command_argument( 4, sBuffer )
	if( len_trim(sBuffer) /= 0 ) alpha = FString_toReal(sBuffer)
	
	symthr = 0.92_8
	call get_command_argument( 5, sBuffer )
	if( len_trim(sBuffer) /= 0 ) symthr = FString_toReal(sBuffer)
	
	method = Molecule_RANDOM_DISTORSION
	call get_command_argument( 6, sBuffer )
	if( len_trim(sBuffer) /= 0 ) then
		select case( trim(adjustl(sBuffer)) )
			case("FIXED_DISTORSION")
				method = Molecule_FIXED_DISTORSION
			case("RANDOM_DISTORSION")
				method = Molecule_RANDOM_DISTORSION
			case default
				write(*,"(A)") "### ERROR ### molecule.distort"
				write(*,"(A)") "              Method = "//trim(adjustl(sBuffer))//" is not implemented ( use FIXED_DISTORSION | RANDOM_DISTORSION )"
		end select
	end if
	
	call mol.init( iFileName.fstr )
	call mol.distort( radius=radius, method=method, overlappingRadius=overlapping, useMassWeight=.true., alpha=alpha, thr=symthr )
	call mol.save()
end program main

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
	use AtomicElementsDB_
	use String_
	use Matrix_
	use Molecule_
	implicit none
	
	character(1000) :: sBuffer
	type(String) :: iFileName1, iFileName2
	type(Molecule) :: mol1, mol2
	real(8) :: thr, alpha
	type(Matrix) :: I1, I2, Idiff, Iaver
	real(8) :: value
	logical :: debug
	logical :: useMassWeight
	logical :: useIm
	logical :: useNodeWeights
	logical :: useEdgeWeights
	
	integer :: i
	real(8) :: averMass, linearR, maxI
	real(8) :: R1, R2
	real(8) :: Ivalue(3)
	
	if( command_argument_count() < 2 ) then
		write(*,"(A)") "Usage: molecule.compare file1 file2 [ debug ] [ thr ] [alpha] [useMassWeight] [useIm] [useNodeWeights] [useEdgeWeights]"
		write(*,"(A)") "                                      false     0.92    1.1        true        true        true              true      "
		stop
	end if
	
	call get_command_argument( 1, sBuffer )
	iFileName1 = sBuffer
	
	call get_command_argument( 2, sBuffer )
	iFileName2 = sBuffer
	
	debug = .false.
	call get_command_argument( 3, sBuffer )
	if( len_trim(sBuffer) /= 0 ) debug = FString_toLogical( sBuffer )
	
	thr = 0.92_8
	call get_command_argument( 4, sBuffer )
	if( len_trim(sBuffer) /= 0 ) thr = FString_toReal(sBuffer)
	
	alpha = 1.1_8
	call get_command_argument( 5, sBuffer )
	if( len_trim(sBuffer) /= 0 ) alpha = FString_toReal(sBuffer)
	
	useMassWeight = .true.
	call get_command_argument( 6, sBuffer )
	if( len_trim(sBuffer) /= 0 ) useMassWeight = FString_toLogical(sBuffer)
	
	useIm = .true.
	call get_command_argument( 7, sBuffer )
	if( len_trim(sBuffer) /= 0 ) useIm = FString_toLogical(sBuffer)
	
	useNodeWeights = .true.
	call get_command_argument( 8, sBuffer )
	if( len_trim(sBuffer) /= 0 ) useNodeWeights = FString_toLogical(sBuffer)
	
	useEdgeWeights = .true.
	call get_command_argument( 9, sBuffer )
	if( len_trim(sBuffer) /= 0 ) useEdgeWeights = FString_toLogical(sBuffer)
	
	write(*,"(A,F10.3)") "Similarity threshold = ", thr
	write(*,"(A,F10.3)") "Bond tolerance scale factor = ", alpha
	write(*,"(A,L)")     "Use Mass Weights = ", useMassWeight
	write(*,"(A,L)")     "Use Im = ", useIm
	write(*,"(A,L)")     "Use Node Weights = ", useNodeWeights
	write(*,"(A,L)")     "Use Edge Weights = ", useEdgeWeights
	write(*,*) ""
	
	mol1 = Molecule( iFileName1.fstr )
	mol2 = Molecule( iFileName2.fstr )
	
	write(*,"(A)", advance="no") "Chemical formula ... "
	if( mol1.compareFormula( mol2, debug=debug ) ) then
		write(*,*) "OK"
	else
		write(*,*) "Failed"
	end if
	
	write(*,"(A)", advance="no") "Geometry ... "
	if( mol1.compareGeometry( mol2, useMassWeight=useMassWeight, useIm=useIm, thr=thr, debug=debug ) ) then
		write(*,*) "OK"
	else
		write(*,*) "Failed"
	end if
	
	write(*,"(A)", advance="no") "Connectivity ... "
	if( mol1.compareConnectivity( mol2, alpha=alpha, thr=thr, useNodeWeights=useNodeWeights, useEdgeWeights=useEdgeWeights, debug=debug ) ) then
		write(*,*) "OK"
	else
		write(*,*) "Failed"
	end if
	
end program main

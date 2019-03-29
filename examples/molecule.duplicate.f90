!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                                                                                   !!
!!  This file is part of SciFT project                                               !!
!!  Copyright (c) 2017-2017 Nestor F. Aguirre (nfaguirrec@gmail.com)                 !!
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
	use Matrix_
	use Molecule_
	use String_
	use IOStream_
	implicit none
	
	character(1000) :: sBuffer
	type(String) :: iFileName
	type(Molecule), allocatable :: molecules(:)
	real(8) :: thr, alpha
	logical :: remove
	logical :: debug
	logical :: useMassWeight
	logical :: useIm
	logical :: useNodeWeights
	logical :: useEdgeWeights
	
	character(100), allocatable :: cFormula(:)
	real(8), allocatable :: gDescriptors(:,:)
	real(8), allocatable :: cDescriptors(:,:)
	real(8) :: gSimilarity
	real(8) :: cSimilarity

	integer :: i, j
	type(IFStream) :: ifile
	type(String), allocatable :: fileNames(:)
	logical, allocatable :: removed(:)
	real(8), allocatable :: energies(:)
	integer :: equal
	character(1000), allocatable :: tokens(:)
	
	if( command_argument_count() < 1 ) then
		write(*,"(A)") "Usage: molecule.duplicate file [remove] [ debug ] [ thr ] [alpha] [useMassWeight] [useIm] [useNodeWeights] [useEdgeWeights]"
		write(*,"(A)") "                                false     false     0.92    1.1        true        true        true              true      "
		stop
	end if
	
	call get_command_argument( 1, sBuffer )
	iFileName = sBuffer
	
	remove = .false.
	call get_command_argument( 2, sBuffer )
	if( len_trim(sBuffer) /= 0 ) remove = FString_toLogical( sBuffer )
	
	debug = .false.
	call get_command_argument( 3, sBuffer )
	if( len_trim(sBuffer) /= 0 ) debug = FString_toLogical( sBuffer )
	
	thr = 0.90_8
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
	
	write(*,"(A,L)") "Remove files = ", remove
	write(*,"(A,F10.3)") "Similarity threshold = ", thr
	write(*,"(A,F10.3)") "Bond tolerance scale factor = ", alpha
	write(*,"(A,L)")     "Use Mass Weights = ", useMassWeight
	write(*,"(A,L)")     "Use Im = ", useIm
	write(*,"(A,L)")     "Use Node Weights = ", useNodeWeights
	write(*,"(A,L)")     "Use Edge Weights = ", useEdgeWeights
	write(*,*) ""
	
	call ifile.init( iFileName.fstr )
	
	allocate( molecules(ifile.numberOfLines) )
	allocate( fileNames(ifile.numberOfLines) )
	allocate( energies(ifile.numberOfLines) )
	allocate( removed(ifile.numberOfLines) )
	
	removed = .false.
	
	write(*,"(A)",advance="no")  "Loading molecules "
	
	do i=1,ifile.numberOfLines
		
		if( mod(i,int(ifile.numberOfLines/10.0_8)) == 0 ) then
			write(*,"(A)",advance="no") "."
		end if
		
		fileNames(i) = trim(ifile.readLine())
		call molecules(i).init( fileNames(i).fstr )
		call FString_split( molecules(i).name, tokens, " " )
		
		! @todo Cerificar que todas las moleculas tienen la energia definida. De lo contrario toca volver al esquema basico
		energies(i) = FString_toReal( trim(tokens(3)) )
		
	end do
	
	write(*,"(A)") " OK"
	
	allocate( cFormula(size(molecules)) )
	allocate( gDescriptors(15,size(molecules)) )
	allocate( cDescriptors( 9,size(molecules)) )
	
	write(*,"(A)",advance="no")  "Calculating descriptors "
	
	do i=1,size(molecules)
	
		if( mod(i,int(ifile.numberOfLines/10.0_8)) == 0 ) then
			write(*,"(A)",advance="no") "."
		end if
	
		cFormula(i) = molecules(i).chemicalFormula()
		gDescriptors(:,i) = molecules(i).ballesterDescriptors( useMassWeight=useMassWeight, useIm=useIm )
		cDescriptors(:,i) = molecules(i).connectivityDescriptors( alpha=alpha, useNodeWeights=useNodeWeights, useEdgeWeights=useEdgeWeights )
		
	end do
	
	write(*,"(A)") " OK"
	write(*,*) ""
	
	do i=1,size(molecules)-1
		if( .not. removed(i) ) then
			
			do j=i+1,size(molecules)
				
				if( .not. removed(j)  .and. abs( energies(i)-energies(j) ) <= 1.0_8*eV ) then
! 					write(*,"(A)") "      Comparing "//trim(fileNames(i).fstr)//" <-> "//trim(fileNames(j).fstr)//" >>>> dE="//adjustl(trim((FString_fromReal(abs( energies(i)-energies(j) ), "(F10.5)"))))
					
					gSimilarity = 1.0_8/( 1.0_8+abs(sum( gDescriptors(:,i)-gDescriptors(:,j) ))/real(size(gDescriptors(:,i)),8) )
					cSimilarity = 1.0_8/( 1.0_8+abs(sum( cDescriptors(:,i)-cDescriptors(:,j) ))/real(size(cDescriptors(:,i)),8) )
					
					if( debug ) then
						write(*,*) ""
						write(*,"(A,<size(gDescriptors)>F15.4)")  " gDescrip1 = ", gDescriptors(:,i)
						write(*,"(A,<size(gDescriptors)>F15.4)")  " gDescrip2 = ", gDescriptors(:,j)
						write(*,"(A,F10.5)")    "Similarity = ", gSimilarity
						write(*,"(A,F10.5)")    " Threshold = ", thr
						write(*,*)              "    Equal? = ", gSimilarity > thr
						write(*,*) ""
						write(*,"(A,<size(cDescriptors)>F15.4)")  " cDescrip1 = ", cDescriptors(:,i)
						write(*,"(A,<size(cDescriptors)>F15.4)")  " cDescrip2 = ", cDescriptors(:,j)
						write(*,"(A,F10.5)")    "Similarity = ", cSimilarity
						write(*,"(A,F10.5)")    " Threshold = ", thr
						write(*,*)              "    Equal? = ", cSimilarity > thr
						write(*,*) ""
					end if
					
					equal = 0
					if( trim(cFormula(i)) == trim(cFormula(j)) ) equal = equal + 1					
					if( gSimilarity > thr ) equal = equal + 1
					if( cSimilarity > thr ) equal = equal + 1
					
					if( equal == 3 ) then
						sBuffer = FString_fromReal(abs( energies(i)-energies(j) )/eV, "(F10.3)")
						
						write(*,"(A)",advance="no") "      "//trim(fileNames(i).fstr)//"  "//trim(fileNames(j).fstr)//" ("// &
													trim(adjustl(adjustr(trim(sBuffer))))// &
													") --> Equal "
						removed(j) = .true.
						
						if( remove ) then
							write(*,"(A)") "  Removed ("//trim(fileNames(j).fstr)//")"
							call system( "[ -f "//trim(fileNames(j).fstr)//" ] && rm "//trim(fileNames(j).fstr) )
						else
							write(*,"(A)") ""
						end if
					end if
				end if
				
			end do
			
		end if
	end do

	deallocate( cFormula )
	deallocate( gDescriptors )
	deallocate( cDescriptors )
	deallocate( molecules )
	deallocate( fileNames )
	deallocate( energies )
	deallocate( removed )
end program main

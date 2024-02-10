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
	use RNFunction_
	use CNFunction_

	implicit none
	character(255) :: fileNameA, fileNameB
	character(255) :: fileNameFuncAB
	character(1) :: oper
	real(8) :: prefactor
	integer :: argc
	character(255) :: buffer
	
	integer :: i, j, nFiles
	integer, allocatable :: fileType(:)
	integer :: gFileType
	type(RNFunction), allocatable :: rFunc(:)
	type(CNFunction), allocatable :: cFunc(:)
	type(RNFunction) :: rFuncOut
	type(CNFunction) :: cFuncOut
	
	argc = command_argument_count()
	
	if( argc < 2 ) then
		write(*,*) "usage:"
		write(*,*) "   n1df.oper fileA fileB [ fileC ... ]"
		stop
	end if
	
	nFiles = argc
	allocate( fileType(nFiles) )
	allocate( rFunc(nFiles) )
	allocate( cFunc(nFiles) )
	
	do i=1,nFiles
		call get_command_argument( i, buffer )
		fileType(i) = CNFunction_checkTypeN1DF( buffer )
	end do
	
	gFileType = min( sum( fileType ), 1 )
	
	if( gFileType == 0 ) then
		
		do i=1,nFiles
			call get_command_argument( i, buffer )
			rFunc(i) = RNFunction( buffer )
		end do
		
		rFuncOut = RNFunction( rFunc(1).xGrid )
		
		do j=1,rFunc(1).nPoints()
			write(*,"(E20.8)",advance="no") rFunc(1).x(j)
			
			do i=1,nFiles
				write(*,"(E20.8)",advance="no") rFunc(i).at(j)
			end do
			
			write(*,"(A)") ""
		end do

	else if( gFileType == 1 ) then
		
		do i=1,nFiles
			call get_command_argument( i, buffer )
			cFunc(i) = CNFunction( buffer )
		end do
		
		cFuncOut = CNFunction( cFunc(1).xGrid )
		
		do j=1,cFunc(1).nPoints()
			write(*,"(E20.8)",advance="no") cFunc(1).x(j)
			
			do i=1,nFiles
				write(*,"(E25.8)",advance="no") cFunc(i).at(j)
			end do
			
			write(*,"(A)") ""
		end do
		
	else
		write(0,*) "### ERROR ### unknown format"
		stop
	end if
	
! 	!---------------------------------------------
! 	! Operations
! 	!---------------------------------------------
! 	if( fileTypeB == 0 .and. fileTypeA == 0 ) then
! 		select case( trim(oper) )
! 			case( '+' )
! 				rFuncOut = rFunc + rFuncOut*prefactor
! 			case( '-' )
! 				rFuncOut = rFunc - rFuncOut*prefactor
! 			case( '*' )
! 				rFuncOut = rFunc * rFuncOut*prefactor
! 			case( '/' )
! 				rFuncOut = rFunc / rFuncOut*prefactor
! 		end select
! 	
! 	else if( ( fileTypeA == 1 .and. fileTypeB == 0 ) .or. &
! 			 ( fileTypeA == 0 .and. fileTypeB == 1 ) .or. &
! 			 ( fileTypeA == 1 .and. fileTypeB == 1 ) ) then
! 		select case( trim(oper) )
! 			case( '+' )
! 				cFuncB = cFunc + cFuncB*prefactor
! 			case( '-' )
! 				cFuncB = cFunc - cFuncB*prefactor
! 			case( '*' )
! 				cFuncB = cFunc * cFuncB*prefactor
! 			case( '/' )
! 				cFuncB = cFunc / cFuncB*prefactor
! 		end select
! 	end if
! 	
! 	!---------------------------------------------
! 	! Saving AB
! 	!---------------------------------------------
! 	if( fileTypeB == 0 .and. fileTypeA == 0 ) then
! 		
! 		call rFuncOut.save( fileNameFuncAB )
! 		
! 	else if( ( fileTypeA == 1 .and. fileTypeB == 0 ) .or. &
! 			 ( fileTypeA == 0 .and. fileTypeB == 1 ) .or. &
! 			 ( fileTypeA == 1 .and. fileTypeB == 1 ) ) then
! 		
! 		call cFuncB.save( fileNameFuncAB )
! 	
! 	end if
	
end program main

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
	use RNFunction3D_
	use CNFunction3D_

	implicit none
	character(255) :: fileNameA
	character(255) :: fileNameB
	character(255) :: fileNameAB
	character(1) :: oper
	real(8) :: prefactorA
	real(8) :: prefactorB
	integer :: argc
	character(255) :: buffer
	
	integer :: fileTypeA, fileTypeB
	type(RNFunction3D) :: rA, rB, rAB
	type(CNFunction3D) :: cA, cB, cAB
	
	argc = command_argument_count()
	
	if( argc < 2 ) then
		write(*,*) "usage:"
		write(*,*) "   n3df.oper fileA "//achar(39)//"oper"//achar(39)//" fileB fileAB [prefactA] [prefactB]"
		write(*,*) ""
		write(*,*) "Implemented operators (Element-by-element):"
		write(*,*) "   +   Addition"
		write(*,*) "   -   Subtraction"
		write(*,*) "   *   Multiplication"
		write(*,*) "   /   Division"
		stop
	end if
	
	call get_command_argument( 1, fileNameA )
	call get_command_argument( 2, oper )
	call get_command_argument( 3, fileNameB )
	call get_command_argument( 4, fileNameAB )
	
	prefactorA = 1.0_8
	prefactorB = 1.0_8
	
	select case( argc )
		case( 5 )
			call get_command_argument( 5, buffer )
			read(buffer,*) prefactorA
			prefactorB = 1.0_8
		case( 6 )
			call get_command_argument( 5, buffer )
			read(buffer,*) prefactorA
			call get_command_argument( 6, buffer )
			read(buffer,*) prefactorB
	end select
	
	!---------------------------------------------
	! Loading A
	!---------------------------------------------
	fileTypeA = cA.checkTypeN3DF( fileNameA )
	if( fileTypeA == 0 ) then
		call rA.init( fileNameA )
	else if( fileTypeA == 1 ) then
		call cA.init( fileNameA )
	else
		write(0,*) "### ERROR ### unknown format for "//trim(fileNameA)
		stop
	end if
	
	!---------------------------------------------
	! Loading B
	!---------------------------------------------
	fileTypeB = cA.checkTypeN3DF( fileNameB )
	if( fileTypeB == 0 ) then
		call rB.init( fileNameB )
	else if( fileTypeB == 1 ) then
		call cB.init( fileNameB )
	else
		write(0,*) "### ERROR ### unknown format for "//trim(fileNameB)
		stop
	end if
	
	!---------------------------------------------
	! Cheking
	!---------------------------------------------
	if( fileTypeA /= fileTypeB ) then
		write(*,*) "### ERROR ### Mixed operation (real-complex) are not implemented yet"
		stop
	end if
	
	if( fileTypeA == 0 .and. .not. rA.checkBox( rB ) ) then
		write(*,*) "### ERROR ### rFunctions have not the same box A != B"
		stop
	else if( fileTypeA == 1 .and. .not. cA.checkBox( cB ) ) then
		write(*,*) "### ERROR ### cFunctions have not the same box A != B"
		stop
	end if
	
	!---------------------------------------------
	! Operations
	!---------------------------------------------
	if( fileTypeA == 0 ) then
		rA = rA*prefactorA
		rB = rB*prefactorB
		
		select case( trim(oper) )
			case( '+' )
				rAB = rA + rB
			case( '-' )
				rAB = rA - rB
			case( '*' )
				rAB = rA * rB
			case( '/' )
				rAB = rA / rB
		end select

	else if( fileTypeA == 1 ) then
		cA = cA*prefactorA
		cB = cB*prefactorB
		
		select case( trim(oper) )
			case( '+' )
				cAB = cA + cB
			case( '-' )
				cAB = cA - cB
			case( '*' )
				cAB = cA * cB
			case( '/' )
				cAB = cA / cB
		end select
	end if
	
	!---------------------------------------------
	! Saving AB
	!---------------------------------------------
	if( fileTypeA == 0 ) then
		call rAB.save( fileNameAB )
	else if( fileTypeA == 1 ) then
		call cAB.save( fileNameAB )
	end if
	
end program main

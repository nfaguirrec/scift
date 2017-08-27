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
	use MathParser_
	use RNFunction_
	use CNFunction_

	implicit none
	character(1000) :: formulaOper
	character(255) :: fileNameA, fileNameB
	integer :: argc
	
	integer :: fileTypeA, fileTypeB
	type(RNFunction) :: rFuncA, rFuncOper, rFuncB
	type(CNFunction) :: cFuncA, cFuncOper, cFuncB
	type(MathParser) :: parser
	
	argc = command_argument_count()
	
	if( argc < 3 ) then
		write(*,*) "usage:"
		write(*,*) "   n1df.func fileA "//achar(39)//"oper"//achar(39)//" fileB"
		write(*,*) ""
		write(*,*) "   output = < A | oper(x) | B >"
		stop
	end if
	
	call get_command_argument( 1, fileNameA )
	call get_command_argument( 2, formulaOper )
	call get_command_argument( 3, fileNameB )
	
	!---------------------------------------------
	! Loading A
	!---------------------------------------------
	fileTypeA = CNFunction_checkTypeN1DF( fileNameA )
	if( fileTypeA == 0 ) then
		call rFuncA.init( fileNameA )
	else if( fileTypeA == 1 ) then
		call cFuncA.init( fileNameA )
	else
		write(0,*) "### ERROR ### unknown format for "//trim(fileNameA)
		stop
	end if
	
	!---------------------------------------------
	! Loading operator
	!---------------------------------------------
	call parser.init()
	
	if( fileTypeA == 0 ) then
		call parser.parseFunction( formulaOper, [ 'x' ]  )
		call rFuncOper.init( rFuncA.xGrid, evaluateFormulaR )
	else if( fileTypeA == 1 ) then
		call parser.parseFunction( formulaOper, [ 'x' ]  )
		call cFuncOper.init( cFuncA.xGrid, evaluateFormulaC )
	end if
	
	!---------------------------------------------
	! Loading B
	!---------------------------------------------
	fileTypeB = CNFunction_checkTypeN1DF( fileNameB )
	if( fileTypeB == 0 ) then
		call rFuncB.init( fileNameB )
	else if( fileTypeB == 1 ) then
		call cFuncB.init( fileNameB )
	else
		write(0,*) "### ERROR ### unknown format for "//trim(fileNameB)
		stop
	end if
	
	!---------------------------------------------
	! Operations
	!---------------------------------------------
	if( fileTypeA == 0 .and. fileTypeB == 0 ) then
		write(*,"(E20.8)") rFuncA.transitionElement( rFuncOper, rFuncB )
	else if( fileTypeA == 1 .and. fileTypeB == 1 ) then
		write(*,"(2E20.8)") cFuncA.transitionElement( cFuncOper, cFuncB )
	else
		write(0,*) "### ERROR ### Both "//trim(fileNameA)//" and "//trim(fileNameB)//" must be either real or complex. Mixing is not implemented yet."
		stop
	end if
	
	contains
	
	!>
	!!
	!!
	function evaluateFormulaR( x ) result( output )
		real(8), intent(in) :: x
		real(8) :: output
		
		output = parser.evaluateFunction( [ x ] )
	end function evaluateFormulaR
	
	!>
	!!
	!!
	function evaluateFormulaC( x ) result( output )
		real(8), intent(in) :: x
		complex(8) :: output
		
		output = cmplx( parser.evaluateFunction( [ x ] ), 0.0_8 )
	end function evaluateFormulaC

end program main

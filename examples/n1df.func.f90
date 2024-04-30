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
	character(1000) :: formula
	character(255) :: fileNameA
	character(255) :: fileNameFuncA
	character(1) :: oper
	real(8) :: prefactor
	integer :: argc
	character(255) :: buffer
	
	integer :: fileTypeA
	type(RNFunction) :: rFunc, rA, rFuncA
	type(CNFunction) :: cFunc, cA, cFuncA
	type(MathParser) :: parser
	
	argc = command_argument_count()
	
	if( argc < 4 ) then
		write(*,*) "usage:"
		write(*,*) "   n1df.func "//achar(39)//"f(x)"//achar(39)//" "//achar(39)//"oper"//achar(39)//" fileA fileFuncA [prefactor]"
		write(*,*) ""
		write(*,*) "   fA = f(x) .oper. a*A"
		stop
	end if
	
	call get_command_argument( 1, formula )
	call get_command_argument( 2, oper )
	call get_command_argument( 3, fileNameA )
	call get_command_argument( 4, fileNameFuncA )
	
	prefactor = 1.0_8
	select case( argc )
		case( 5 )
			call get_command_argument( 5, buffer )
			read(buffer,*) prefactor
	end select
	
	!---------------------------------------------
	! Loading A
	!---------------------------------------------
	fileTypeA = CNFunction_checkTypeN1DF( fileNameA )
	if( fileTypeA == 0 ) then
		rA = RNFunction( fileNameA )
	else if( fileTypeA == 1 ) then
		cA = CNFunction( fileNameA )
	else
		write(0,*) "### ERROR ### unknown format for "//trim(fileNameA)
		stop
	end if
	
	call parser.init()
	
	if( fileTypeA == 0 ) then
		call parser.parseFunction( formula, [ 'x' ]  )
		rFunc = RNFunction( rA.xGrid, evaluateFormulaR )
	else if( fileTypeA == 1 ) then
		call parser.parseFunction( formula, [ 'x' ]  )
		cFunc = CNFunction( cA.xGrid, evaluateFormulaC )
	end if
	
	!---------------------------------------------
	! Operations
	!---------------------------------------------
	if( fileTypeA == 0 ) then
		rA = rA*prefactor
		
		select case( trim(oper) )
			case( '+' )
				rFuncA = rFunc + rA
			case( '-' )
				rFuncA = rFunc - rA
			case( '*' )
				rFuncA = rFunc * rA
			case( '/' )
				rFuncA = rFunc / rA
		end select

	else if( fileTypeA == 1 ) then
		cA = cA*prefactor
		
		select case( trim(oper) )
			case( '+' )
				cFuncA = cFunc + cA
			case( '-' )
				cFuncA = cFunc - cA
			case( '*' )
				cFuncA = cFunc * cA
			case( '/' )
				cFuncA = cFunc / cA
		end select
	end if
	
	!---------------------------------------------
	! Saving AB
	!---------------------------------------------
	if( fileTypeA == 0 ) then
		call rFuncA.save( fileNameFuncA )
	else if( fileTypeA == 1 ) then
		call cFuncA.save( fileNameFuncA )
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

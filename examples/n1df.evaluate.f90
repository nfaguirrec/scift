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
	use RNFunction_
	use CNFunction_

	implicit none
	integer :: argc
	character(255) :: fileName
	character(255) :: xreal_str, ximag_str
	
	integer :: fileType
	type(RNFunction) :: rFunc
	type(CNFunction) :: cFunc
	real(8) :: rx
	complex(8) :: cx
	
	argc = command_argument_count()
	
	if( argc < 1 ) then
		write(*,*) "usage:"
		write(*,*) "   n1df.evaluate fileA ( real(x) | real(x) imag(x) )"
		stop
	end if
	
	call get_command_argument( 1, fileName )
	call get_command_argument( 2, xreal_str )
	call get_command_argument( 3, ximag_str )
	
	fileType = RNFunction_checkTypeN1DF( fileName )
	
	if( argc == 2 ) then
		rx = FString_toReal( xreal_str )
	else if( argc == 3 ) then
		cx = cmplx( FString_toReal( xreal_str ), FString_toReal( ximag_str ) )
	end if
	
	if( fileType == 0 ) then
		call rFunc.init( fileName )
		write(*,*) rFunc.interpolate( rx )
	else if( fileType == 1 ) then
		call cFunc.init( fileName )
		write(0,*) "### ERROR ### Interpolation for complex functions is not implemented yet "//trim(fileName)
		stop
! 		write(*,*) cFunc.interpolate( cx )
	else
		write(0,*) "### ERROR ### unknown type for "//trim(fileName)
		stop
	end if
	
end program main

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
	use GOptions_
	use RNFunction_
	use CNFunction_
	implicit none
	
	integer :: argc
	character(1000) :: fileNameA, fileNameB
	type(RNFunction) :: rNFuncA, rNFuncB
	type(CNFunction) :: cNFuncA, cNFuncB
	integer :: fileTypeA, fileTypeB
	real(8) :: diff
	
    argc = command_argument_count()
    
    if( argc < 2 ) then
            write(*,*) "usage:"
            write(*,*) "   n1df.compare fileA fileB"
            write(*,*) ""
            write(*,*) "   diff² = 1.0-|<fA|fB>|²/( |fA|² * |fB|² )"
            stop
    end if
    
    call get_command_argument( 1, fileNameA )
    call get_command_argument( 2, fileNameB )
    
    write(0,*) "### ERROR ### Command not implemented yet"//trim(fileNameB)

! 	fileTypeA = cNFunc.checkTypeN1DF( fileNameA )
! 	fileTypeB = cNFunc.checkTypeN1DF( fileNameB )
! 	
! 	if( fileTypeA == 0 ) then
! 		call rNFunc.init( fileNameA )
! 	else if( fileTypeA == 1 ) then
		cNFuncA = CNFunction( fileNameA )
! 	else
! 		write(0,*) "### ERROR ### unknown format for "//trim(fileNameA)
! 		stop
! 	end if
! 	
! 	if( fileTypeB == 0 ) then
! 		call rNFunc.init( fileNameB )
! 	else if( fileTypeB == 1 ) then
		cNFuncB = CNFunction( fileNameB )
! 	else
! 		write(0,*) "### ERROR ### unknown format for "//trim(fileNameB)
! 		stop
! 	end if
	
	diff = sqrt( 1.0_8-abs( cNFuncA.innerProduct( cNFuncB ) )**2/cNFuncA.norm()**2/cNFuncB.norm()**2 )
	write(*,*) diff
end program main

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
	use String_
	use CommandLineParser_
	use RNFunction_
	use CNFunction_

	implicit none
	type(CommandLineParser) :: parser
	type(String) :: iFileRef
	type(String) :: oFileName
	real(8) :: rInitValue
	complex(8) :: cInitValue
	
	integer :: fileTypeA
	type(RNFunction) :: rFunc, rA
	type(CNFunction) :: cFunc, cA
	
	type(String) :: strBuffer
	character(5), allocatable :: tokens(:)
	
	if( command_argument_count() < 4 ) then
		write(*,"(A)") "usage:"
		write(*,"(A)") "   n1df.init -o oFileName ( -b iFileRef | -xgrid xmin,xmax,nPoints ) [-init value]"
		write(*,"(A)") "                                                                             0.0  "
		write(*,"(A)") ""
		stop
	end if
	
	oFileName = parser.getString( "-o" )
	iFileRef = parser.getString( "-b", def=FString_NULL )
	
	if( iFileRef /= FString_NULL ) then
		fileTypeA = CNFunction_checkTypeN1DF( iFileRef.fstr )
		if( fileTypeA == 0 ) then
			call rA.init( iFileRef.fstr )
		else if( fileTypeA == 1 ) then
			call cA.init( iFileRef.fstr )
		else
			write(0,*) "### ERROR ### unknown format for "//trim(iFileRef.fstr)
			stop
		end if
		
		if( fileTypeA == 0 ) then
			
			rInitValue = parser.getReal( "-init", def=0.0_8 )
			call rFunc.init( rA.xGrid, value=rInitValue )
			
		else if( fileTypeA == 1 ) then
			
			CInitValue = parser.getReal( "-init", def=0.0_8 )
			call cFunc.init( cA.xGrid, value=cInitValue )
			
		end if
	else
		fileTypeA = 0
		
		strBuffer = parser.getString( "-xgrid" )
		call strBuffer.split( tokens, "," )
		
		rInitValue = parser.getReal( "-init", def=0.0_8 )
		call rFunc.init( FString_toReal(tokens(1)), FString_toReal(tokens(2)), nPoints=FString_toInteger(tokens(3)), value=rInitValue )
	end if
	
	!---------------------------------------------
	! Saving AB
	!---------------------------------------------
	if( fileTypeA == 0 ) then
		call rFunc.save( oFileName.fstr )
	else if( fileTypeA == 1 ) then
		call cFunc.save( oFileName.fstr )
	end if
	
	if( allocated(tokens) ) deallocate( tokens )
	
end program main

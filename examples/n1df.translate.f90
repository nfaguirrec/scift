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
	use CommandLineParser_
	use String_
	use RNFunction_
	use CNFunction_

	implicit none
	
	type(CommandLineParser) :: parser
	type(String) :: iFileName
	type(String) :: oFileName
	real(8) :: dx
	real(8) :: dy
	
	integer :: argc
	integer :: fileType
	type(RNFunction) :: rFunc
	type(CNFunction) :: cFunc
	
	argc = command_argument_count()
	
	if( argc < 4 ) then
		write(*,*) "Usage:"
		write(*,*) "   n1df.translate -i ifile -o ofile -dx xstep -dy ystep"
		stop
	end if
	
	iFileName = parser.getString( "-i" )
	oFileName = parser.getString( "-o" )
	dx = parser.getReal( "-dx", def=0.0_8 )
	dy = parser.getReal( "-dy", def=0.0_8 )
	
	fileType = RNFunction_checkTypeN1DF( iFileName.fstr )
	
	if( fileType == 0 ) then
		rFunc = RNFunction( iFileName.fstr )
		call rFunc.translate( dx=dx, dy=dy )
		call rFunc.save( oFileName.fstr )
	else if( fileType == 1 ) then
		cFunc = CNFunction( iFileName.fstr )
		call cFunc.translate( dx=dx, dy=dy )
		call cFunc.save( oFileName.fstr )
	else
		write(0,*) "### ERROR ### unknown type for "//trim(iFileName.fstr)
		stop
	end if
	
end program main

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
	use CommandLineParser_
	use RNFunction3D_
	use CNFunction3D_
	implicit none
	
	type(String) :: iFileName
	type(String) :: oFileName
	type(RNFunction3D) :: rNFunc
	type(CNFunction3D) :: cNFunc
	type(CommandLineParser) :: parser
	integer :: fileType
	logical :: describe
	
	iFileName = parser.getString( "-i" )
	
! 	write(0,"(A)",advance="no") "Reading file "//trim(iFileName.fstr)//" ... "
	
	fileType = cNFunc.checkTypeN3DF( iFileName.fstr )
	
	if( fileType == 0 ) then
		call rNFunc.init( iFileName.fstr )
	else if( fileType == 1 ) then
		call cNFunc.init( iFileName.fstr )
	else
		write(0,*) "### ERROR ### unknown format for "//trim(iFileName.fstr)
		stop
	end if
		
! 	write(0,"(A)") "OK"
	
	describe = parser.getLogical( "-s", def=.false. )
	if( describe ) then
		if( fileType == 0 ) then
			write(*,*) "min  = ", rNFunc.min()
			write(*,*) "max  = ", rNFunc.max()
			write(*,*) "step = ", rNFunc.stepSize()
		else if( fileType == 1 ) then
			write(*,*) "min  = ", cNFunc.min()
			write(*,*) "max  = ", cNFunc.max()
			write(*,*) "step = ", cNFunc.stepSize()
		else
			write(0,*) "### ERROR ### unknown format for "//trim(iFileName.fstr)
			stop
		end if
		
		stop
	end if
	
	oFileName = FString_replace( iFileName.fstr, ".n3df", "" )
	
	if( fileType == 0 ) then
	
		write(*,"(A)",advance="no") "Saving real part of the function to "//trim(oFileName.fstr)//".rcube ... "
		call rNFunc.save( trim(oFileName.fstr)//".rcube" )
		write(*,"(A)") "OK"
		
		write(*,"(A)",advance="no") "Saving imaginary part of the function to "//trim(oFileName.fstr)//".icube ... "
		call rNFunc.save( trim(oFileName.fstr)//".icube" )
		write(*,"(A)") "OK"
		
		write(*,"(A)",advance="no") "Saving module of the function to "//trim(oFileName.fstr)//".cube ... "
		call rNFunc.save( trim(oFileName.fstr)//".cube" )
		write(*,"(A)") "OK"
		
	else if( fileType == 1 ) then
	
		write(*,"(A)",advance="no") "Saving real part of the function to "//trim(oFileName.fstr)//".rcube ... "
		call cNFunc.save( trim(oFileName.fstr)//".rcube" )
		write(*,"(A)") "OK"
		
		write(*,"(A)",advance="no") "Saving imaginary part of the function to "//trim(oFileName.fstr)//".icube ... "
		call cNFunc.save( trim(oFileName.fstr)//".icube" )
		write(*,"(A)") "OK"
		
		write(*,"(A)",advance="no") "Saving module of the function to "//trim(oFileName.fstr)//".cube ... "
		call cNFunc.save( trim(oFileName.fstr)//".cube" )
		write(*,"(A)") "OK"
		
	end if
end program main

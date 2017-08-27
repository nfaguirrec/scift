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
	type(RNFunction3D) :: rNFunc
	type(CNFunction3D) :: cNFunc
	type(String) :: strBuffer
	type(CommandLineParser) :: parser
	
	character(5), allocatable :: tokens(:)
	real(8) :: xyz(3)
	real(8) :: point(2)
	integer :: i, dir, smooth
	real(8) :: minDir, maxDir, valDir, stepSizeDir
	complex(8) :: cValue, ssum
	real(8) :: rValue
	integer :: fileType
	logical :: describe
	integer :: argc
	
	argc = command_argument_count()
	
	if( argc < 2 ) then
		write(*,"(X,A)") "Usage:"
		write(*,"(X,A)") "   n3df.eval -i ifile [-s true/false] [-xyz xval,yval,zval] [-line xval,zval,dir] [-smooth smoothFactor]"
		write(*,"(X,A)") ""
		stop
	end if
	
	iFileName = parser.getString( "-i" )
	
! 	write(0,"(A)",advance="no") "Reading file "//trim(iFileName.fstr)//" ... "
	
! 	fileType = cNFunc.checkTypeN3DF( iFileName.fstr )
	fileType = 0
	
! 	if( fileType == 0 ) then
		call rNFunc.init( iFileName.fstr )
! 	else if( fileType == 1 ) then
! 		call cNFunc.init( iFileName.fstr )
! 	else
! 		write(0,*) "### ERROR ### unknown format for "//trim(iFileName.fstr)
! 		stop
! 	end if
		
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
	
! 	if( fileType == 0 ) then
! 		write(0,"(A)") "Type = REAL"
! 	else if( fileType == 1 ) then
! 		write(0,"(A)") "Type = COMPLEX"
! 	end if
	
! 	strBuffer = parser.getString( "-prop", def="@@NONE@@" )
! 	if( strBuffer.fstr == "@@NONE@@" ) then
! 		write(*,"(A40,3F15.5,A)") &
! 			"min = ( ", cNFunc.xyzGrid.min(1), cNFunc.xyzGrid.min(2), cNFunc.xyzGrid.min(3), " ) A"
! 		write(*,"(A40,3F15.5,A)") &
! 			"max = ( ", cNFunc.xyzGrid.max(1), cNFunc.xyzGrid.max(2), cNFunc.xyzGrid.max(3), " ) A"
! 		write(*,"(A40,3I15,A)") &
! 			"size = ( ", cNFunc.xyzGrid.size(1), cNFunc.xyzGrid.size(2), cNFunc.xyzGrid.size(3), " ) A"
! 		write(*,"(A40,3F15.5,A)") &
! 			"stepSize = ( ", cNFunc.xyzGrid.stepSize(1), cNFunc.xyzGrid.stepSize(2), cNFunc.xyzGrid.stepSize(3), " ) A"
! 		stop
! 	end if
	
	strBuffer = parser.getString( "-xyz", def="@@NONE@@" )
	if( strBuffer.fstr /= "@@NONE@@" ) then
! 		write(0,"(A)") "RUN = point interpolation"
		
		call strBuffer.split( tokens, "," )
		xyz = [ FString_toReal(tokens(1)), FString_toReal(tokens(2)), FString_toReal(tokens(3)) ]
		deallocate( tokens )
		
! 		write(0,"(A,3F10.5)") "POINT = ", xyz
		
		if( fileType == 0 ) then
			write(*,"(2E15.5)") rNFunc.interpolate( xyz(1), xyz(2), xyz(3) )
		else if( fileType == 1 ) then
			write(*,"(2E15.5)") cNFunc.interpolate( xyz(1), xyz(2), xyz(3) )
		end if
		
		stop
	end if
	
	strBuffer = parser.getString( "-line", def="@@NONE@@" )
	if( strBuffer /= "@@NONE@@" ) then
! 		write(0,"(A)") "RUN = line interpolation"
		
		call strBuffer.split( tokens, "," )
		point = [ FString_toReal(tokens(1)), FString_toReal(tokens(2)) ]
		dir = FString_toInteger(tokens(3))
		deallocate( tokens )
		
		smooth = parser.getInteger( "-smooth", def=1 )
		
! 		write(0,"(A,I5)") "SMOOTH = ", smooth
! 		write(0,"(A,2F10.5)") "LINE = ", point
! 		write(0,"(A,I5)") "DIRECTION = ", dir
		
		if( fileType == 0 ) then
			if( smooth /= 1 ) then
				minDir = rNFunc.xyzGrid.component(dir).at( 2 )
				maxDir = rNFunc.xyzGrid.component(dir).at( rNFunc.size(dir)-2 )
				stepSizeDir = rNFunc.xyzGrid.stepSize(dir)/real(smooth,8)
			else
				minDir = rNFunc.xyzGrid.component(dir).at( 1 )
				maxDir = rNFunc.xyzGrid.component(dir).at( rNFunc.size(dir) )
				stepSizeDir = rNFunc.xyzGrid.stepSize(dir)
			end if
		else if( fileType == 1 ) then
			if( smooth /= 1 ) then
				minDir = cNFunc.xyzGrid.component(dir).at( 2 )
				maxDir = cNFunc.xyzGrid.component(dir).at( cNFunc.size(dir)-2 )
				stepSizeDir = cNFunc.xyzGrid.stepSize(dir)/real(smooth,8)
			else
				minDir = cNFunc.xyzGrid.component(dir).at( 1 )
				maxDir = cNFunc.xyzGrid.component(dir).at( cNFunc.size(dir) )
				stepSizeDir = cNFunc.xyzGrid.stepSize(dir)
			end if
		end if
		
! 		write(0,"(A,2F10.5)") "RANGE = ", minDir, maxDir
! 		write(0,"(A,F10.5)") "STEPSIZE = ", stepSizeDir
		
		valDir = minDir
		ssum = 0.0_8
		do while( valDir <= maxDir )
			if( fileType == 0 ) then
				if( smooth /= 1 ) then
					rValue = rNFunc.interpolate( point(1), point(2), valDir )
				else
					rValue = rNFunc.evaluateXYZ( point(1), point(2), valDir )
				end if
				
				if( abs(rValue) > 1d-16 ) then
! 					write(*,"(2E15.5E3)") valDir, rValue
					write(*,"(2E15.5)") valDir, rValue
				else
					write(*,"(2E15.5)") valDir, 0.0_8 
				end if
				
				ssum = ssum  + rValue**2
			else if( fileType == 1 ) then
				if( smooth /= 1 ) then
					cValue = cNFunc.interpolate( point(1), point(2), valDir )
				else
					cValue = cNFunc.evaluateXYZ( point(1), point(2), valDir )
				end if
				
				if( abs(cValue) > 1d-16 ) then
! 					write(*,"(3E15.5E3)") valDir, cValue
					write(*,"(3E15.5)") valDir, cValue
				else
					write(*,"(3E15.5)") valDir, cmplx(0.0_8,0.0_8)
				end if
				
				ssum = ssum  + abs(cValue)**2
			end if
			
			valDir = valDir + stepSizeDir
		end do
		
		write(*,*) "# norm = ", 1.0_8/(stepSizeDir*ssum)
		
		stop
	end if
	
end program main

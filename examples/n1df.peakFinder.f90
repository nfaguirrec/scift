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
	use IOStream_
	use String_
	use CommandLineParser_
	use Spline_
	use RNFunction_
	use NPeakFinder_
	implicit none
	
	integer :: argc
	type(String) :: iFileName
	type(String) :: oFileName
	type(IFStream) :: iFile
	type(RNFunction) :: nFunc, peaks
	type(NPeakFinder) :: pFinder
	type(String) :: strBuffer
	type(CommandLineParser) :: parser
	integer :: method
	integer :: windowSize
	real(8) :: tolerance
	integer :: bandwidth
	logical :: test
	
	character(5), allocatable :: tokens(:)
	integer :: columns(2)
	
	argc = command_argument_count()
	
	if( argc < 4 ) then
			write(*,"(A)") "Usage:"
			write(*,"(A)") "   n1df.peakFinder -i ifile -o ofile [-c columns -m method -w windowSize -t tolerance -bw bandwidth]"
			write(*,"(A)") "                                             1,2         0            10          0.2             5 "
			stop
	end if
	
	method = parser.getInteger( "-m", def=0 )
	windowSize = parser.getInteger( "-w", def=10 )
	tolerance = parser.getReal( "-t", def=0.2_8 )
	bandwidth = parser.getInteger( "-bw", def=5 )
	test = parser.getLogical( "-test", def=.false. )
	
	if( test ) then
		nFunc = NPeakFinder_generateSignal( 0.0_8, 100.0_8, 0.1_8, 10, 0.1_8, 50.0_8, 0.1_8, 1.0_8 )
		call nFunc.save()
	else
		iFileName = parser.getString( "-i" )
		oFileName = parser.getString( "-o" )
		
		strBuffer = parser.getString( "-c", def="1,2" )
		call strBuffer.split( tokens, "," )
		columns = [ FString_toInteger(tokens(1)), FString_toInteger(tokens(2)) ]
		
		call iFile.init( iFileName.fstr )
		call nFunc.fromFStream( iFile, columns=columns )
		call iFile.close()
	end if
	
	call pFinder.init( nFunc, method, windowSize, tolerance, bandwidth )
	peaks = pFinder.execute()
	call peaks.save( trim(oFileName.fstr) )
end program main

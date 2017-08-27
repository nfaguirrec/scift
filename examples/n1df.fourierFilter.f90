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
	use CNFunction_
	use FourierTransform_
	implicit none
	
	type(String) :: iFileName
	type(String) :: oFileName
	type(IFStream) :: ifile
	type(CNFunction) :: nFunc, FnFunc
	type(String) :: strBuffer
	type(CommandLineParser) :: parser
	
	character(5), allocatable :: tokens(:)
	integer, allocatable :: columns(:)
	
	type(String) :: filterName
	real(8) :: frequencyCutoff, frequencyCutoffL, frequencyCutoffH
	
! 	type(String) :: typeOfMethod
! 	integer :: idTypeOfMethod
	
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	! General parameters
	! -i
	! -o
	! -c
	! -t
	!-----------------------------------------------------------------
	iFileName = parser.getString( "-i" )
	oFileName = parser.getString( "-o" )
	
	strBuffer = parser.getString( "-c", def="1,2,3" )
	call strBuffer.split( tokens, "," )
	
	if( size(tokens) == 3 ) then
		allocate( columns(3) )
		columns = [ FString_toInteger(tokens(1)), FString_toInteger(tokens(2)), FString_toInteger(tokens(3)) ]
	else if( size(tokens) == 2 ) then
		allocate( columns(2) )
		columns = [ FString_toInteger(tokens(1)), FString_toInteger(tokens(2)) ]
	else
		write(*,*) "### ERROR ### Bad value for parameter -c"
		stop
	end if
	
	filterName = parser.getString( "-t", def="LOW_PASS" ) ! HIGH_PASS, BAND_PASS
	
	select case( trim(filterName.fstr) )
		case( "LOW_PASS" )
			frequencyCutoff = parser.getReal( "-f" )
		case( "HIGH_PASS" )
			frequencyCutoff = parser.getReal( "-f" )
		case( "BAND_PASS" )
			frequencyCutoffL = parser.getReal( "-fL" )
			frequencyCutoffH = parser.getReal( "-fH" )
		case( "BAND_STOP" )
			frequencyCutoffL = parser.getReal( "-fL" )
			frequencyCutoffH = parser.getReal( "-fH" )
		case default
			write(*,*) "### ERROR ### Bad value for parameter -t ( LOW_PASS | HIGH_PASS | BAND_PASS | BAND_STOP )"
			stop
	end select
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	
! 	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 	! Type of method
! 	! -m
! 	!-----------------------------------------------------------------
! 	typeOfMethod = parser.getString( "-m", def="FFT" )
! 
! 	select case( trim(typeOfMethod.fstr) )
! 		case( "FFT" )
! 			idTypeOfMethod = FourierTransform_FFT_METHOD
! 		case( "NUMERICAL" )
! 			idTypeOfMethod = FourierTransform_NUMERICAL_METHOD
! 	end select
! 	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	
	call ifile.init( iFileName.fstr )
	call nFunc.fromFStream( ifile, columns=columns )
	call ifile.close()
	
	FnFunc = FourierTransform_fft( nFunc, sgn=FourierTransform_FORWARD )
	
	select case( trim(filterName.fstr) )
		case( "LOW_PASS" )
			FnFunc.fArray = merge( FnFunc.fArray, FnFunc.fArray*0.0_8, FnFunc.xGrid.data > frequencyCutoff )
		case( "HIGH_PASS" )
			FnFunc.fArray = merge( FnFunc.fArray, FnFunc.fArray*0.0_8, FnFunc.xGrid.data < frequencyCutoff )
		case( "BAND_PASS" )
			FnFunc.fArray = merge( FnFunc.fArray, FnFunc.fArray*0.0_8, FnFunc.xGrid.data > frequencyCutoffL .and. FnFunc.xGrid.data < frequencyCutoffH )
		case( "BAND_STOP" )
			FnFunc.fArray = merge( FnFunc.fArray, FnFunc.fArray*0.0_8, FnFunc.xGrid.data < frequencyCutoffL .or. FnFunc.xGrid.data > frequencyCutoffH )
	end select
	
	nFunc = FourierTransform_fft( FnFunc, sgn=FourierTransform_FORWARD )
	
	call nFunc.save( oFileName.fstr )
	
	deallocate( columns )
	
end program main

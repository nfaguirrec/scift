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
! 	use IFPORT   ! para llamar a makedirqq
! 	use GOptions_
! 	use Math_
! 	use IOStream_
! 	use String_
! 	use CommandLineParser_
! 	use RNFunction_
! 	use CNFunction_
! 	use FourierTransform_
! 	implicit none
! 	
! 	type(String) :: iFileName
! 	type(IFStream) :: ifile
! 	type(CNFunction) :: cFunc, nFunc, nFuncWindow
! 	type(RNFunction) :: rFunc, spectrum
! 	type(String) :: strBuffer
! 	type(CommandLineParser) :: parser
! 	integer :: fftSgn
! 	
! 	character(5), allocatable :: tokens(:)
! 	integer, allocatable :: columns(:)
! 	integer :: nPoints
! 	
! 	real(8) :: t, sigma
! 	integer :: freq
! 	real(8) :: resolution
! 	logical :: isXRange
! 	real(8) :: xrange(2)
! 	
! 	type(String) :: typeOfSpectrum
! 	integer :: idTypeOfSpectrum
! 	
! 	type(String) :: typeOfMethod
! 	integer :: idTypeOfMethod
! 	
! 	integer :: n
! 	type(String) :: dirName, oFileName, ext
! 	logical :: ok, exist
! 	integer :: status
! 	
! 	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 	! General parameters
! 	! -i
! 	! -o
! 	! -c
! 	! -n
! 	!-----------------------------------------------------------------
! 	iFileName = parser.getString( "-i" )
! 	oFileName = parser.getString( "-o" )
! 	
! 	strBuffer = parser.getString( "-s", def="FORWARD" )
! 	if( trim(strBuffer.fstr) == "FORWARD" ) then
! 		fftSgn = FourierTransform_FORWARD
! 	else if( trim(strBuffer.fstr) == "BACKWARD" ) then
! 		fftSgn = FourierTransform_BACKWARD
! 	else
! 		write(*,*) "### ERROR ### Bad value for parameter -s (FORWARD|BACKWARD)"
! 		stop
! 	end if
! 	
! 	strBuffer = parser.getString( "-c", def="1,2,3" )
! 	call strBuffer.split( tokens, "," )
! 	
! 	if( size(tokens) == 3 ) then
! 		allocate( columns(3) )
! 		columns = [ FString_toInteger(tokens(1)), FString_toInteger(tokens(2)), FString_toInteger(tokens(3)) ]
! 	else if( size(tokens) == 2 ) then
! 		allocate( columns(2) )
! 		columns = [ FString_toInteger(tokens(1)), FString_toInteger(tokens(2)) ]
! 	else
! 		write(*,*) "### ERROR ### Bad value for parameter -c"
! 		stop
! 	end if
! 	
! 	nPoints = parser.getInteger( "-n", def=-1 )
! 	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 	
! 	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 	! Type of spectrum
! 	! -t
! 	!-----------------------------------------------------------------
! 	typeOfSpectrum = parser.getString( "-t", def="NORM" )
! 
! 	select case( trim(typeOfSpectrum.fstr) )
! 		case( "NORM" )
! 			idTypeOfSpectrum = FourierTransform_NORM_SPECTRUM
! 		case( "REALPART" )
! 			idTypeOfSpectrum = FourierTransform_REALPART_SPECTRUM
! 		case( "IMAGPART" )
! 			idTypeOfSpectrum = FourierTransform_IMAGPART_SPECTRUM
! 		case( "PHASE" )
! 			idTypeOfSpectrum = FourierTransform_PHASE_SPECTRUM
! 		case( "POWER" )
! 			idTypeOfSpectrum = FourierTransform_POWER_SPECTRUM
! 	end select
! 	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 	
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
! 	
! 	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 	! Window parameters
! 	! -m
! 	!-----------------------------------------------------------------
! 	sigma = parser.getReal( "-s", def=0.1_8 )
! 	freq = parser.getInteger( "-f", def=1 )
! 	resolution = parser.getReal( "-r", def=1.0_8 )
! 	
! 	isXRange = .false.
! 	strBuffer = parser.getString( "-x", def="" )
! 	call strBuffer.split( tokens, "," )
! 	if( size(tokens) == 2 ) then
! 		isXRange = .true.
! 		xrange = [ FString_toReal(tokens(1)), FString_toReal(tokens(2)) ]
! 	end if
! 	!----------------------------------------------------------------------	
! 	
! ! 	dirName = this.cFunction(keyFunc).tFile.removeFileExtension( ext )
! 	dirName = "gabor"
! 	inquire( directory=dirName.fstr, exist=exist, iostat=status )
! 	
! 	if( status > 0 ) call GOptions_error( "Problems with directory "//trim(dirName.fstr), "n1df.gabor.main()" )
! 	if( .not. exist ) ok = makedirqq( trim(dirName.fstr) )
! 	
! 	n = 1
! 	t = cFunc.xGrid.min!+3.0_8*sigma
! 	do while( t <= cFunc.xGrid.max )!-3.0_8*sigma )
! 		call nFuncWindow.fromFunction( cFunc.xGrid, window )
! 		
! 		nFunc = nFuncWindow*cFunc
! ! 		spectrum = FourierTransform_spectrum( nFunc, sgn=fftSgn, ixrange=[1,nPoints] )
! 		
! 		spectrum = FourierTransform_spectrum( nFunc, sgn=fftSgn, type=idTypeOfSpectrum, method=idTypeOfMethod )
! 
! 		
! ! 		write(*,*) "### Error ### Code is not available yet"
! ! 		if( isXRange ) then
! ! 			call spectrum.saveDAT( append=.true., xrange=xrange, beforeLine=FString_fromReal( t, "(F10.5)" ), resolution=resolution )
! ! 		else
! ! 			call spectrum.saveDAT( append=.true., beforeLine=FString_fromReal( t, "(F10.5)" ), resolution=resolution )
! ! 		end if
! 
! ! 		if( mod(n-1,this.cFunction(keyFunc).tFileFrequency) == 0 ) then
! 			oFileName = trim(dirName.fstr)//"/"//trim(FString_fromInteger(n-1,format="(I0.5)"))//trim(ext.fstr)
! 			call spectrum.save( oFileName.fstr, metadata="time = "//trim(FString_fromReal(t)) )
! ! 		end if
! 		
! 		t = t + freq*cFunc.xGrid.stepSize
! 		n = n + 1
! 	end do

	use IFPORT   ! para llamar a makedirqq
	use GOptions_
	use Math_
	use IOStream_
	use String_
	use CommandLineParser_
	use Spline_
	use RNFunction_
	use CNFunction_
	use FourierTransform_
	implicit none
	
	type(String) :: iFileName
	type(String) :: oFileName
	type(IFStream) :: ifile
	type(CNFunction) :: nFunc, FnFunc, nFunc2
	type(RNFunction) :: spectrum
	type(String) :: strBuffer
	type(CommandLineParser) :: parser
	integer :: fftSgn
	
	character(5), allocatable :: tokens(:)
	integer, allocatable :: columns(:)
	integer :: nPoints
	integer :: smoothFactor
	integer :: i
	
	type(String) :: typeOfSpectrum
	integer :: idTypeOfSpectrum
	
	type(String) :: typeOfMethod
	integer :: idTypeOfMethod
	
	type(CNFunction) :: nFuncWindow
	real(8) :: t, sigma
	integer :: freq
	real(8) :: resolution
	logical :: isXRange
	real(8) :: xrange(2)
	
	integer :: n
	type(String) :: dirName, ext
	logical :: ok, exist
	integer :: status
	
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	! General parameters
	! -i
	! -o
	! -c
	! -n
	!-----------------------------------------------------------------
	iFileName = parser.getString( "-i" )
! 	oFileName = parser.getString( "-o" )
	
	strBuffer = parser.getString( "-s", def="FORWARD" )
	if( trim(strBuffer.fstr) == "FORWARD" ) then
		fftSgn = FourierTransform_FORWARD
	else if( trim(strBuffer.fstr) == "BACKWARD" ) then
		fftSgn = FourierTransform_BACKWARD
	else
		write(*,*) "### ERROR ### Bad value for parameter -s (FORWARD|BACKWARD)"
		stop
	end if
	
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
	
	nPoints = parser.getInteger( "-n", def=-1 )
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	! Type of spectrum
	! -t
	!-----------------------------------------------------------------
	typeOfSpectrum = parser.getString( "-t", def="NORM" )

	select case( trim(typeOfSpectrum.fstr) )
		case( "NORM" )
			idTypeOfSpectrum = FourierTransform_NORM_SPECTRUM
		case( "REALPART" )
			idTypeOfSpectrum = FourierTransform_REALPART_SPECTRUM
		case( "IMAGPART" )
			idTypeOfSpectrum = FourierTransform_IMAGPART_SPECTRUM
		case( "PHASE" )
			idTypeOfSpectrum = FourierTransform_PHASE_SPECTRUM
		case( "POWER" )
			idTypeOfSpectrum = FourierTransform_POWER_SPECTRUM
	end select
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	! Type of method
	! -m
	!-----------------------------------------------------------------
	typeOfMethod = parser.getString( "-m", def="FFT" )

	select case( trim(typeOfMethod.fstr) )
		case( "FFT" )
			idTypeOfMethod = FourierTransform_FFT_METHOD
		case( "NUMERICAL" )
			idTypeOfMethod = FourierTransform_NUMERICAL_METHOD
	end select
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	! Window parameters
	! -m
	!-----------------------------------------------------------------
	sigma = parser.getReal( "-sigma", def=0.1_8 )
	freq = parser.getInteger( "-freq", def=1 )
	resolution = parser.getReal( "-resol", def=1.0_8 )
	
	isXRange = .false.
	strBuffer = parser.getString( "-x", def="" )
	call strBuffer.split( tokens, "," )
	if( size(tokens) == 2 ) then
		isXRange = .true.
		xrange = [ FString_toReal(tokens(1)), FString_toReal(tokens(2)) ]
	end if
	!----------------------------------------------------------------------	
	
	call ifile.init( iFileName.fstr )
	nFunc = CNFunction( ifile, columns=columns )
	call ifile.close()
	
	if( nPoints == -1 ) nPoints = nFunc.nPoints()
	
! 	dirName = this.cFunction(keyFunc).tFile.removeFileExtension( ext )
	ext = ".dat"
	dirName = "gabor"
	inquire( directory=dirName.fstr, exist=exist, iostat=status )
	
	if( status > 0 ) call GOptions_error( "Problems with directory "//trim(dirName.fstr), "n1df.gabor.main()" )
	if( .not. exist ) ok = makedirqq( trim(dirName.fstr) )
	
	n = 1
	t = nFunc.xGrid.min!+3.0_8*sigma
	do while( t <= nFunc.xGrid.max )!-3.0_8*sigma )
		write(*,"(A,F20.6,A)", advance="no") "Generating ", t, " ... "
		
		nFuncWindow = CNFunction( nFunc.xGrid, window )
		
		nFunc2 = nFuncWindow*nFunc
		
		spectrum = FourierTransform_spectrum( nFunc2, sgn=fftSgn, type=idTypeOfSpectrum, method=idTypeOfMethod )
		
! 		if( mod(n-1,this.cFunction(keyFunc).tFileFrequency) == 0 ) then
! 			oFileName = trim(dirName.fstr)//"/"//trim(FString_fromInteger(n-1,format="(I0.5)"))//trim(ext.fstr)
			oFileName = trim(dirName.fstr)//"/"//trim(adjustl(FString_fromReal(t,format="(F20.6)")))//trim(ext.fstr)
			
			if( isXRange ) then
				call spectrum.save( oFileName.fstr, metadata="time = "//trim(FString_fromReal(t)), xrange=xrange )
			else
				call spectrum.save( oFileName.fstr, metadata="time = "//trim(FString_fromReal(t)) )
			end if
! 		end if
		
		t = t + freq*nFunc.xGrid.stepSize
		n = n + 1
		
		write(*,"(A)") "OK"
	end do

	deallocate( columns )
	
	contains
	
	function window( tprime ) result( output )
		real(8), intent(in) :: tprime
		complex(8) :: output
		
		output = exp(-(tprime-t)**2/2.0_8/sigma**2)/sigma/sqrt(2.0_8*Math_PI)
	end function window
end program main

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                                                                                   !!
!!  This file is part of SciFT project                                               !!
!!  Copyright (c) 2016-2016 Nestor F. Aguirre (nfaguirrec@gmail.com)                 !!
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
!! E0=0.05; omega=0.20279; t0=500.0; w=800.0; dw=100.0; tMax=1000.0
!! echo "n=2**11; x0=0.0; h=$tMax/(n-1); do for [i=1:n] { x=x0+(i-1)*h; print sprintf( '%10.5f%10.5f', x, $E0*cos($omega*x)*erfhat(x,$t0,$w,$dw) ); }" | gnuplot 2> signalErfHat.dat
!! ./fft1D -i signalErfHat.dat > fftErfHat.dat
!! plot "signalErfHat.dat" w lp pt 7 ps 0.5 lw 2
!! plot [0:0.4] "fftErfHat.dat" w lp pt 7 ps 0.5 lw 2
!!
!! echo "n=2**11; x0=0.0; h=$tMax/(n-1); do for [i=1:n] { x=x0+(i-1)*h; print sprintf( '%10.5f%10.5f', x, $E0*cos($omega*x)*coshat(x,$t0,$w,$dw) ); }" | gnuplot 2> signalCosHat.dat
!! ./fft1D -i signalCosHat.dat > fftCosHat.dat
!! plot "signalCosHat.dat" w lp pt 7 ps 0.5 lw 2
!! plot [0:0.4] "fftCosHat.dat" w lp pt 7 ps 0.5 lw 2
!!
!! plot "signalErfHat.dat" w l lw 2, "signalCosHat.dat" w l lw 2 lt 3
!! set size square
!! plot [0:0.4] "fftErfHat.dat" w lp pt 7 ps 0.5 lw 2, "fftCosHat.dat" w lp pt 7 ps 0.5 lw 2 lt 3
!!
program main
	use GOptions_
	use IOStream_
	use String_
	use CommandLineParser_
	use Spline_
	use RNFunction_
	use CNFunction_
	use FourierTransform_
	implicit none
	
	type(String) :: iFileName1
	type(String) :: iFileName2
	type(String) :: oFileName
	type(IFStream) :: ifile
	type(CNFunction) :: nFunc1, FnFunc1
	type(CNFunction) :: nFunc2, FnFunc2
	type(CNFunction) :: ProdFunc, ProdFFunc
	type(String) :: strBuffer
	type(CommandLineParser) :: parser
	
	character(5), allocatable :: tokens(:)
	integer, allocatable :: columns1(:)
	integer, allocatable :: columns2(:)
	
	type(String) :: typeOfMethod
	integer :: idTypeOfMethod
	
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	! General parameters
	! -i
	! -o
	! -c
	! -n
	!-----------------------------------------------------------------
	iFileName1 = parser.getString( "-f1" )
	iFileName2 = parser.getString( "-f2" )
	oFileName = parser.getString( "-o" )
	
	strBuffer = parser.getString( "-c1", def="1,2,3" )
	call strBuffer.split( tokens, "," )
	
	if( size(tokens) == 3 ) then
		allocate( columns1(3) )
		columns1 = [ FString_toInteger(tokens(1)), FString_toInteger(tokens(2)), FString_toInteger(tokens(3)) ]
	else if( size(tokens) == 2 ) then
		allocate( columns1(2) )
		columns1 = [ FString_toInteger(tokens(1)), FString_toInteger(tokens(2)) ]
	else
		write(*,*) "### ERROR ### Bad value for parameter -c1"
		stop
	end if
	
	strBuffer = parser.getString( "-c2", def="1,2,3" )
	call strBuffer.split( tokens, "," )
	
	if( size(tokens) == 3 ) then
		allocate( columns2(3) )
		columns2 = [ FString_toInteger(tokens(1)), FString_toInteger(tokens(2)), FString_toInteger(tokens(3)) ]
	else if( size(tokens) == 2 ) then
		allocate( columns2(2) )
		columns2 = [ FString_toInteger(tokens(1)), FString_toInteger(tokens(2)) ]
	else
		write(*,*) "### ERROR ### Bad value for parameter -c1"
		stop
	end if
	
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
	
	call ifile.init( iFileName1.fstr )
	nFunc1 = CNFunction( ifile, columns=columns1 )
	call ifile.close()
	
	call ifile.init( iFileName2.fstr )
	nFunc2 = CNFunction( ifile, columns=columns2 )
	call ifile.close()
	
	select case( idTypeOfMethod )
		case( FourierTransform_FFT_METHOD )
			FnFunc1 = FourierTransform_fft( nFunc1, sgn=FourierTransform_FORWARD )
			FnFunc2 = FourierTransform_fft( nFunc2, sgn=FourierTransform_FORWARD )
			
			ProdFFunc = FnFunc1*FnFunc2
			
			ProdFunc = FourierTransform_fft( ProdFFunc, sgn=FourierTransform_BACKWARD )
			
			call ProdFunc.save( oFileName.fstr )
		case( FourierTransform_NUMERICAL_METHOD )
			FnFunc1 = FourierTransform_nft( nFunc1, sgn=FourierTransform_FORWARD )
			FnFunc2 = FourierTransform_nft( nFunc2, sgn=FourierTransform_FORWARD )
			
			ProdFFunc = FnFunc1*FnFunc2
			
			ProdFunc = FourierTransform_fft( ProdFFunc, sgn=FourierTransform_FORWARD )
			
			call ProdFunc.save( oFileName.fstr )
		case default
			call GOptions_error( "The method = "//trim(typeOfMethod.fstr)//" is not available", "n1df.convolution.f90" )
	end select
	
	deallocate( columns1 )
	deallocate( columns2 )
	
end program main

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                                                                                   !!
!!  This file is part of SciFT project                                               !!
!!  Copyright (c) 2013-2013 Nestor F. Aguirre (nfaguirrec@gmail.com)                 !!
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

module NPeakFinder_
	use GOptions_
	use Math_
	use Grid_
	use RNFunction_
	use IntegerList_
	implicit none
	private
	
	public :: &
		NPeakFinder_generateSignal, &
		NPeakFinder_test
		
	type, public :: NPeakFinder
		type(RNFunction), pointer, private :: func
		
		integer :: method
		integer :: windowSize
		real(8) :: tolerance
		integer :: bandwidth  !< Only valid for S4
		
		contains
			procedure :: init
			final :: destroy
			procedure :: str
			procedure :: show
			procedure :: execute
	end type NPeakFinder
	
	contains
	
	!>
	!! @brief Constructor.
	!!
	!! Reference:
	!!   Simple Algorithms for Peak Detection in Time-Series
	!!   Girish Keshav Palshikar
	!!   Tata Research Development and Design Centre (TRDDC)
	!!   54B Hadapsar Industrial Estate
	!!   Pune 411013, India.
	!!   Email: gk.palshikar@tcs.com
	!!
	!! @input func
	!! @input wSize   Window size
	!! @input h
	!!
	subroutine init( this, func, method, windowSize, tolerance, bandwidth )
		class(NPeakFinder) :: this 
		type(RNFunction), target, intent(in) :: func
		integer, optional :: method
		integer, optional :: windowSize
		real(8), optional :: tolerance
		integer, optional :: bandwidth
		
		if( associated(this%func) ) nullify(this%func)
		this%func => func
		
		this%method = NPeakFinder_MAX_DIST
		if( present(method) ) this%method = method
		
		this%windowSize = 10
		if( present(windowSize) ) this%windowSize = windowSize
		
		this%tolerance = 0.2
		if( present(tolerance) ) this%tolerance = tolerance
		
		this%bandwidth = 5
		if( present(bandwidth) ) this%bandwidth = bandwidth
	end subroutine init
	
	!>
	!! @brief Destructor
	!!
	subroutine destroy( this )
		type(NPeakFinder) :: this
		
		nullify(this%func)
	end subroutine destroy
	
	!>
	!! @brief
	!!
	function str( this ) result( output )
		class(NPeakFinder) :: this 
		character(len=200) :: output
		
		integer :: fmt
		character(len=200) :: strBuffer
		
		output = ""
		
! 		output = trim(output)//"<NPeakFinder:"
! 		
! 		output = trim(output)//"min="
! 		fmt = int(log10(this%min+1.0))+1
! 		write(strBuffer, "(f<fmt+7>.6)") this%min
! 		output = trim(output)//trim(strBuffer)
! 		
! 		output = trim(output)//",max="
! 		fmt = int(log10(this%max+1.0))+1
! 		write(strBuffer, "(f<fmt+7>.6)") this%max
! 		output = trim(output)//trim(strBuffer)
! 		
! 		output = trim(output)//",h="
! 		fmt = int(log10(this%h+1.0))+1
! 		write(strBuffer, "(f<fmt+7>.6)") this%h
! 		output = trim(output)//trim(strBuffer)
! 		
! 		output = trim(output)//",size="
! 		fmt = int(log10(float(this%size+1)))+1
! 		write(strBuffer, "(i<fmt>)") this%size
! 		output = trim(output)//trim(strBuffer)
! 		
! 		output = trim(output)//">"
	end function str
	
	!>
	!! @brief
	!!
	subroutine show( this, unit )
		class(NPeakFinder) :: this
		integer, optional, intent(in) :: unit
		
		integer :: effunit
		
		if( present(unit) ) then
			effunit = unit
		else
			effunit = 6
		end if
		
		write(effunit,"(a)") trim(this%str())
	end subroutine show
	
	!>
	!! @brief
	!!
	function kernelS1( k, i, yArray ) result( output )
		integer, intent(in) :: k
		integer, intent(in) :: i
		real(8), allocatable, intent(in) :: yArray(:)
		real(8) :: output
		
		real(8) :: maxBackward, maxForward
		
		if( i-1 < 1 .or. i-k < 1 .or. i+1 > size(yArray) .or. i+k > size(yArray) ) then
			output = -1.0_8
			return
		end if
		
		maxBackward = maxval( yArray(i)-yArray(i-k:i-1) )
		maxForward = maxval( yArray(i)-yArray(i+1:i+k) )
		
		output = 0.5_8*(maxBackward+maxForward)
	end function kernelS1
	
	!>
	!! @brief
	!!
	function kernelS2( k, i, yArray ) result( output )
		integer, intent(in) :: k
		integer, intent(in) :: i
		real(8), allocatable, intent(in) :: yArray(:)
		real(8) :: output
		
		real(8) :: averBackward, averForward
		
		if( i-1 < 1 .or. i-k < 1 .or. i+1 > size(yArray) .or. i+k > size(yArray) ) then
			output = -1.0_8
			return
		end if
		
		averBackward = sum( yArray(i)-yArray(i-k:i-1) )/k
		averForward = sum( yArray(i)-yArray(i+1:i+k) )/k
		
		output = 0.5_8*(averBackward+averForward)
	end function kernelS2
	
	!>
	!! @brief
	!!
	function kernelS3( k, i, yArray ) result( output )
		integer, intent(in) :: k
		integer, intent(in) :: i
		real(8), allocatable, intent(in) :: yArray(:)
		real(8) :: output
		
		real(8) :: averBackward, averForward
		
		if( i-1 < 1 .or. i-k < 1 .or. i+1 > size(yArray) .or. i+k > size(yArray) ) then
			output = -1.0_8
			return
		end if
		
		averBackward = yArray(i) - sum(yArray(i-k:i-1))/k
		averForward = yArray(i) - sum(yArray(i+1:i+k))/k
		
		output = 0.5_8*(averBackward+averForward)
	end function kernelS3
	
	!>
	!! @brief
	!!
	function kernelS4_K( x ) result( output )
		real(8), intent(in) :: x
		real(8) :: output
		
		output = exp(-0.5_8*x**2)/sqrt(2.0_8*Math_PI)
		
! 		if( abs(x) < 1.0_8 ) then
! 			output = 0.75_8*(1.0_8-x**2)
! 		else
! 			output = 0.0_8
! 		end if
	end function kernelS4_K
	
	!>
	!! @brief
	!!
	function kernelS4_P( w, i, array ) result( output )
		integer, intent(in) :: w
		integer, intent(in) :: i
		real(8), allocatable, intent(in) :: array(:)
		real(8) :: output
		
		integer :: j, M
		real(8) :: nconst
		
		M = size(array)
		
		if( i+w > M ) then
			output = -1.0_8
			return
		end if
		
		nconst = abs(array(i)-array(i+w))
		
		output = 0.0_8
		do j=1,M
! 			output = output + kernelS4_K( (array(i)-array(j))/nconst )
			output = output + kernelS4_K( array(i)-array(j) )
		end do
		
! 		output = output/real(M,8)/nconst
		output = output/real(M,8)
	end function kernelS4_P
	
	!>
	!! @brief
	!!
	function kernelS4_H( w, array ) result( output )
		integer, intent(in) :: w
		real(8), allocatable, intent(in) :: array(:)
		real(8) :: output
		
		integer :: i, M
		real(8) :: pw
		
		M = size(array)
		
		output = 0.0_8
		do i=1,M
			pw = kernelS4_P( w, i, array )
			
			if( pw > 0.0_8 ) then
				output = output - pw*log(pw)
			end if
		end do
	end function kernelS4_H
	
	!>
	!! @brief
	!!
	function kernelS4( k, w, i, yArray ) result( output )
		integer, intent(in) :: k
		integer, intent(in) :: w
		integer, intent(in) :: i
		real(8), allocatable, intent(in) :: yArray(:)
		real(8) :: output
		
		integer :: j
		real(8), allocatable :: Nvec(:), NvecPrime(:)
		
		if( i-1 < 1 .or. i-k < 1 .or. i+1 > size(yArray) .or. i+k > size(yArray) ) then
			output = -1.0_8
			return
		end if
		
		allocate( Nvec(2*k) )
		allocate( NvecPrime(2*k+1) )
		
		Nvec(1:k) = yArray(i-k:i-1)
		Nvec(k+1:2*k) = yArray(i+1:i+k)
		
		NvecPrime(1:k) = yArray(i-k:i-1)
		NvecPrime(k+1) = yArray(i)
		NvecPrime(k+2:2*k+1) = yArray(i+1:i+k)
		
		! En el paper original la resta es al contrario
		output = kernelS4_H( w, NvecPrime ) - kernelS4_H( w, Nvec )
		
		deallocate( Nvec )
		deallocate( NvecPrime )
	end function kernelS4
	
	!>
	!! @brief
	!!
	function execute( this, posVec ) result( output )
		class(NPeakFinder) :: this
		type(IntegerList), optional :: posVec  ! Peak positions in this%func
		type(RNFunction) :: output
		
		type(IntegerList) :: effPosVec
		
		integer :: k, h, w
		real(8) :: m, s
		integer :: nPoints
		
		integer :: i
		real(8), allocatable :: a(:)
		real(8), allocatable :: x(:), y(:)
		
		k = this%windowSize
		h = this%tolerance
		w = this%bandwidth
		
		nPoints = this%func%xGrid%nPoints
		
		allocate( a(nPoints) )
		call effPosVec%init()
		
		!------------------------------------------------------------
		! Compute peak function value for each of the N points in T
		!------------------------------------------------------------
		do i=1,nPoints
			select case( this%method )
				case( NPeakFinder_MAX_DIST )
					a(i) = kernelS1(k,i,this%func%fArray)
				case( NPeakFinder_MAX_AVER_NEIG )
					a(i) = kernelS2(k,i,this%func%fArray)
				case( NPeakFinder_MAX_AVER_NEIG_AVER )
					a(i) = kernelS3(k,i,this%func%fArray)
				case( NPeakFinder_ENTROPY )
					a(i) = kernelS4(k,w,i,this%func%fArray)
				case default
					write(*,*) "### ERROR ### PeakFinder.execute. Method XXX is not implemented yet"
					stop
			end select
		end do
		
		!-------------------------------------------------
		! Compute the mean m and standard deviation s of
		! all positive values in array a
		!-------------------------------------------------
		m = sum( a(k+1:nPoints-k), mask=a(k+1:nPoints-k)>0.0_8 )/count( a(k+1:nPoints-k)>0.0_8 )
		s = sqrt( sum( (a(k+1:nPoints-k)-m)**2, mask=a(k+1:nPoints-k)>0.0_8 )/count( a(k+1:nPoints-k)>0.0_8 ) )
		
		!--------------------------------------
		! Remove local peaks which are “small”
		! in global context
		!--------------------------------------
		do i=1,nPoints
			if( a(i)>0.0_8 .and. (a(i)-m) > h*s ) then
				call effPosVec%append( i )
			end if
		end do
		
		!----------------------------------------------
		! Retain only one peak out of any set of peaks
		! within distance k of each other
		!----------------------------------------------
		i=2
		do while( i<=effPosVec%size() )
			if( abs( effPosVec%at(i)-effPosVec%at(i-1) ) <= k ) then
				if( this%func%at( effPosVec%at(i) ) < this%func%at( effPosVec%at(i-1) ) ) then
					call effPosVec%erase( i )
				else
					call effPosVec%erase( i-1 )
				end if
				i=2
			else
				i=i+1
			end if
		end do
		
		!----------------------------
		! Build the output function
		!----------------------------
		if( present(posVec) ) then
			posVec = effPosVec
		end if
		
		allocate( x(effPosVec%size()) )
		allocate( y(effPosVec%size()) )
		
		do i=1,effPosVec%size()
			x(i) = this%func%xGrid%at( effPosVec%at(i) )
			y(i) = this%func%at( effPosVec%at(i) )
		end do
		
		call output%init( x, y )
		
		deallocate( x )
		deallocate( y )
	end function execute
	
	real(8) function gaussian( t, I, t0, s )
		real(8), intent(in) :: t
		real(8), intent(in) :: I
		real(8), intent(in) :: t0
		real(8), intent(in) :: s
		
		gaussian = I*exp(-0.5_8*(t-t0)**2/s**2)
		
	end function gaussian
	
	!>
	!! @brief
	!!
	function NPeakFinder_generateSignal( tMin, tMax, dt, nGauss, SNFactor, maxI, minDesv, maxDesv ) result( output )
		real(8), intent(in) :: tMin
		real(8), intent(in) :: tMax
		real(8), intent(in) :: dt
		integer, intent(in) :: nGauss
		real(8), intent(in) :: SNFactor
		real(8), intent(in) :: maxI
		real(8), intent(in) :: minDesv, maxDesv
		type(RNFunction) :: output
		
		integer :: nPoints
		real(8) :: tn
		integer :: n, m
		
		real(8), allocatable :: I(:)
		real(8), allocatable :: t0(:)
		real(8), allocatable :: s(:)
		real(8) :: value
		real(8) :: randNumber
		
		real(8), allocatable :: x(:), y(:)
		
		nPoints = int( abs( tMax-tMin )/dt+1 )
		
		allocate( x(nPoints), y(nPoints) )
		allocate( I(nGauss), t0(nGauss), s(nGauss) )
		
		call random_seed()
		do n=1,nGauss
			call random_number( randNumber )
			I(n) = randNumber*maxI
			
			call random_number( randNumber )
			t0(n) = tMin+randNumber*abs( tMax-tMin )
			
			call random_number( randNumber )
			s(n) = minDesv+randNumber*abs( maxDesv-minDesv )
		end do
		
		tn = tMin
		do n=1,nPoints
			value = 0.0_8
			do m=1,nGauss
				value = value + gaussian( tn, I(m), t0(m), s(m) )
			end do
			
			call random_number( randNumber )
			value = value + ( SNFactor*maxval(I)*( 2.0_8*randNumber-1.0_8 ) )
			value = max( value, 0.0_8 )
			
			x(n) = tn
			y(n) = cmplx( value, 0.0_8 )
			
			tn = tn + dt
		end do
		
		call output%init( x, y )
	end function NPeakFinder_generateSignal
	
	!>
	!! @bief test
	!!
	subroutine NPeakFinder_test()
		type(RNFunction) :: func
		type(RNFunction) :: peaks
		type(NPeakFinder) :: peakFinder
		
		func = NPeakFinder_generateSignal( 0.0_8, 100.0_8, 0.1_8, 10, 0.1_8, 50.0_8, 0.1_8, 1.0_8 )
		call peakFinder%init( func, method=NPeakFinder_MAX_DIST, windowSize=12, tolerance=1.0_8 )
		
		peaks = peakFinder.execute()
		call func%save()
		call peaks%save()
	end subroutine NPeakFinder_test
	
end module NPeakFinder_

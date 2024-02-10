!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                                                                                   !!
!!  This file is part of SciFT project                                               !!
!!  Copyright (c) 2010-2013 Nestor F. Aguirre (nfaguirrec@gmail.com)                 !!
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

module NIntegrator_
	use GOptions_
	use Grid_
	use RNFunction_
	implicit none
	private
	
	public :: &
		NIntegrator_test
	
	type, public :: NIntegrator
		type(RNFunction), pointer :: func
		integer :: method
		
		contains
			generic :: init => initDefault
			procedure :: initDefault
			final :: destroy
			procedure :: str
			procedure :: show
			procedure :: evaluate
	end type NIntegrator
	
	contains
	
	!>
	!! @brief Constructor
	!!
	subroutine initDefault( this, func, method )
		class(NIntegrator) :: this 
		type(RNFunction), target, intent(in) :: func
		integer, optional, intent(in) :: method
		
		this.func => func
		
		if( present(method) ) then
			this.method = method
		else
			this.method = NIntegrator_SIMPSON
		end if
	end subroutine initDefault
	
	!>
	!! @brief Destructor
	!!
	subroutine destroy( this )
		type(NIntegrator) :: this
		
		nullify(this.func)
	end subroutine destroy
	
	!>
	!! @brief
	!!
	function str( this ) result( output )
		class(NIntegrator) :: this 
		character(len=200) :: output
		
		integer :: fmt
		character(len=200) :: strBuffer
		
		output = ""
		
! 		output = trim(output)//"<NIntegrator:"
! 		
! 		output = trim(output)//"min="
! 		fmt = int(log10(this.min+1.0))+1
! 		write(strBuffer, "(f<fmt+7>.6)") this.min
! 		output = trim(output)//trim(strBuffer)
! 		
! 		output = trim(output)//",max="
! 		fmt = int(log10(this.max+1.0))+1
! 		write(strBuffer, "(f<fmt+7>.6)") this.max
! 		output = trim(output)//trim(strBuffer)
! 		
! 		output = trim(output)//",h="
! 		fmt = int(log10(this.h+1.0))+1
! 		write(strBuffer, "(f<fmt+7>.6)") this.h
! 		output = trim(output)//trim(strBuffer)
! 		
! 		output = trim(output)//",size="
! 		fmt = int(log10(float(this.size+1)))+1
! 		write(strBuffer, "(i<fmt>)") this.size
! 		output = trim(output)//trim(strBuffer)
! 		
! 		output = trim(output)//">"
	end function str
	
	!>
	!! @brief
	!!
	subroutine show( this, unit )
		class(NIntegrator) :: this
		integer, optional, intent(in) :: unit
		
		integer :: effunit
		
		if( present(unit) ) then
			effunit = unit
		else
			effunit = 6
		end if
		
		write(effunit,"(a)") trim(this.str())
	end subroutine show
	
	!>
	!! @brief
	!!
	function evaluate( this, a, b ) result( output )
		class(NIntegrator) :: this
		real(8), optional, intent(in) :: a
		real(8), optional, intent(in) :: b
		real(8) :: output
		
		real(8) :: effA
		real(8) :: effB
		
		effA = this.func.min()
		if( present(a) ) then
			if( a >= this.func.min() .and. a <= this.func.max() ) then
				effA = a
			else
				call GOptions_error( "The integration lower limit 'a' is outside of numerical grid.", "NIntegrator.evaluate(a,b)" )
			end if
		end if
		
		effB = this.func.max()
		if( present(b) ) then
			if( b >= this.func.min() .and. b <= this.func.max() ) then
				effB = b
			else
				call GOptions_error( "The integration upper limit 'b' is outside of numerical grid.", "NIntegrator.evaluate(a,b)" )
			end if
		end if
		
		select case( this.method )
			case( NIntegrator_SIMPSON )
				output = simpsonRule( this, effA, effB )
			case( NIntegrator_EXTSIMPSON )
				output = extendedSimpsonRule( this, effA, effB )
			case( NIntegrator_SIMPSON38 )
				output = simpson38Rule( this, effA, effB )
			case( NIntegrator_TRAPEZOIDAL )
				output = trapezoidalRule( this, effA, effB )
			case( NIntegrator_FIXED_QUADRATURE )
				output = fixedQuadratureRule( this, effA, effB )
			case( NIntegrator_QUADRATURE )
				output = quadratureRule( this, effA, effB )
			case( NIntegrator_ADAPTIVE_QUADRATURE )
				output = adaptiveQuadratureRule( this, effA, effB )
			case( NIntegrator_BOOLE )
				output = booleRule( this, effA, effB )
		end select
	end function evaluate
	
	!!>
	!! @brief Simpson rule
	!! \int_a^b f(x)dx = \frac{h}{3}\left[ f(x_0) + 2\sum_{i=1}^{n-1}f(x_{i}) + f(x_n)\right]
	!!
	function simpsonRule( this, a, b ) result( output )
		class(NIntegrator) :: this
		real(8), intent(in) :: a
		real(8), intent(in) :: b
		real(8) :: output
		
		real(8) :: ssum
		integer :: i, ia, ib
		
		ia = this.func.xGrid.pos( a )
		ib = this.func.xGrid.pos( b )
		
		ssum = 0.0_8
		do i=ia+1,ib-1
			ssum = ssum + this.func.fArray(i);
		end do
		
		output = (this.func.xGrid.stepSize/2.0_8)*( this.func.fArray(ia) + 2.0_8*ssum + this.func.fArray(ib) )
	end function simpsonRule
	
	!>
	!! @brief Extended Simpson rule
	!!
	function extendedSimpsonRule( this, a, b ) result( output )
		class(NIntegrator) :: this
		real(8), intent(in) :: a
		real(8), intent(in) :: b
		real(8) :: output
		
		real(8) :: sum
		integer :: size
		integer :: i
		
		size = this.func.nPoints()
		
		call GOptions_warning( "This method has not implemented yet defined integration.", "NIntegrator.extendedSimpsonRule(a,b)" )
		
		sum = 0.0
		do i=5,size-4
			sum = sum + this.func.fArray(i)
		end do
		sum = 48.0_8*sum
		
		sum = sum + 17.0_8*this.func.fArray(1) + 59.0_8*this.func.fArray(2) + 43.0_8*this.func.fArray(3) + 49.0_8*this.func.fArray(4)
		sum = sum + 49.0_8*this.func.fArray(size-3) + 43.0_8*this.func.fArray(size-2) + 59.0_8*this.func.fArray(size-1) + 17.0_8*this.func.fArray(size)
		
		output = sum*( this.func.xGrid.stepSize/48.0_8 )
	end function extendedSimpsonRule
	
	function simpson38Rule( this, a, b ) result( output )
		class(NIntegrator) :: this
		real(8), intent(in) :: a
		real(8), intent(in) :: b
		real(8) :: output
		
		call GOptions_error( "This method is not implemented yet.", "NIntegrator.simpson38Rule(a,b)" )
	end function simpson38Rule
	
	!!>
	!! @brief Simpson rule
	!! \int_a^b f(x)dx = h\left[ 2*f(x_0) + \sum_{i=1}^{n-1}f(x_{i}) + 2*f(x_n)\right]
	function trapezoidalRule( this, a, b ) result( output )
		class(NIntegrator) :: this
		real(8), intent(in) :: a
		real(8), intent(in) :: b
		real(8) :: output
		
		real(8) :: ssum
		integer :: i, ia, ib
		
		ia = this.func.xGrid.pos( a )
		ib = this.func.xGrid.pos( b )
		
		ssum = 0.0_8
		do i=ia+1,ib-1
			ssum = ssum + this.func.fArray(i);
		end do
		
		output = this.func.xGrid.stepSize*( 2.0_8*this.func.fArray(ia) + ssum + 2.0_8*this.func.fArray(ib) )
	end function trapezoidalRule

	function fixedQuadratureRule( this, a, b ) result( output )
		class(NIntegrator) :: this
		real(8), intent(in) :: a
		real(8), intent(in) :: b
		real(8) :: output
		
		call GOptions_error( "This method is not implemented yet.", "NIntegrator.fixedQuadratureRule(a,b)" )
	end function fixedQuadratureRule

	function quadratureRule( this, a, b ) result( output )
		class(NIntegrator) :: this
		real(8), intent(in) :: a
		real(8), intent(in) :: b
		real(8) :: output
		
		call GOptions_error( "This method is not implemented yet.", "NIntegrator.quadratureRule(a,b)" )
	end function quadratureRule

	function adaptiveQuadratureRule( this, a, b ) result( output )
		class(NIntegrator) :: this
		real(8), intent(in) :: a
		real(8), intent(in) :: b
		real(8) :: output
		
		call GOptions_error( "This method is not implemented yet.", "NIntegrator.adaptiveQuadratureRule(a,b)" )
	end function adaptiveQuadratureRule
	
	!>
	!! @brief Boole rule
	!!        http://fourier.eng.hmc.edu/e176/lectures/ch6/node6.html
	!!
	function booleRule( this, a, b ) result( output )
		class(NIntegrator) :: this
		real(8), intent(in) :: a
		real(8), intent(in) :: b
		real(8) :: output
		
		real(8) :: ssum
		integer :: i, ia, ib
		
		ia = this.func.xGrid.pos( a )
		ib = this.func.xGrid.pos( b )
		
		ssum = 0.0_8
		do i=ia,ib-4,4
			ssum = ssum + 7.0_8*this.func.fArray(i) + 32.0_8*this.func.fArray(i+1) &
					+ 12.0_8*this.func.fArray(i+2) + 32.0_8*this.func.fArray(i+3) + 7.0_8*this.func.fArray(i+4)
		end do
		
		output = ssum*2.0_8*this.func.xGrid.stepSize/45.0_8
	end function booleRule
	
	!>
	!! This is neccesary only for NFunction_test()
	!!
	function funcTest( x ) result( output )
		real(8), intent(in) :: x
		real(8) :: output
		
		output = exp(-0.44*abs(x))*sin(x)**2.0_8
	end function funcTest
	
	!>
	!! This is neccesary only for NFunction_test()
	!!
	subroutine NIntegrator_test()
		type(Grid) :: xGrid
		type(RNFunction) :: nFunc
		type(NIntegrator) :: integrator
		real(8) :: exactValue
		real(8) :: value, Math_PI
		integer :: i
		
		Math_PI = acos(-1.0_8)
		
		call xGrid.init( -30.0_8, 30.0_8, 1000 )
		call xGrid.show()
		
		nFunc = RNFunction( xGrid, funcTest )
		call nFunc.show()
		
		exactValue = 2.16780136532979_8
		
		write(*,*) "NIntegrator_SIMPSON"
		write(*,*) "======="
		call integrator.init( nFunc, NIntegrator_SIMPSON )
		write(*,'(A,F30.8)') "Exact     = ", exactValue
		write(*,'(A,F30.8)') "Numerical = ", integrator.evaluate()
		write(*,'(A,F27.5,A3)') "Error(%)  = ", 100.0_8*( integrator.evaluate()-exactValue )/exactValue, "%"
		write(*,*) ""
		
		write(*,*) "NIntegrator_BOOLE"
		write(*,*) "====="
		call integrator.init( nFunc, NIntegrator_BOOLE )
		write(*,'(A,F30.8)') "Exact     = ", exactValue
		write(*,'(A,F30.8)') "Numerical = ", integrator.evaluate()
		write(*,'(A,F27.5,A3)') "Error(%)  = ", 100.0_8*( integrator.evaluate()-exactValue )/exactValue, "%"
		write(*,*) ""
		
		exactValue = 1.623685454371397_8
		
		write(*,*) "NIntegrator_SIMPSON"
		write(*,*) "======="
		call integrator.init( nFunc, NIntegrator_SIMPSON )
		write(*,'(A,F30.8)') "Exact     = ", exactValue
		write(*,'(A,F30.8)') "Numerical = ", integrator.evaluate( -Math_PI, Math_PI )
		write(*,'(A,F27.5,A3)') "Error(%)  = ", 100.0_8*( integrator.evaluate( -Math_PI, Math_PI )-exactValue )/exactValue, "%"
		write(*,*) ""
		
		write(*,*) "NIntegrator_BOOLE"
		write(*,*) "====="
		call integrator.init( nFunc, NIntegrator_BOOLE )
		write(*,'(A,F30.8)') "Exact     = ", exactValue
		write(*,'(A,F30.8)') "Numerical = ", integrator.evaluate( -Math_PI, Math_PI )
		write(*,'(A,F27.5,A3)') "Error(%)  = ", 100.0_8*( integrator.evaluate( -Math_PI, Math_PI )-exactValue )/exactValue, "%"
		write(*,*) ""
		
	end subroutine NIntegrator_test
	
end module NIntegrator_

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!  This file is part of scift (Scientific Fortran Tools).
!!  Copyright (C) by authors (2010-2013)
!!  
!!  Authors (alphabetic order):
!!    * Aguirre N.F. (nfaguirrec@gmail.com)  (2010-2013)
!!  
!!  Contributors (alphabetic order):
!!  
!!  Redistribution and use in source and binary forms, with or
!!  without modification, are permitted provided that the
!!  following conditions are met:
!!  
!!   * Redistributions of binary or source code must retain
!!     the above copyright notice and this list of conditions
!!     and/or other materials provided with the distribution.
!!   * All advertising materials mentioning features or use of
!!     this software must display the following acknowledgement:
!!     
!!     This product includes software from scift
!!     (Scientific Fortran Tools) project and its contributors.
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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
		
		call nFunc.init( xGrid, funcTest )
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

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
	use Grid_
	use RNFunction_
	implicit none
	private
	
	!>
	!! @brief Public parameters
	!!
	integer, public, parameter :: SIMPSON  = 0
	integer, public, parameter :: EXTSIMPSON  = 1
	integer, public, parameter :: SIMPSON38  = 2
	integer, public, parameter :: TRAPEZOIDAL = 3
	integer, public, parameter :: FIXED_QUADRATURE = 4
	integer, public, parameter :: QUADRATURE = 5
	integer, public, parameter :: ADAPTIVE_QUADRATURE = 6
	integer, public, parameter :: BOOLE = 7
	
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
			this.method = SIMPSON
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
	function evaluate( this ) result( output )
		class(NIntegrator) :: this
		real(8) :: output
		
		select case( this.method )
			case( SIMPSON )
				output = simpsonRule( this )
			case( EXTSIMPSON )
				output = extendedSimpsonRule( this )
			case( SIMPSON38 )
				output = simpson38Rule( this )
			case( TRAPEZOIDAL )
				output = trapezoidalRule( this )
			case( FIXED_QUADRATURE )
				output = fixedQuadratureRule( this )
			case( QUADRATURE )
				output = quadratureRule( this )
			case( ADAPTIVE_QUADRATURE )
				output = adaptiveQuadratureRule( this )
			case( BOOLE )
				output = booleRule( this )
		end select
	end function evaluate
	
	!!>
	!! @brief Simpson rule
	!! \int_a^b f(x)dx = \frac{1}{3}\left[ f(x_0) + 2\sum_{i=1}^{n/2-1}f(x_{2i}) + 4\sum_{i=1}^{n/2-1}f(x_{2i-1}) + f(x_n)\right]
	!!
	function simpsonRule( this ) result( output )
		class(NIntegrator) :: this
		real(8) :: output
		
		real(8) :: sum
		integer :: size
		integer :: i
		
		size = this.func.xGrid.nPoints
		
		sum = this.func.fArray(1)
		
		do i=1,size/2-1
			sum = sum + 2.0_8*this.func.fArray(2*i) + 4.0_8*this.func.fArray(2*i+1)
		end do
		
		sum = sum + this.func.fArray(size)
		
		output = sum*( this.func.xGrid.stepSize/3.0_8 )
	end function simpsonRule
	
	!>
	!! @brief Extended Simpson rule
	!!
	function extendedSimpsonRule( this ) result( output )
		class(NIntegrator) :: this
		real(8) :: output
		
		real(8) :: sum
		integer :: size
		integer :: i
		
		size = this.func.xGrid.nPoints
		
		sum = 0.0
		do i=5,size-4
			sum = sum + this.func.fArray(i)
		end do
		sum = 48.0_8*sum
		
		sum = sum + 17.0_8*this.func.fArray(1) + 59.0_8*this.func.fArray(2) + 43.0_8*this.func.fArray(3) + 49.0_8*this.func.fArray(4)
		sum = sum + 49.0_8*this.func.fArray(size-3) + 43.0_8*this.func.fArray(size-2) + 59.0_8*this.func.fArray(size-1) + 17.0_8*this.func.fArray(size)
		
		output = sum*( this.func.xGrid.stepSize/48.0_8 )
	end function extendedSimpsonRule
	
	function simpson38Rule( this ) result( output )
		class(NIntegrator) :: this
		real(8) :: output
		
	end function simpson38Rule

	function trapezoidalRule( this ) result( output )
		class(NIntegrator) :: this
		real(8) :: output
		
	end function trapezoidalRule

	function fixedQuadratureRule( this ) result( output )
		class(NIntegrator) :: this
		real(8) :: output
		
	end function fixedQuadratureRule

	function quadratureRule( this ) result( output )
		class(NIntegrator) :: this
		real(8) :: output
		
	end function quadratureRule

	function adaptiveQuadratureRule( this ) result( output )
		class(NIntegrator) :: this
		real(8) :: output
		
	end function adaptiveQuadratureRule
	
	!>
	!! @brief Boole rule
	!!
	function booleRule( this ) result( output )
		class(NIntegrator) :: this
		real(8) :: output
		
		real(8) :: sum
		integer :: size
		integer :: i
		
		size = this.func.xGrid.nPoints
		
		sum = 0.0_8
		
		do i=1,min(size,size-4),4
			sum = sum + 14.0_8*this.func.fArray(i) + 64.0_8*this.func.fArray(i+1) &
			        + 24.0_8*this.func.fArray(i+2) + 64.0_8*this.func.fArray(i+3) + 14.0_8*this.func.fArray(i+4)
		end do
		
! 		sum = this.func.fArray(1)
! 		do i=1,(size-2)/4
! 			sum = sum + 14.0_8*this.func.fArray(4*i-2) + 64.0_8*this.func.fArray(4*i-1) &
! 			        + 24.0_8*this.func.fArray(4*i) + 64.0_8*this.func.fArray(4*i+1) + 14.0_8*this.func.fArray(4*i+2)
! 		end do
		
		output = sum*( this.func.xGrid.stepSize/45.0_8 )
	end function booleRule
	
	!**
	! This is neccesary only for NFunction_test()
	!**
	function funcTest( x ) result( output )
		real(8), intent(in) :: x
		real(8) :: output
		
		output = exp(-0.44*x)*sin(x)**2.0_8
	end function funcTest
	
	subroutine NIntegrator_test()
		type(Grid) :: xGrid
		type(RNFunction) :: nFunc
		type(NIntegrator) :: integrator
		real(8) :: exactValue
		real(8) :: value
		integer :: i
		
		call xGrid.init( 0.0_8, 50.0_8, 102 )
		call xGrid.show()
		
		call nFunc.init( xGrid, funcTest )
		call nFunc.show()
		
		exactValue = 1.083902743303941_8
		
		write(*,*) "SIMPSON"
		write(*,*) "======="
		call integrator.init( nFunc, SIMPSON )
		write(*,'(A,F30.8)') "Exact     = ", exactValue
		write(*,'(A,F30.8)') "Numerical = ", integrator.evaluate()
		write(*,'(A,F27.5,A3)') "Error(%)  = ", 100.0_8*( integrator.evaluate()-exactValue )/exactValue, "%"
		write(*,*) ""
		
		write(*,*) "BOOLE"
		write(*,*) "====="
		call integrator.init( nFunc, BOOLE )
		write(*,'(A,F30.8)') "Exact     = ", exactValue
		write(*,'(A,F30.8)') "Numerical = ", integrator.evaluate()
		write(*,'(A,F27.5,A3)') "Error(%)  = ", 100.0_8*( integrator.evaluate()-exactValue )/exactValue, "%"
		write(*,*) ""
		
	end subroutine NIntegrator_test
	
end module NIntegrator_

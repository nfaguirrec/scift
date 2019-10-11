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

module NDerivator_
	use Math_
	use Grid_
	use RNFunction_
	implicit none
	private
	
	public :: &
		NDerivator_test
		
	type, public :: NDerivator
		type(RNFunction), pointer, private :: func
		integer, private :: nPoints
		
		contains
			procedure :: init
			final :: destroy
			procedure :: str
			procedure :: show
			generic :: evaluate => evaluateAPoint, evaluateOnGrid
			procedure :: evaluateAPoint
			procedure :: evaluateOnGrid
	end type NDerivator
	
	contains
	
	!>
	!! @brief Constructor
	!!
	subroutine init( this, func, nPoints )
		class(NDerivator) :: this 
		type(RNFunction), target, intent(in) :: func
		integer, optional :: nPoints
		
		this%nPoints = 5
		if( present(nPoints) ) then
			this%nPoints = nPoints
		end if
		
		this%func => func
	end subroutine init
	
	!>
	!! @brief Destructor
	!!
	subroutine destroy( this )
		type(NDerivator) :: this
		
		nullify(this%func)
	end subroutine destroy
	
	!>
	!! @brief
	!!
	function str( this ) result( output )
		class(NDerivator) :: this 
		character(len=200) :: output
		
		integer :: fmt
		character(len=200) :: strBuffer
		
		output = ""
		
! 		output = trim(output)//"<NDerivator:"
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
		class(NDerivator) :: this
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
	!! \brief Weights for finite difference derivatives
	!! \author Stamatis Stamatiadis
	!!
	!! \sa  B. Fornberg (1988), Math. Comput. 51(1988), 699-706
	!! \details
	!! Given:
	!! x(0), x(1),..., x(n) : grid points (nonrepeated, otherwise arbitrary)
	!! ksi: point x=ksi at which the approximations are wanted 
	!!      (may, but need not be a grid point)
	!! m : highest order of derivative of interest
	!
	!! weights c_{i,j}^k [c(i,j,k)] such that the approximations 
	!!
	!!  f^{(k)}(ksi) \approx \sum_{j=0}^i c_{i,j}^k f(x(j)),
	!!
	!!  k=0,1,...,m,        i = k, k+1,...,n
	!!
	!!  are all optimal.
	!!
	!!  C(i,j,k) is the weight to be applied at x(j) when the kth derivative is 
	!!  approximated by a stencil extending over x(0), x(1), ..., x(i). (j <= i)
	!!
	!!  The case m=0 provides the fastest way known for polynomial interpolation  
	!!  at a single point.
	!!
	!!  \param[in]  x a zero-based array of reals; the grid points
	!!  \param[in]  ksi a real; point at which the approximations are wanted
	!!  \param[in]  m  an integer; highest order of derivative of interest
	!!  \param[out] c tridimensional zero-based array of reals; 
	!!  C(i,j,k) is the weight to be applied at x(j) when the kth derivative is 
	!!  approximated by a stencil extending over x(0), x(1), ..., x(i). (j <= i)
	!!   
	!!  \return nothing
	!!
	subroutine fornbergWeights( x, ksi, m, c )
		integer, parameter :: dpk = kind(1.d0)
		
		real(dpk), intent(in) :: x(0:)
		real(dpk), intent(in) :: ksi
		integer, intent(in) :: m
		real(dpk), intent(out) :: c(0:,0:,0:)
			
		real (dpk) :: a, b, temp1, temp2, d
		integer :: i, j, k
		integer :: minim
		
		c(0,0,0) = 1.0_dpk
		
		a = 1.0_dpk
		
		do i=1,ubound(x,1)
			minim = min(i,m)
			b = 1.0_dpk
				
			temp1 = x(i) - ksi
				
			do j=0,i-1
				temp2 = x(i) - x(j)
				
				b = b * temp2
				
				if( i <= m ) c(i-1,j,i) = 0.0_dpk
					
				c(i,j,0) = temp1 * c(i-1,j,0) / temp2
					
				do k = 1, minim
					c(i,j,k) = (temp1 * c(i-1,j,k) - k * c(i-1,j,k-1)) / temp2
				enddo
			enddo
		
			d = a / b
			temp1 = d * (ksi - x(i-1))
			
			c(i,i,0) = temp1 * c(i-1,i-1,0)
			
			do k = 1, minim
				c(i,i,k) = d * k * c(i-1,i-1,k-1) + temp1 * c(i-1,i-1,k)
			enddo
			
			a = b
		enddo
		
	end subroutine fornbergWeights
	
	!>
	!! @brief Returns the numeric value of its derivative in the point x
	!!
	!! @input     x  Point where the derivative will be evaluated
	!! @input order  order of the derivative
	!!
	function evaluateAPoint( this, x, order ) result( output )
		class(NDerivator) :: this
		real(8), intent(in) :: x
		integer, intent(in) :: order
		real(8) :: output
		
		integer :: i, ind
		real(8), allocatable :: coeff(:,:,:)
		
		allocate( coeff(this%nPoints,this%nPoints,0:order) )
		
		if( this%func%xGrid%isEquallyspaced ) then
		
			ind = (x-this%func%xGrid%min)/this%func%xGrid%stepSize+1
			
		else
			do i=1,this%func%xGrid%nPoints
				if( x < this%func%xGrid%data(i) ) then
					exit
				end if
			end do
			
			ind = i-1
			
! 			write(6,"(A)") "### ERROR ### Numerical derivatives for non equally spaced grids is not implemented"
! 			stop
		end if
		
		! Diferencias hacia adelante
		if( ind <= this%nPoints/2 ) then
			call fornbergWeights( this%func%xGrid%data(1:this%nPoints), x, order, coeff )
			output = sum( coeff( this%nPoints, :, order )*this%func%fArray(1:this%nPoints) )
		! Diferencias hacia atrás
		else if( ind >= this%func%xGrid%nPoints-this%nPoints/2+1  ) then
			call fornbergWeights( this%func%xGrid%data(this%func%xGrid%nPoints-this%nPoints+1:this%func%xGrid%nPoints), x, order, coeff )
			output = sum( coeff( this%nPoints, :, order )*this%func%fArray(this%func%xGrid%nPoints-this%nPoints+1:this%func%xGrid%nPoints) )
		! Diferencias centradas
		else
			call fornbergWeights( this%func%xGrid%data(ind-this%nPoints/2:ind+this%nPoints/2), x, order, coeff )
			output = sum( coeff( this%nPoints, :, order )*this%func%fArray(ind-this%nPoints/2:ind+this%nPoints/2) )
		end if
		
		deallocate( coeff )
	end function evaluateAPoint
	
	!>
	!! @brief Returns a numerical function (@see RNFunction) which corresponds to its derivative
	!!
	!! @input order order of the derivative
	!!
	function evaluateOnGrid( this, order ) result( output )
		class(NDerivator) :: this
		integer, intent(in) :: order
		type(RNFunction) :: output
		
		real(8), allocatable :: dArray(:)
		integer :: i
		
		allocate( dArray(this%func%xGrid%nPoints) )
		
		do i=1,this%func%xGrid%nPoints
			dArray(i) = this%evaluateAPoint( this%func%xGrid%data(i), order )
		end do
		
		call output%fromGridArray( this%func%xGrid, dArray )
		
		deallocate( dArray )
	end function evaluateOnGrid
	
	!>
	!! This is neccesary only for NFunction_test()
	!!       f = exp(-0.44*x)*sin(x)**2
	!!   df/dx = exp(-0.44*x)*(2.0*sin(x)*cos(x)-0.44*sin(x)**2)
	!! d2f/dx2 = exp(-0.44*x)*(2.0*cos(x)**2 - 1.76*cos(x)*sin(x) - 2.0*sin(x)**2 + 0.1936*sin(x)**2)
	!!
	function funcTest( x ) result( output )
		real(8), intent(in) :: x
		real(8) :: output
		
		output = exp(-0.44*x)*sin(x)**2.0_8
	end function funcTest
	
	!>
	!! This is neccesary only for NFunction_test()
	!!       f = exp(-0.44*x)*sin(x)**2
	!!   df/dx = exp(-0.44*x)*(2.0*sin(x)*cos(x)-0.44*sin(x)**2)
	!! d2f/dx2 = exp(-0.44*x)*(2.0*cos(x)**2 - 1.76*cos(x)*sin(x) - 2.0*sin(x)**2 + 0.1936*sin(x)**2)
	!!
	function dfuncTest( x ) result( output )
		real(8), intent(in) :: x
		real(8) :: output
		
		output = exp(-0.44*x)*(2.0*sin(x)*cos(x)-0.44*sin(x)**2)
	end function dfuncTest
	
	!>
	!! This is neccesary only for NFunction_test()
	!!       f = exp(-0.44*x)*sin(x)**2
	!!   df/dx = exp(-0.44*x)*(2.0*sin(x)*cos(x)-0.44*sin(x)**2)
	!! d2f/dx2 = exp(-0.44*x)*(2.0*cos(x)**2 - 1.76*cos(x)*sin(x) - 2.0*sin(x)**2 + 0.1936*sin(x)**2)
	!!
	function d2funcTest( x ) result( output )
		real(8), intent(in) :: x
		real(8) :: output
		
		output = exp(-0.44*x)*(2.0*cos(x)**2 - 1.76*cos(x)*sin(x) - 2.0*sin(x)**2 + 0.1936*sin(x)**2)
	end function d2funcTest
	
	!>
	!! @bief test
	!!
	subroutine NDerivator_test()
		type(Grid) :: xGrid
		type(RNFunction), target :: func, dFunc, ndFunc
		type(NDerivator) :: derivator
		real(8) :: exactValue
		real(8) :: value
		integer :: i, j
		
		call xGrid%init( 0.0_8, 10.0_8, 101 )
		call func.fromFunction( xGrid, funcTest )
		call dFunc.fromFunction( xGrid, dfuncTest )
		call derivator%init( func )
		
! 		do i=1,xGrid%nPoints
! 			write(*,"(3F15.5)") xGrid%data(i), dnFunc%fArray(i), derivator.evaluate( xGrid%data(i), 2 )
! 		end do
! 		
! 		write(*,*)
! 		write(*,*)
! 		
! 		call nFunc.fromFunction( xGrid, dfuncTest )
! 		do i=1,xGrid%nPoints
! 			write(*,"(3F15.5)") xGrid%data(i), dnFunc%fArray(i), derivator.evaluate( xGrid%data(i), 1 )
! 		end do
		
		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		! Para verificar:
		!   $ make && ./test > salida
		!   gnuplot> plot "salida.dat" w l, "salida1.dat" w l, "salida2.dat" w p pt 7
		! verde y azul deben dar igual
		ndFunc = derivator.evaluate( order=0 )
		call func%save( "salida.dat" )
		call dFunc%save( "salida1.dat" )
		call ndFunc%save( "salida2.dat" )
		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		
	end subroutine NDerivator_test
	
end module NDerivator_

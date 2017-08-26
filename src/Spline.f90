!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!  This file is part of scift (Scientific Fortran Tools).
!!  Copyright (C) by authors (2011-2013)
!!  
!!  Authors (alphabetic order):
!!    * Aguirre N.F. (nfaguirrec@gmail.com)  (2011-2013)
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

module Spline_
	use IOStream_
	use Grid_
	use RNFunction_
	implicit none
	private

#define _XMIN this.nFunc.xGrid.min
#define _XMAX this.nFunc.xGrid.max
#define _X this.nFunc.xGrid.data
#define _Y this.nFunc.fArray
#define _B this.b
#define _N this.size
	
	public :: &
		Spline_test
	
	type, public :: Spline
		integer :: size
		type(RNFunction), pointer :: nFunc
		real(8), allocatable :: b(:)
		
		contains
			procedure :: init
			final :: destroy
			procedure :: str
			procedure :: show
			procedure :: evaluate
			procedure :: smooth
			procedure :: save
	end type Spline
	
	contains
	
	!>
	!! @brief
	!!
	subroutine init( this, nFunc, tol )
		class(Spline) :: this
		class(RNFunction), target, intent(in) :: nFunc
		real(8), optional :: tol
		
		real(8), allocatable :: a(:)
		real(8), allocatable :: c(:)
		real(8), allocatable :: s(:)
		real(8) :: efftol
		real(8) :: t
		real(8) :: yt
		integer :: i
		
		if( present(tol) ) then
			efftol = tol
		else
			efftol = 1.0e-12
		end if
		
! 		call this.nFunc.copy( nFunc )
		this.nFunc => nFunc
		this.size = nFunc.nPoints()
		
		if( allocated(_B) ) deallocate( _B )
		
		allocate( _B(_N) )
		allocate( a(_N) )
		allocate( c(_N) )
		allocate( s(_N) )
		
		_X = nFunc.xGrid.data
		_Y = nFunc.fArray
		
		_B(1) = 0.0_8
		_B(_N) = 0.0_8
		a(1) = _X(2)-_X(1)
		c(1) = (_Y(2)-_Y(1))/a(1)
		
		do i=2, _N-1
			a(i) = _X(i+1)-_X(i)
			c(i) = (_Y(i+1)-_Y(i))/a(i)
			_B(i) = 2.0_8*(c(i)-c(i-1))/(a(i)+a(i-1))
			s(i) = 1.5_8*_B(i)
		end do
		
		do while ( t > efftol )
			t = 0.0_8
			
			! oblige derivee a etre horizontale pour extremitee de la courbe
			! pou  supprimer enlever les deux cartes suivantes
			_B(_N) = 3.d0*(_Y(_N-1)-_Y(_N))/((_X(_N)-_X(_N-1))**2)+_B(_N-1)*0.5d0
			_B(_N) = _B(_N)-_B(_N-1)
			
			do i=2,_N-1
				yt = 0.5_8*((_B(i+1)-_B(i-1))/(1.0_8+a(i)/a(i-1))-_B(i+1))-_B(i)+s(i)
				yt = yt*4.0_8*(2.0_8-dsqrt(3.0_8))
				
				if( abs(yt) > t ) then
					t = abs(yt)
				end if
				
				_B(i)=yt+_B(i)
			end do
			
			_B(_N) = _B(_N)-0.5_8*yt
		end do
		
		deallocate( a )
		deallocate( c )
		deallocate( s )
	end subroutine init
	
	!>
	!! @brief
	!!
	subroutine destroy( this )
		type(Spline) :: this
		
		this.size = 0
		this.nFunc => null()
		if( allocated( this.b ) ) deallocate( this.b )
	end subroutine destroy
	
	!>
	!! @brief
	!!
	function str( this ) result( output )
		class(Spline) :: this 
		character(len=200) :: output
		
		integer :: fmt
		character(len=200) :: strBuffer
		
		output = ""
		
		output = trim(output)//"<Spline:"
		output = trim(output)//this.nFunc.str()
		output = trim(output)//">"
	end function str
	
	!>
	!! @brief
	!!
	subroutine show( this, unit )
		class(Spline) :: this
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
	function evaluate( this, xValue ) result( output )
		class(Spline) :: this 
		real(8), intent(in) :: xValue
		real(8) :: output
		
		integer :: i, k
		real(8) :: xi
		real(8) :: dx, dy, db
		real(8) :: pa, pb, pc, pd
		
		i=0
		do k=1,_N-1
			xi = xValue-_X(k)
			dx = xi*( _X(k+1)-xValue )
			
			if( dx >= 0.0_8 ) then
				i=k
				exit
			end if
		end do
		
		dy = _Y(i+1)-_Y(i)
		dx = _X(i+1)-_X(i)
		db = _B(i+1)-_B(i)
		pa = db/(6.0_8*dx)
		pb = (_B(i)+_B(i)+_B(i+1))/6.0_8
		pc = dy/dx-dx*pb
		pd = _Y(i)
		pb = pb-dx*pa
		
		output = pd+xi*(pc+xi*(pb+xi*pa))
	end function evaluate
	
	!>
	!! @brief
	!!
	function smooth( this, factor ) result( output )
		class(Spline) :: this 
		integer, intent(in) :: factor
		type(RNFunction) :: output
		
		integer :: n
		real(8) :: stepSize
		type(Grid) :: xGrid
		real(8), allocatable :: y(:)
		integer :: i
		
		n = _N*factor
		
		allocate( y(n) )
		
		stepSize = abs(_XMAX-_XMIN)/( n - 1 )
		call xGrid.init( _XMIN, _XMAX, n, stepSize )
		
		do i=1,xGrid.nPoints
			y(i) = this.evaluate( xGrid.data(i) )
		end do
		
		call output.fromGridArray( xGrid, y )
	end function smooth
	
	!>
	!! Save the data in two column format in a
	!! selected unit
	!!
	subroutine save( this, ofile, nPoints )
		class(Spline) :: this
		type(OFStream), optional, intent(in) :: ofile
		integer, optional :: nPoints
		
		integer :: effunit
		integer :: effnPoints
		real(8) :: xi
		real(8) :: stepSize
		integer :: i
		
		if( present(ofile) ) then
			effunit = ofile.unit
		else
			effunit = IO_STDOUT
		end if
		
		if( present(nPoints) ) then
			effnPoints = nPoints
		else
			effnPoints = 100
		end if
		
		write(effunit,"(a)") "#"//trim(this.str())
		write(effunit,"(a)") ""
		write(effunit,"(a)") "# Raw values"
		do i=1,this.size
			write(effunit,"(f15.7,f15.7)") this.nFunc.xGrid.data(i), this.nFunc.fArray(i)
		end do
		
		write(effunit,"(a)") ""
		write(effunit,"(a)") ""
		write(effunit,"(a)") "# Interpolated values"
		
		stepSize = abs(_XMAX-_XMIN)/( effnPoints - 1 )
		
		do i=1,effnPoints
			xi = this.nFunc.xGrid.min + dble(i-1)*stepSize
			write(effunit,"(f15.7,f15.7)") xi, this.evaluate(xi)
		end do

	end subroutine save
	
	!>
	!! @brief
	!!
	subroutine Spline_test()
		type(IFStream) :: ifile
		type(OFStream) :: ofile
		type(RNFunction) :: nFunc
		type(RNFunction) :: nFuncSmooth
		type(Spline) :: nFuncSpline
! 		type(ThrularNumerovMethod) :: solver
		
! 		integer :: i
		
		call ifile.init( "morse.dat" )
		call nFunc.fromFStream( ifile )
		call nFunc.show()
		call ifile.close()
		
		call nFuncSpline.init( nFunc )
		call nFuncSpline.show()
		
		call ofile.init( "spline.out" )
		call nFuncSpline.save( ofile )
		call ofile.close()
		
		nFuncSmooth = nFuncSpline.smooth( 10 )
		
		call ofile.init( "smooth.out" )
! 		call nFuncSmooth.toFStream( ofile )
		call ofile.close()
		
! 		call solver.init( nFuncSmooth, rMass=0.5_8*34.9689_8*amu )
! 		call solver.run()
! 		
! 		stop
! 		
! 		write(*,"(a5,a20,a20)") "\nu", "eigenValue"
! 		do i=1,solver.nStates
! 			if ( solver.eigenValue(i) < 0.0_8 ) then
! 					write(*,"(i5,f20.10)") i, solver.eigenValue(i)
! 			end if
! 		end do
		
	end subroutine Spline_test
	
end module Spline_

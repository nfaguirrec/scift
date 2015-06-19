!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!  This file is part of scift (Scientific Fortran Tools).
!!  Copyright (C) by authors (2010-2014)
!!  
!!  Authors (alphabetic order):
!!    * Aguirre N.F. (nfaguirrec@gmail.com)  (2010-2014)
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

module Morse_
	use IOStream_
	use UnitsConverter_
	use Grid_
	use RNFunction_
	use ThrularNumerovMethod_
	implicit none
	private
	
	public :: &
		Morse_test
	
	type, public, extends( RNFunction ):: Morse
		real(8) :: De
		real(8) :: alpha
		real(8) :: Re
		
		contains
			generic :: init => initDefaultMorse
			procedure :: initDefaultMorse
			procedure :: fromExp
			procedure :: destroy
			procedure :: parent
			procedure :: exactEigenValue
			procedure :: str
			procedure :: show
			procedure :: evaluate
	end type Morse
	
	contains
	
	!>
	!! @brief Constructor
	!!
	subroutine initDefaultMorse( this, rGrid, Re, De, alpha )
		class(Morse) :: this
		type(Grid), intent(in) :: rGrid
		real(8), intent(in) :: Re
		real(8), intent(in) :: De
		real(8), intent(in) :: alpha
		
		integer :: i
		real(8) :: r
		real(8), allocatable :: V(:)
		
		this.De = De
		this.alpha = alpha
		this.Re = Re
		
		allocate( V(rGrid.nPoints) )
		do i=1,rGrid.nPoints
			V(i) = this.evaluate( rGrid.data(i) )
		end do
		
		call this.fromGridArray( rGrid, V )
		deallocate( V )
	end subroutine initDefaultMorse
		
	!>
	!! @brief Constructor
	!! De = 0.25*ωe²/ωexe
	!! α = (ωe/ħ)*sqrt(m/2De)
	!!
	subroutine fromExp( this, rGrid, Re, we, wexe, rMass )
		class(Morse) :: this
		type(Grid), intent(in) :: rGrid
		real(8), intent(in) :: Re
		real(8), intent(in) :: we
		real(8), intent(in) :: wexe
		real(8), intent(in) :: rMass
		
		integer :: i
		real(8) :: r
		real(8), allocatable :: V(:)
		
		this.Re = Re
		this.De = 0.25_8*we**2.0_8/wexe
		this.alpha = we*sqrt(0.5*rMass/this.De)
		
		allocate( V(rGrid.nPoints) )
		do i=1,rGrid.nPoints
			V(i) = this.evaluate( rGrid.data(i) )
		end do
		
		call this.fromGridArray( rGrid, V )
		deallocate( V )
	end subroutine fromExp
	
	!>
	!! @brief Destructor
	!!
	subroutine destroy( this )
		class(Morse) :: this
		
	end subroutine destroy
	
	!>
	!! @brief Convert the object in its
	!!        parent object
	!!
	function parent( this ) result( output )
		class(Morse) :: this
		type(RNFunction) :: output
		
		call output.fromGridArray( this.xGrid, this.fArray )
	end function parent
	
	!>
	!! @brief String representation of
	!!        the object
	!!
	function str( this ) result( output )
		class(Morse) :: this 
		character(len=200) :: output
		
		integer :: fmt
		character(len=200) :: strBuffer
		
		output = ""
		
		write(strBuffer, "(a)") "<Morse:"
		output = trim(output)//trim(strBuffer)
		
		output = trim(output)//"Re="
		fmt = int(log10(this.Re+1.0))+1
		write(strBuffer, "(f<fmt+7>.6)") this.Re
		output = trim(output)//trim(strBuffer)
		
		output = trim(output)//",De="
		fmt = int(log10(this.De+1.0))+1
		write(strBuffer, "(f<fmt+7>.6)") this.De
		output = trim(output)//trim(strBuffer)
		
		output = trim(output)//",alpha="
		fmt = int(log10(this.alpha+1.0))+1
		write(strBuffer, "(f<fmt+7>.6)") this.alpha
		output = trim(output)//trim(strBuffer)
		
		output = trim(output)//">"
	end function str
	
	!>
	!! @brief Write the string
	!! representation of the object in a
	!! selected unit
	!!
	subroutine show( this, unit )
		class(Morse) :: this
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
	!! @brief Evaluates the function
	!! @param r
	!!
	function evaluate( this, r ) result( output )
		class( Morse ), intent(in) :: this
		real(8), intent(in) :: r
		real(8) :: output
		
		output = this.De*( exp(2.0_8*this.alpha*(this.Re-r)) &
					- 2.0_8*exp(this.alpha*(this.Re-r)) )
	end function evaluate
	
	!>
	!! @brief Returns the eigenvalue with the
	!!        quantum number as nu
	!!
	function exactEigenValue( this, nu, rMass ) result( output )
		class( Morse ), intent(in) :: this
		integer, intent(in) :: nu
		real(8), intent(in) :: rMass
		real(8) :: output
		
		real(8) :: we
		real(8) :: wexe
		
		we = this.alpha*sqrt(2.0_8*this.De/rMass)
		wexe = 0.25_8*we**2.0_8/this.De
		
		output = we*(dble(nu)+0.5_8)-wexe*(dble(nu)+0.5_8)**2.0_8-this.De
	end function exactEigenValue
	
	!>
	!! @brief Write the string
	!! representation of the object in a
	!! selected unit
	!!
	subroutine Morse_test()
		real(8) :: Re, we, wexe, rMass
		type(Morse) :: morse
		type(Grid) :: rGrid
		type(ThrularNumerovMethod) :: solver
		type(RNFunction) :: nf
		type(OFStream) :: ofile
		integer :: i ! dummy variable
		
		!! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		!! Cl2 X¹Σg+ 
		!! NIST Standard Reference Data Program
		!! -------------------------------------
		!! re    = 1.9879 Å
		!! ωe    = 559.72 cm⁻¹
		!! ωexe  = 2.675 cm⁻¹
		!! m(Cl) = 35.4257 amu
		!! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		Re = 1.9879_8*angs
		we = 559.72_8*cm1
		wexe = 2.675_8*cm1
		rMass = 0.5_8*35.4257_8*amu
		
		call rGrid.init( 0.5_8, 20.0_8, 10000 )
		call morse.fromExp( rGrid, Re, we, wexe, rMass )
		call morse.show()
		
		call solver.init( morse.parent(), 10, rMass )
		call solver.run()
		
		write(*,"(a5,a20,a20)") "\nu", "exact", "numeric"
		do i=1,solver.nStates
			if ( solver.eigenValue(i) < 0.0_8 ) then
					write(*,"(i5,f20.10,f20.10)") i, morse.exactEigenValue(i-1, rMass), solver.eigenValue(i)
			end if
		end do
		
		call solver.eigenFunction(7).save( "salida" )
		
		call morse.destroy()
		call solver.destroy()
		
	end subroutine Morse_test
	
end module Morse_

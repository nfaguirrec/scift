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

!*
! @brief
!*
module Atom_
	use AtomicElementsDB_
	
	implicit none
	private
	
	public :: &
		Atom_test
	
	type, public :: Atom
		character(3) :: symbol  !< Atomic symbol
		real(8), pointer :: x !< x coordinate
		real(8), pointer :: y !< y coordinate
		real(8), pointer :: z !< z coordinate
		
		real(8) :: r(3) !< position vector
		
		contains
			generic :: init => initDefault
			generic :: assignment(=) => copyAtom
			procedure :: initDefault
			procedure :: copyAtom
			final :: destroyAtom
			procedure :: str
			procedure :: show
			
			procedure :: covalentRadius
			procedure :: mass
			procedure :: atomicNumber
			procedure :: isConnectedWith
	end type Atom
	
	contains
	
	!>
	!! @brief Default constructor
	!!
	!! Build an atom by choosing its symbol and coordinates
	!!
	!! @param symbol Atomic symbol
	!! @param x x coordinate for the atom
	!! @param y y coordinate for the atom
	!! @param z z coordinate for the atom
	!!
	!! @code
	!!	type(Atom) :: atm
	!!	call atm.init( " He", 0.156_8, 1.456_8, 0.725_8 )
	!! @endcode
	!!
	subroutine initDefault( this, symbol, x, y, z )
		class(Atom), target :: this 
		
		character(*), optional, intent(in) :: symbol
		real(8), optional, intent(in) :: x
		real(8), optional, intent(in) :: y
		real(8), optional, intent(in) :: z
		
		this.symbol = "X"
		if( present(symbol) ) this.symbol = adjustl(trim(symbol))
		
		if( present(x) ) this.r(1) = x
		if( present(y) ) this.r(2) = y
		if( present(z) ) this.r(3) = z
		
		this.x => this.r(1)
		this.y => this.r(2)
		this.z => this.r(3)
	end subroutine initDefault
	
	!>
	!! @brief Copy constructor
	!!
	!! Build an atom taking all parameters from other
	!!
	!! @param other source atom
	!!
	subroutine copyAtom( this, other )
		class(Atom), target, intent(inout) :: this
		class(Atom), intent(in) :: other
		
		this.symbol = other.symbol
		
		this.r = other.r
		
		this.x => this.r(1)
		this.y => this.r(2)
		this.z => this.r(3)
	end subroutine copyAtom
	
	!>
	!! @brief Destructor
	!!
	subroutine destroyAtom( this )
		type(Atom) :: this
		
		if( associated(this.x) ) nullify(this.x)
		if( associated(this.y) ) nullify(this.y)
		if( associated(this.z) ) nullify(this.z)
	end subroutine destroyAtom
	
	!>
	!! @brief Convert to string
	!!
	function str( this, formatted, prefix ) result( output )
		class(Atom) :: this 
		character(:), allocatable :: output
		logical, optional :: formatted
		character(*), optional :: prefix
		
		logical :: effFormatted
		character(:), allocatable :: effPrefix
		
		integer :: fmt
		character(200) :: fstr
		
		effFormatted = .false.
		if( present(formatted) ) effFormatted = formatted
		
		effPrefix = ""
		if( present(prefix) ) effPrefix = prefix
		
		output = ""
		
		if( .not. effFormatted ) then
			output = trim(output)//"<Atom:"
		
#define RFMT(v) int(log10(max(abs(v),1.0)))+merge(1,2,v>=0)
#define ITEMS(l,v) output = trim(output)//effPrefix//trim(l)//trim(adjustl(v))
#define ITEMI(l,v) output = trim(output)//l; fmt = RFMT(v); write(fstr, "(i<fmt>)") v; output = trim(output)//trim(fstr)
#define ITEMR(l,v) output = trim(output)//l; fmt = RFMT(v); write(fstr, "(f<fmt+7>.6)") v; output = trim(output)//trim(fstr)
		
			ITEMS( "symbol=", this.symbol )
			ITEMR( ",x=", this.x )
			ITEMR( ",y=", this.y )
			ITEMR( ",z=", this.z )
#undef RFMT
#undef ITEMS
#undef ITEMI
#undef ITEMR
			output = trim(output)//">"
		else
		
#define LINE(l) output = trim(output)//effPrefix//l//new_line('')
#define ITEMS(l,v) output = trim(output)//effPrefix//l; write(fstr, "(a10)") trim(v); output = trim(output)//trim(fstr)//new_line('')
#define ITEMI(l,v) output = trim(output)//effPrefix//l; write(fstr, "(i10)") v; output = trim(output)//trim(fstr)//new_line('')
#define ITEMR(l,v) output = trim(output)//effPrefix//l; write(fstr, "(f10.5)") v; output = trim(output)//trim(fstr)//new_line('')

			LINE("Atom")
			LINE("----")
			ITEMS( "symbol  =", this.symbol )
			ITEMR( "     x  =", this.x )
			ITEMR( "     y  =", this.y )
			ITEMR( "     z  =", this.z )
#undef LINE
#undef ITEMS
#undef ITEMI
#undef ITEMR
		end if
		
	end function str
	
	!>
	!! @brief Show 
	!!
	subroutine show( this, unit, formatted )
		class(Atom) :: this
		integer, optional, intent(in) :: unit
		logical, optional :: formatted
		
		integer :: effunit
		logical :: effFormatted
		
		effFormatted = .false.
		if( present(formatted) ) effFormatted = formatted
		
		effunit = 6
		if( present(unit) ) effunit = unit
		
		write(effunit,"(a)") trim(str(this,effFormatted))
	end subroutine show
	
	!>
	!! @brief
	!!
	function isConnectedWith( this, other, alpha ) result( output )
		class(Atom), intent(in) :: this
		class(Atom), intent(in) :: other
		real(8), optional :: alpha
		logical :: output
		
		real(8) :: dist, cutoff
		real(8) :: effAlpha
		
		effAlpha = 1.0_8
		if( present(alpha) ) effAlpha = alpha
		
		dist = sqrt(sum((this.r - other.r)**2))
		cutoff = AtomicElementsDB_instance.covalentRadius( this.symbol ) + AtomicElementsDB_instance.covalentRadius( other.symbol )
		
		if( dist <= effAlpha*cutoff .and. dist > 0.01_8 ) then
				output = .true.
		else
				output = .false.
		end if
	end function isConnectedWith
	
	!>
	!! @brief
	!!
	pure function covalentRadius( this ) result( output )
		class(Atom), intent(in) :: this
		real(8) :: output
		
		output = AtomicElementsDB_instance.covalentRadius( this.symbol )
	end function covalentRadius
	
	!>
	!! @brief
	!!
	pure function mass( this ) result( output )
		class(Atom), intent(in) :: this
		real(8) :: output
		
		output = AtomicElementsDB_instance.atomicMass( this.symbol )
	end function mass
	
	!>
	!! @brief
	!!
	pure function atomicNumber( this ) result( output )
		class(Atom), intent(in) :: this
		real(8) :: output
		
		output = AtomicElementsDB_instance.atomicNumber( this.symbol )
	end function atomicNumber
	
	!>
	!! @test Testing the Atom class
	!! @brief Testing the Atom class
	!!
	subroutine Atom_test()
		
		type(Atom) :: atom0, atom1
		character(10) :: buffer
		
		call atom0.init()
		call atom0.show()
		
		call atom0.init( " He", 0.156_8, 1.456_8, 0.725_8 )
		call atom0.show()
		call atom0.show( formatted=.true. )
		write(*,*) atom0.r
		
		buffer = "  He  "
		call atom0.init( trim(buffer), 0.156_8, 1.456_8, 0.725_8 )
		call atom0.show()
		call atom0.show( formatted=.true. )
		write(*,*) atom0.r
		
		write(*,*) "Testing copy constructor"
		write(*,*) "========================"
		write(*,*) "atom1 = atom0"
		atom1 = atom0
		call atom1.show()
		write(*,*) atom1.r
		
		write(*,*) ""
		write(*,*) "Cambiando r en atom1"
		write(*,*) "===================="
		write(*,*) "atom1.r <== [ 2.000_8, 1.000_8, 0.000_8 ]"
		atom1.r = [ 2.000_8, 1.000_8, 0.000_8 ]
		write(*,"(A,3F10.5)") "atom1.r     = ", atom1.r
		write(*,"(A,3F10.5)") "atom1.x,y,z = ", atom1.x, atom1.y, atom1.z
		write(*,"(A,3F10.5)") "atom0.r     = ", atom0.r
		write(*,"(A,3F10.5)") "atom0.x,y,z = ", atom0.x, atom0.y, atom0.z
		
		write(*,*) ""
		write(*,*) "Cambiando x en atom1"
		write(*,*) "===================="
		write(*,*) "atom1.x <== -1.000_8"
		atom1.x = -1.000_8
		write(*,"(A,3F10.5)") "atom1.r     = ", atom1.r
		write(*,"(A,3F10.5)") "atom1.x,y,z = ", atom1.x, atom1.y, atom1.z
		write(*,"(A,3F10.5)") "atom0.r     = ", atom0.r
		write(*,"(A,3F10.5)") "atom0.x,y,z = ", atom0.x, atom0.y, atom0.z
	end subroutine Atom_test
	
end module Atom_

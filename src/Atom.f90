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

!>
!! @brief
!!
module Atom_
	use AtomicElementsDB_
	use UnitsConverter_
	
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
			
			procedure :: radius
			procedure :: mass
			procedure :: massNumber
			procedure :: atomicNumber
			procedure :: color
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
	function isConnectedWith( this, other, alpha, radiusType, distance ) result( output )
		class(Atom), intent(in) :: this
		class(Atom), intent(in) :: other
		real(8), optional :: alpha
		integer, optional :: radiusType
		real(8), optional, intent(out) :: distance
		logical :: output
		
		real(8) :: dist, cutoff
		real(8) :: effAlpha
		integer :: i, loc
		
		effAlpha = 1.0_8
		if( present(alpha) ) effAlpha = alpha
		
		dist = sqrt(sum((this.r - other.r)**2))
		cutoff = AtomicElementsDB_instance.radius( this.symbol, type=radiusType ) + AtomicElementsDB_instance.radius( other.symbol, type=radiusType )
		
		if( allocated(AtomicElementsDB_instance.specialPairs) ) then
			loc = -1
			do i=1,size(AtomicElementsDB_instance.specialPairs)
				if( ( trim(this.symbol) == AtomicElementsDB_instance.specialPairs(i).symbol1 &
						.and. trim(other.symbol) == AtomicElementsDB_instance.specialPairs(i).symbol2 ) &
					.or. ( trim(this.symbol) == AtomicElementsDB_instance.specialPairs(i).symbol2 &
						.and. trim(other.symbol) == AtomicElementsDB_instance.specialPairs(i).symbol1 ) ) then
						loc = i
						exit
				end if
			end do
			
			if( loc /= -1 ) then
				if( dist < AtomicElementsDB_instance.specialPairs(i).bondCutoff ) then
					output = .true.
				else
					output = .false.
				end if
			end if
		else
			if( dist <= effAlpha*cutoff .and. dist > 0.01_8 ) then
				output = .true.
			else
				output = .false.
			end if
		end if
		
		if( present(distance) ) distance = dist
	end function isConnectedWith
	
	!>
	!! @brief
	!!
	pure function radius( this, type ) result( output )
		class(Atom), intent(in) :: this
		integer, optional, intent(in) :: type
		real(8) :: output
		
		output = AtomicElementsDB_instance.radius( this.symbol, type=type )
	end function radius
	
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
	pure function massNumber( this ) result( output )
		class(Atom), intent(in) :: this
		real(8) :: output
		
		output = AtomicElementsDB_instance.atomicMassNumber( this.symbol )
	end function massNumber
	
	!>
	!! @brief
	!!
	pure function atomicNumber( this ) result( output )
		class(Atom), intent(in) :: this
		real(8) :: output
		
		output = AtomicElementsDB_instance.atomicNumber( this.symbol )
	end function atomicNumber
	
	!>
	!! @brief
	!!
	pure function color( this ) result( output )
		class(Atom), intent(in) :: this
		character(6) :: output
		
		output = AtomicElementsDB_instance.color( this.symbol )
	end function color
	
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

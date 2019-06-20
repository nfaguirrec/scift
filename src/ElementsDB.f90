!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                                                                                   !!
!!  This file is part of SciFT project                                               !!
!!  Copyright (c) 2015-2015 Nestor F. Aguirre (nfaguirrec@gmail.com)                 !!
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
module ElementsDB_
	use String_
	use UnitsConverter_
	
	implicit none
	private
	
	public :: &
		ElementsDB_test
		
	type, public :: Isotope
		integer :: massNumber
		integer :: abundance
		integer :: atomicMass
	end type Isotope
		
	!>
	!! @brief Chemical element.
	!! 
	!! Attributes
	!! ----------
	!! atomicNumber : int
	!!     Atomic number
	!! symbol : str of length 1 or 2
	!!     Chemical symbol
	!! name : str
	!!     Name in english
	!! group : int
	!!     Group in periodic table
	!! period : int
	!!     Period in periodic table
	!! block : int
	!!     Block in periodic table
	!! series : int
	!!     Index to chemical series
	!! protons : int
	!!     Number of protons
	!! neutrons : int
	!!     Number of neutrons in the most abundant naturally occurring stable
	!!     isotope
	!! nominalmass : int
	!!     Mass number of the most abundant naturally occurring stable isotope
	!! electrons : int
	!!     Number of electrons
	!! mass : float
	!!     Relative atomic mass. Ratio of the average mass of atoms
	!!     of the element to 1/12 of the mass of an atom of 12C
	!! exactmass : float
	!!     Relative atomic mass calculated from the isotopic composition
	!! electronegativity : float
	!!     Electronegativity (Pauling scale)
	!! covalentRadius : float
	!!     Covalent radius in Angstrom
	!! atomicRadius :
	!!     Atomic radius in Angstrom
	!! vdWRadius : float
	!!     Van der Waals radius in Angstrom
	!! boilingTemperature : float
	!!     Boiling temperature in K
	!! meltingTemperature : float
	!!     Melting temperature in K
	!! density : float
	!!     Density at 295K in g/cm3 respectively g/L
	!! oxidationStates : str
	!!     Oxidation states
	!! electronAffinity : float
	!!     Electron affinity in eV
	!! electronicConfiguration : str
	!!     Ground state electron configuration
	!! ionizationEnergies : tuple
	!!     Ionization energies in eV
	!! isotopes : dict
	!!     Isotopic composition.
	!!     keys: isotope mass number
	!!     values: Isotope(relative atomic mass, abundance)
	!!
	type, public :: Element
		integer :: atomicNumber
		character(3) :: symbol
		character(10) :: name
		real(8) :: atomicWeight
		integer :: mostStableIsotopeMassNumber
		type(Isotope), allocatable :: isotopes(:)   !< keys are massNumbers
	end type Element
	
	type, public :: ElementsDB
		type(Element), allocatable :: elements(:)
		
		contains
			generic :: init => initElementsDB
			generic :: assignment(=) => copyElementsDB
			
			procedure :: initElementsDB
			procedure :: copyElementsDB
			final :: destroyElementsDB
			procedure :: str
			procedure :: show
			
			procedure :: nElements
			
			generic :: atomicMass => atomicMassFromAtomicNumber, atomicMassFromSymbol
			procedure :: atomicMassFromAtomicNumber
			procedure :: atomicMassFromSymbol
	end type ElementsDB
	
	contains
	
	!>
	!! @brief Constructor
	!!
	subroutine initElementsDB( this )
		class(ElementsDB) :: this 
		
		type(Element) :: elem
		type(Isotope), allocatable :: isot(:)
		
		elem.atomicNumber = 1
		elem.symbol = "H"
		elem.name = "Hydrogen"
		elem.atomicWeight = 1.00794
		elem.mostStableIsotopeMassNumber = 1
		allocate( isot(1:7) )
			isot(1).massNumber = 1
			isot(1).abundance  = 0.999885
			isot(1).atomicMass = 1.00782503207
			
			isot(2).massNumber = 2
			isot(2).abundance  = 0.000115
			isot(2).atomicMass = 2.0141017778
		deallocate( isot )
	end subroutine initElementsDB
	
	!>
	!! @brief Copy constructor
	!!
	subroutine copyElementsDB( this, other )
		class(ElementsDB), intent(inout) :: this
		class(ElementsDB), intent(in) :: other

	end subroutine copyElementsDB
	
	!>
	!! @brief Destructor
	!!
	subroutine destroyElementsDB( this )
		type(ElementsDB) :: this
		
	end subroutine destroyElementsDB
	
	!>
	!! @brief Convert to string
	!!
	function str( this, formatted, prefix ) result( output )
		class(ElementsDB) :: this 
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
#define ITEMS(l,v) output = trim(output)//effPrefix//trim(l)//trim(adjustl(v))
#define ITEMI(l,v) output = trim(output)//l; write(fstr, "(I20)") v; output = trim(output)//trim(adjustl(fstr))
#define ITEMR(l,v) output = trim(output)//l; write(fstr, "(F20.6)") v; output = trim(output)//trim(adjustl(fstr))
#define ITEML(l,v) output = trim(output)//l; write(fstr, "(L3)") v; output = trim(output)//trim(adjustl(fstr))
		
			output = trim(output)//"<ElementsDB:"
! 			ITEMI( "min=", this.min )
! 			ITEMR( ",size=", this.size )
#undef ITEMS
#undef ITEMI
#undef ITEMR
#undef ITEML
			output = trim(output)//">"
		else
#define LINE(l) output = trim(output)//effPrefix//l//new_line('')
#define ITEMS(l,v) output = trim(output)//effPrefix//l; write(fstr, "(x,a)") trim(v); output = trim(output)//trim(fstr)//new_line('')
#define ITEMI(l,v) output = trim(output)//effPrefix//l; write(fstr, "(i20)") v; output = trim(output)//trim(fstr)//new_line('')
#define ITEMR(l,v) output = trim(output)//effPrefix//l; write(fstr, "(f20.6)") v; output = trim(output)//trim(fstr)//new_line('')

			LINE("ElementsDB")
			LINE("---------")
! 			ITEMI( "min=", this.min )
! 			ITEMR( ",size=", this.size )
			LINE("")
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
		class(ElementsDB) :: this
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
	pure function nElements( this ) result( output ) 
		class(ElementsDB), intent(in) :: this
		integer :: output
		
		output = size( this.elements )
	end function nElements
	
	!>
	!! @brief
	!!
	pure function atomicMassFromAtomicNumber( this, atomicNumber ) result( atomicMass ) 
		class(ElementsDB), intent(in) :: this
		integer, intent(in) :: atomicNumber
		real(8) :: atomicMass
		
		atomicMass = this.elements( atomicNumber ).isotopes( this.elements( atomicNumber ).mostStableIsotopeMassNumber ).atomicMass*amu
	end function atomicMassFromAtomicNumber
	
	!>
	!! @brief
	!!
	function atomicMassFromSymbol( this, symbol ) result( atomicMass ) 
		class(ElementsDB), intent(in) :: this
		character(*), intent(in) :: symbol
		real(8) :: atomicMass
		
		integer :: atomicNumber
		character(255) :: upperSymb
		character(255) :: upperSymbInt
		
		upperSymb = FString_toUpper( symbol )
		
		atomicMass = -1.0_8
		do atomicNumber=1,this.nElements()
			upperSymbInt = FString_toUpper( this.elements(atomicNumber).symbol )
			if( trim(adjustl(upperSymb)) == trim(adjustl(upperSymbInt)) ) then
				atomicMass = this.elements( atomicNumber ).isotopes( this.elements( atomicNumber ).mostStableIsotopeMassNumber ).atomicMass*amu
				exit
			end if
		end do
	end function atomicMassFromSymbol
	
	!>
	!! @brief Test method
	!!
	subroutine ElementsDB_test()
! 		type(Element) :: a
! 		type(Isotope) :: b
! 		
! 		a = ElementsDB_instance.get( 16 )
! 		write(*,*) "atomic mass = ", a.mass()
! 		write(*,*) "electrons = ", a.electrons()
! 		write(*,*) "symbol = ", a.symbol()
! 		write(*,*) "name = ", a.name()
! 		write(*,*) "covalentRadius = ", a.covalentRadius()
! 		write(*,*) "atomicRadius = ", a.atomicRadius()
! 		write(*,*) "vdWRadius = ", a.vdWRadius()
! 		write(*,*) "electronegativity = ", a.property( "electronegativity" )
! 		write(*,*) "electronegativity = ", a.property( "electronegativity" )
! 		write(*,*) "isotope = ", a.property( "electronegativity" )
! 		
! 		b = a.isotope( 18 )
		
	end subroutine ElementsDB_test
	
end module ElementsDB_

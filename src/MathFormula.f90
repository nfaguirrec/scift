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
module MathFormula_
	use String_
	
	use MathParser_
	
	implicit none
	private
	
	public :: &
		MathFormula_test
	
	type, public :: MathFormula
		type(MathParser) :: parser
		type(String) :: formula
		character(100), allocatable :: variables(:)
		
		character(100), allocatable, private :: constants(:)
		real(8), allocatable, private :: constantsValues(:)
		
		contains
			generic :: init => initFromCharacterVec, initFromFString
			procedure :: initFromCharacterVec
			procedure :: initFromFString
			generic :: assignment(=) => copyMathFormula
			
			procedure :: copyMathFormula
			final :: destroyMathFormula
			procedure :: str
			procedure :: show
			
			procedure :: nVariables
			procedure :: nConstants
			procedure :: evaluate
	end type MathFormula
	
	contains
	
	!>
	!! @brief Constructor
	!!
	subroutine initFromCharacterVec( this, formula, variables, constants, constantsValues )
		class(MathFormula) :: this
		character(*) :: formula
		character(*), optional :: variables(:)
		character(*), optional :: constants(:)
		real(8), optional :: constantsValues(:)
		
		integer :: i
		
		call this%parser%init()
		
		this%formula = formula
		
		if( allocated( this%variables ) ) deallocate( this%variables )
		allocate( this%variables(size(variables)) )
		do i=1,size(variables)
			this%variables(i) = trim(variables(i))
		end do
		
		if( present(constants) .and. present(constantsValues) ) then
			if( allocated( this%constants ) ) deallocate( this%constants )
			allocate( this%constants(size(constants)) )
			this%constants = constants
			
			if( allocated( this%constantsValues ) ) deallocate( this%constantsValues )
			allocate( this%constantsValues(size(constantsValues)) )
			this%constantsValues = constantsValues
			
			call this%parser%parseFunction( formula, [variables,constants] )
		else
			call this%parser%parseFunction( formula, this%variables )
		end if
		
	end subroutine initFromCharacterVec
	
	!>
	!! @brief Constructor
	!!
	subroutine initFromFString( this, formula, variables, constants, constantsValues )
		class(MathFormula) :: this
		character(*) :: formula
		character(*) :: variables
		character(*), optional :: constants(:)
		real(8), optional :: constantsValues(:)
		
		integer :: i
		character(100), allocatable :: tokens(:)
		
		call this%parser%init()
		
		this%formula = formula
		
		call FString_split( variables, tokens, "," )
		
		if( allocated( this%variables ) ) deallocate( this%variables )
		allocate( this%variables(size(tokens)) )
		do i=1,size(tokens)
			this%variables(i) = trim(tokens(i))
		end do
		
		if( present(constants) .and. present(constantsValues) ) then
			if( allocated( this%constants ) ) deallocate( this%constants )
			allocate( this%constants(size(constants)) )
			this%constants = constants
			
			if( allocated( this%constantsValues ) ) deallocate( this%constantsValues )
			allocate( this%constantsValues(size(constantsValues)) )
			this%constantsValues = constantsValues
			
			call this%parser%parseFunction( formula, [tokens,constants] )
		else
			call this%parser%parseFunction( formula, this%variables )
		end if
		
		deallocate( tokens )

	end subroutine initFromFString
	
	!>
	!! @brief Copy constructor
	!!
	subroutine copyMathFormula( this, other )
		class(MathFormula), intent(inout) :: this
		class(MathFormula), intent(in) :: other
		
		this%parser = other%parser
		this%formula = other%formula
		
		if( allocated(this%variables) ) deallocate(this%variables)
		allocate(this%variables(other%nVariables()))
		this%variables = other%variables
		
		if( allocated(this%constants) ) deallocate(this%constants)
		allocate(this%constants(other%nConstants()))
		this%constants = other%constants
		
		if( allocated(this%constantsValues) ) deallocate(this%constantsValues)
		allocate(this%constantsValues(other%nConstants()))
		this%constantsValues = other%constantsValues
	end subroutine copyMathFormula
	
	!>
	!! @brief Destructor
	!!
	subroutine destroyMathFormula( this )
		type(MathFormula) :: this
		
		if( allocated(this%variables) ) deallocate(this%variables)
		if( allocated( this%constants ) ) deallocate( this%constants )
		if( allocated( this%constantsValues ) ) deallocate( this%constantsValues )
	end subroutine destroyMathFormula
	
	!>
	!! @brief Convert to string
	!!
	function str( this, formatted, prefix ) result( output )
		class(MathFormula) :: this 
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
		
			output = trim(output)//"<MathFormula:"
! 			ITEMI( "min=", this%min )
! 			ITEMR( ",size=", this%size )
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

			LINE("MathFormula")
			LINE("---------")
! 			ITEMI( "min=", this%min )
! 			ITEMR( ",size=", this%size )
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
		class(MathFormula) :: this
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
	pure function nVariables( this ) result( output )
		class(MathFormula), intent(in) :: this
		integer :: output
		
		output = size(this%variables)
	end function nVariables
	
	!>
	!! @brief
	!!
	pure function nConstants( this ) result( output )
		class(MathFormula), intent(in) :: this
		integer :: output
		
		output = size(this%constants)
	end function nConstants
	
	!>
	!! @brief
	!!
	pure function evaluate( this, values ) result( output )
		class(MathFormula), intent(in) :: this
		real(8), intent(in) :: values(:)
		real(8) :: output
		
		output = this%parser%evaluateFunction( [ values, this%constantsValues ] )
	end function evaluate
	
	!>
	!! @brief Test method
	!!
	subroutine MathFormula_test()
		type(MathFormula) :: formula
		character(100) :: cVars(3)
		real(8) :: cVals(3)
		
		cVars = [ "pi", "c", "a" ]
		cVals = [ 3.141592_8, 137.0_8, 1.0_8 ]
		
		call formula%init( "0.5*x**2+y**2+a**2+c*pi", variables="x,y", constants=cVars, constantsValues=cVals )
		call formula%show()
		
		write(*,*) "val = ", formula.evaluate( [0.5_8,0.6_8] )
	end subroutine MathFormula_test
	
end module MathFormula_

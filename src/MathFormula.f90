!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!  This file is part of scift (Scientific Fortran Tools).
!!  Copyright (C) by authors (2015-2015)
!!  
!!  Authors (alphabetic order):
!!    * Aguirre N.F. (nfaguirrec@gmail.com)  (2015-2015)
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
		
		call this.parser.init()
		
		this.formula = formula
		
		if( allocated( this.variables ) ) deallocate( this.variables )
		allocate( this.variables(size(variables)) )
		do i=1,size(variables)
			this.variables(i) = trim(variables(i))
		end do
		
		if( present(constants) .and. present(constantsValues) ) then
			if( allocated( this.constants ) ) deallocate( this.constants )
			allocate( this.constants(size(constants)) )
			this.constants = constants
			
			if( allocated( this.constantsValues ) ) deallocate( this.constantsValues )
			allocate( this.constantsValues(size(constantsValues)) )
			this.constantsValues = constantsValues
			
			call this.parser.parseFunction( formula, [variables,constants] )
		else
			call this.parser.parseFunction( formula, this.variables )
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
		
		call this.parser.init()
		
		this.formula = formula
		
		call FString_split( variables, tokens, "," )
		
		if( allocated( this.variables ) ) deallocate( this.variables )
		allocate( this.variables(size(tokens)) )
		do i=1,size(tokens)
			this.variables(i) = trim(tokens(i))
		end do
		
		if( present(constants) .and. present(constantsValues) ) then
			if( allocated( this.constants ) ) deallocate( this.constants )
			allocate( this.constants(size(constants)) )
			this.constants = constants
			
			if( allocated( this.constantsValues ) ) deallocate( this.constantsValues )
			allocate( this.constantsValues(size(constantsValues)) )
			this.constantsValues = constantsValues
			
			call this.parser.parseFunction( formula, [tokens,constants] )
		else
			call this.parser.parseFunction( formula, this.variables )
		end if
		
		deallocate( tokens )

	end subroutine initFromFString
	
	!>
	!! @brief Copy constructor
	!!
	subroutine copyMathFormula( this, other )
		class(MathFormula), intent(inout) :: this
		class(MathFormula), intent(in) :: other
		
		this.parser = other.parser
		this.formula = other.formula
		
		if( allocated(this.variables) ) deallocate(this.variables)
		allocate(this.variables(other.nVariables()))
		this.variables = other.variables
		
		if( allocated(this.constants) ) deallocate(this.constants)
		allocate(this.constants(other.nConstants()))
		this.constants = other.constants
		
		if( allocated(this.constantsValues) ) deallocate(this.constantsValues)
		allocate(this.constantsValues(other.nConstants()))
		this.constantsValues = other.constantsValues
	end subroutine copyMathFormula
	
	!>
	!! @brief Destructor
	!!
	subroutine destroyMathFormula( this )
		type(MathFormula) :: this
		
		if( allocated(this.variables) ) deallocate(this.variables)
		if( allocated( this.constants ) ) deallocate( this.constants )
		if( allocated( this.constantsValues ) ) deallocate( this.constantsValues )
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

			LINE("MathFormula")
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
		
		output = size(this.variables)
	end function nVariables
	
	!>
	!! @brief
	!!
	pure function nConstants( this ) result( output )
		class(MathFormula), intent(in) :: this
		integer :: output
		
		output = size(this.constants)
	end function nConstants
	
	!>
	!! @brief
	!!
	pure function evaluate( this, values ) result( output )
		class(MathFormula), intent(in) :: this
		real(8), intent(in) :: values(:)
		real(8) :: output
		
		output = this.parser.evaluateFunction( [ values, this.constantsValues ] )
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
		
		call formula.init( "0.5*x**2+y**2+a**2+c*pi", variables="x,y", constants=cVars, constantsValues=cVals )
		call formula.show()
		
		write(*,*) "val = ", formula.evaluate( [0.5_8,0.6_8] )
	end subroutine MathFormula_test
	
end module MathFormula_

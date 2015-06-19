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
module GaborTransform_
	use RNFunction_
	use CNFunction_
	use RNFunction2D_
	use CNFunction2D_
	implicit none
	private
	
	public :: &
		GaborTransform_fgt, &
		GaborTransform_test
	
	type, public :: GaborTransform
		real(8) :: val
		
		contains
			generic :: init => initGaborTransform
			generic :: assignment(=) => copyGaborTransform
			
			procedure :: initGaborTransform
			procedure :: copyGaborTransform
			final :: destroyGaborTransform
			procedure :: str
			procedure :: show
	end type GaborTransform
	
    interface GaborTransform_fgt
	    module procedure GaborTransform_fgt_RNFunction
        module procedure GaborTransform_fgt_CNFunction
    end interface GaborTransform_fgt
	
	contains
	
	!>
	!! @brief Constructor
	!!
	subroutine initGaborTransform( this )
		class(GaborTransform) :: this 
		
	end subroutine initGaborTransform
	
	!>
	!! @brief Copy constructor
	!!
	subroutine copyGaborTransform( this, other )
		class(GaborTransform), intent(inout) :: this
		class(GaborTransform), intent(in) :: other

		this.val = other.val
	end subroutine copyGaborTransform
	
	!>
	!! @brief Destructor
	!!
	subroutine destroyGaborTransform( this )
		type(GaborTransform) :: this
		
	end subroutine destroyGaborTransform
	
	!>
	!! @brief Convert to string
	!!
	function str( this, formatted, prefix ) result( output )
		class(GaborTransform) :: this 
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
		
			output = trim(output)//"<GaborTransform:"
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

			LINE("GaborTransform")
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
		class(GaborTransform) :: this
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
	!! 
	!!
	function GaborTransform_fgt_RNFunction( iFunc, sgn ) result( oFunc )
		type(RNFunction) :: iFunc
		integer, optional :: sgn
		type(RNFunction2D) :: oFunc
		
! 		integer :: n
! 		real(8) :: dx, dp
! 		type(Grid) :: xGrid
! 		
! 		! @todo Check for checkEquallyspaced
! 		n = iFunc.nPoints()
! 		dx = iFunc.xGrid.stepSize
! 		dp = 2.0_8*Math_PI/dx/real(n,8)
! 		
! 		oFunc = iFunc
! 		call FourierTransform_dft( iFunc.yArray, oFunc.yArray, sgn=FourierTransform_FORWARD )
! 		oFunc.xGrid = FourierTransform_omegaGrid( n, dx, order=FourierTransform_SORDER )
! 		
! 		call FourierTransform_phase( oFunc )
! 		call FourierTransform_shift( oFunc )
! 		
! 		oFunc = oFunc*dx/sqrt(2.0_8*Math_PI)
	end function GaborTransform_fgt_RNFunction
	
	!>
	!! 
	!!
	function GaborTransform_fgt_CNFunction( iFunc, sgn ) result( oFunc )
		type(CNFunction) :: iFunc
		integer, optional :: sgn
		type(CNFunction2D) :: oFunc
		
! 		integer :: n
! 		real(8) :: dx, dp
! 		type(Grid) :: xGrid
! 		
! 		! @todo Check for checkEquallyspaced
! 		n = iFunc.nPoints()
! 		dx = iFunc.xGrid.stepSize
! 		dp = 2.0_8*Math_PI/dx/real(n,8)
! 		
! 		oFunc = iFunc
! 		call FourierTransform_dft( iFunc.yArray, oFunc.yArray, sgn=FourierTransform_FORWARD )
! 		oFunc.xGrid = FourierTransform_omegaGrid( n, dx, order=FourierTransform_SORDER )
! 		
! 		call FourierTransform_phase( oFunc )
! 		call FourierTransform_shift( oFunc )
! 		
! 		oFunc = oFunc*dx/sqrt(2.0_8*Math_PI)
	end function GaborTransform_fgt_CNFunction
	
	!>
	!! @brief Test method
	!!
	subroutine GaborTransform_test()
		
	end subroutine GaborTransform_test
	
end module GaborTransform_

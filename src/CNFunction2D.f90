!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!  This file is part of scift (Scientific Fortran Tools).
!!  Copyright (C) by authors (2013-2015)
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
module CNFunction2D_
	use GOptions_
	use Math_
	use String_
	use IOStream_
	use RNFunction_
	use Grid2D_
	use RNFunction2D_
	implicit none
	private
	
	public :: &
		CNFunction2D_test
	
!>
!! This class use the List template declared into List.h90 file,
!! please take a look to this file for details
!!
#define NFunction2D CNFunction2D
#define __TYPE_VALUE__ complex(8)
#define __ADD_METHODS__ \
	procedure :: fromRNFunction2D
#define __ID_TYPE__ 1
#include "NFunction2D.h90"
#undef NFunction2D
#undef __TYPE_VALUE__
#undef __ADD_METHODS__
#undef __ID_TYPE__

	!>
	!! @brief 
	!!
	subroutine fromRNFunction2D( this, nfunc )
		class(CNFunction2D) :: this
		class(RNFunction2D) :: nfunc
		
		write(*,*) "### ERROR ### Function CNFunction2D::fromRNFunction2D is not implemented yet"
		stop
	end subroutine fromRNFunction2D
	
	!>
	!! @brief Convert to string
	!!
	function str( this, formatted, prefix ) result( output )
		class(CNFunction2D) :: this 
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
#define ITEMI(l,v) output = trim(output)//l; write(fstr, "(i20)") v; output = trim(output)//trim(adjustl(fstr))
#define ITEMR(l,v) output = trim(output)//l; write(fstr, "(f20.6)") v; output = trim(output)//trim(adjustl(fstr))
		
			output = trim(output)//"<CNFunction2D:"
			output = trim(output)//trim(this.xyGrid.str())
! 			ITEMI( "min=", this.min )
			ITEMI( ",sizex=", this.nPoints(1) )
			ITEMI( ",sizey=", this.nPoints(2) )
#undef RFMT
#undef ITEMS
#undef ITEMI
#undef ITEMR
			output = trim(output)//">"
		else
#define LINE(l) output = trim(output)//effPrefix//l//new_line('')
#define ITEMS(l,v) output = trim(output)//effPrefix//l; write(fstr, "(x,a)") trim(v); output = trim(output)//trim(fstr)//new_line('')
#define ITEMI(l,v) output = trim(output)//effPrefix//l; write(fstr, "(i10)") v; output = trim(output)//trim(fstr)//new_line('')
#define ITEMR(l,v) output = trim(output)//effPrefix//l; write(fstr, "(f10.5)") v; output = trim(output)//trim(fstr)//new_line('')

			LINE("CNFunction2D")
			LINE("---------")
! 			ITEMI( "min=", this.min )
! 			ITEMR( ",size=", this.nPoints )
			LINE("")
#undef LINE
#undef ITEMS
#undef ITEMI
#undef ITEMR
		end if
	end function str
	
	!>
	!! Save the data in two column format in a
	!! selected unit
	!!
	subroutine toFStream( this, ofile )
		class(CNFunction2D) :: this
		type(OFStream), optional, intent(in) :: ofile
		
		integer :: unitEff
		
		if( present(ofile) ) then
			unitEff = ofile.unit
		else
			unitEff = STDOUT
		end if
		
		write(*,*) "### ERROR ### CNFunction2D.toFStream is no implemented yet"
		stop
	end subroutine toFStream

	!>
	!! @brief Show 
	!!
	subroutine show( this, unit, formatted )
		class(CNFunction2D) :: this
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
	!! This is neccesary only for NFunction_test()
	!!
	pure function funcTest( x, y ) result( output )
		real(8), intent(in) :: x, y
		complex(8) :: output
		
		output = sin( Math_PI*sqrt(x**2+y**2) ) /sqrt(x**2+y**2)  ! Mexican Hat function
	end function funcTest
	
	!>
	!! @test Testing the CNFunction2D class
	!! @brief Testing the CNFunction2D class
	!!
	subroutine CNFunction2D_test()
		type(CNFunction2D) :: func, func2
		
		real(8) :: xVec(3)
		real(8) :: yVec(2)
		complex(8) :: fArray(3,2)
		
		type(Grid2D) :: xyGrid
	
		xVec(:) = [ 1.0, 2.0, 3.0 ]
		yVec(:) = [-1.0, 0.0 ]
		
		fArray(1,:) = [ 0.0, 4.0 ]
		fArray(2,:) = [ 2.0, 2.0 ]
		
		fArray(1,:) = [ 6.0,-1.0 ]
		fArray(2,:) = [ 7.0, 0.0 ]
		
		write(*,*) ""
		write(*,*) "----------------------------"
		write(*,*) "Testing constructors"
		write(*,*) "----------------------------"
		
		call func.init( xVec, yVec, fArray )
		call func.show()
		
		call xyGrid.fromArray( xVec, yVec )
		call func.init( xyGrid, fArray )
		call func.show()
		
		call xyGrid.init( min=[-5.0_8,-5.0_8], max=[5.0_8,5.0_8], size=[100,100] )
		call func.init( xyGrid, funcTest )
		call func.show()
		
		call func.init( "data/formats/complex-N2DF", format=N2DF_FORMAT )
		call func.show()
		
		write(*,*) ""
		write(*,*) "----------------------------"
		write(*,*) "Testing copy constructors"
		write(*,*) "----------------------------"
		
		func2 = func
		call func2.show()
		
		write(*,*) ""
		write(*,*) "----------------------------"
		write(*,*) "Testing I/O methods"
		write(*,*) "----------------------------"
		
		call func.save( "salida.n2df", format=N2DF_FORMAT )
		call func.load( "salida.n2df", format=N2DF_FORMAT )
		call func.show()
		
		write(*,*) ""
		write(*,*) "----------------------------"
		write(*,*) "Testing resize(+3,+2)"
		write(*,*) "----------------------------"
		
		call func.init( xVec, yVec, fArray )
		call func.show()
		call func.resize( 3, 2, +1, +1 )
		call func.show()
				
	end subroutine CNFunction2D_test
	
end module CNFunction2D_

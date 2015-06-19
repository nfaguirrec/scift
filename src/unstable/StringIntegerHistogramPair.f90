!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!  This file is part of scift (Scientific Fortran Tools).
!!  Copyright (C) by authors (2012-2013)
!!  
!!  Authors (alphabetic order):
!!    * Aguirre N.F. (nfaguirrec@gmail.com)  (2014-2014)
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

module StringIntegerHistogramPair_
	use String_
	use IntegerHistogram_
	implicit none
	private
	
	public :: &
		StringIntegerHistogramPair_test
	
#define Pair StringIntegerHistogramPair
#define __CLASS_ITEMFIRST__ class(String)
#define __TYPE_ITEMFIRST__  type(String)
#define __CLASS_ITEMSECOND__ class(IntegerHistogram)
#define __TYPE_ITEMSECOND__  type(IntegerHistogram)
#include "Pair.h90"
#undef __CLASS_ITEMFIRST__
#undef __TYPE_ITEMFIRST__
#undef __CLASS_ITEMSECOND__
#undef __TYPE_ITEMSECOND__
#undef Pair
	
	!>
	!! @brief Convert to string
	!!
	function str( this, formatted, prefix ) result( output )
		class(StringIntegerHistogramPair) :: this 
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
#define RFMT(v) int(log10(max(real(abs(v),8),1.0)))+merge(1,2,v>=0)
#define ITEMS(l,v) output = trim(output)//effPrefix//trim(l)//trim(adjustl(v))
#define ITEMI(l,v) output = trim(output)//l; fmt = RFMT(v); write(fstr, "(i<fmt>)") v; output = trim(output)//trim(fstr)
#define ITEMR(l,v) output = trim(output)//l; fmt = RFMT(v); write(fstr, "(f<fmt+7>.6)") v; output = trim(output)//trim(fstr)
		
			output = trim(output)//"<Pair:"
			ITEMS( "first=", this.first.fstr )
			ITEMI( ",second=", this.second.size() )
#undef RFMT
#undef ITEMS
#undef ITEMI
#undef ITEMR
			output = trim(output)//">"
! 		else
! #define LINE(l) output = trim(output)//effPrefix//l//new_line('')
! #define ITEMS(l,v) output = trim(output)//effPrefix//l; write(fstr, "(x,a)") trim(v); output = trim(output)//trim(fstr)//new_line('')
! #define ITEMI(l,v) output = trim(output)//effPrefix//l; write(fstr, "(i10)") v; output = trim(output)//trim(fstr)//new_line('')
! #define ITEMR(l,v) output = trim(output)//effPrefix//l; write(fstr, "(f10.5)") v; output = trim(output)//trim(fstr)//new_line('')
! 
! 			LINE("Pair")
! 			LINE("---------")
! ! 			ITEMI( "min=", this.min )
! ! 			ITEMR( ",size=", this.size )
! 			LINE("")
! #undef LINE
! #undef ITEMS
! #undef ITEMI
! #undef ITEMR
		end if
	end function str
	
	!>
	!! @brief Test method
	!!
	subroutine StringIntegerHistogramPair_test()
		type(String) :: str
		type(IntegerHistogram) :: hist
		type(StringIntegerHistogramPair) :: mypair1
		type(StringIntegerHistogramPair) :: mypair2
		
! 		integer :: i
		
		write(*,*) "------------------------------"
		write(*,*) "Testing for empty constructor"
		write(*,*) "-----------------------------"
		
		write(*,*) "call mypair1.init( str, hist )"
		
		call hist.init( STURGES )
		call hist.add( [24, 19, 27, 23, 25, 25, 23, 22] )
		
		str = "Hola"
		mypair1 = StringIntegerHistogramPair( str, hist )
		call mypair1.show()
		
		write(*,*) "call mypair2.init( str, hist )"
		
		str = "Entonces"
		call hist.add( [23, 25, 28, 22, 19] )
		mypair2 = StringIntegerHistogramPair( str, hist )
		call mypair2.show()
		
		write(*,*) "------------------------------"
		write(*,*) "Testing for copy constructor"
		write(*,*) "-----------------------------"
		
		write(*,*) "mypair1 = mypair2"
		
		mypair1 = mypair2
		call mypair1.show()
	end subroutine StringIntegerHistogramPair_test

end module StringIntegerHistogramPair_

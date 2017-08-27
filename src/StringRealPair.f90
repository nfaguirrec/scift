!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                                                                                   !!
!!  This file is part of SciFT project                                               !!
!!  Copyright (c) 2012-2013 Nestor F. Aguirre (nfaguirrec@gmail.com)                 !!
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

module StringRealPair_
	use String_
	implicit none
	private
	
	public :: &
		StringRealPair_test
	
#define Pair StringRealPair
#define __CLASS_ITEMFIRST__ class(String)
#define __TYPE_ITEMFIRST__  type(String)
#define __CLASS_ITEMSECOND__ real(8)
#define __TYPE_ITEMSECOND__  real(8)
#define __ADD_ATTRIBUTES__
#define __ADD_METHODS__
#include "Pair.h90"
#undef Pair
#undef __CLASS_ITEMFIRST__
#undef __TYPE_ITEMFIRST__
#undef __CLASS_ITEMSECOND__
#undef __TYPE_ITEMSECOND__
#undef __ADD_ATTRIBUTES__
#undef __ADD_METHODS__
	
	!>
	!! @brief Convert to string
	!!
	function str( this, formatted, prefix ) result( output )
		class(StringRealPair) :: this 
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
			ITEMI( ",second=", this.second )
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
	subroutine StringRealPair_test()
		type(String) :: str
		type(StringRealPair) :: mypair1
		type(StringRealPair) :: mypair2
		
		integer :: i
		
		write(*,*) "------------------------------"
		write(*,*) "Testing for empty constructor"
		write(*,*) "-----------------------------"
		
		write(*,*) "call mypair1.init( str, 3 )"
		
		str = "Hola"
! 		call mypair1.init( str, 3 )
		mypair1 = StringRealPair( str, 3.0_8 )
		call mypair1.show()
		
		write(*,*) "call mypair2.init( str, 4 )"
		
		str = "Entonces"
! 		call mypair2.init( str, 4 )
		mypair1 = StringRealPair( str, 4.0_8 )
		call mypair2.show()
		
		write(*,*) "------------------------------"
		write(*,*) "Testing for copy constructor"
		write(*,*) "-----------------------------"
		
		write(*,*) "mypair1 = mypair2"
		
		mypair1 = mypair2
		call mypair1.show()
	end subroutine StringRealPair_test

end module StringRealPair_

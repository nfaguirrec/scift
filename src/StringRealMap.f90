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

module StringRealMap_
	use IOStream_
	use String_
	use StringRealPair_
	use StringRealPairList_, StringRealMapIterator => StringRealPairListIterator
	implicit none
	private
	
	public :: StringRealMapIterator
	
	public :: &
		StringRealMap_test
		
#define Map StringRealMap
! #define __CLASS_ITEMMAP__ class(String)
#define __CLASS_MAPITERATOR__  class(StringRealMapIterator)
#define __MAPLIST__            StringRealPairList
#define __TYPE_MAPLIST__       type(StringRealPairList)
#define __TYPE_MAPPAIR__       type(StringRealPair)
#define __CLASS_MAPPAIR__      class(StringRealPair)
! #define __CLASS_MAPKEY__       class(String)
#define __CLASS_MAPVALUE__     real(8)
#define __TYPE_MAPVALUE__     real(8)
#define __ADD_METHODS__
#include "Map.h90"
#undef __CLASS_MAPITERATOR__
#undef __MAPLIST__
#undef __TYPE_MAPLIST__
#undef __TYPE_MAPPAIR__
#undef __CLASS_MAPPAIR__
! #undef __CLASS_MAPKEY__
#undef __CLASS_MAPVALUE__
#undef __TYPE_MAPVALUE__
#undef __ADD_METHODS__
#undef Map
	
	!>
	!! @brief Convert to string
	!!
	function str( this, formatted, prefix ) result( output )
		class(StringRealMap) :: this 
		character(:), allocatable :: output
		logical, optional :: formatted
		character(*), optional :: prefix
		
		logical :: effFormatted
		character(:), allocatable :: effPrefix
		
		class(StringRealMapIterator), pointer :: iter
		type(StringRealPair) :: pair
		integer :: fmt
		character(200) :: fstr
		
		effFormatted = .false.
		if( present(formatted) ) effFormatted = formatted
		
		effPrefix = ""
		if( present(prefix) ) effPrefix = prefix
		
		output = ""
		
		if( .not. effFormatted ) then
#define RFMT(v) int(log10(max(abs(v),1.0)))+merge(1,2,v>=0)
#define ITEMR(l,v) output = trim(output)//l; fmt = RFMT(v); write(fstr, "(f<fmt+7>.6)") v; output = trim(output)//trim(fstr)
#define ITEMI(l,v) output = trim(output)//l; write(fstr,*) v; output = trim(output)//trim(adjustl(fstr))
		
			output = trim(output)//"<Map:"
			
			ITEMI( "size=", this.size() )
#undef RFMT
#undef ITEMR
#undef ITEMI
			output = trim(output)//">"
! 		else
! #define LINE(l) output = trim(output)//effPrefix//l//new_line('')
! #define ITEMS(l,v) output = trim(output)//effPrefix//l; write(fstr, "(x,a)") trim(v); output = trim(output)//trim(fstr)//new_line('')
! #define ITEMI(l,v) output = trim(output)//effPrefix//l; write(fstr, "(i10)") v; output = trim(output)//trim(fstr)//new_line('')
! #define ITEMR(l,v) output = trim(output)//effPrefix//l; write(fstr, "(f10.5)") v; output = trim(output)//trim(fstr)//new_line('')
! 
! 			LINE("Map")
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
	!! Save the data in two column format in a
	!! selected unit
	!!
	subroutine toFStream( this, ofile )
		class(StringRealMap) :: this
		type(OFStream), optional, intent(in) :: ofile
		
		integer :: unitEff
		integer :: maxLen
		
		class(StringRealMapIterator), pointer :: iter
		type(StringRealPair) :: pair
		
		if( present(ofile) ) then
			unitEff = ofile.unit
		else
			unitEff = IO_STDOUT
		end if
		
		maxLen = 0
		iter => this.begin
		do while( associated(iter) )
			pair = this.pair( iter )
			if( len(pair.first.fstr) > maxLen ) maxLen = len(pair.first.fstr)
			
			iter => iter.next
		end do
		
		write(unitEff,"(a)") "#"//trim(str(this))
		
		iter => this.begin
		do while( associated(iter) )
			pair = this.pair( iter )
			write(unitEff,"(A<maxLen>,F15.7)") pair.first.fstr, pair.second
			
			iter => iter.next
		end do
	end subroutine toFStream
	
	subroutine showMyMap( mymap )
		type(StringRealMap) :: mymap
		
		class(StringRealMapIterator), pointer :: iter
		type(StringRealPair) :: pair
		
		iter => mymap.begin
		do while( associated(iter) )
			pair = mymap.pair( iter )
			write(*,"(I20,A15,F10.2)") pair.first.hashKey(), pair.first.fstr, pair.second
			
			iter => iter.next
		end do
		write(*,*)
	end subroutine
	
	!>
	!! @brief Test method
	!!
	subroutine StringRealMap_test()
		use TestUtils_
		type(String) :: str
		type(StringRealPair) :: pair
		type(StringRealMap) :: mymap
		class(StringRealMapIterator), pointer :: ptr
		
		mymap = StringRealMap()
		call assert_equal( mymap%size(), 0, "StringRealMap_test: size empty" )
		
		str = "Ademas"
		call mymap.insert( str, 45.0_8 )
		call assert_equal( mymap%size(), 1, "StringRealMap_test: size after 1 insert" )
		call assert_true( abs(mymap%at( str ) - 45.0_8) < 1e-12_8, "StringRealMap_test: value of Ademas" )
		
		str = "Amor"
		call mymap.insert( str, 3.0_8 )
		call assert_equal( mymap%size(), 2, "StringRealMap_test: size after 2 inserts" )
		call assert_true( abs(mymap%at( str ) - 3.0_8) < 1e-12_8, "StringRealMap_test: value of Amor" )
		
		str = "Entonces"
		call mymap.insert( str, 8.0_8 )
		
		str = "Corazon"
		call mymap.insert( str, 9.0_8 )
		
		str = "Hola"
		call mymap.insert( str, 4.0_8 )
		
		str = "Conejo"
		call mymap.insert( str, 24.0_8 )
		
		call assert_equal( mymap%size(), 6, "StringRealMap_test: size after 6 inserts" )
		
		str = "Corazon"
		call assert_true( mymap.find( str, ptr ), "StringRealMap_test: find Corazon" )
		pair = mymap.pair( ptr )
		call assert_equal( pair%first%fstr, "Corazon", "StringRealMap_test: key of Corazon" )
		call assert_true( abs(pair%second - 9.0_8) < 1e-12_8, "StringRealMap_test: value of Corazon" )
		
		str = "Corazon "
		call assert_true( .not. mymap.find( str, ptr ), "StringRealMap_test: find Corazon with trailing space" )
		
		str = "Conejo"
		call mymap.erase( str )
		call assert_equal( mymap%size(), 5, "StringRealMap_test: size after erase Conejo" )
		
		str = "Entonces"
		call mymap.erase( str )
		
		str = "Amor"
		call mymap.erase( str )
		call assert_equal( mymap%size(), 3, "StringRealMap_test: size after erase and 3 left" )
		
		call mymap.clear()
		call assert_equal( mymap%size(), 0, "StringRealMap_test: size after clear" )
		
		str = "Ademas"
		call mymap.insert( str, 45.0_8 )
		
		str = "Amor"
		call mymap.insert( str, 3.0_8 )
		
		str = "Amor"
		call mymap.set( str, 8.0_8 )
		call assert_true( abs(mymap%at( str ) - 8.0_8) < 1e-12_8, "StringRealMap_test: value of Amor after set" )
		
		str = "Hola"
		call mymap.set( str, 56.0_8 )
		call assert_true( abs(mymap%at( str ) - 56.0_8) < 1e-12_8, "StringRealMap_test: value of Hola after set non-existent" )
		call assert_equal( mymap%size(), 3, "StringRealMap_test: size after set non-existent" )
		
		call mymap.clear()
		
		str = "HHHHHHH"
		call mymap.set( str, 56.0_8 )
		call assert_true( abs(mymap%at( str ) - 56.0_8) < 1e-12_8, "StringRealMap_test: set HHHHHHH" )
		
		str = "HHHHHHH"
		call mymap.insert( str, 60.0_8 )
		call assert_true( abs(mymap%at( str ) - 60.0_8) < 1e-12_8, "StringRealMap_test: insert HHHHHHH duplicates key but updates" )
		
	end subroutine StringRealMap_test

end module StringRealMap_

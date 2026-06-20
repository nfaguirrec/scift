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

module StringIntegerPairList_
	use IOStream_
	use String_
	use StringIntegerPair_
	implicit none
	private
	
	public :: &
		StringIntegerPairList_test
	
!>
!! This class use the List template declared into List.h90 file,
!! please take a look to this file for details
!!
#define List StringIntegerPairList
#define ListIterator StringIntegerPairListIterator
#define __CLASS_ITEMLIST__ class(StringIntegerPair)
#define __TYPE_ITEMLIST__ type(StringIntegerPair)
#include "List.h90"
#undef List
#undef ListIterator
#undef __CLASS_ITEMLIST__
#undef __TYPE_ITEMLIST__
	
	!>
	!! @brief Converts to string
	!!
	function str( this, formatted, prefix ) result( output )
		class(StringIntegerPairList) :: this 
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
#define RFMT(v) int(log10(max(abs(v),1.0)))+merge(1,2,v>=0)
#define ITEMS(l,v) output = trim(output)//effPrefix//trim(l)//trim(adjustl(v))
#define ITEMI(l,v) output = trim(output)//l; fmt = RFMT(v); write(fstr, "(i<fmt>)") v; output = trim(output)//trim(fstr)
#define ITEMR(l,v) output = trim(output)//l; fmt = RFMT(v); write(fstr, "(f<fmt+7>.6)") v; output = trim(output)//trim(fstr)
		
			output = trim(output)//"<StringIntegerPairList:"
! 			ITEMI( "min=", this.min )
! 			ITEMR( ",size=", this.size )
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
! 			LINE("List")
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
		class(StringIntegerPairList) :: this
		type(OFStream), optional, intent(in) :: ofile
		
		integer :: unitEff
		
		type(StringIntegerPairListIterator), pointer :: iter
		type(StringIntegerPair) :: pair
		
		if( present(ofile) ) then
			unitEff = ofile.unit
		else
			unitEff = IO_STDOUT
		end if
		
		write(unitEff,"(a)") "#"//trim(str(this))
		
		iter => this.begin
		do while ( associated(iter) )
			pair = iter.data
			write(unitEff,"(A15,I15)") pair.first.fstr, pair.second
			
			iter => iter.next
		end do
	end subroutine toFStream
	
	!>
	!! @brief Test method
	!!
	subroutine StringIntegerPairList_test()
		use TestUtils_
		type(String) :: str
		type(StringIntegerPair) :: mypair
		type(StringIntegerPairList) :: mylist
		class(StringIntegerPairListIterator), pointer :: iter
		class(StringIntegerPairListIterator), pointer :: iterPos
		
		mylist = StringIntegerPairList()
		call assert_equal( mylist%size(), 0, "StringIntegerPairList_test: empty size" )
		
		str = "Hello"
		mypair = StringIntegerPair( str, 3 )
		call mylist.append( mypair )
		
		str = "class"
		mypair = StringIntegerPair( str, 2 )
		call mylist.append( mypair )
		
		str = "string"
		mypair = StringIntegerPair( str, 6 )
		call mylist.append( mypair )
		
		str = "list"
		mypair = StringIntegerPair( str, 9 )
		call mylist.append( mypair )
		
		call assert_equal( mylist%size(), 4, "StringIntegerPairList_test: size after append" )
		
		str = "day"
		mypair = StringIntegerPair( str, 3 )
		call mylist.prepend( mypair )
		
		str = "control"
		mypair = StringIntegerPair( str, 2 )
		call mylist.prepend( mypair )
		
		call assert_equal( mylist%size(), 6, "StringIntegerPairList_test: size after prepend" )
		
		! List should be: control, day, Hello, class, string, list
		mypair = mylist.at( mylist.begin )
		call assert_equal( mypair%first%fstr, "control", "StringIntegerPairList_test: at begin key" )
		call assert_equal( mypair%second, 2, "StringIntegerPairList_test: at begin val" )
		
		mypair = mylist.at( 1 )
		call assert_equal( mypair%first%fstr, "control", "StringIntegerPairList_test: at 1 key" )
		call assert_equal( mypair%second, 2, "StringIntegerPairList_test: at 1 val" )
		
		iter => mylist.begin
		iter => iter.next
		iter => iter.next
		iterPos => iter ! Should point to "Hello", 3
		
		mypair = mylist.at( iterPos )
		call assert_equal( mypair%first%fstr, "Hello", "StringIntegerPairList_test: at iterPos key" )
		call assert_equal( mypair%second, 3, "StringIntegerPairList_test: at iterPos val" )
		
		mypair = mylist.at( mylist.end )
		call assert_equal( mypair%first%fstr, "list", "StringIntegerPairList_test: at end key" )
		call assert_equal( mypair%second, 9, "StringIntegerPairList_test: at end val" )
		
		mypair = mylist.at( mylist.size() )
		call assert_equal( mypair%first%fstr, "list", "StringIntegerPairList_test: at size key" )
		
		str = "Prueba"
		mypair = StringIntegerPair( str, 15 )
		call mylist.insert( iterPos, mypair ) ! Inserts at iterPos (before Hello)
		call assert_equal( mylist%size(), 7, "StringIntegerPairList_test: size after insert" )
		
		mypair = mylist.at( 4 )
		call assert_equal( mypair%first%fstr, "Prueba", "StringIntegerPairList_test: inserted key" )
		call assert_equal( mypair%second, 15, "StringIntegerPairList_test: inserted val" )
		
		! iterPos still points to "Hello", which is now at index 4
		call mylist.erase( iterPos )
		call assert_equal( mylist%size(), 6, "StringIntegerPairList_test: size after erase" )
		
		mypair = mylist.at( 4 )
		call assert_equal( mypair%first%fstr, "class", "StringIntegerPairList_test: key after erase" )
		
		call mylist.clear()
		call assert_equal( mylist%size(), 0, "StringIntegerPairList_test: size after clear" )
		
		str = "Hello1 aaaaaa"
		mypair = StringIntegerPair( str, 21 )
		call mylist.append( mypair )
		
		str = "Hello2 bbbbb ccccc"
		mypair = StringIntegerPair( str, 31 )
		call mylist.append( mypair )
		
		call assert_equal( mylist%size(), 2, "StringIntegerPairList_test: size after second append" )
		
		call mylist.clear()
		call assert_equal( mylist%size(), 0, "StringIntegerPairList_test: size after second clear" )
		
	end subroutine StringIntegerPairList_test

end module StringIntegerPairList_

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

module StringList_
	use String_
	use IOStream_
	implicit none
	private
	
	public :: &
		StringList_test

!>
!! This class use the List template declared into List.h90 file,
!! please take a look to this file for details
!!
#define List StringList
#define ListIterator StringListIterator
#define __CLASS_ITEMLIST__ class(String)
#define __TYPE_ITEMLIST__ type(String)
#include "List.h90"
#undef List
#undef ListIterator
#undef __CLASS_ITEMLIST__
#undef __TYPE_ITEMLIST__
	
	!>
	!! @brief Converts to string
	!!
	function str( this, formatted, prefix ) result( output )
		class(StringList) :: this 
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
		
			output = trim(output)//"<StringList:"
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
		class(StringList) :: this
		type(OFStream), optional, intent(in) :: ofile
		
		integer :: unitEff
		
		type(StringListIterator), pointer :: iter
		
		if( present(ofile) ) then
			unitEff = ofile.unit
		else
			unitEff = IO_STDOUT
		end if
		
		write(unitEff,"(a)") "#"//trim(str(this))
		
		iter => this.begin
		do while ( associated(iter) )
			write(unitEff,"(A15)") iter.data.fstr
			
			iter => iter.next
		end do
	end subroutine toFStream
	
	subroutine showMyList( mylist )
		type(StringList) :: mylist
		class(StringListIterator), pointer :: iter
		
		iter => mylist.begin
		do while( associated(iter) )
			write(*,"(10A,A)", advance="no") iter.data.fstr, " --> "
			
			iter => iter.next
		end do
		write(*,*)
	end subroutine showMyList
	
	!>
	!! @brief Test method
	!!
	subroutine StringList_test()
		use TestUtils_
		type(String) :: str
		type(StringList) :: mylist
		class(StringListIterator), pointer :: iter
		class(StringListIterator), pointer :: iterPos
		
		mylist = StringList()
		call assert_equal( mylist%size(), 0, "StringList_test: empty size" )
		
		str = "Hello"
		call mylist.append( str )
		str = "class"
		call mylist.append( str )
		str = "string"
		call mylist.append( str )
		str = "list"
		call mylist.append( str )
		
		call assert_equal( mylist%size(), 4, "StringList_test: size after append" )
		
		str = "day"
		call mylist.prepend( str )
		str = "control"
		call mylist.prepend( str )
		
		call assert_equal( mylist%size(), 6, "StringList_test: size after prepend" )
		
		! List: control, day, Hello, class, string, list
		str = mylist.at( mylist.begin )
		call assert_equal( str%fstr, "control", "StringList_test: at begin" )
		
		str = mylist.at( 1 )
		call assert_equal( str%fstr, "control", "StringList_test: at 1" )
		
		iter => mylist.begin
		iter => iter.next
		iter => iter.next
		iterPos => iter ! Should point to "Hello" (index 3)
		
		str = mylist.at( iterPos )
		call assert_equal( str%fstr, "Hello", "StringList_test: at iterPos" )
		
		str = mylist.at( mylist.end )
		call assert_equal( str%fstr, "list", "StringList_test: at end" )
		
		str = mylist.at( mylist.size() )
		call assert_equal( str%fstr, "list", "StringList_test: at size" )
		
		str = "Prueba"
		call mylist.insert( iterPos, str ) ! Inserts after Hello
		call assert_equal( mylist%size(), 7, "StringList_test: size after insert Prueba" )
		
		str = mylist.at( 4 )
		call assert_equal( str%fstr, "Prueba", "StringList_test: at 4" )
		
		str = "Prueba2"
		call mylist.insert( mylist.end, str ) ! Inserts after list
		call assert_equal( mylist%size(), 8, "StringList_test: size after insert Prueba2" )
		
		str = mylist.at( 8 )
		call assert_equal( str%fstr, "Prueba2", "StringList_test: at 8" )
		
		str = "Corazon"
		call mylist.insert( mylist.begin, str ) ! Inserts after control
		call assert_equal( mylist%size(), 9, "StringList_test: size after insert Corazon" )
		
		str = mylist.at( 2 )
		call assert_equal( str%fstr, "Corazon", "StringList_test: at 2" )
		
		! Reset iterPos to index 4 (which is Hello in: control, Corazon, day, Hello, Prueba, ...)
		iter => mylist.begin
		iter => iter.next
		iter => iter.next
		iter => iter.next
		iterPos => iter
		
		str = mylist.at( iterPos )
		call assert_equal( str%fstr, "Hello", "StringList_test: Hello position check" )
		
		str = "PruebaRep"
		call mylist.replace( iterPos, str )
		
		str = mylist.at( iterPos )
		call assert_equal( str%fstr, "PruebaRep", "StringList_test: after replace" )
		
		call mylist.erase( iterPos )
		call assert_equal( mylist%size(), 8, "StringList_test: size after erase" )
		
		str = mylist.at( 4 )
		call assert_equal( str%fstr, "Prueba", "StringList_test: index 4 after erase" )
		
		call mylist.erase( mylist.begin )
		call assert_equal( mylist%size(), 7, "StringList_test: size after erase begin" )
		str = mylist.at( 1 )
		call assert_equal( str%fstr, "Corazon", "StringList_test: begin element after erase" )
		
		call mylist.erase( mylist.end )
		call assert_equal( mylist%size(), 6, "StringList_test: size after erase end" )
		str = mylist.at( mylist.end )
		call assert_equal( str%fstr, "list", "StringList_test: end element after erase" )
		
		call mylist.clear()
		call assert_equal( mylist%size(), 0, "StringList_test: size after clear" )
		
		str = "Hello1 aaaaaa"
		call mylist.append( str )
		str = "Hello2 bbbbb ccccc"
		call mylist.append( str )
		call assert_equal( mylist%size(), 2, "StringList_test: size after second append" )
		
		call mylist.clear()
		call assert_equal( mylist%size(), 0, "StringList_test: size after second clear" )
		
	end subroutine StringList_test

end module StringList_

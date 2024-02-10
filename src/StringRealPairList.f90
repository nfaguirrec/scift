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

module StringRealPairList_
	use IOStream_
	use String_
	use StringRealPair_
	implicit none
	private
	
	public :: &
		StringRealPairList_test

!>
!! This class use the List template declared into List.h90 file,
!! please take a look to this file for details
!!	
#define List StringRealPairList
#define ListIterator StringRealPairListIterator
#define __CLASS_ITEMLIST__ class(StringRealPair)
#define __TYPE_ITEMLIST__ type(StringRealPair)
#include "List.h90"
#undef List
#undef ListIterator
#undef __CLASS_ITEMLIST__
#undef __TYPE_ITEMLIST__
	
	!>
	!! @brief Converts to string
	!!
	function str( this, formatted, prefix ) result( output )
		class(StringRealPairList) :: this 
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
		
			output = trim(output)//"<StringRealPairList:"
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
		class(StringRealPairList) :: this
		type(OFStream), optional, intent(in) :: ofile
		
		integer :: unitEff
		
		type(StringRealPairListIterator), pointer :: iter
		type(StringRealPair) :: pair
		
		if( present(ofile) ) then
			unitEff = ofile.unit
		else
			unitEff = IO_STDOUT
		end if
		
		write(unitEff,"(a)") "#"//trim(str(this))
		
		iter => this.begin
		do while ( associated(iter) )
			pair = iter.data
			write(unitEff,"(A15,F15.7)") pair.first.fstr, pair.second
			
			iter => iter.next
		end do
	end subroutine toFStream
	
	!>
	!! @brief Test method
	!!
	subroutine StringRealPairList_test()
		type(String) :: str
		type(StringRealPair) :: mypair
		type(StringRealPairList) :: mylist
		class(StringRealPairListIterator), pointer :: iter
		class(StringRealPairListIterator), pointer :: iterPos
		
		integer :: i
		
		write(*,*) "------------------------------"
		write(*,*) "Testing for empty constructor"
		write(*,*) "-----------------------------"
		
		write(*,*) "call mylist.init()"
		mylist = StringRealPairList()
		
		iter => mylist.begin
		do while( associated(iter) )
			write(*,*) iter.data.first.fstr, iter.data.second
			
			iter => iter.next
		end do
		
		write(*,*) "-------------------------"
		write(*,*) "Testing for append method"
		write(*,*) "-------------------------"
		
		write(*,*) "call mylist.append( Hello, 3 )"
		write(*,*) "call mylist.append( class, 2 )"
		write(*,*) "call mylist.append( string, 6 )"
		write(*,*) "call mylist.append( list, 9 )"
		write(*,*)
		
		str = "Hello"
		call mypair.init( str, 3.0_8 )
		call mylist.append( mypair )
		
		str = "class"
		call mypair.init( str, 2.0_8 )
		call mylist.append( mypair )
		
		str = "string"
		call mypair.init( str, 6.0_8 )
		call mylist.append( mypair )
		
		str = "list"
		call mypair.init( str, 9.0_8 )
		call mylist.append( mypair )
		
		iter => mylist.begin
		do while( associated(iter) )
			write(*,*) iter.data.first.fstr, iter.data.second
			
			iter => iter.next
		end do
		write(*,*)
		
		write(*,*) "--------------------------"
		write(*,*) "Testing for prepend method"
		write(*,*) "--------------------------"
		
		write(*,*) "call mylist.prepend( day )"
		write(*,*) "call mylist.prepend( control )"
		write(*,*)
		
		str = "day"
		call mypair.init( str, 3.0_8 )
		call mylist.prepend( mypair )
		
		str = "control"
		call mypair.init( str, 2.0_8 )
		call mylist.prepend( mypair )
		
		iter => mylist.begin
		do while( associated(iter) )
			write(*,*) iter.data.first.fstr, iter.data.second
			
			iter => iter.next
		end do
		write(*,*)
		
		iter => mylist.begin
		iter => iter.next
		
		write(*,*) "--------------------------"
		write(*,*) "Testing the access methods"
		write(*,*) "--------------------------"
		
		write(*,*) "mylist.size() = ", mylist.size()
		
		mypair = mylist.at( mylist.begin )
		write(*,*) "mylist.at( mylist.begin ) = ", mypair.first.fstr, mypair.second
		mypair = mylist.at( 1 )
		write(*,*) "mylist.at( 1 ) = ", mypair.first.fstr, mypair.second
		
		iter => mylist.begin
		iter => iter.next
		iter => iter.next
		iterPos => iter
		iter => iter.next
		mypair = mylist.at( iterPos )
		write(*,*) "iter => mylist.begin"
		write(*,*) "iter => iter.next"
		write(*,*) "iter => iter.next"
		write(*,*) "iterPos => iter"
		write(*,*) "mylist.at( iterPos ) = ", mypair.first.fstr, mypair.second
		
		mypair = mylist.at( mylist.end )
		write(*,*) "mylist.at( mylist.end ) = ", mypair.first.fstr, mypair.second
		mypair = mylist.at( mylist.size() )
		write(*,*) "mylist.at( mylist.size() ) = ", mypair.first.fstr, mypair.second
		
		write(*,*) "--------------------------------"
		write(*,*) "Testing insert and erase methods"
		write(*,*) "--------------------------------"
		
		write(*,*) "call mylist.insert( iterPos, Prueba )"
		
		str = "Prueba"
		call mypair.init( str, 15.0_8 )
		call mylist.insert( iterPos, mypair )
		
		iter => mylist.begin
		do while( associated(iter) )
			write(*,*) iter.data.first.fstr, iter.data.second
			
			iter => iter.next
		end do
		write(*,*)
		
		write(*,*) "iter => mylist.begin"
		write(*,*) "iter => iter.next"
		write(*,*) "iter => iter.next"
		write(*,*) "iter => iter.next"
		write(*,*) "iterPos => iter"
		write(*,*) "call mylist.erase( iterPos )"
		
		iter => mylist.begin
		iter => iter.next
		iter => iter.next
		iter => iter.next
		iterPos => iter
		call mylist.erase( iterPos )
		
		iter => mylist.begin
		do while( associated(iter) )
			write(*,*) iter.data.first.fstr, iter.data.second
			
			iter => iter.next
		end do
		write(*,*)
		
		write(*,*) "--------------------"
		write(*,*) "Testing clear method"
		write(*,*) "--------------------"
		
		write(*,*) "call mylist.clear()"
		
		call mylist.clear()
		
		iter => mylist.begin
		do while( associated(iter) )
			write(*,*) iter.data.first.fstr, iter.data.second
			
			iter => iter.next
		end do
		write(*,*)
		
		write(*,*) "call mylist.append( Hello1 aaaaaa )"
		write(*,*) "call mylist.append( Hello2 bbbbb ccccc )"
		
		str = "Hello1 aaaaaa"
		call mypair.init( str, 21.0_8 )
		call mylist.append( mypair )
		
		str = "Hello2 bbbbb ccccc"
		call mypair.init( str, 31.0_8 )
		call mylist.append( mypair )
		
		iter => mylist.begin
		do while( associated(iter) )
			write(*,*) iter.data.first.fstr, iter.data.second
			
			iter => iter.next
		end do
		write(*,*)
		
		write(*,*) "call mylist.clear()"
		call mylist.clear()
		
	end subroutine StringRealPairList_test

end module StringRealPairList_

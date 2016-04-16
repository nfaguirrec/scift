!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!  This file is part of scift (Scientific Fortran Tools).
!!  Copyright (C) by authors (2012-2013)
!!  
!!  Authors (alphabetic order):
!!    * Aguirre N.F. (nfaguirrec@gmail.com)  (2012-2013)
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
		type(String) :: str
		type(StringList) :: mylist
		class(StringListIterator), pointer :: iter
		class(StringListIterator), pointer :: iterPos
		
		integer :: i
		
		write(*,*) "------------------------------"
		write(*,*) "Testing for empty constructor"
		write(*,*) "-----------------------------"
		
		write(*,*) "call mylist.init()"
		call mylist.init()
		
		call showMyList( mylist )
		
		write(*,*) "-------------------------"
		write(*,*) "Testing for append method"
		write(*,*) "-------------------------"
		
		write(*,*) "call mylist.append( Hello )"
		write(*,*) "call mylist.append( class )"
		write(*,*) "call mylist.append( string )"
		write(*,*) "call mylist.append( list )"
		write(*,*)
		
		str = "Hello"
		call mylist.append( str )  ! 1
		str = "class"
		call mylist.append( str )  ! 2
		str = "string"
		call mylist.append( str )  ! 3
		str = "list"
		call mylist.append( str )  ! 4
		
		call showMyList( mylist )
		
		write(*,*) "--------------------------"
		write(*,*) "Testing for prepend method"
		write(*,*) "--------------------------"
		
		write(*,*) "call mylist.prepend( day )"
		write(*,*) "call mylist.prepend( control )"
		write(*,*)
		
		str = "day"
		call mylist.prepend( str )
		str = "control"
		call mylist.prepend( str )
		
		call showMyList( mylist )
		
		iter => mylist.begin
		iter => iter.next
		
		write(*,*) "--------------------------"
		write(*,*) "Testing the access methods"
		write(*,*) "--------------------------"
		
		write(*,*) "mylist.size() = ", mylist.size()
		
		str = mylist.at( mylist.begin )
		write(*,*) "mylist.at( mylist.begin ) = ", str.fstr
		str = mylist.at( 1 )
		write(*,*) "mylist.at( 1 ) = ", str.fstr
		
		iter => mylist.begin
		iter => iter.next
		iter => iter.next
		iterPos => iter
		iter => iter.next
		str = mylist.at( iterPos )
		write(*,*) "iter => mylist.begin"
		write(*,*) "iter => iter.next"
		write(*,*) "iter => iter.next"
		write(*,*) "iterPos => iter"
		write(*,*) "mylist.at( iterPos ) = ", str.fstr
		
		str = mylist.at( mylist.begin )
		write(*,*) "mylist.at( mylist.begin ) = ", str.fstr
		str = mylist.at( mylist.end )
		write(*,*) "mylist.at( mylist.end ) = ", str.fstr
		str = mylist.at( mylist.size() )
		write(*,*) "mylist.at( mylist.size() ) = ", str.fstr
		
		call showMyList( mylist )
		
		write(*,*) "-----------------------------------------"
		write(*,*) "Testing insert, replace and erase methods"
		write(*,*) "-----------------------------------------"
		
		write(*,*) "call mylist.insert( iterPos, Prueba )"
		
		str = "Prueba"
		call mylist.insert( iterPos, str )
		call showMyList( mylist )
		
		write(*,*) "call mylist.insert( end, Prueba2 )"
		
		str = "Prueba2"
		call mylist.insert( mylist.end, str )
		call showMyList( mylist )
		
		write(*,*) "call mylist.insert( begin, Corazon )"
		
		str = "Corazon"
		call mylist.insert( mylist.begin, str )
		call showMyList( mylist )
		
		write(*,*) "iter => mylist.begin"
		write(*,*) "iter => iter.next"
		write(*,*) "iter => iter.next"
		write(*,*) "iter => iter.next"
		write(*,*) "iterPos => iter"
		
		iter => mylist.begin
		iter => iter.next
		iter => iter.next
		iter => iter.next
		iterPos => iter
		
		write(*,*) "call mylist.replace( iterPos, PruebaRep )"
		str = "PruebaRep"
		call mylist.replace( iterPos, str )
		call showMyList( mylist )
		
		write(*,*) "call mylist.erase( iterPos )"
		call mylist.erase( iterPos )
		call showMyList( mylist )
		
		write(*,*) "call mylist.erase( begin )"
		call mylist.erase( mylist.begin )
		call showMyList( mylist )
		
		write(*,*) "call mylist.erase( end )"
		call mylist.erase( mylist.end )
		call showMyList( mylist )
		
		write(*,*) "--------------------"
		write(*,*) "Testing clear method"
		write(*,*) "--------------------"
		
		write(*,*) "call mylist.clear()"
		
		call mylist.clear()
		
		call showMyList( mylist )
		
		write(*,*) "call mylist.append( Hello1 aaaaaa )"
		write(*,*) "call mylist.append( Hello2 bbbbb ccccc )"
		
		str = "Hello1 aaaaaa"
		call mylist.append( str )  ! 1
		str = "Hello2 bbbbb ccccc"
		call mylist.append( str )  ! 2
		
		call showMyList( mylist )
		
		write(*,*) "call mylist.clear()"
		call mylist.clear()
		
		call showMyList( mylist )
		
! 		@todo Hay que implementar esto
! 			it = task.begin()
! 			do while( it /= task.end() )
! 				write(*,*) task.get(i)
! 				it.next()
! 			end do
		
	end subroutine StringList_test

end module StringList_

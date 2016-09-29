!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!  This file is part of scift (Scientific Fortran Tools).
!!  Copyright (C) by authors (2012-2016)
!!  
!!  Authors (alphabetic order):
!!    * Aguirre N.F. (nfaguirrec@gmail.com)  (2016-2016)
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

module IntegerHyperList_
	use Math_
	use IOStream_
	use IntegerList_
	use Matrix_
	implicit none
	private
	
	public :: &
		IntegerHyperList_test
		
!>
!! This class use the List template declared into List.h90 file,
!! please take a look to this file for details
!!
#define List IntegerHyperList
#define ListIterator IntegerHyperListIterator
#define __CLASS_ITEMLIST__ class(IntegerList)
#define __TYPE_ITEMLIST__ type(IntegerList)
#define __ADD_ATTRIBUTES__
#define __ADD_METHODS__
#include "List.h90"
#undef List
#undef ListIterator
#undef __CLASS_ITEMLIST__
#undef __TYPE_ITEMLIST__
#undef __ADD_ATTRIBUTES__
#undef __ADD_METHODS__
	
	!>
	!! @brief Converts to string
	!!
	function str( this, formatted, prefix ) result( output )
		class(IntegerHyperList) :: this 
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
		
			output = trim(output)//"<IntegerHyperList:"
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
		class(IntegerHyperList) :: this
		type(OFStream), optional, intent(in) :: ofile
		
		integer :: unitEff
		
		type(IntegerHyperListIterator), pointer :: iter
		
		if( present(ofile) ) then
			unitEff = ofile.unit
		else
			unitEff = IO_STDOUT
		end if
		
		write(unitEff,"(a)") "#"//trim(str(this))
		
! 		iter => this.begin
! 		do while ( associated(iter) )
! 			write(unitEff,"(I15)") iter.data
! 			
! 			iter => iter.next
! 		end do
	end subroutine toFStream
	
	subroutine showMyList( mylist )
		type(IntegerList) :: mylist
		class(IntegerListIterator), pointer :: iter
		
		iter => mylist.begin
		do while( associated(iter) )
			write(*,"(I2,A)", advance="no") iter.data, "  --> "
			
			iter => iter.next
		end do
		write(*,*)
	end subroutine showMyList
	
	subroutine showMyHList( myhlist )
		type(IntegerHyperList), target :: myhlist
		
		class(IntegerHyperListIterator), pointer :: iter
		type(IntegerList), pointer :: mylist
		integer :: i
		
		i = 1
		iter => myhlist.begin
		do while( associated(iter) )
			mylist => iter.data
			
			write(*,"(I2,A)", advance="no") i, " : "
			call showMyList( mylist )
! 			write(*,"(I2,A)", advance="no") iter.data, "  --> "
			
			iter => iter.next
			i = i + 1
		end do
		write(*,*)
	end subroutine showMyHList
	
	!>
	!! @brief Test method
	!!
	subroutine IntegerHyperList_test()
		type(IntegerHyperList) :: hilist
		class(IntegerHyperListIterator), pointer :: iter

		type(IntegerList) :: ilist
		integer :: id
		
		type(Matrix) :: dMatrix
		
		integer :: i
		
		call hilist.init()
		
		write(*,*) "-------------------------"
		write(*,*) "Testing for append method"
		write(*,*) "-------------------------"
		
		write(*,*) "call hilist.append( [1,1,1] )"
		write(*,*) "call hilist.append( [2,2,2,2] )"
		write(*,*) "call hilist.append( [1,1] )"
		write(*,*)
		
		call ilist.init( 3, value=1 )
		call hilist.append( ilist )
		call ilist.init( 4, value=2 )
		call hilist.append( ilist )
		call ilist.init( 2, value=1 )
		call hilist.append( ilist )
		
		call showMyHList( hilist )
		
		write(*,*) "-------------------------"
		write(*,*) "Testing for prepend method"
		write(*,*) "-------------------------"
		
		write(*,*) "call hilist.prepend( [4,4] )"
		write(*,*) "call hilist.prepend( [5,5,5,5] )"
		write(*,*) "call hilist.prepend( [8,8,8] )"
		write(*,*)
		
		call ilist.init( 2, value=4 )
		call hilist.prepend( ilist )
		call ilist.init( 4, value=5 )
		call hilist.prepend( ilist )
		call ilist.init( 3, value=8 )
		call hilist.prepend( ilist )
		
		call showMyHList( hilist )

		write(*,*) "-------------------------"
		write(*,*) "Testing for insert method"
		write(*,*) "-------------------------"
		
		write(*,*) "iter => hilist.begin"
		write(*,*) "iter => iter.next"
		write(*,*) "iter => iter.next"
		write(*,*) "call hilist.insert( iter, [9, 9, 9] )"
		write(*,*)
		
		iter => hilist.begin
		iter => iter.next
		iter => iter.next
		
		call ilist.init( 3, value=9 )
		
		call hilist.insert( iter, ilist )
		call showMyHList( hilist )
		
		write(*,*)
		write(*,*) "call hilist.insert( iter, [8, 8, 8, 8] )"
		write(*,*)
		
		call ilist.init( 4, value=8 )
		
		call hilist.insert( iter, ilist )
		call showMyHList( hilist )
		
		write(*,*)
		write(*,*) "call hilist.insert( hilist.end, [7, 7] )"
		write(*,*)
				
		call ilist.init( 2, value=7 )
		
		call hilist.insert( hilist.end, ilist )
		call showMyHList( hilist )

		write(*,*) "------------------------"
		write(*,*) "Testing for erase method"
		write(*,*) "------------------------"
		
		write(*,*) "call hilist.erase( 2 )"
		write(*,*)
		
		call hilist.erase( 2 )
		call showMyHList( hilist )

		write(*,*) "iter => hilist.begin"
		write(*,*) "iter => iter.next"
		write(*,*) "call hilist.erase( iter )"
		write(*,*)
		
		iter => hilist.begin
		iter => iter.next
		
		call hilist.erase( iter )
		call showMyHList( hilist )
		
		write(*,*)
		write(*,*) "call hilist.erase( hilist.begin )"
		write(*,*)
		
		call hilist.erase( hilist.begin )
		call showMyHList( hilist )
		
		write(*,*)
		write(*,*) "call hilist.erase( hilist.end )"
		write(*,*)
		call hilist.erase( hilist.end )
		call showMyHList( hilist )
		
		write(*,*) "------------------------"
		write(*,*) "Testing for clear method"
		write(*,*) "------------------------"
		
		write(*,*) "call hilist.clear()"
		write(*,*)
		call hilist.clear()
		call showMyHList( hilist )

	end subroutine IntegerHyperList_test
	
end module IntegerHyperList_

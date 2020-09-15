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

module RealList_
	use IOStream_
	implicit none
	private
	
	public :: &
		RealList_test

!>
!! This class use the List template declared into List.h90 file,
!! please take a look to this file for details
!!
#define List RealList
#define ListIterator RealListIterator
#define __CLASS_ITEMLIST__ real(8)
#define __TYPE_ITEMLIST__ real(8)
#include "List.h90"
#undef List
#undef ListIterator
#undef __CLASS_ITEMLIST__
#undef __TYPE_ITEMLIST__
	
	!>
	!! @brief Converts to string
	!!
	function str( this, formatted, prefix ) result( output )
		class(RealList) :: this 
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
		
			output = trim(output)//"<RealList:"
! 			ITEMI( "min=", this%min )
! 			ITEMR( ",size=", this%size )
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
! ! 			ITEMI( "min=", this%min )
! ! 			ITEMR( ",size=", this%size )
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
		class(RealList) :: this
		type(OFStream), optional, intent(in) :: ofile
		
		integer :: unitEff
		
		type(RealListIterator), pointer :: iter
		
		if( present(ofile) ) then
			unitEff = ofile%unit
		else
			unitEff = IO_STDOUT
		end if
		
		write(unitEff,"(a)") "#"//trim(str(this))
		
		iter => this%begin
		do while ( associated(iter) )
			write(unitEff,"(F15.7)") iter%data
			
			iter => iter%next
		end do
	end subroutine toFStream
	
	subroutine showMyListForward( mylist )
		type(RealList) :: mylist
		class(RealListIterator), pointer :: iter
		
		iter => mylist%begin
		do while( associated(iter) )
			write(*,"(F5.3,A)", advance="no") iter%data, "  --> "
			
			iter => iter%next
		end do
		write(*,*)
	end subroutine showMyListForward
	
	subroutine showMyListBackward( mylist )
		type(RealList) :: mylist
		class(RealListIterator), pointer :: iter
		
		iter => mylist%end
		do while( associated(iter) )
			write(*,"(A,F5.3)", advance="no") "  <-- ", iter%data
			
			iter => iter%prev
		end do
		write(*,*)
	end subroutine showMyListBackward
	
	!>
	!! @brief Test method
	!!
	subroutine RealList_test()
		type(RealList) :: mylist
		class(RealListIterator), pointer :: iter
		
		call mylist%init()
		
		write(*,*) "-------------------------"
		write(*,*) "Testing for append method"
		write(*,*) "-------------------------"
		
		write(*,*) "call mylist%append( 8.0 )"
		write(*,*) "call mylist%append( 5.0 )"
		write(*,*) "call mylist%append( 1.0 )"
		write(*,*)
		
		call mylist%append( 8.0_8 )
		call mylist%append( 5.0_8 )
		call mylist%append( 1.0_8 )
		
		call showMyListForward( mylist )
		
		write(*,*) "-------------------------"
		write(*,*) "Testing for prepend method"
		write(*,*) "-------------------------"
		
		write(*,*) "call mylist%prepend( 2.0 )"
		write(*,*) "call mylist%prepend( 7.0 )"
		write(*,*) "call mylist%prepend( 0.0 )"
		write(*,*)
		
		call mylist%prepend( 2.0_8 )
		call mylist%prepend( 7.0_8 )
		call mylist%prepend( 0.0_8 )
		
		call showMyListForward( mylist )
		
		write(*,*) "-------------------------"
		write(*,*) "Testing for insert method"
		write(*,*) "-------------------------"
		
		write(*,*) "iter => mylist%begin"
		write(*,*) "iter => iter%next"
		write(*,*) "iter => iter%next"
		write(*,*) "call mylist%insert( iter, 1.0 )"
		write(*,*)
		
		iter => mylist%begin
		iter => iter%next
		iter => iter%next
		
		call mylist%insert( iter, 1.0_8 )
		call showMyListForward( mylist )
		
		write(*,*)
		write(*,*) "call mylist%insert( iter, 2.0 )"
		write(*,*)
		
		call mylist%insert( iter, 2.0_8 )
		call showMyListForward( mylist )
		
		write(*,*)
		write(*,*) "call mylist%insert( mylist%end, 9.0 )"
		write(*,*)
				
		call mylist%insert( mylist%end, 9.0_8 )
		call showMyListForward( mylist )

		write(*,*) "------------------------"
		write(*,*) "Testing for erase method"
		write(*,*) "------------------------"
		
		write(*,*) "iter => mylist%begin"
		write(*,*) "iter => iter%next"
		write(*,*) "call mylist%erase( iter )"
		write(*,*)
		
		iter => mylist%begin
		iter => iter%next
		
		call mylist%erase( iter )
		call showMyListForward( mylist )
		
		write(*,*)
		write(*,*) "call mylist%erase( mylist%begin )"
		write(*,*)
		
		call mylist%erase( mylist%begin )
		call showMyListForward( mylist )
		
		write(*,*)
		write(*,*) "call mylist%erase( mylist%end )"
		write(*,*)
		call mylist%erase( mylist%end )
		call showMyListForward( mylist )
		
		write(*,*) "------------------------"
		write(*,*) "Testing for clear method"
		write(*,*) "------------------------"
		
		write(*,*) "call mylist%clear()"
		write(*,*)
		call mylist%clear()
		call showMyListForward( mylist )
		
		write(*,*) "call mylist%clear()"
		write(*,"(A)") "call mylist%append( ( [ 1.0_8, 2.0_8, 3.0_8, 4.0_8, 5.0_8, 6.0_8, 7.0_8, 8.0_8], 8 )"
		write(*,*)
		
		call mylist%clear()
		call mylist%append( [ 1.0_8, 2.0_8, 3.0_8, 4.0_8, 5.0_8, 6.0_8, 7.0_8, 8.0_8] )
		call showMyListForward( mylist )
		call showMyListBackward( mylist )
		
! 		iter => mylist%begin
! 		do while( associated(iter) )
! 			if( iter%data > 1.5_8 .and. iter%data < 3.5_8 ) then
! 				call mylist%erase( iter )
! 				iter => mylist%begin
! 			else
! 				iter => iter%next
! 			end if
! 		end do
! 		
! 		call showMyListForward( mylist )
		
		write(*,*)
		write(*,*) "call mylist%clear()"
		write(*,"(X,A)") "call mylist%append( ( [ 1.0_8, 2.0_8, 3.0_8, 4.0_8, 5.0_8, 6.0_8, 7.0_8, 8.0_8] )"
		call mylist%clear()
		call mylist%append( [ 1.0_8, 2.0_8, 3.0_8, 4.0_8, 5.0_8, 6.0_8, 7.0_8, 8.0_8] )
		write(*,*) "call mylist%eraseAllExcept( [1,3,5] )"
		call mylist%eraseAllExcept( [1,3,5] )
		call showMyListForward( mylist )
		
		write(*,*)
		write(*,*) "call mylist%clear()"
		write(*,"(X,A)") "call mylist%append( ( [ 1.0_8, 2.0_8, 3.0_8, 4.0_8, 5.0_8, 6.0_8, 7.0_8, 8.0_8] )"
		call mylist%clear()
		call mylist%append( [ 1.0_8, 2.0_8, 3.0_8, 4.0_8, 5.0_8, 6.0_8, 7.0_8, 8.0_8] )
		write(*,*) "call mylist%eraseAllExceptFirst( 4 )"
		call mylist%eraseAllExceptFirst( 4 )
		call showMyListForward( mylist )
		
		write(*,*)
		write(*,*) "call mylist%clear()"
		write(*,"(X,A)") "call mylist%append( ( [ 1.0_8, 2.0_8, 3.0_8, 4.0_8, 5.0_8, 6.0_8, 7.0_8, 8.0_8] )"
		call mylist%clear()
		call mylist%append( [ 1.0_8, 2.0_8, 3.0_8, 4.0_8, 5.0_8, 6.0_8, 7.0_8, 8.0_8] )
		write(*,*) "call mylist%eraseAllExceptLast( 4 )"
		call mylist%eraseAllExceptLast( 4 )
		call showMyListForward( mylist )
		
		write(*,*) "------------------------"
		write(*,*) "Testing for get methods"
		write(*,*) "------------------------"
		
		write(*,*) "call mylist%at(3) ==>", mylist%at(3)
		write(*,*) "call mylist%at(1) ==>", mylist%at(1)
		
	end subroutine RealList_test

end module RealList_

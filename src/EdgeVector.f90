!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                                                                                   !!
!!  This file is part of SciFT project                                               !!
!!  Copyright (c) 2012-2016 Nestor F. Aguirre (nfaguirrec@gmail.com)                 !!
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

module EdgeVector_
	use IOStream_
	use Edge_
	implicit none
	private
	
	public :: &
		EdgeVector_test

!>
!! This class use the Vector template declared into Vector.h90 file,
!! please take a look to this file for details
!!
#define Vector EdgeVector
#define VectorIterator EdgeVectorIterator
#define __CLASS_ITEMVECTOR__ class(Edge)
#define __TYPE_ITEMVECTOR__ type(Edge)
#define __ADD_ATTRIBUTES__
#define __ADD_METHODS__
#include "Vector.h90"
#undef Vector
#undef VectorIterator
#undef __CLASS_ITEMVECTOR__
#undef __TYPE_ITEMVECTOR__
#undef __ADD_ATTRIBUTES__
#undef __ADD_METHODS__

	!>
	!! @brief
	!!
	function equal( this, other ) result( output )
		class(EdgeVector), intent(in) :: this
		class(EdgeVector), intent(in) :: other
		logical :: output
		
! 		this.nItems = other.nItems
! 		this.resizeIncrement = other.resizeIncrement
		
! 		output = all( this.data(1:this.size()) == other.data(1:other.size()) )

		write(*,*) "### ERROR ### EdgeVector.equal  is not implemented yet"
		stop
	end function equal
	
	!>
	!! @brief Converts to string
	!!
	function str( this, formatted, prefix ) result( output )
		class(EdgeVector) :: this 
		character(:), allocatable :: output
		logical, optional :: formatted
		character(*), optional :: prefix
		
		logical :: effFormatted
		character(:), allocatable :: effPrefix
		
		integer :: i
		integer :: fmt
		character(200) :: fstr
		
		effFormatted = .false.
		if( present(formatted) ) effFormatted = formatted
		
		effPrefix = ""
		if( present(prefix) ) effPrefix = prefix
		
		output = ""
		
		if( .not. effFormatted ) then
			output = trim(output)//"<EdgeVector:("
! 			do i=1,this.size()
! 				if( i==1 ) then
! 					output = trim(output)//trim(FString_fromInteger(this.at(i)))
! 				else
! 					output = trim(output)//","//trim(FString_fromInteger(this.at(i)))
! 				end if
! 			end do
			output = trim(output)//")>"
! 		else
! 			LINE("Vector")
! 			LINE("---------")
! ! 			ITEMI( "min=", this.min )
! ! 			ITEMR( ",size=", this.size )
! 			LINE("")
		end if
	end function str
	
	!>
	!! Save the data in two column format in a
	!! selected unit
	!!
	subroutine toFStream( this, ofile )
		class(EdgeVector) :: this
		type(OFStream), optional, intent(in) :: ofile
		
		integer :: unitEff
		
		type(EdgeVectorIterator), pointer :: iter
		
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
	
	subroutine showMyVector( myvector )
		type(EdgeVector) :: myvector
		class(EdgeVectorIterator), pointer :: iter
		
! 		iter => myvector.begin
! 		do while( associated(iter) )
! 			write(*,"(I2,A)", advance="no") iter.data, "  --> "
! 			
! 			iter => iter.next
! 		end do
		
		integer :: i
		
		do i=1,myvector.size()
			write(*,"(I2,A)", advance="no") myvector.at(i), "  --> "
		end do
		
		write(*,*)
	end subroutine showMyVector
	
	!>
	!! @brief Test method
	!!
	subroutine EdgeVector_test()
		type(EdgeVector) :: myvector
		class(EdgeVectorIterator), pointer :: iter
		
! 		call myvector.init()
! 		
! 		write(*,*) "-------------------------"
! 		write(*,*) "Testing for append method"
! 		write(*,*) "-------------------------"
! 		
! 		write(*,*) "call myvector.append( 8 )"
! 		write(*,*) "call myvector.append( 5 )"
! 		write(*,*) "call myvector.append( 1 )"
! 		write(*,*)
! 		
! 		call myvector.append( 8 )
! 		call myvector.append( 5 )
! 		call myvector.append( 1 )
! 		
! 		call showMyVector( myvector )
! 		
! 		write(*,*) "--------------------------"
! 		write(*,*) "Testing for prepend method"
! 		write(*,*) "--------------------------"
! 		
! 		write(*,*) "call myvector.prepend( 8 )"
! 		write(*,*) "call myvector.prepend( 5 )"
! 		write(*,*) "call myvector.prepend( 1 )"
! 		write(*,*)
! 		
! 		call myvector.prepend( 8 )
! 		call myvector.prepend( 5 )
! 		call myvector.prepend( 1 )
! 		
! 		call showMyVector( myvector )
! 		
! 		write(*,*) "------------------------"
! 		write(*,*) "Testing for erase method"
! 		write(*,*) "------------------------"
! 		
! 		write(*,*) "call myvector.erase( 1 )"
! 		call myvector.erase( 1 )
! 		call showMyVector( myvector )
! 		
! 		write(*,*) "call myvector.erase( 2 )"
! 		call myvector.erase( 2 )
! 		call showMyVector( myvector )
! 		
! 		write(*,*) "call myvector.erase( 3 )"
! 		write(*,*)
! 		call myvector.erase( 3 )
! 		call showMyVector( myvector )
! 		
! 		write(*,*) "call myvector.erase( 1 )"
! 		write(*,*)
! 		call myvector.erase( 1 )
! 		call showMyVector( myvector )
! 		
! 		write(*,*) "call myvector.erase( 1 )"
! 		write(*,*)
! 		call myvector.erase( 1 )
! 		call showMyVector( myvector )
! 		
! 		write(*,*) "call myvector.erase( 1 )"
! 		write(*,*)
! 		call myvector.erase( 1 )
! 		call showMyVector( myvector )
! 		
! 		write(*,*) "call myvector.erase( 1 )"
! 		write(*,*)
! 		call myvector.erase( 1 )
! 		call showMyVector( myvector )
! 
! ! 		
! ! 		write(*,*) "-------------------------"
! ! 		write(*,*) "Testing for insert method"
! ! 		write(*,*) "-------------------------"
! ! 		
! ! 		write(*,*) "iter => myvector.begin"
! ! 		write(*,*) "iter => iter.next"
! ! 		write(*,*) "iter => iter.next"
! ! 		write(*,*) "call myvector.insert( iter, 1 )"
! ! 		write(*,*)
! ! 		
! ! 		iter => myvector.begin
! ! 		iter => iter.next
! ! 		iter => iter.next
! ! 		
! ! 		call myvector.insert( iter, 1 )
! ! 		call showMyVector( myvector )
! ! 		
! ! 		write(*,*)
! ! 		write(*,*) "call myvector.insert( iter, 2 )"
! ! 		write(*,*)
! ! 		
! ! 		call myvector.insert( iter, 2 )
! ! 		call showMyVector( myvector )
! ! 		
! ! 		write(*,*)
! ! 		write(*,*) "call myvector.insert( myvector.end, 9 )"
! ! 		write(*,*)
! ! 				
! ! 		call myvector.insert( myvector.end, 9 )
! ! 		call showMyVector( myvector )
! 
! 		write(*,*) "------------------------"
! 		write(*,*) "Testing for erase method"
! 		write(*,*) "------------------------"
! 		
! 		write(*,*) "call myvector.erase( 2 )"
! 		write(*,*)
! 		
! 		call myvector.erase( 2 )
! 		call showMyVector( myvector )
! 
! ! 		write(*,*) "iter => myvector.begin"
! ! 		write(*,*) "iter => iter.next"
! ! 		write(*,*) "call myvector.erase( iter )"
! ! 		write(*,*)
! ! 		
! ! 		iter => myvector.begin
! ! 		iter => iter.next
! ! 		
! ! 		call myvector.erase( iter )
! ! 		call showMyVector( myvector )
! ! 		
! ! 		write(*,*)
! ! 		write(*,*) "call myvector.erase( myvector.begin )"
! ! 		write(*,*)
! ! 		
! ! 		call myvector.erase( myvector.begin )
! ! 		call showMyVector( myvector )
! ! 		
! ! 		write(*,*)
! ! 		write(*,*) "call myvector.erase( myvector.end )"
! ! 		write(*,*)
! ! 		call myvector.erase( myvector.end )
! ! 		call showMyVector( myvector )
! 		
! 		write(*,*) "------------------------"
! 		write(*,*) "Testing for clear method"
! 		write(*,*) "------------------------"
! 		
! 		write(*,*) "call myvector.clear()"
! 		write(*,*)
! 		call myvector.clear()
! 		call showMyVector( myvector )
! 
! 		write(*,*) "call myvector.append( 1 )"
! 		write(*,*) "call myvector.append( 2 )"
! 		write(*,*) "call myvector.append( 3 )"
! 		write(*,*)
! 		
! 		call myvector.append( 1 )
! 		call myvector.append( 2 )
! 		call myvector.append( 3 )
! 		call showMyVector( myvector )

	end subroutine EdgeVector_test

end module EdgeVector_
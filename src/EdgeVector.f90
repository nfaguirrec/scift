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
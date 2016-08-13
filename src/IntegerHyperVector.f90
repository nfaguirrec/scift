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

module IntegerHyperVector_
	use Math_
	use IOStream_
	use IntegerVector_
	use Matrix_
	implicit none
	private
	
	public :: &
		IntegerHyperVector_test
		
!>
!! This class use the Vector template declared into Vector.h90 file,
!! please take a look to this file for details
!!
#define Vector IntegerHyperVector
#define VectorIterator IntegerHyperVectorIterator
#define __CLASS_ITEMVECTOR__ class(IntegerVector)
#define __TYPE_ITEMVECTOR__ type(IntegerVector)
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
		class(IntegerHyperVector), intent(in) :: this
		class(IntegerHyperVector), intent(in) :: other
		logical :: output
		
! 		this.nItems = other.nItems
! 		this.resizeIncrement = other.resizeIncrement
		
		write(*,*) "### ERROR ### IntegerHyperVector.equal  is not implemented yet"
		stop
! 		output = all( this.data(1:this.size()) == other.data(1:other.size()) )
	end function equal

	!>
	!! @brief Converts to string
	!!
	function str( this, formatted, prefix ) result( output )
		class(IntegerHyperVector) :: this 
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
		
			output = trim(output)//"<IntegerHyperVector:"
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
! 			LINE("Vector")
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
		class(IntegerHyperVector) :: this
		type(OFStream), optional, intent(in) :: ofile
		
		integer :: unitEff
		
		type(IntegerHyperVectorIterator), pointer :: iter
		
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
	
	subroutine showMyHVector( myvec )
		type(IntegerHyperVector), target :: myvec
		
		type(IntegerVector), pointer :: ivec
		integer :: i, j
		
		do i=1,myvec.size()
			ivec => myvec.data(i)
			
			write(*,"(I2,A)", advance="no") i, ") "
			
			do j=1,ivec.size()
				write(*,"(I2,A)", advance="no") ivec.at(j), " --> "
			end do
			
			write(*,*)
		end do
		write(*,*)
	end subroutine showMyHVector
	
	!>
	!! @brief Test method
	!!
	subroutine IntegerHyperVector_test()
		type(IntegerHyperVector) :: hvec
! 		class(IntegerHyperVectorIterator), pointer :: iter

		type(IntegerVector) :: ivec
		integer :: id
		
		type(Matrix) :: dMatrix
		
		integer :: i
		
		call hvec.init()
		
		write(*,*) "-------------------------"
		write(*,*) "Testing for append method"
		write(*,*) "-------------------------"
		
		write(*,*) "call hvec.append( [1,1,1] )"
		write(*,*) "call hvec.append( [2,2,2,2] )"
		write(*,*) "call hvec.append( [1,1] )"
		write(*,*)
		
		call ivec.init( 3, value=1 )
		call hvec.append( ivec )
		call ivec.init( 4, value=2 )
		call hvec.append( ivec )
		call ivec.init( 2, value=1 )
		call hvec.append( ivec )
		
		call showMyHVector( hvec )
		
		write(*,*) "-------------------------"
		write(*,*) "Testing for prepend method"
		write(*,*) "-------------------------"
		
		write(*,*) "call hvec.prepend( [4,4] )"
		write(*,*) "call hvec.prepend( [5,5,5,5] )"
		write(*,*) "call hvec.prepend( [8,8,8] )"
		write(*,*)
		
		call ivec.init( 2, value=4 )
		call hvec.prepend( ivec )
		call ivec.init( 4, value=5 )
		call hvec.prepend( ivec )
		call ivec.init( 3, value=8 )
		call hvec.prepend( ivec )
		
		call showMyHVector( hvec )

! ! 		write(*,*) "-------------------------"
! ! 		write(*,*) "Testing for insert method"
! ! 		write(*,*) "-------------------------"
! ! 		
! ! 		write(*,*) "iter => hvec.begin"
! ! 		write(*,*) "iter => iter.next"
! ! 		write(*,*) "iter => iter.next"
! ! 		write(*,*) "call hvec.insert( iter, 1 )"
! ! 		write(*,*)
! ! 		
! ! 		iter => hvec.begin
! ! 		iter => iter.next
! ! 		iter => iter.next
! ! 		
! ! 		call hvec.insert( iter, 1 )
! ! 		call showMyGraph( hvec )
! ! 		
! ! 		write(*,*)
! ! 		write(*,*) "call hvec.insert( iter, 2 )"
! ! 		write(*,*)
! ! 		
! ! 		call hvec.insert( iter, 2 )
! ! 		call showMyGraph( hvec )
! ! 		
! ! 		write(*,*)
! ! 		write(*,*) "call hvec.insert( hvec.end, 9 )"
! ! 		write(*,*)
! ! 				
! ! 		call hvec.insert( hvec.end, 9 )
! ! 		call showMyGraph( hvec )
! 
! 		write(*,*) "------------------------"
! 		write(*,*) "Testing for erase method"
! 		write(*,*) "------------------------"
! 		
! 		write(*,*) "call hvec.erase( 2 )"
! 		write(*,*)
! 		
! 		call hvec.erase( 2 )
! 		call showMyGraph( hvec )
! 
! ! 		write(*,*) "iter => hvec.begin"
! ! 		write(*,*) "iter => iter.next"
! ! 		write(*,*) "call hvec.erase( iter )"
! ! 		write(*,*)
! ! 		
! ! 		iter => hvec.begin
! ! 		iter => iter.next
! ! 		
! ! 		call hvec.erase( iter )
! ! 		call showMyGraph( hvec )
! ! 		
! ! 		write(*,*)
! ! 		write(*,*) "call hvec.erase( hvec.begin )"
! ! 		write(*,*)
! ! 		
! ! 		call hvec.erase( hvec.begin )
! ! 		call showMyGraph( hvec )
! ! 		
! ! 		write(*,*)
! ! 		write(*,*) "call hvec.erase( hvec.end )"
! ! 		write(*,*)
! ! 		call hvec.erase( hvec.end )
! ! 		call showMyGraph( hvec )
		
		write(*,*) "------------------------"
		write(*,*) "Testing for clear method"
		write(*,*) "------------------------"
		
		write(*,*) "call hvec.clear()"
		write(*,*)
		call hvec.clear()
		call showMyHVector( hvec )

	end subroutine IntegerHyperVector_test
	
end module IntegerHyperVector_
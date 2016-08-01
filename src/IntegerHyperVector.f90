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
	
	!>
	!! @brief Test method
	!!
	subroutine IntegerHyperVector_test()
		type(IntegerHyperVector) :: mygraph
! 		class(IntegerHyperVectorIterator), pointer :: iter

! 		type(IntegerVector) :: ivec
! 		integer :: id
! 		type(IntegerEdge) :: edge
! 		type(IntegerVector) :: path
! 		
! 		type(Matrix) :: dMatrix
! 		
! 		integer :: i
! 		
! 		call mygraph.init()
! 		
! 		!------------------------------------------
! 		! Ejemplo 1
! ! 		call mygraph.newNode()
! ! 		call mygraph.newNode()
! ! 		call mygraph.newNode()
! ! 		call mygraph.newNode()
! ! 		call mygraph.newNode()
! ! 		call mygraph.newNode()
! ! 		
! ! 		call mygraph.newEdges( 1, [2,3,6] )
! ! 		call mygraph.newEdges( 2, [1,3,4] )
! ! 		call mygraph.newEdges( 3, [1,2,4,6] )
! ! 		call mygraph.newEdges( 4, [2,3,5] )
! ! 		call mygraph.newEdges( 5, [4,6] )
! ! 		call mygraph.newEdges( 6, [1,3,5] )
! 
! 		!------------------------------------------
! 		! Ejemplo 3
! ! 		call mygraph.newNode()
! ! 		call mygraph.newNode()
! ! 		call mygraph.newNode()
! ! 		call mygraph.newNode()
! ! 		call mygraph.newNode()
! ! 		call mygraph.newNode()
! ! 		call mygraph.newNode()
! ! 		call mygraph.newNode()
! ! 		
! ! 		call mygraph.newEdges( 1, [2,3,4] )
! ! 		call mygraph.newEdges( 2, [5,1] )
! ! 		call mygraph.newEdges( 3, [8] )
! ! 		call mygraph.newEdges( 4, [1] )
! ! 		call mygraph.newEdges( 5, [2,6,7] )
! ! 		call mygraph.newEdges( 6, [5] )
! ! 		call mygraph.newEdges( 7, [5] )
! ! 		call mygraph.newEdges( 8, [3] )
! ! 		
! ! 		call showMyGraph( mygraph )
! ! 		
! ! 		call mygraph.computeDijkstraPaths( 1 )
! ! 		write(*,*) "distance from 1 to 6 = ", mygraph.distance(6)
! ! 		path = mygraph.shortestPath(6)
! ! 		call path.show()
! 
! ! 		!------------------------------------------
! ! 		! Ejemplo 2
! ! 		call mygraph.newNode()
! ! 		call mygraph.newNode()
! ! 		call mygraph.newNode()
! ! 		call mygraph.newNode()
! ! 		call mygraph.newNode()
! ! 		call mygraph.newNode()
! ! 		
! ! 		call mygraph.newEdges( 1, [2] )
! ! 		call mygraph.newEdges( 2, [1,3,4] )
! ! 		call mygraph.newEdges( 3, [2,4] )
! ! 		call mygraph.newEdges( 4, [2,3,5] )
! ! 		call mygraph.newEdges( 5, [4,6] )
! ! 		call mygraph.newEdges( 6, [5] )
! 
! 		!------------------------------------------
! 		! Ejemplo Diego
! 		call mygraph.newNode()
! 		call mygraph.newNode()
! 		call mygraph.newNode()
! 		call mygraph.newNode()
! 		
! 		call mygraph.newEdges( 1, [2,4] )
! 		call mygraph.newEdges( 2, [1,3] )
! 		call mygraph.newEdges( 3, [2,4] )
! 		call mygraph.newEdges( 4, [1,3] )
! 		
! 		dMatrix = mygraph.distanceMatrix()
! 		call dMatrix.show( formatted=.true. )
! 		write(*,*) "Wiener index = ", mygraph.wienerIndex()
! 		
! ! 		write(*,*) "-------------------------"
! ! 		write(*,*) "Testing for append method"
! ! 		write(*,*) "-------------------------"
! ! 		
! ! 		write(*,*) "call mygraph.append( 8 )"
! ! 		write(*,*) "call mygraph.append( 5 )"
! ! 		write(*,*) "call mygraph.append( 1 )"
! ! 		write(*,*)
! 		
! ! 		call mygraph.append( 8 )
! ! 		call mygraph.append( 5 )
! ! 		call mygraph.append( 1 )
! ! 		
! ! 		call showMyGraph( mygraph )
! ! 		
! ! 		write(*,*) "-------------------------"
! ! ! 		write(*,*) "Testing for prepend method"
! ! ! 		write(*,*) "-------------------------"
! ! ! 		
! ! ! 		write(*,*) "call mygraph.prepend( 2 )"
! ! ! 		write(*,*) "call mygraph.prepend( 7 )"
! ! ! 		write(*,*) "call mygraph.prepend( 0 )"
! ! ! 		write(*,*)
! ! ! 		
! ! ! 		call mygraph.prepend( 2 )
! ! ! 		call mygraph.prepend( 7 )
! ! ! 		call mygraph.prepend( 0 )
! ! ! 		
! ! ! 		call showMyGraph( mygraph )
! ! 
! ! 		write(*,*) "--------------------------"
! ! 		write(*,*) "Testing for prepend method"
! ! 		write(*,*) "--------------------------"
! ! 		
! ! 		write(*,*) "call mygraph.prepend( 8 )"
! ! 		write(*,*) "call mygraph.prepend( 5 )"
! ! 		write(*,*) "call mygraph.prepend( 1 )"
! ! 		write(*,*)
! ! 		
! ! 		call mygraph.prepend( 8 )
! ! 		call mygraph.prepend( 5 )
! ! 		call mygraph.prepend( 1 )
! ! 		
! ! 		call showMyGraph( mygraph )
! ! ! 		
! ! ! 		write(*,*) "-------------------------"
! ! ! 		write(*,*) "Testing for insert method"
! ! ! 		write(*,*) "-------------------------"
! ! ! 		
! ! ! 		write(*,*) "iter => mygraph.begin"
! ! ! 		write(*,*) "iter => iter.next"
! ! ! 		write(*,*) "iter => iter.next"
! ! ! 		write(*,*) "call mygraph.insert( iter, 1 )"
! ! ! 		write(*,*)
! ! ! 		
! ! ! 		iter => mygraph.begin
! ! ! 		iter => iter.next
! ! ! 		iter => iter.next
! ! ! 		
! ! ! 		call mygraph.insert( iter, 1 )
! ! ! 		call showMyGraph( mygraph )
! ! ! 		
! ! ! 		write(*,*)
! ! ! 		write(*,*) "call mygraph.insert( iter, 2 )"
! ! ! 		write(*,*)
! ! ! 		
! ! ! 		call mygraph.insert( iter, 2 )
! ! ! 		call showMyGraph( mygraph )
! ! ! 		
! ! ! 		write(*,*)
! ! ! 		write(*,*) "call mygraph.insert( mygraph.end, 9 )"
! ! ! 		write(*,*)
! ! ! 				
! ! ! 		call mygraph.insert( mygraph.end, 9 )
! ! ! 		call showMyGraph( mygraph )
! ! 
! ! 		write(*,*) "------------------------"
! ! 		write(*,*) "Testing for erase method"
! ! 		write(*,*) "------------------------"
! ! 		
! ! 		write(*,*) "call mygraph.erase( 2 )"
! ! 		write(*,*)
! ! 		
! ! 		call mygraph.erase( 2 )
! ! 		call showMyGraph( mygraph )
! ! 
! ! ! 		write(*,*) "iter => mygraph.begin"
! ! ! 		write(*,*) "iter => iter.next"
! ! ! 		write(*,*) "call mygraph.erase( iter )"
! ! ! 		write(*,*)
! ! ! 		
! ! ! 		iter => mygraph.begin
! ! ! 		iter => iter.next
! ! ! 		
! ! ! 		call mygraph.erase( iter )
! ! ! 		call showMyGraph( mygraph )
! ! ! 		
! ! ! 		write(*,*)
! ! ! 		write(*,*) "call mygraph.erase( mygraph.begin )"
! ! ! 		write(*,*)
! ! ! 		
! ! ! 		call mygraph.erase( mygraph.begin )
! ! ! 		call showMyGraph( mygraph )
! ! ! 		
! ! ! 		write(*,*)
! ! ! 		write(*,*) "call mygraph.erase( mygraph.end )"
! ! ! 		write(*,*)
! ! ! 		call mygraph.erase( mygraph.end )
! ! ! 		call showMyGraph( mygraph )
! ! 		
! ! 		write(*,*) "------------------------"
! ! 		write(*,*) "Testing for clear method"
! ! 		write(*,*) "------------------------"
! ! 		
! ! 		write(*,*) "call mygraph.clear()"
! ! 		write(*,*)
! ! 		call mygraph.clear()
! ! 		call showMyGraph( mygraph )
! ! 
! ! 		write(*,*) "call mygraph.append( 1 )"
! ! 		write(*,*) "call mygraph.append( 2 )"
! ! 		write(*,*) "call mygraph.append( 3 )"
! ! 		write(*,*)
! ! 		
! ! 		call mygraph.append( 1 )
! ! 		call mygraph.append( 2 )
! ! 		call mygraph.append( 3 )
! ! 		call showMyGraph( mygraph )

	end subroutine IntegerHyperVector_test
	
end module IntegerHyperVector_
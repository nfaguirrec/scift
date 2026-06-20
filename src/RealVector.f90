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

module RealVector_
	use IOStream_
	use String_
	implicit none
	private
	
	public :: &
		RealVector_test

!>
!! This class use the Vector template declared into Vector.h90 file,
!! please take a look to this file for details
!!
#define Vector RealVector
#define VectorIterator RealVectorIterator
#define __CLASS_ITEMVECTOR__ real(8)
#define __TYPE_ITEMVECTOR__ real(8)
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
		class(RealVector), intent(in) :: this
		class(RealVector), intent(in) :: other
		logical :: output
		
! 		this.nItems = other.nItems
! 		this.resizeIncrement = other.resizeIncrement
		
		output = all( this.data(1:this.size()) == other.data(1:other.size()) )
	end function equal
	
	!>
	!! @brief Converts to string
	!!
	function str( this, formatted, prefix ) result( output )
		class(RealVector) :: this 
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
			output = trim(output)//"<RealVector:("
			do i=1,this.size()
				if( i==1 ) then
					output = trim(output)//trim(FString_fromReal(this.at(i)))
				else
					output = trim(output)//","//trim(FString_fromReal(this.at(i)))
				end if
			end do
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
		class(RealVector) :: this
		type(OFStream), optional, intent(in) :: ofile
		
		integer :: unitEff
		
		type(RealVectorIterator), pointer :: iter
		
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
		type(RealVector) :: myvector
		class(RealVectorIterator), pointer :: iter
		
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
	subroutine RealVector_test()
		use TestUtils_
		type(RealVector) :: myvector
		
		call myvector.init()
		call assert_equal( myvector%size(), 0, "RealVector_test: size init" )
		
		call myvector.append( 8.0_8 )
		call myvector.append( 5.0_8 )
		call myvector.append( 1.0_8 )
		call assert_equal( myvector%size(), 3, "RealVector_test: size append" )
		call assert_true( abs(myvector%at(1) - 8.0_8) < 1e-12_8, "RealVector_test: append 1" )
		call assert_true( abs(myvector%at(2) - 5.0_8) < 1e-12_8, "RealVector_test: append 2" )
		call assert_true( abs(myvector%at(3) - 1.0_8) < 1e-12_8, "RealVector_test: append 3" )
		
		call myvector.prepend( 8.0_8 )
		call myvector.prepend( 5.0_8 )
		call myvector.prepend( 1.0_8 )
		call assert_equal( myvector%size(), 6, "RealVector_test: size prepend" )
		call assert_true( abs(myvector%at(1) - 1.0_8) < 1e-12_8, "RealVector_test: prepend 1" )
		call assert_true( abs(myvector%at(2) - 5.0_8) < 1e-12_8, "RealVector_test: prepend 2" )
		call assert_true( abs(myvector%at(3) - 8.0_8) < 1e-12_8, "RealVector_test: prepend 3" )
		call assert_true( abs(myvector%at(4) - 8.0_8) < 1e-12_8, "RealVector_test: prepend 4" )
		call assert_true( abs(myvector%at(5) - 5.0_8) < 1e-12_8, "RealVector_test: prepend 5" )
		call assert_true( abs(myvector%at(6) - 1.0_8) < 1e-12_8, "RealVector_test: prepend 6" )
		
		call myvector.erase( 1 )
		call assert_equal( myvector%size(), 5, "RealVector_test: size erase 1" )
		call assert_true( abs(myvector%at(1) - 5.0_8) < 1e-12_8, "RealVector_test: erase 1" )
		
		call myvector.erase( 2 )
		call assert_equal( myvector%size(), 4, "RealVector_test: size erase 2" )
		call assert_true( abs(myvector%at(2) - 8.0_8) < 1e-12_8, "RealVector_test: erase 2" )
		
		call myvector.erase( 3 )
		call assert_equal( myvector%size(), 3, "RealVector_test: size erase 3" )
		call assert_true( abs(myvector%at(3) - 1.0_8) < 1e-12_8, "RealVector_test: erase 3" )
		
		call myvector.erase( 1 )
		call assert_equal( myvector%size(), 2, "RealVector_test: size erase 4" )
		call assert_true( abs(myvector%at(1) - 8.0_8) < 1e-12_8, "RealVector_test: erase 4" )
		
		call myvector.erase( 1 )
		call assert_equal( myvector%size(), 1, "RealVector_test: size erase 5" )
		call assert_true( abs(myvector%at(1) - 1.0_8) < 1e-12_8, "RealVector_test: erase 5" )
		
		call myvector.erase( 1 )
		call assert_equal( myvector%size(), 0, "RealVector_test: size erase 6" )
		
		call myvector.erase( 1 )
		call assert_equal( myvector%size(), 0, "RealVector_test: size erase 7" )
		
		call myvector.clear()
		call assert_equal( myvector%size(), 0, "RealVector_test: size clear" )
		
		call myvector.append( 1.0_8 )
		call myvector.append( 2.0_8 )
		call myvector.append( 3.0_8 )
		call assert_equal( myvector%size(), 3, "RealVector_test: size final append" )
		call assert_true( abs(myvector%at(1) - 1.0_8) < 1e-12_8, "RealVector_test: final append 1" )
		call assert_true( abs(myvector%at(2) - 2.0_8) < 1e-12_8, "RealVector_test: final append 2" )
		call assert_true( abs(myvector%at(3) - 3.0_8) < 1e-12_8, "RealVector_test: final append 3" )

	end subroutine RealVector_test

end module RealVector_
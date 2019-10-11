!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                                                                                   !!
!!  This file is part of SciFT project                                               !!
!!  Copyright (c) 2013-2015 Nestor F. Aguirre (nfaguirrec@gmail.com)                 !!
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

!>
!! @brief
!!
module RNFunction2D_
	use GOptions_
	use Math_
	use String_
	use IOStream_
	use RNFunction_
	use Grid2D_
	implicit none
	private
	
	public :: &
		RNFunction2D_test
	
!>
!! This class use the List template declared into List.h90 file,
!! please take a look to this file for details
!!
#define NFunction2D RNFunction2D
#define __TYPE_VALUE__ real(8)
#define __ADD_METHODS__
#define __ID_TYPE__ 0
#include "NFunction2D.h90"
#undef NFunction2D
#undef __TYPE_VALUE__
#undef __ADD_METHODS__
#undef __ID_TYPE__
	
	!>
	!! @brief Convert to string
	!!
	function str( this, formatted, prefix ) result( output )
		class(RNFunction2D) :: this 
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
#define ITEMS(l,v) output = trim(output)//effPrefix//trim(l)//trim(adjustl(v))
#define ITEMI(l,v) output = trim(output)//l; write(fstr, "(i20)") v; output = trim(output)//trim(adjustl(fstr))
#define ITEMR(l,v) output = trim(output)//l; write(fstr, "(f20.6)") v; output = trim(output)//trim(adjustl(fstr))
		
			output = trim(output)//"<RNFunction2D:"
			output = trim(output)//trim(this%xyGrid%str())
! 			ITEMI( "min=", this%min )
			ITEMI( ",sizex=", this%nPoints(1) )
			ITEMI( ",sizey=", this%nPoints(2) )
#undef ITEMS
#undef ITEMI
#undef ITEMR
			output = trim(output)//">"
		else
#define LINE(l) output = trim(output)//effPrefix//l//new_line('')
#define ITEMS(l,v) output = trim(output)//effPrefix//l; write(fstr, "(x,a)") trim(v); output = trim(output)//trim(fstr)//new_line('')
#define ITEMI(l,v) output = trim(output)//effPrefix//l; write(fstr, "(i10)") v; output = trim(output)//trim(fstr)//new_line('')
#define ITEMR(l,v) output = trim(output)//effPrefix//l; write(fstr, "(f10.5)") v; output = trim(output)//trim(fstr)//new_line('')

			LINE("RNFunction2D")
			LINE("---------")
! 			ITEMI( "min=", this%min )
! 			ITEMR( ",size=", this%size )
			LINE("")
#undef LINE
#undef ITEMS
#undef ITEMI
#undef ITEMR
		end if
	end function str
	
	!>
	!! Save the data in two column format in a
	!! selected unit
	!!
	subroutine toFStream( this, ofile )
		class(RNFunction2D) :: this
		type(OFStream), optional, intent(in) :: ofile
		
		integer :: unitEff
		
		if( present(ofile) ) then
			unitEff = ofile.unit
		else
			unitEff = IO_STDOUT
		end if
		
		write(*,*) "### ERROR ### RNFunction2D%toFStream is no implemented yet"
		stop
	end subroutine toFStream

	!>
	!! @brief Show 
	!!
	subroutine show( this, unit, formatted )
		class(RNFunction2D) :: this
		integer, optional, intent(in) :: unit
		logical, optional :: formatted
		
		integer :: effunit
		logical :: effFormatted
		
		effFormatted = .false.
		if( present(formatted) ) effFormatted = formatted
		
		effunit = 6
		if( present(unit) ) effunit = unit
		
		write(effunit,"(a)") trim(str(this,effFormatted))
	end subroutine show
	
	!>
	!! This is neccesary only for NFunction_test()
	!!
	pure function funcTest( x, y ) result( output )
		real(8), intent(in) :: x, y
		real(8) :: output
		
		output = sin( Math_PI*sqrt(x**2+y**2) ) /sqrt(x**2+y**2)  ! Mexican Hat function
	end function funcTest
	
	!>
	!! @brief Test method
	!!
	subroutine RNFunction2D_test()
		type(RNFunction2D) :: func, func2
		
		real(8) :: xVec(3)
		real(8) :: yVec(2)
		real(8) :: fArray(3,2)
		
		type(Grid2D) :: xyGrid
	
		xVec(:) = [ 1.0, 2.0, 3.0 ]
		yVec(:) = [-1.0, 0.0 ]
		
		fArray(1,:) = [ 0.0, 4.0 ]
		fArray(2,:) = [ 2.0, 2.0 ]
		
		fArray(1,:) = [ 6.0,-1.0 ]
		fArray(2,:) = [ 7.0, 0.0 ]
		
		write(*,*) ""
		write(*,*) "----------------------------"
		write(*,*) "Testing constructors"
		write(*,*) "----------------------------"
		
		call func%init( xVec, yVec, fArray )
		call func%show()
		
		call xyGrid%fromArray( xVec, yVec )
		call func%init( xyGrid, fArray )
		call func%show()
		
		call xyGrid%init( min=[-5.0_8,-5.0_8], max=[5.0_8,5.0_8], size=[100,100] )
		call func%init( xyGrid, funcTest )
		call func%show()
		
		call func%init( "data/formats/real-N2DF", format=N2DF_FORMAT )
		call func%show()
		
		write(*,*) ""
		write(*,*) "----------------------------"
		write(*,*) "Testing copy constructors"
		write(*,*) "----------------------------"
		
		func2 = func
		call func2%show()
		
		write(*,*) ""
		write(*,*) "----------------------------"
		write(*,*) "Testing I/O methods"
		write(*,*) "----------------------------"
		
		call func%save( "salida.n2df", format=N2DF_FORMAT )
		call func.load( "salida.n2df", format=N2DF_FORMAT )
		call func%show()
		
		write(*,*) ""
		write(*,*) "----------------------------"
		write(*,*) "Testing resize(+3,+2)"
		write(*,*) "----------------------------"
		
		call func%init( xVec, yVec, fArray )
		call func%show()
		call func%resize( 3, 2, +1, +1 )
		call func%show()
		
	end subroutine RNFunction2D_test
	
end module RNFunction2D_

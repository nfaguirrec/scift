!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                                                                                   !!
!!  This file is part of SciFT project                                               !!
!!  Copyright (c) 2013-2013 Nestor F. Aguirre (nfaguirrec@gmail.com)                 !!
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
module RNFunction3D_
	use GOptions_
	use Math_
	use String_
	use IOStream_
	use RNFunction_
	use Grid3D_
	implicit none
	private
	
	public :: &
		RNFunction3D_test
	
!>
!! This class use the List template declared into List.h90 file,
!! please take a look to this file for details
!!
#define NFunction3D RNFunction3D
#define __TYPE_VALUE__ real(8)
#define __ADD_METHODS__
#define __ID_TYPE__ 0
#include "NFunction3D.h90"
#undef NFunction3D
#undef __TYPE_VALUE__
#undef __ADD_METHODS__
#undef __ID_TYPE__
	
	!>
	!! @brief Convert to string
	!!
	function str( this, formatted, prefix ) result( output )
		class(RNFunction3D) :: this 
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
		
			output = trim(output)//"<RNFunction3D:"
			output = trim(output)//trim(this.xyzGrid.str())
! 			ITEMI( "min=", this.min )
			ITEMI( ",size=", this.size() )
#undef ITEMS
#undef ITEMI
#undef ITEMR
			output = trim(output)//">"
		else
#define LINE(l) output = trim(output)//effPrefix//l//new_line('')
#define ITEMS(l,v) output = trim(output)//effPrefix//l; write(fstr, "(x,a)") trim(v); output = trim(output)//trim(fstr)//new_line('')
#define ITEMI(l,v) output = trim(output)//effPrefix//l; write(fstr, "(i10)") v; output = trim(output)//trim(fstr)//new_line('')
#define ITEMR(l,v) output = trim(output)//effPrefix//l; write(fstr, "(f10.5)") v; output = trim(output)//trim(fstr)//new_line('')

			LINE("RNFunction3D")
			LINE("---------")
! 			ITEMI( "min=", this.min )
! 			ITEMR( ",size=", this.size )
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
		class(RNFunction3D) :: this
		type(OFStream), optional, intent(in) :: ofile
		
		integer :: unitEff
		
		if( present(ofile) ) then
			unitEff = ofile.unit
		else
			unitEff = IO_STDOUT
		end if
		
		write(*,*) "### ERROR ### RNFunction3D.toFStream is no implemented yet"
		stop
	end subroutine toFStream

	!>
	!! @brief Show 
	!!
	subroutine show( this, unit, formatted )
		class(RNFunction3D) :: this
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
	!!       f = exp(-0.44*x)*sin(x)**2
	!!   df/dx = exp(-0.44*x)*(2.0*sin(x)*cos(x)-0.44*sin(x)**2)
	!! d2f/dx2 = exp(-0.44*x)*(2.0*cos(x)**2 - 1.76*cos(x)*sin(x) - 2.0*sin(x)**2 + 0.1936*sin(x)**2)
	!!
	pure function funcTest( x, y, z ) result( output )
		real(8), intent(in) :: x, y, z
		real(8) :: output
		
! 		output = sqrt( x**2 + y**2 + z**2 )
! 		output = (x**2+y**2-1.0_8)**3-x**2*y**3+z**2
		output = (x**2+2.25_8*y**2+z**2-1.0_8)**3-x**2*z**3-0.1125_8*y**2*z**3  ! http://mathworld.wolfram.com/HeartSurface.html
	end function funcTest
	
	!>
	!! This is neccesary only for NFunction_test()
	!!       f = exp(-0.44*x)*sin(x)**2
	!!   df/dx = exp(-0.44*x)*(2.0*sin(x)*cos(x)-0.44*sin(x)**2)
	!! d2f/dx2 = exp(-0.44*x)*(2.0*cos(x)**2 - 1.76*cos(x)*sin(x) - 2.0*sin(x)**2 + 0.1936*sin(x)**2)
	!!
	function dfuncTest( x ) result( output )
		real(8), intent(in) :: x
		real(8) :: output
		
		output = exp(-0.44*x)*(2.0*sin(x)*cos(x)-0.44*sin(x)**2)
	end function dfuncTest
	
	!>
	!! This is neccesary only for NFunction_test()
	!!       f = exp(-0.44*x)*sin(x)**2
	!!   df/dx = exp(-0.44*x)*(2.0*sin(x)*cos(x)-0.44*sin(x)**2)
	!! d2f/dx2 = exp(-0.44*x)*(2.0*cos(x)**2 - 1.76*cos(x)*sin(x) - 2.0*sin(x)**2 + 0.1936*sin(x)**2)
	!!
	function d2funcTest( x ) result( output )
		real(8), intent(in) :: x
		real(8) :: output
		
		output = exp(-0.44*x)*(2.0*cos(x)**2 - 1.76*cos(x)*sin(x) - 2.0*sin(x)**2 + 0.1936*sin(x)**2)
	end function d2funcTest
	
	!>
	!! This is neccesary only for NFunction_test()
	!!
	function funcTest2( x ) result( output )
		real(8), intent(in) :: x
		real(8) :: output
		
		output = sin(x)
	end function funcTest2
	
	!>
	!! @brief Test method
	!!
	subroutine RNFunction3D_test()
		type(RNFunction3D) :: func, func2
		
		real(8) :: xVec(3)
		real(8) :: yVec(2)
		real(8) :: zVec(2)
		real(8) :: fArray(3,2,2)
		
		type(Grid3D) :: xyzGrid
	
		xVec(:) = [ 1.0, 2.0, 3.0 ]
		yVec(:) = [-1.0, 0.0 ]
		zVec(:) = [ 2.0, 1.0 ]
		
		fArray(1,:,1) = [ 0.0, 4.0 ]
		fArray(2,:,1) = [ 2.0, 2.0 ]
		fArray(3,:,1) = [ 0.0, 4.0 ]
		
		fArray(1,:,2) = [ 6.0,-1.0 ]
		fArray(2,:,2) = [ 7.0, 0.0 ]
		fArray(3,:,2) = [ 2.0, 1.0 ]
		
		write(*,*) ""
		write(*,*) "----------------------------"
		write(*,*) "Testing constructors"
		write(*,*) "----------------------------"
		
		call func.init( xVec, yVec, zVec, fArray )
		call func.show()
		
		call xyzGrid.init( xVec, yVec, zVec )
		call func.init( xyzGrid, fArray )
		call func.show()
		
		call xyzGrid.init( min=[-3.0_8,-3.0_8,-3.0_8], max=[3.0_8,3.0_8,3.0_8], size=[50,50,50] )
		call func.init( xyzGrid, funcTest )
		call func.show()
		
		call func.init( "data/formats/real-N3DF", format=N3DF_FORMAT )
		call func.show()
		
		call func.init( "data/formats/CUBE", format=CUBE_FORMAT )
		call func.show()
		
		write(*,*) ""
		write(*,*) "----------------------------"
		write(*,*) "Testing copy constructors"
		write(*,*) "----------------------------"
		
		func2 = func
		call func2.show()
		
		write(*,*) ""
		write(*,*) "----------------------------"
		write(*,*) "Testing I/O methods"
		write(*,*) "----------------------------"
		
		call func.save( "salida.cube", format=CUBE_FORMAT )
		call func.save( "salida.n3df", format=N3DF_FORMAT )
		
		call func.load( "salida.cube", format=CUBE_FORMAT )
		call func.save( "salida2.cube", format=CUBE_FORMAT )
		call func.show()
		
		call func.load( "salida.n3df", format=N3DF_FORMAT )
		call func.save( "salida3.cube", format=CUBE_FORMAT )
		call func.show()
		
		write(*,*) ""
		write(*,*) "----------------------------"
		write(*,*) "Testing resize(+3,+2,+1)"
		write(*,*) "----------------------------"
		
		call func.init( xVec, yVec, zVec, fArray )
		call func.show()
		call func.resize( 3, 2, 1, +1, +1, +1 )
		call func.show()
		
	end subroutine RNFunction3D_test
	
end module RNFunction3D_

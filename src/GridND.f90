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
module GridND_
	use IOStream_
	use Grid_
	implicit none
	private
	
	public :: &
		GridND_test
		
	type, public :: GridND
		type(Grid) :: component(3)
		
		contains
			generic :: init => initDefault, fromComponents, fromArray, fromFile
			procedure :: initDefault
			procedure :: fromComponents
			procedure :: fromArray
			procedure :: fromFile
			generic :: assignment(=) => copyGrid3D
			procedure :: copyGrid3D
			final :: destroyGrid3D
			
			procedure :: isEqualTo
			procedure :: isEquallyspaced
			
			procedure :: addition
			procedure :: subtraction
			procedure :: multiplication
			procedure :: division
			procedure :: exponentiation
			
			procedure :: additionFC
			procedure :: subtractionFC
			procedure :: multiplicationFC
			procedure :: divisionFC
			procedure :: exponentiationFC
			
			generic :: operator(+) => addition, additionFC
			generic :: operator(-) => subtraction, subtractionFC
			generic :: operator(*) => multiplication, multiplicationFC
			generic :: operator(/) => division, divisionFC
			generic :: operator(**) => exponentiation, exponentiationFC
			
			procedure :: str
			procedure :: show
			procedure :: save
			procedure :: toFStream
			procedure :: setUnits

			generic :: size => sizeInCoord, sizeVec
			procedure :: sizeInCoord
			procedure :: sizeVec
			
			generic :: min => minInCoord, minVec
			procedure :: minInCoord
			procedure :: minVec
			
			generic :: max => maxInCoord, maxVec
			procedure :: maxInCoord
			procedure :: maxVec
			
			generic :: stepSize => stepSizeInCoord, stepSizeVec
			procedure :: stepSizeInCoord
			procedure :: stepSizeVec
			
			procedure :: x
			procedure :: y
			procedure :: z
			procedure :: at
			procedure :: dV
	end type GridND
	
	contains
	
	!>
	!! @brief Constructor
	!!
	subroutine initDefault( this, min, max, size, stepSize )
		class(GridND) :: this 
		real(8) :: min(3)
		real(8) :: max(3)
		integer, optional :: size(3)
		real(8), optional :: stepSize(3)
		
		integer :: i
		
		do i=1,3
			call this%component(i)%init( min(i), max(i), size(i), stepSize(i) )
		end do
	end subroutine initDefault
	
	!>
	!! @brief Constructor
	!!
	subroutine fromComponents( this, x, y, z )
		class(GridND) :: this 
		type(Grid), intent(in) :: x
		type(Grid), intent(in) :: y
		type(Grid), intent(in) :: z
		
		this%component(1) = x
		this%component(2) = y
		this%component(3) = z
	end subroutine fromComponents
	
	!>
	!! @brief Constructor
	!!
	subroutine fromArray( this, xArray, yArray, zArray )
		class(GridND) :: this 
		real(8), intent(in) :: xArray(:)
		real(8), intent(in) :: yArray(:)
		real(8), intent(in) :: zArray(:)
		
		call this%component(1)%fromArray( xArray )
		call this%component(2)%fromArray( yArray )
		call this%component(3)%fromArray( zArray )
	end subroutine fromArray
	
	!>
	!! @brief Constructor
	!!
	subroutine fromFile( this, iFileName, columns, tol, cComments )
		class(GridND) :: this
		character(*), intent(in) :: iFileName
		integer, optional, intent(in) :: columns(3)
		real(8), optional, intent(in) :: tol
		character(*), optional, intent(in) :: cComments
		
		integer :: columnsEff(3)
		
		integer :: i
		
		if( present(columns) ) then
			columnsEff = columns
		else
			columnsEff = [1,2,3]
		end if
		
		do i=1,3
			call this%component(i)%fromFile( iFileName, columnsEff(i), tol=tol, cComments=cComments )
		end do
	end subroutine fromFile
	
	!>
	!! @brief Copy constructor
	!!
	subroutine copyGrid3D( this, other )
		class(GridND), intent(out) :: this
		class(GridND), intent(in) :: other
		
		integer :: i
		
		do i=1,3
			this%component(i) = other%component(i)
		end do
	end subroutine copyGrid3D
	
	!>
	!! @brief Destructor
	!!
	subroutine destroyGrid3D( this )
		type(GridND) :: this
		
	end subroutine destroyGrid3D
	
	!>
	!! @brief
	!!
	function isEqualTo( this, other, tol ) result( output )
		class(GridND), intent(in) :: this
		class(GridND), intent(in) :: other
		real(8), optional :: tol
		logical :: output
		
		output = &
			this%component(1)%isEqualTo( other%component(1), tol ) .and. &
			this%component(2)%isEqualTo( other%component(2), tol ) .and. &
			this%component(3)%isEqualTo( other%component(3), tol )
	end function isEqualTo
	
	!>
	!! @brief
	!!
	function isEquallyspaced( this ) result( output )
		class(GridND), intent(in) :: this
		logical :: output
		
		output = &
			this%component(1)%isEquallyspaced .and. &
			this%component(2)%isEquallyspaced .and. &
			this%component(3)%isEquallyspaced
	end function isEquallyspaced

	
	!>
	!! @brief
	!!
	function addition( this, other ) result( output )
		class(GridND), intent(in) :: this
		class(GridND), intent(in) :: other
		type(GridND) :: output
		
		integer :: i
		
		if( this%size(1) /= other%size(1) .or. &
		    this%size(2) /= other%size(2) .or. &
		    this%size(3) /= other%size(3) ) then
			write(*,*) "## ERROR ## the Grids have not the same size"
			stop
		end if
		
		do i=1,3
			output%component(i) = this%component(i) + other%component(i)
		end do
	end function addition
	
	!>
	!! @brief
	!!
	function additionFC( this, constant ) result( output )
		class(GridND), intent(in) :: this
		real(8), intent(in) :: constant
		type(GridND) :: output
		
		integer :: i
		
		do i=1,3
			output%component(i) = this%component(i) + constant
		end do
	end function additionFC
	
	!>
	!! @brief
	!!
	function subtraction( this, other ) result( output )
		class(GridND), intent(in) :: this
		type(GridND), intent(in) :: other
		type(GridND) :: output
		
		integer :: i
		
		if( this%size(1) /= other%size(1) .or. &
		    this%size(2) /= other%size(2) .or. &
		    this%size(3) /= other%size(3) ) then
			write(*,*) "## ERROR ## the Grids have not the same size"
			stop
		end if
		
		do i=1,3
			output%component(i) = this%component(i) - other%component(i)
		end do
	end function subtraction
	
	!>
	!! @brief
	!!
	function subtractionFC( this, constant ) result( output )
		class(GridND), intent(in) :: this
		real(8), intent(in) :: constant
		type(GridND) :: output
		
		integer :: i
		
		do i=1,3
			output%component(i) = this%component(i) + constant
		end do
	end function subtractionFC
	
	!>
	!! @brief
	!!
	function multiplication( this, other ) result( output )
		class(GridND), intent(in) :: this
		type(GridND), intent(in) :: other
		type(GridND) :: output
		
		integer :: i
		
		if( this%size(1) /= other%size(1) .or. &
		    this%size(2) /= other%size(2) .or. &
		    this%size(3) /= other%size(3) ) then
			write(*,*) "## ERROR ## the Grids have not the same size"
			stop
		end if
		
		do i=1,3
			output%component(i) = this%component(i)*other%component(i)
		end do
	end function multiplication
	
	!>
	!! @brief
	!!
	function multiplicationFC( this, constant ) result( output )
		class(GridND), intent(in) :: this
		real(8), intent(in) :: constant
		type(GridND) :: output
		
		integer :: i
		
		do i=1,3
			output%component(i) = this%component(i)*constant
		end do
	end function multiplicationFC
	
	!>
	!! @brief
	!!
	function division( this, other ) result( output )
		class(GridND), intent(in) :: this
		type(GridND), intent(in) :: other
		type(GridND) :: output
		
		integer :: i
		
		if( this%size(1) /= other%size(1) .or. &
		    this%size(2) /= other%size(2) .or. &
		    this%size(3) /= other%size(3) ) then
			write(*,*) "## ERROR ## the Grids have not the same size"
			stop
		end if
		
		do i=1,3
			output%component(i) = this%component(i)/other%component(i)
		end do
	end function division
	
	!>
	!! @brief
	!!
	function divisionFC( this, constant ) result( output )
		class(GridND), intent(in) :: this
		real(8), intent(in) :: constant
		type(GridND) :: output
		
		integer :: i
		
		do i=1,3
			output%component(i) = this%component(i)/constant
		end do
	end function divisionFC
	
	!>
	!! @brief
	!!
	function exponentiation( this, other ) result( output )
		class(GridND), intent(in) :: this
		type(GridND), intent(in) :: other
		type(GridND) :: output
		
		integer :: i
		
		if( this%size(1) /= other%size(1) .or. &
		    this%size(2) /= other%size(2) .or. &
		    this%size(3) /= other%size(3) ) then
			write(*,*) "## ERROR ## the Grids have not the same size"
			stop
		end if
		
		do i=1,3
			output%component(i) = this%component(i)**other%component(i)
		end do
	end function exponentiation
	
	!>
	!! @brief
	!!
	function exponentiationFC( this, constant ) result( output )
		class(GridND), intent(in) :: this
		real(8), intent(in) :: constant
		type(GridND) :: output
		
		integer :: i
		
		do i=1,3
			output%component(i) = this%component(i)**constant
		end do
	end function exponentiationFC
	
	!>
	!! @brief Convert to string
	!!
	function str( this, formatted, prefix ) result( output )
		class(GridND) :: this 
		character(:), allocatable :: output
		logical, optional :: formatted
		character(*), optional :: prefix
		
		logical :: effFormatted
		character(:), allocatable :: effPrefix
		
		character(200) :: fstr
		
		effFormatted = .false.
		if( present(formatted) ) effFormatted = formatted
		
		effPrefix = ""
		if( present(prefix) ) effPrefix = prefix
		
		output = ""
		
		if( .not. effFormatted ) then
#define ITEMS(l,v) output = trim(output)//effPrefix//trim(l)//trim(adjustl(v))
#define ITEMI(l,v) output = trim(output)//l; write(fstr, "(I20)") v; output = trim(output)//trim(adjustl(fstr))
#define ITEMR(l,v) output = trim(output)//l; write(fstr, "(F20.6)") v; output = trim(output)//trim(adjustl(fstr))
#define ITEML(l,v) output = trim(output)//l; write(fstr, "(L3)") v; output = trim(output)//trim(adjustl(fstr))
		
			output = trim(output)//"<GridND:"
			ITEMR( "min=(", this%component(1)%min )
			ITEMR( ";", this%component(2)%min )
			ITEMR( ";", this%component(3)%min )
			ITEMR( "),max=(", this%component(1)%max )
			ITEMR( ";", this%component(2)%max )
			ITEMR( ";", this%component(3)%max )
			ITEMI( "),nPoints=(", this%component(1)%nPoints )
			ITEMI( ";", this%component(2)%nPoints )
			ITEMI( ";", this%component(3)%nPoints )
			ITEML( "),isEquallyspaced=(", this%component(1)%isEquallyspaced )
			ITEML( ";", this%component(2)%isEquallyspaced )
			ITEML( ";", this%component(3)%isEquallyspaced )
			ITEMS( "", ")" )
#undef ITEMS
#undef ITEMI
#undef ITEMR
#undef ITEML
			output = trim(output)//">"
		else
#define LINE(l) output = trim(output)//effPrefix//l//new_line('')
#define ITEMS(l,v) output = trim(output)//effPrefix//l; write(fstr, "(x,a)") trim(v); output = trim(output)//trim(fstr)//new_line('')
#define ITEMI(l,v) output = trim(output)//effPrefix//l; write(fstr, "(i10)") v; output = trim(output)//trim(fstr)//new_line('')
#define ITEMR(l,v) output = trim(output)//effPrefix//l; write(fstr, "(f10.5)") v; output = trim(output)//trim(fstr)//new_line('')

			LINE("GridND")
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
	!! @brief Show 
	!!
	subroutine show( this, unit, formatted )
		class(GridND) :: this
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
	!! Save the data in one column format in a
	!! selected unit
	!!
	subroutine save( this, ofileName, units )
		class(GridND) :: this
		character(*), optional, intent(in) :: ofileName
		real(8), optional, intent(in) :: units
		
		type(OFStream) :: ofile
		
		if( present(units) .and. present(ofileName) ) then
			call ofile%init( ofileName )
			call toFStream( this, ofile, units )
			call ofile.close()
		else if( present(ofileName) ) then
			call ofile%init( ofileName )
			call toFStream( this, ofile )
			call ofile.close()
		else
			call toFStream( this )
		end if
		
	end subroutine save
	
	!>
	!! Save the data in one column format in a
	!! selected unit
	!!
	subroutine toFStream( this, ofile, units )
		class(GridND) :: this
		type(OFStream), optional, intent(in) :: ofile
		real(8), optional, intent(in) :: units
		
		integer :: unitEff
		integer :: i
		
		if( present(ofile) ) then
			unitEff = ofile.unit
		else
			unitEff = IO_STDOUT
		end if
		
		do i=1,3
			select case( i )
				case(1)
					write( unitEff, "(A)" ) "# X"
				case(2)
					write( unitEff, "(A)" ) "# Y"
				case(3)
					write( unitEff, "(A)" ) "# Z"
			end select
			
			call this%component(i)%toFStream( ofile, units )
			write( unitEff, * ) ""
			write( unitEff, * ) ""
		end do
	end subroutine toFStream
	
	!>
	!! @brief
	!!
	subroutine setUnits( this, unit )
		class(GridND) :: this
		real(8), intent(in) :: unit
		
		integer :: i
		
		do i=1,3
			call this%component(i)%setUnits( unit )
		end do
	end subroutine setUnits
	
	!>
	!! @brief Returns the size of the component i-th
	!!
	pure function sizeInCoord( this, i ) result( output )
		class(GridND), intent(in) :: this
		integer, intent(in) :: i
		integer :: output
		
		output = this%component(i)%nPoints
	end function sizeInCoord
	
	!>
	!! @brief
	!!
	pure function sizeVec( this ) result( output )
		class(GridND), intent(in) :: this
		integer :: output(3)
		
		output = [ this%component(1)%nPoints, this%component(2)%nPoints, this%component(3)%nPoints ]
	end function sizeVec
	
	!>
	!! @brief
	!!
	pure function minInCoord( this, i ) result( output )
		class(GridND), intent(in) :: this
		integer, intent(in) :: i
		real(8) :: output
		
		output = this%component(i)%min
	end function minInCoord
	
	!>
	!! @brief
	!!
	pure function minVec( this ) result( output )
		class(GridND), intent(in) :: this
		real(8) :: output(3)
		
		output = [ this%component(1)%min, this%component(2)%min, this%component(3)%min ]
	end function minVec
	
	!>
	!! @brief
	!!
	pure function maxInCoord( this, i ) result( output )
		class(GridND), intent(in) :: this
		integer, intent(in) :: i
		real(8) :: output
		
		output = this%component(i)%max
	end function maxInCoord
	
	!>
	!! @brief
	!!
	pure function maxVec( this ) result( output )
		class(GridND), intent(in) :: this
		real(8) :: output(3)
		
		output = [ this%component(1)%max, this%component(2)%max, this%component(3)%max ]
	end function maxVec
	
	!>
	!! @brief Returns the size of the component i-th
	!!
	pure function stepSizeInCoord( this, i ) result( output )
		class(GridND), intent(in) :: this
		integer, intent(in) :: i
		real(8) :: output
		
		output = this%component(i)%stepSize
	end function stepSizeInCoord
	
	!>
	!! @brief
	!!
	pure function stepSizeVec( this ) result( output )
		class(GridND), intent(in) :: this
		real(8) :: output(3)
		
		output = [ this%component(1)%stepSize, this%component(2)%stepSize, this%component(3)%stepSize ]
	end function stepSizeVec
	
	!>
	!! @brief
	!!
	pure function x( this, i ) result( output )
		class(GridND), intent(in) :: this
		integer, intent(in) :: i
		real(8) :: output
		
		output = this%component(1)%data(i)
	end function x
	
	!>
	!! @brief
	!!
	pure function y( this, i ) result( output )
		class(GridND), intent(in) :: this
		integer, intent(in) :: i
		real(8) :: output
		
		output = this%component(2)%data(i)
	end function y
	
	!>
	!! @brief
	!!
	pure function z( this, i ) result( output )
		class(GridND), intent(in) :: this
		integer, intent(in) :: i
		real(8) :: output
		
		output = this%component(3)%data(i)
	end function z
	
	!>
	!! @brief
	!!
	function at( this, i, j, k ) result( output )
		class(GridND), intent(in) :: this 
		integer, intent(in) :: i
		integer, intent(in) :: j
		integer, intent(in) :: k
		real(8) :: output(3)
		
		output = [ this%component(1)%data(i), this%component(2)%data(j), this%component(3)%data(k) ]
	end function at
	
	!>
	!! @brief
	!!
	pure function dV( this ) result( output )
		class(GridND), intent(in) :: this
		real(8) :: output
		
		output = this%component(1)%stepSize*this%component(2)%stepSize*this%component(3)%stepSize
	end function dV
	
	!>
	!! @brief Auxiliar function for test method
	!!
	subroutine showTest( xyzGrid )
		type(GridND), intent(in) :: xyzGrid
		
		integer :: i, j
		
		do i=1,3
			select case( i )
				case( 1 )
					write(*,*) ""
					write(*,*) "X = "
				case( 2 )
					write(*,*) ""
					write(*,*) "Y = "
				case( 3 )
					write(*,*) ""
					write(*,*) "Z = "
			end select
			
			do j=1,xyzGrid%size(i)
				write(*,"(f10.5)", advance="no") xyzGrid%component(i)%data(j)
				
				if ( mod(j-1,10) == 9 ) then
					write(*,"(A)") ""
				end if
			end do
		end do
	end subroutine showTest
	
	!>
	!! @brief Test method
	!!
	subroutine GridND_test()
		type(GridND) :: xyzGrid, xyzGrid2
		
		integer :: i, j
		real(8) :: rMin(3), rMax(3)
		integer :: gridSize(3)
		
		rMin = [-5.0_8,-5.0_8,-10.0_8]
		rMax = [ 5.0_8, 5.0_8, 10.0_8]
		gridSize = [10, 10, 20]
		
		call xyzGrid%init( rMin, rMax, gridSize )
		
		write(*,*) ""
		write(*,*) "----------------------------"
		write(*,*) "Testing constructor"
		write(*,*) "----------------------------"
		call xyzGrid%show()
		call showtest( xyzGrid )
		
		write(*,*) ""
		write(*,*) "----------------------------"
		write(*,*) "Testing copy constructor"
		write(*,*) "----------------------------"
		
		xyzGrid2 = xyzGrid
		call xyzGrid2%show()
		call showtest( xyzGrid2 )
		
		write(*,*) ""
		write(*,*) "----------------------------"
		write(*,*) "Testing save method"
		write(*,*) "----------------------------"
		call xyzGrid%save()
		
		write(*,*) ""
		write(*,*) "----------------------------"
		write(*,*) "Testing operators"
		write(*,*) "----------------------------"
		xyzGrid2 = xyzGrid*2.0_8
		call xyzGrid%save()
		call xyzGrid2%save()
		
		xyzGrid2 = xyzGrid*xyzGrid
		call xyzGrid%save()
		call xyzGrid2%save()
		
	end subroutine GridND_test
	
end module GridND_

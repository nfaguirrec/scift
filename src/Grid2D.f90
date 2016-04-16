!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!  This file is part of scift (Scientific Fortran Tools).
!!  Copyright (C) by authors (2013-2015)
!!  
!!  Authors (alphabetic order):
!!    * Aguirre N.F. (nfaguirrec@gmail.com)  (2015-2015)
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

!>
!! @brief
!!
module Grid2D_
	use GOptions_
	use IOStream_
	use Grid_
	implicit none
	private
	
	public :: &
		Grid2D_test
		
	type, public :: Grid2D
		type(Grid) :: component(2)
		
		contains
! 			generic :: init => initDefault, fromComponents, fromArray, fromFile
			generic :: init => initDefault, fromComponents, fromFile
			procedure :: initDefault
			procedure :: fromComponents
			procedure :: fromArray
			procedure :: fromFile
			generic :: assignment(=) => copyGrid2D
			procedure :: copyGrid2D
			final :: destroyGrid2D
			
			procedure :: isEqualTo
			procedure :: isEquallyspaced
			procedure :: checkEquallyspaced
			
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

			generic :: nPoints => nPointsInCoord, nPointsVec
			procedure :: nPointsInCoord
			procedure :: nPointsVec
			
			generic :: min => minInCoord, minVec
			procedure :: minInCoord
			procedure :: minVec
			
			generic :: max => maxInCoord, maxVec
			procedure :: maxInCoord
			procedure :: maxVec
			
			generic :: stepSize => stepSizeInCoord, stepSizeVec
			procedure :: stepSizeInCoord
			procedure :: stepSizeVec
			
			generic :: resize => resize1, resize2
			procedure :: resize1
			procedure :: resize2
			
			procedure :: x
			procedure :: y
			procedure :: at
			procedure :: dV
			
			procedure :: set
	end type Grid2D
	
	contains
	
	!>
	!! @brief Constructor
	!!
	subroutine initDefault( this, min, max, size, stepSize )
		class(Grid2D) :: this 
		real(8) :: min(2)
		real(8) :: max(2)
		integer, optional :: size(2)
		real(8), optional :: stepSize(2)
		
		integer :: i
		
		do i=1,2
			call this.component(i).init( min(i), max(i), size(i), stepSize(i) )
		end do
	end subroutine initDefault
	
	!>
	!! @brief Constructor
	!!
	subroutine fromComponents( this, x, y )
		class(Grid2D) :: this 
		type(Grid), intent(in) :: x
		type(Grid), intent(in) :: y
		
		this.component(1) = x
		this.component(2) = y
	end subroutine fromComponents
	
	!>
	!! @brief Constructor
	!!
	subroutine fromArray( this, xArray, yArray )
		class(Grid2D) :: this 
		real(8), intent(in) :: xArray(:)
		real(8), intent(in) :: yArray(:)
		
		call this.component(1).fromArray( xArray )
		call this.component(2).fromArray( yArray )
	end subroutine fromArray
	
	!>
	!! @brief Constructor
	!!
	subroutine fromFile( this, iFileName, columns, tol, cComments )
		class(Grid2D) :: this
		character(*), intent(in) :: iFileName
		integer, optional, intent(in) :: columns(2)
		real(8), optional, intent(in) :: tol
		character(*), optional, intent(in) :: cComments
		
		integer :: columnsEff(2)
		
		integer :: i
		
		if( present(columns) ) then
			columnsEff = columns
		else
			columnsEff = [1,2]
		end if
		
		do i=1,2
			call this.component(i).fromFile( iFileName, columnsEff(i), tol=tol, cComments=cComments )
		end do
	end subroutine fromFile
	
	!>
	!! @brief Copy constructor
	!!
	subroutine copyGrid2D( this, other )
		class(Grid2D), intent(out) :: this
		class(Grid2D), intent(in) :: other
		
		integer :: i
		
		do i=1,2
			this.component(i) = other.component(i)
		end do
	end subroutine copyGrid2D
	
	!>
	!! @brief Destructor
	!!
	subroutine destroyGrid2D( this )
		type(Grid2D) :: this
		
	end subroutine destroyGrid2D
	
	!>
	!! @brief
	!!
	function isEqualTo( this, other, tol ) result( output )
		class(Grid2D), intent(in) :: this
		class(Grid2D), intent(in) :: other
		real(8), optional :: tol
		logical :: output
		
		output = &
			this.component(1).isEqualTo( other.component(1), tol ) .and. &
			this.component(2).isEqualTo( other.component(2), tol )
	end function isEqualTo
	
	!>
	!! @brief
	!!
	function isEquallyspaced( this ) result( output )
		class(Grid2D), intent(in) :: this
		logical :: output
		
		output = &
			this.component(1).isEquallyspaced .and. &
			this.component(2).isEquallyspaced
	end function isEquallyspaced
	
	!>
	!! @brief
	!!
	subroutine checkEquallyspaced( this )
		class(Grid2D) :: this
		
		call this.component(1).checkEquallyspaced()
		call this.component(2).checkEquallyspaced()
	end subroutine checkEquallyspaced

	
	!>
	!! @brief
	!!
	function addition( this, other ) result( output )
		class(Grid2D), intent(in) :: this
		class(Grid2D), intent(in) :: other
		type(Grid2D) :: output
		
		integer :: i
		
		if( this.nPoints(1) /= other.nPoints(1) .or. &
		    this.nPoints(2) /= other.nPoints(2) ) then
			call GOptions_error( "Grids have not the same size", "Grid2D.fromArrays()" )
		end if
		
		do i=1,2
			output.component(i) = this.component(i) + other.component(i)
		end do
	end function addition
	
	!>
	!! @brief
	!!
	function additionFC( this, constant ) result( output )
		class(Grid2D), intent(in) :: this
		real(8), intent(in) :: constant
		type(Grid2D) :: output
		
		integer :: i
		
		do i=1,2
			output.component(i) = this.component(i) + constant
		end do
	end function additionFC
	
	!>
	!! @brief
	!!
	function subtraction( this, other ) result( output )
		class(Grid2D), intent(in) :: this
		type(Grid2D), intent(in) :: other
		type(Grid2D) :: output
		
		integer :: i
		
		if( this.nPoints(1) /= other.nPoints(1) .or. &
		    this.nPoints(2) /= other.nPoints(2) ) then
			call GOptions_error( "Grids have not the same size", "Grid2D.subtraction()" )
		end if
		
		do i=1,2
			output.component(i) = this.component(i) - other.component(i)
		end do
	end function subtraction
	
	!>
	!! @brief
	!!
	function subtractionFC( this, constant ) result( output )
		class(Grid2D), intent(in) :: this
		real(8), intent(in) :: constant
		type(Grid2D) :: output
		
		integer :: i
		
		do i=1,2
			output.component(i) = this.component(i) + constant
		end do
	end function subtractionFC
	
	!>
	!! @brief
	!!
	function multiplication( this, other ) result( output )
		class(Grid2D), intent(in) :: this
		type(Grid2D), intent(in) :: other
		type(Grid2D) :: output
		
		integer :: i
		
		if( this.nPoints(1) /= other.nPoints(1) .or. &
		    this.nPoints(2) /= other.nPoints(2) ) then
			call GOptions_error( "Grids have not the same size", "Grid2D.multiplication()" )
		end if
		
		do i=1,2
			output.component(i) = this.component(i)*other.component(i)
		end do
	end function multiplication
	
	!>
	!! @brief
	!!
	function multiplicationFC( this, constant ) result( output )
		class(Grid2D), intent(in) :: this
		real(8), intent(in) :: constant
		type(Grid2D) :: output
		
		integer :: i
		
		do i=1,2
			output.component(i) = this.component(i)*constant
		end do
	end function multiplicationFC
	
	!>
	!! @brief
	!!
	function division( this, other ) result( output )
		class(Grid2D), intent(in) :: this
		type(Grid2D), intent(in) :: other
		type(Grid2D) :: output
		
		integer :: i
		
		if( this.nPoints(1) /= other.nPoints(1) .or. &
		    this.nPoints(2) /= other.nPoints(2) ) then
			call GOptions_error( "Grids have not the same size", "Grid2D.division()" )
		end if
		
		do i=1,2
			output.component(i) = this.component(i)/other.component(i)
		end do
	end function division
	
	!>
	!! @brief
	!!
	function divisionFC( this, constant ) result( output )
		class(Grid2D), intent(in) :: this
		real(8), intent(in) :: constant
		type(Grid2D) :: output
		
		integer :: i
		
		do i=1,2
			output.component(i) = this.component(i)/constant
		end do
	end function divisionFC
	
	!>
	!! @brief
	!!
	function exponentiation( this, other ) result( output )
		class(Grid2D), intent(in) :: this
		type(Grid2D), intent(in) :: other
		type(Grid2D) :: output
		
		integer :: i
		
		if( this.nPoints(1) /= other.nPoints(1) .or. &
		    this.nPoints(2) /= other.nPoints(2) ) then
			call GOptions_error( "Grids have not the same size", "Grid2D.exponentiation()" )
		end if
		
		do i=1,2
			output.component(i) = this.component(i)**other.component(i)
		end do
	end function exponentiation
	
	!>
	!! @brief
	!!
	function exponentiationFC( this, constant ) result( output )
		class(Grid2D), intent(in) :: this
		real(8), intent(in) :: constant
		type(Grid2D) :: output
		
		integer :: i
		
		do i=1,2
			output.component(i) = this.component(i)**constant
		end do
	end function exponentiationFC
	
	!>
	!! @brief Convert to string
	!!
	function str( this, formatted, prefix ) result( output )
		class(Grid2D) :: this 
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
		
			output = trim(output)//"<Grid2D:"
			ITEMR( "min=(", this.component(1).min )
			ITEMR( ";", this.component(2).min )
			ITEMR( "),first=(", this.component(1).first() )
			ITEMR( ";", this.component(2).first() )
			ITEMR( "),max=(", this.component(1).max )
			ITEMR( ";", this.component(2).max )
			ITEMR( "),last=(", this.component(1).last() )
			ITEMR( ";", this.component(2).last() )
			ITEMI( "),nPoints=(", this.component(1).nPoints )
			ITEMI( ";", this.component(2).nPoints )
			ITEMR( "),stepSize=(", this.component(1).stepSize )
			ITEMR( ";", this.component(2).stepSize )
			ITEML( "),isEquallyspaced=(", this.component(1).isEquallyspaced )
			ITEML( ";", this.component(2).isEquallyspaced )
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

			LINE("Grid2D")
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
	!! @brief Show 
	!!
	subroutine show( this, unit, formatted )
		class(Grid2D) :: this
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
		class(Grid2D) :: this
		character(*), optional, intent(in) :: ofileName
		real(8), optional, intent(in) :: units
		
		type(OFStream) :: ofile
		
		if( present(units) .and. present(ofileName) ) then
			call ofile.init( ofileName )
			call toFStream( this, ofile, units )
			call ofile.close()
		else if( present(ofileName) ) then
			call ofile.init( ofileName )
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
		class(Grid2D) :: this
		type(OFStream), optional, intent(in) :: ofile
		real(8), optional, intent(in) :: units
		
		integer :: unitEff
		integer :: i
		
		if( present(ofile) ) then
			unitEff = ofile.unit
		else
			unitEff = IO_STDOUT
		end if
		
		do i=1,2
			select case( i )
				case(1)
					write( unitEff, "(A)" ) "# X"
				case(2)
					write( unitEff, "(A)" ) "# Y"
			end select
			
			call this.component(i).toFStream( ofile, units )
			write( unitEff, * ) ""
			write( unitEff, * ) ""
		end do
	end subroutine toFStream
	
	!>
	!! @brief
	!!
	subroutine setUnits( this, unit )
		class(Grid2D) :: this
		real(8), intent(in) :: unit
		
		integer :: i
		
		do i=1,2
			call this.component(i).setUnits( unit )
		end do
	end subroutine setUnits
	
	!>
	!! @brief Returns the size of the component i-th
	!!
	pure function nPointsInCoord( this, i ) result( output )
		class(Grid2D), intent(in) :: this
		integer, intent(in) :: i
		integer :: output
		
		output = this.component(i).nPoints
	end function nPointsInCoord
	
	!>
	!! @brief
	!!
	pure function nPointsVec( this ) result( output )
		class(Grid2D), intent(in) :: this
		integer :: output(2)
		
		output = [ this.component(1).nPoints, this.component(2).nPoints ]
	end function nPointsVec
	
	!>
	!! @brief
	!!
	pure function minInCoord( this, i ) result( output )
		class(Grid2D), intent(in) :: this
		integer, intent(in) :: i
		real(8) :: output
		
		output = this.component(i).min
	end function minInCoord
	
	!>
	!! @brief
	!!
	pure function minVec( this ) result( output )
		class(Grid2D), intent(in) :: this
		real(8) :: output(2)
		
		output = [ this.component(1).min, this.component(2).min ]
	end function minVec
	
	!>
	!! @brief
	!!
	pure function maxInCoord( this, i ) result( output )
		class(Grid2D), intent(in) :: this
		integer, intent(in) :: i
		real(8) :: output
		
		output = this.component(i).max
	end function maxInCoord
	
	!>
	!! @brief
	!!
	pure function maxVec( this ) result( output )
		class(Grid2D), intent(in) :: this
		real(8) :: output(2)
		
		output = [ this.component(1).max, this.component(2).max ]
	end function maxVec
	
	!>
	!! @brief Returns the size of the component i-th
	!!
	pure function stepSizeInCoord( this, i ) result( output )
		class(Grid2D), intent(in) :: this
		integer, intent(in) :: i
		real(8) :: output
		
		output = this.component(i).stepSize
	end function stepSizeInCoord
	
	!>
	!! @brief
	!!
	pure function stepSizeVec( this ) result( output )
		class(Grid2D), intent(in) :: this
		real(8) :: output(2)
		
		output = [ this.component(1).stepSize, this.component(2).stepSize ]
	end function stepSizeVec
	
	!>
	!! @brief Resizes the function by making a larger grid with the same
	!!        step size by adding zeros at the end of each coordinate.
	!! @param nx Number of points to add in X coordinate
	!! @param ny Number of points to add in Y coordinate
	!! @param nz Number of points to add in Z coordinate
	!!
	subroutine resize1( this, dnx, dny, dirx, diry )
		class(Grid2D) :: this
		integer, intent(in) :: dnx, dny
		integer, optional, intent(in) :: dirx, diry
		
		if( dnx > 0 ) call this.component(1).resize( dnx, dirx )
		if( dny > 0 ) call this.component(2).resize( dny, diry )
	end subroutine resize1
	
	!>
	!! @brief Resizes the function by making a larger grid with the same
	!!        step size by adding zeros at the end of each coordinate.
	!! @param nx Number of points to add in X coordinate
	!! @param ny Number of points to add in Y coordinate
	!! @param nz Number of points to add in Z coordinate
	!!
	subroutine resize2( this, dn, dir )
		class(Grid2D) :: this
		integer, intent(in) :: dn(2)
		integer, optional, intent(in) :: dir(2)
		
		call this.resize1( dn(1), dn(2), dir(1), dir(2) )
	end subroutine resize2
	
	!>
	!! @brief
	!!
	pure function x( this, i ) result( output )
		class(Grid2D), intent(in) :: this
		integer, intent(in) :: i
		real(8) :: output
		
		output = this.component(1).data(i)
	end function x
	
	!>
	!! @brief
	!!
	pure function y( this, i ) result( output )
		class(Grid2D), intent(in) :: this
		integer, intent(in) :: i
		real(8) :: output
		
		output = this.component(2).data(i)
	end function y
	
	!>
	!! @brief
	!!
	function at( this, i, j ) result( output )
		class(Grid2D), intent(in) :: this 
		integer, intent(in) :: i
		integer, intent(in) :: j
		real(8) :: output(2)
		
		output = [ this.component(1).data(i), this.component(2).data(j) ]
	end function at
	
	!>
	!! @brief
	!!
	pure function dV( this ) result( output )
		class(Grid2D), intent(in) :: this
		real(8) :: output
		
		output = this.component(1).stepSize*this.component(2).stepSize
	end function dV
	
	!>
	!! @brief
	!!
	subroutine set( this, i, j, value )
		class(Grid2D) :: this 
		integer, intent(in) :: i, j
		real(8), intent(in) :: value(2)
		
		call this.component(1).set( i, value(1) )
		call this.component(2).set( j, value(2) )
	end subroutine set
	
	!>
	!! @brief Auxiliar function for test method
	!!
	subroutine showTest( xyzGrid )
		type(Grid2D), intent(in) :: xyzGrid
		
		integer :: i, j
		
		do i=1,2
			select case( i )
				case( 1 )
					write(*,*) ""
					write(*,*) "X = "
				case( 2 )
					write(*,*) ""
					write(*,*) "Y = "
			end select
			
			do j=1,xyzGrid.nPoints(i)
				write(*,"(f10.5)", advance="no") xyzGrid.component(i).data(j)
				
				if ( mod(j-1,10) == 9 ) then
					write(*,"(A)") ""
				end if
			end do
		end do
	end subroutine showTest
	
	!>
	!! @brief Test method
	!!
	subroutine Grid2D_test()
		type(Grid2D) :: xyzGrid, xyzGrid2
		
		integer :: i, j
		real(8) :: rMin(2), rMax(2)
		integer :: gridSize(2)
		
		rMin = [-5.0_8,-5.0_8]
		rMax = [ 5.0_8, 5.0_8]
		gridSize = [10, 10]
		
		call xyzGrid.init( rMin, rMax, gridSize )
		
		write(*,*) ""
		write(*,*) "----------------------------"
		write(*,*) "Testing constructor"
		write(*,*) "----------------------------"
		call xyzGrid.show()
		call showtest( xyzGrid )
		
		write(*,*) ""
		write(*,*) "----------------------------"
		write(*,*) "Testing copy constructor"
		write(*,*) "----------------------------"
		
		xyzGrid2 = xyzGrid
		call xyzGrid2.show()
		call showtest( xyzGrid2 )
		
		write(*,*) ""
		write(*,*) "----------------------------"
		write(*,*) "Testing save method"
		write(*,*) "----------------------------"
		call xyzGrid.save()
		
		write(*,*) ""
		write(*,*) "----------------------------"
		write(*,*) "Testing operators"
		write(*,*) "----------------------------"
		xyzGrid2 = xyzGrid*2.0_8
		call xyzGrid.save()
		call xyzGrid2.save()
		
		xyzGrid2 = xyzGrid*xyzGrid
		call xyzGrid.save()
		call xyzGrid2.save()
		
		write(*,*) ""
		write(*,*) "----------------------------"
		write(*,*) "Testing resize"
		write(*,*) "----------------------------"
		call xyzGrid.init( rMin, rMax, gridSize )
		call xyzGrid.show()
		call showtest( xyzGrid )
		
		write(*,*) "resize (+5,-5)"
		write(*,*) "-----------------"
		write(*,*) ""
		call xyzGrid.resize( 5, 5, +1, -1 )
		call xyzGrid.show()
		call showtest( xyzGrid )
		
		write(*,*) ""
	end subroutine Grid2D_test
	
end module Grid2D_

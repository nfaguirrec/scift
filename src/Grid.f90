!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!  This file is part of scift (Scientific Fortran Tools).
!!  Copyright (C) by authors (2011-2014)
!!  
!!  Authors (alphabetic order):
!!    * Aguirre N.F. (nfaguirrec@gmail.com)  (2011-2014)
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
!! @brief extract parameters from the commands line
!!
!! This class represents a one-dimensional grid 
!!
module Grid_
	use GOptions_
	use Math_
	use IOStream_
	use String_
	implicit none
	private
	
	public :: &
		Grid_test
	
	type, public :: Grid
		real(8) :: min
		real(8) :: max
		real(8) :: stepSize    !< It will be calculated as the difference between the first two elements in data, both in case of elements equally spaced as unequally spaced
		integer :: nPoints
		logical :: isEquallyspaced
		real(8), allocatable :: data(:)
		
		contains
			generic :: init => initDefault, fromArray, fromFile
			procedure :: initDefault
			procedure :: fromArray
			procedure :: fromFile
			generic :: assignment(=) => copy
			procedure :: copy
			final :: destroy
			
			procedure :: isEqualTo
			
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
			
			procedure :: resize
			procedure :: translate
			
			procedure :: str
			procedure :: show
			procedure :: save
			procedure :: toFStream
			procedure :: setUnits
			
			procedure :: at
			procedure :: pos
			procedure :: first
			procedure :: last
			procedure :: x => at
			procedure :: dV
			procedure :: lenght
			
			procedure :: set
	end type Grid
	
	contains
	
	!>
	!! @brief Constructor
	!!
	subroutine initDefault( this, min, max, nPoints, stepSize )
		class(Grid) :: this 
		real(8), intent(in) :: min
		real(8), intent(in) :: max
		integer, optional, intent(in) :: nPoints
		real(8), optional, intent(in) :: stepSize
		
		integer :: i
		integer :: allocStat
		
		this.min = min
		this.max = max
		
		if( present(stepSize) ) then
			this.stepSize = stepSize
			this.nPoints = nint( abs(max-min)/stepSize )+1
		else if( present(nPoints) ) then
			this.nPoints = nPoints
			this.stepSize = abs(max-min)/real(nPoints-1,8)
		else
			call GOptions_error( "nPoints or stepSize one of them are required parameters", "Grid.initDefault()" )
		end if
		
		if( allocated(this.data) ) deallocate(this.data)
		allocate( this.data(this.nPoints), stat=allocStat )
		if( allocStat /= 0 ) call GOptions_error( "Memory allocation error", "Grid.initDefault()" )
		
		do i=1,this.nPoints
			this.data( i ) = this.min+real(i-1,8)*this.stepSize
		end do
		
		if( present(nPoints) .and. abs( this.data(this.nPoints) - this.max ) > GOptions_ZERO ) then
			call GOptions_error( &
				"lastPoint /= max ( "//FString_fromReal(this.data(this.nPoints))//" /= "//FString_fromReal(this.max)//" )", &
				"Grid.initDefault(nPoints)" &
			)
		end if
		
		call this.checkEquallyspaced()
	end subroutine initDefault
	
	!>
	!! @brief Constructor
	!!
	subroutine fromArray( this, array, tol )
		class(Grid) :: this 
		real(8), intent(in) :: array(:)
		real(8), optional, intent(in) :: tol
		
		integer :: i
		real(8) :: stepSize
		integer :: allocStat
		
		this.min = minval(array)
		this.max = maxval(array)
		this.nPoints = size(array)
		
		if( allocated(this.data) ) deallocate(this.data)
		allocate( this.data(this.nPoints), stat=allocStat )
		if( allocStat /= 0 ) call GOptions_error( "Memory allocation error", "Grid.fromArray()" )
		
		do i=1,this.nPoints
			this.data( i ) = array( i )
		end do
		
		this.stepSize = this.data(2)-this.data(1)
		
		call this.checkEquallyspaced( tol )  !< Calcula el tamaño de paso y determina si es o no equiespaciada
	end subroutine fromArray
	
	!>
	!! @brief Constructor
	!!
	subroutine fromFile( this, iFileName, column, tol, cComments )
		class(Grid) :: this
		character(*), intent(in) :: iFileName
		integer, optional, intent(in) :: column
		real(8), optional, intent(in) :: tol
		character(*), optional, intent(in) :: cComments
		
		type(IFStream) :: ifile
		integer :: columnEff
		character(:), allocatable :: cCommentsEff
		type(String) :: buffer
		character(20), allocatable :: tokens(:)
		integer :: i
		real(8), allocatable :: data(:)
		integer :: nData
		real(8) :: stepSize
		integer :: allocStat
		
		columnEff = 1
		if( present(column) ) then
			columnEff = column
		end if
		
		cCommentsEff = "#"
		if( present(cComments) ) then
			cCommentsEff = cComments
		end if
		
		call ifile.init( trim(iFileName) )
		
		!! En el peor de los casos cada
		!! línea es un valor
		allocate( data(ifile.numberOfLines), stat=allocStat )
		if( allocStat /= 0 ) call GOptions_error( "Memory allocation error", "Grid.fromFile()" )
		
		nData = 1
		do while( .not. ifile.eof() )
			buffer = ifile.readLine( cCommentsEff )
			
			call buffer.split( tokens, " " )
			
			if( columnEff <= size(tokens) ) then
				if( len(trim(tokens(columnEff))) /= 0 ) then
					read( tokens(columnEff),* ) data(nData)
					nData = nData + 1
				end if
			end if
		end do
		
		if( allocated(this.data) ) deallocate(this.data)
		allocate( this.data(nData-1), stat=allocStat )
		if( allocStat /= 0 ) call GOptions_error( "Memory allocation error", "Grid.fromFile()" )
		
		this.data = data(1:nData-1)
		
		deallocate( data )
		call ifile.close()
		
		this.min = minval(this.data)
		this.max = maxval(this.data)
		this.nPoints = size(this.data)
		this.stepSize = this.data(2)-this.data(1)
		
		call this.checkEquallyspaced( tol )  !< Calcula el tamaño de paso y determina si es o no equiespaciada
		
	end subroutine fromFile
	
	!>
	!! @brief Copy constructor
	!!
	subroutine copy( this, other )
		class(Grid), intent(out) :: this
		class(Grid), intent(in) :: other
		
		integer :: allocStat
		
		this.min = other.min
		this.max = other.max
		this.stepSize = other.stepSize
		this.nPoints = other.nPoints
		this.isEquallyspaced = other.isEquallyspaced
		
		if( allocated(this.data) ) deallocate(this.data)
		allocate( this.data(this.nPoints), stat=allocStat )
		
		if( allocStat /= 0 ) call GOptions_error( "Memory allocation error", "Grid.copy()" )
		
		this.data = other.data
	end subroutine copy
	
	!>
	!! @brief Destructor
	!!
	subroutine destroy( this )
		type(Grid) :: this
		
		this.min = 0.0_8
		this.max = 0.0_8
		this.nPoints = 0
		this.stepSize = 0.0_8
		this.isEquallyspaced = .false.
		
		if( allocated( this.data ) ) deallocate( this.data )
	end subroutine destroy
	
	!>
	!! @brief
	!!
	function isEqualTo( this, other, tol ) result( output )
		class(Grid), intent(in) :: this
		class(Grid), intent(in) :: other
		real(8), optional :: tol
		logical :: output
		
		real(8) :: effTol
		
		effTol = 1d-6
		if( present(tol) ) effTol = tol
		
		output = .true.
		
		if( &
			abs( this.min - other.min ) > effTol .or. &
			abs( this.max - other.max ) > effTol .or. &
			abs( this.stepSize - other.stepSize ) > effTol .or. &
			this.nPoints /= other.nPoints .or. &
			this.isEquallyspaced /= other.isEquallyspaced &
		) then
			output = .false.
			return
		end if
		
		if( sum( abs(this.data - other.data)/this.nPoints ) > effTol )then
			output = .false.
			return
		end if
	end function isEqualTo
	
	!>
	!! @brief
	!!
	subroutine checkEquallyspaced( this, tol )
		class(Grid) :: this
		real(8), optional :: tol
		logical :: output
		
		real(8) :: effTol
		
		integer :: i
		
		effTol = 1d-6
		if( present(tol) ) effTol = tol
		
		this.stepSize = this.data(2)-this.data(1)
		this.isEquallyspaced = .true.
		
		if( this.nPoints <= 2 ) return
		
		do i=2,this.nPoints
			if( abs( abs(this.data(i)-this.data(i-1)) - this.stepSize) > effTol ) then
				this.isEquallyspaced = .false.
				exit
			end if
		end do
	end subroutine checkEquallyspaced
	
	!>
	!! @brief
	!!
	function addition( this, other ) result( output )
		class(Grid), intent(in) :: this
		class(Grid), intent(in) :: other
		type(Grid) :: output
		
		if( this.nPoints /= other.nPoints ) then
			write(*,*) "## ERROR ## the Grids have not the same size"
			stop
		end if
		
		call output.copy( this )
		output.data = this.data + other.data
	end function addition
	
	!>
	!! @brief
	!!
	function additionFC( this, constant ) result( output )
		class(Grid), intent(in) :: this
		real(8), intent(in) :: constant
		type(Grid) :: output
		
		call output.copy( this )
		output.data = this.data+constant
	end function additionFC
	
	!>
	!! @brief
	!!
	function subtraction( this, other ) result( output )
		class(Grid), intent(in) :: this
		type(Grid), intent(in) :: other
		type(Grid) :: output
		
		if( this.nPoints /= other.nPoints ) then
			write(*,*) "## ERROR ## the Grids have not the same size"
			stop
		end if
		
		call output.copy( this )
		output.data = this.data - other.data
	end function subtraction
	
	!>
	!! @brief
	!!
	function subtractionFC( this, constant ) result( output )
		class(Grid), intent(in) :: this
		real(8), intent(in) :: constant
		type(Grid) :: output
		
		call output.copy( this )
		output.data = this.data-constant
	end function subtractionFC
	
	!>
	!! @brief
	!!
	function multiplication( this, other ) result( output )
		class(Grid), intent(in) :: this
		type(Grid), intent(in) :: other
		type(Grid) :: output
		
		if( this.nPoints /= other.nPoints ) then
			write(*,*) "## ERROR ## the Grids have not the same size"
			stop
		end if
		
		call output.copy( this )
		output.data = this.data*other.data
		
		! @todo Hay que hacer algo con el stepSize
	end function multiplication
	
	!>
	!! @brief
	!!
	function multiplicationFC( this, constant ) result( output )
		class(Grid), intent(in) :: this
		real(8), intent(in) :: constant
		type(Grid) :: output
		
		call output.copy( this )
		output.data = this.data*constant
		output.stepSize = this.stepSize*constant
	end function multiplicationFC
	
	!>
	!! @brief
	!!
	function division( this, other ) result( output )
		class(Grid), intent(in) :: this
		type(Grid), intent(in) :: other
		type(Grid) :: output
		
		if( this.nPoints /= other.nPoints ) then
			write(*,*) "## ERROR ## the Grids have not the same size"
			stop
		end if
		
		call output.copy( this )
		output.data = this.data/other.data
	end function division
	
	!>
	!! @brief
	!!
	function divisionFC( this, constant ) result( output )
		class(Grid), intent(in) :: this
		real(8), intent(in) :: constant
		type(Grid) :: output
		
		call output.copy( this )
		output.data = this.data/constant
	end function divisionFC
	
	!>
	!! @brief
	!!
	function exponentiation( this, other ) result( output )
		class(Grid), intent(in) :: this
		type(Grid), intent(in) :: other
		type(Grid) :: output
		
		if( this.nPoints /= other.nPoints ) then
			write(*,*) "## ERROR ## the Grids have not the same size"
			stop
		end if
		
		call output.copy( this )
		output.data = this.data**other.data
	end function exponentiation
	
	!>
	!! @brief
	!!
	function exponentiationFC( this, constant ) result( output )
		class(Grid), intent(in) :: this
		real(8), intent(in) :: constant
		type(Grid) :: output
		
		call output.copy( this )
		output.data = this.data**constant
	end function exponentiationFC
	
	!>
	!! @brief Size
	!!
	pure function ssize( this ) result( output )
		class(Grid), intent(in) :: this 
		integer :: output
		
		output = this.nPoints
	end function ssize
	
	!>
	!! @brief
	!!
	!! Original distribution
	!! ---------------------
	!! x1 = x1
	!! x2 = x1+dx
	!! x3 = x1+2*dx
	!! ...
	!! xn = x1+(n-1)*dx
	!!
	!!      ( xn - x1 )/dx + 1 = ( x1+(n-1)*dx - x1 )/dx + 1
	!!                         = n
	!!
	!! Right resize by m
	!! -----------------
	!! x1   = x1
	!! x2   = x1+dx
	!! x3   = x1+2*dx
	!! ...
	!! xn   = x1+(n-1)*dx
	!! xn+1 = x1+n*dx
	!! xn+2 = x1+(n+1)*dx
	!! ...
	!! xn+m = x1+(n+m-1)*dx
	!!
	!!      ( xn+m - x1 )/dx + 1 = ( x1+(n+m-1)*dx - x1 )/dx + 1
	!!                           = n+m
	!!
	!! Left resize by m
	!! -----------------
	!! x1   =  x-(m-1) = x1-m*dx
	!! x2   =  x-(m-2) = x1-(m-1)*dx
	!! ...
	!! xm-2 =  x-2     = x1-3*dx
	!! xm-1 =  x-1     = x1-2*dx
	!! xm   =  x0      = x1-dx
	!! xm+1 =  x1      = x1
	!! xm+2 =  x2      = x1+dx
	!! xm+3 =  x3      = x1+2*dx
	!! ...
	!! xm+n =  xn      = x1+(n-1)*dx
	!!
	!!      ( xm+n - x1 )/dx + 1 = ( x1+(n-1)*dx - x1+m*dx )/dx + 1
	!!                           = ( (n-1)*dx + m*dx )/dx + 1
	!!                           = ( n-1 + m ) + 1
	!!                           = n+m
	!!
	!! Both resize by m for each side
	!! ------------------------------
	!! x1       =  xn-m-1  = x1-m*dx
	!! x2       =  xn-m    = x1-(m-1)*dx
	!! x3       =  xn-m+1  = x1-(m-2)*dx
	!! ...
	!! xm       =  x0      = x1-dx
	!! xm+1     =  x1      = x1
	!! xm+2     =  x2      = x1+dx
	!! ...
	!! xm+n     =  xn      = x1+(n-1)*dx
	!! xm+n+1   =  xn+1    = x1+n*dx
	!! xm+n+2   =  xn+2    = x1+(n+1)*dx
	!! ...
	!! xm+n+m   =  xn+m    = x1+(m+n-1)*dx
	!!
	!!      ( xm+n+m - x1 )/dx + 1 = ( x1+(m+n-1)*dx - ( x1-m*dx ) )/dx + 1
	!!                             = ( x1+m*dx+n*dx-dx - x1+m*dx )/dx + 1
	!!                             = ( n*dx-dx +2m*dx )/dx + 1
	!!                             = n-1 +2m + 1
	!!                             = n+2m
	!!
	subroutine resize( this, m, dir )
		class(Grid) :: this
		integer, intent(in) :: m
		integer, optional, intent(in) :: dir
		
		integer :: effDir
		
		integer :: n
		integer :: newNPoints
		real(8) :: newMin, newMax
		
		if( .not. this.isEquallyspaced ) then
			call this.show()
			call GOptions_error( &
				"This function is not available for grids wich are not equally spaced", &
				"Grid.resize()" &
			)
		end if
		
		effDir = 1
		if( present(dir) ) effDir = dir
		
		n = this.nPoints
		
		if( effDir == 1 ) then
			
			newNPoints = n+m
			
			newMin = this.min
			newMax = this.min + real(n+m-1,8)*this.stepSize

		else if( effDir == -1 ) then
			
			newNPoints = n+m
			
			newMin = this.min - real(m,8)*this.stepSize
			newMax = this.max
			
		else if( effDir == 0 ) then
			
			newNPoints = n + 2*m
			
			newMin = this.min - real(m,8)*this.stepSize
			newMax = this.min + real(n+m-1,8)*this.stepSize
			
		else
			call GOptions_error( &
				"Bad value for dir. (+1|0|-1)", &
				"Grid.resize()" &
			)
		end if
		
		call this.init( newMin, newMax, nPoints=newNPoints )
	end subroutine resize
	
	!>
	!! Moves the values delta along the axis relative to the current position.
	!! Positive values move the function to the right.
	!!
	subroutine translate( this, delta )
		class(Grid) :: this
		real(8), optional, intent(in) :: delta
		
		if( present(delta) ) then
			this.data = this.data + delta
		end if
		
		this.min = minval( this.data )
		this.max = maxval( this.data )
	end subroutine translate
	
	!>
	!! @brief String representation
	!!
	function str( this ) result( output )
		class(Grid), intent(in) :: this 
		character(len=200) :: output
		
		integer :: fmt
		character(len=200) :: strBuffer
		
		output = ""
		
		output = trim(output)//"<Grid:"
		
		output = trim(output)//"min="
		output = trim(output)//trim(adjustl(FString_fromReal(this.min,"(F20.6)")))
		
		output = trim(output)//",max="
		output = trim(output)//trim(adjustl(FString_fromReal(this.max,"(F20.6)")))
		
		output = trim(output)//",stepSize="
		output = trim(output)//trim(adjustl(FString_fromReal(this.stepSize,"(F20.6)")))
		
		output = trim(output)//",isEquallyspaced="
		output = trim(output)//trim(adjustl(FString_fromLogical(this.isEquallyspaced)))
		
		output = trim(output)//",nPoints="
		output = trim(output)//trim(adjustl(FString_fromInteger(this.nPoints,"(I20)")))
		
		output = trim(output)//">"
	end function str
	
	!>
	!! @brief
	!!
	subroutine show( this, unit )
		class(Grid), intent(in) :: this
		integer, optional, intent(in) :: unit
		
		integer :: effunit
		
		if( present(unit) ) then
			effunit = unit
		else
			effunit = 6
		end if
		
		write(effunit,"(a)") trim(this.str())
	end subroutine show
	
	!>
	!! @brief
	!!
	subroutine setUnits( this, unit )
		class(Grid) :: this
		real(8), intent(in) :: unit
		
		this.min = this.min*unit
		this.max = this.max*unit
		this.stepSize = this.stepSize*unit
		this.data = this.data*unit
	end subroutine setUnits
	
	!>
	!! Save the data in one column format in a
	!! selected unit
	!!
	subroutine save( this, ofileName, units )
		class(Grid), intent(in) :: this
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
		class(Grid), intent(in) :: this
		type(OFStream), optional, intent(in) :: ofile
		real(8), optional, intent(in) :: units
		
		integer :: unitEff
		real(8) :: unitsEff
		integer :: i
		
		if( present(ofile) ) then
			unitEff = ofile.unit
		else
			unitEff = IO_STDOUT
		end if
		
		if( present(units) ) then
			unitsEff = units
		else
			unitsEff = 1.0_8
		end if
		
		write(unitEff,"(a)") "#"//trim(str(this))
		
		do i=1,this.nPoints
			write(unitEff,"(e15.7,e15.7)") this.data(i)/unitsEff
		end do
	end subroutine toFStream
	
	!>
	!! @brief
	!!
	pure function at( this, i ) result( output )
		class(Grid), intent(in) :: this 
		integer, intent(in) :: i
		real(8) :: output
		
		output = this.data(i)
	end function at
	
	!>
	!! @brief
	!!
	pure function pos( this, x ) result( output )
		class(Grid), intent(in) :: this
		real(8), intent(in) :: x
		integer :: output
		
		output = nint((x-this.min)/this.stepSize)+1
	end function pos
	
	!>
	!! @brief
	!!
	pure function first( this ) result( output )
		class(Grid), intent(in) :: this 
		real(8) :: output
		
		output = this.data(1)
	end function first
	
	!>
	!! @brief
	!!
	pure function last( this ) result( output )
		class(Grid), intent(in) :: this 
		real(8) :: output
		
		output = this.data( size(this.data) )
	end function last
	
	!>
	!! @brief
	!!
	pure function dV( this ) result( output )
		class(Grid), intent(in) :: this
		real(8) :: output
		
		output = this.stepSize
	end function dV
	
	!>
	!! @brief
	!!
	pure function lenght( this ) result( output )
		class(Grid), intent(in) :: this
		real(8) :: output
		
		output = real(this.nPoints-1,8)*this.stepSize
	end function lenght
	
	!>
	!! @brief Sets the i-th element to the value "value"
	!!
	subroutine set( this, i, value )
		class(Grid) :: this 
		integer, intent(in) :: i
		real(8), intent(in) :: value
		
		this.data(i) = value
	end subroutine set
	
	!>
	!! @brief
	!!
	subroutine Grid_test()
		real(8) :: rMin, rMax
		integer :: gridSize
		type(Grid) :: rGrid
		type(Grid) :: rGrid2
		
		integer :: i
			
		rMin=0.0_8
		rMax=5.0_8
		gridSize=10
		
		call rGrid.init( rMin, rMax, gridSize )
		call rGrid.show()
		
		do i=1,rGrid.nPoints
			write(*,"(i5,f10.5)") i, rGrid.data(i)
		end do
		
		write(*,*) "---"
		write(*,*) "Testing copy constructor"
		write(*,*) "---"
		call rGrid2.copy( rGrid )
		call rGrid2.show()
		do i=1,rGrid2.nPoints
			write(*,"(i5,f10.5)") i, rGrid2.data(i)
		end do
		
		write(*,*) "===================================================================="
		
		call rGrid2.copy( rGrid )
		
		write(*,*) ""
		write(*,*) " Testing resize grid 5, dir = +1 "
		write(*,*) "---------------------------------"
		call rGrid2.resize( 5, +1 )
		call rGrid2.show()
		do i=1,rGrid2.nPoints
			write(*,"(i5,f10.5)") i, rGrid2.data(i)
		end do
		
		write(*,*) ""
		write(*,*) " Testing resize grid -5, dir = +1 "
		write(*,*) "----------------------------------"
		call rGrid2.resize( -5, +1 )
		call rGrid2.show()
		do i=1,rGrid2.nPoints
			write(*,"(i5,f10.5)") i, rGrid2.data(i)
		end do
		
		write(*,*) "===================================================================="
		write(*,*) " TESTING RESIZE"
		write(*,*) "===================================================================="
		
		call rGrid2.copy( rGrid )
		
		write(*,*) ""
		write(*,*) " Testing resize grid 5, dir = -1"
		write(*,*) "---------------------------------"
		call rGrid2.resize( 5, -1 )
		call rGrid2.show()
		do i=1,rGrid2.nPoints
			write(*,"(i5,f10.5)") i, rGrid2.data(i)
		end do
		
		write(*,*) ""
		write(*,*) " Testing resize grid -5, dir = -1"
		write(*,*) "----------------------------------"
		call rGrid2.resize( -5, -1 )
		call rGrid2.show()
		do i=1,rGrid2.nPoints
			write(*,"(i5,f10.5)") i, rGrid2.data(i)
		end do
		
		write(*,*) "===================================================================="
		
		call rGrid2.copy( rGrid )
		
		write(*,*) ""
		write(*,*) " Testing resize grid 5, dir = 0"
		write(*,*) "--------------------------------"
		call rGrid2.resize( 5, 0 )
		call rGrid2.show()
		do i=1,rGrid2.nPoints
			write(*,"(i5,f10.5)") i, rGrid2.data(i)
		end do
		
		write(*,*) ""
		write(*,*) " Testing resize grid -5, dir = 0"
		write(*,*) "---------------------------------"
		call rGrid2.resize( -5, 0 )
		call rGrid2.show()
		do i=1,rGrid2.nPoints
			write(*,"(i5,f10.5)") i, rGrid2.data(i)
		end do
		
		write(*,*) "===================================================================="
		write(*,*) " Even number of points"
		write(*,*) "===================================================================="		
		
		call rGrid2.init( rMin, rMax, nPoints=10 )
		call rGrid2.show()
		do i=1,rGrid2.nPoints
			write(*,"(i5,f10.5)") i, rGrid2.data(i)
		end do
		
		write(*,*) ""
		write(*,*) " Resized grid "
		write(*,*) "--------------"
		call rGrid2.resize( 5, 0 )
		call rGrid2.show()
		do i=1,rGrid2.nPoints
			write(*,"(i5,f10.5)") i, rGrid2.data(i)
		end do
		
		write(*,*) ""
		write(*,*) " Generated grid from nPoints"
		write(*,*) "-----------------------------"
		call rGrid2.init( rGrid2.min, rGrid2.max, rGrid2.nPoints )
		call rGrid2.show()
		do i=1,rGrid2.nPoints
			write(*,"(i5,f10.5)") i, rGrid2.data(i)
		end do
		
		write(*,*) ""
		write(*,*) " Generated grid from stepSize"
		write(*,*) "------------------------------"
		call rGrid2.init( rGrid2.min, rGrid2.max, stepSize=rGrid2.stepSize )
		call rGrid2.show()
		do i=1,rGrid2.nPoints
			write(*,"(i5,f10.5)") i, rGrid2.data(i)
		end do
		
		write(*,*) "===================================================================="
		write(*,*) " Odd number of points"
		write(*,*) "===================================================================="		
		
		call rGrid2.init( rMin, rMax, nPoints=11 )
		call rGrid2.show()
		do i=1,rGrid2.nPoints
			write(*,"(i5,f10.5)") i, rGrid2.data(i)
		end do
		
		write(*,*) ""
		write(*,*) " Resized grid "
		write(*,*) "--------------"
		call rGrid2.resize( 5, 0 )
		call rGrid2.show()
		do i=1,rGrid2.nPoints
			write(*,"(i5,f10.5)") i, rGrid2.data(i)
		end do
		
		write(*,*) ""
		write(*,*) " Generated grid from nPoints"
		write(*,*) "-----------------------------"
		call rGrid2.init( rGrid2.min, rGrid2.max, rGrid2.nPoints )
		call rGrid2.show()
		do i=1,rGrid2.nPoints
			write(*,"(i5,f10.5)") i, rGrid2.data(i)
		end do
		
		write(*,*) ""
		write(*,*) " Generated grid from stepSize"
		write(*,*) "------------------------------"
		call rGrid2.init( rGrid2.min, rGrid2.max, stepSize=rGrid2.stepSize )
		call rGrid2.show()
		do i=1,rGrid2.nPoints
			write(*,"(i5,f10.5)") i, rGrid2.data(i)
		end do

	end subroutine Grid_test
	
end module Grid_

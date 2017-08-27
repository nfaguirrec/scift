!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                                                                                   !!
!!  This file is part of SciFT project                                               !!
!!  Copyright (c) 2011-2014 Nestor F. Aguirre (nfaguirrec@gmail.com)                 !!
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
!! @brief extract parameters from the commands line
!!
!! This class represents a one-dimensional grid 
!!
module GridBase_
	use IOStream_
	use String_
	implicit none
	private
	
	public :: &
		GridBase_test
	
	type, public :: GridBase
		real(8) :: min
		real(8) :: max
		real(8) :: stepSize
		integer :: size
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
			
! 			generic :: operator(==) => isEqualTo
			generic :: operator(+) => addition, additionFC
			generic :: operator(-) => subtraction, subtractionFC
			generic :: operator(*) => multiplication, multiplicationFC
			generic :: operator(/) => division, divisionFC
			generic :: operator(**) => exponentiation, exponentiationFC
			
			procedure :: isEqualTo
			
			procedure, private :: addition
			procedure, private :: subtraction
			procedure, private :: multiplication
			procedure, private :: division
			procedure, private :: exponentiation
			
			procedure, private :: additionFC
			procedure, private :: subtractionFC
			procedure, private :: multiplicationFC
			procedure, private :: divisionFC
			procedure, private :: exponentiationFC
			
			procedure :: str
			procedure :: show
			procedure :: save
			procedure :: toFStream
			procedure :: setUnits
			
			procedure :: at
	end type GridBase
	
	contains
	
	!>
	!! @brief Constructor
	!!
	subroutine initDefault( this, min, max, size, stepSize )
		class(GridBase) :: this 
		real(8) :: min
		real(8) :: max
		integer, optional :: size
		real(8), optional :: stepSize
		
		integer :: i
		
		this.min = min
		this.max = max
		this.isEquallyspaced = .true.
		
		if( present(stepSize) ) then
			this.stepSize = stepSize
			this.size = int( abs(max-min)/stepSize )+1
		else if( present(size) ) then
			this.size = size
			this.stepSize = abs(max-min)/float(size-1)
		end if
		
		if( allocated(this.data) ) deallocate(this.data)
		allocate( this.data(this.size) )
		
		do i=1,this.size
			this.data( i ) = this.min+float(i-1)*this.stepSize
		end do
	end subroutine initDefault
	
	!>
	!! @brief Constructor
	!!
	subroutine fromArray( this, array, tol )
		class(GridBase) :: this 
		real(8), intent(in) :: array(:)
		real(8), optional, intent(in) :: tol
		
		real(8) :: effTol
		
		integer :: i
		real(8) :: stepSize
		
		effTol = 1d-3
		if( present(tol) ) effTol = tol
		
		this.min = minval(array)
		this.max = maxval(array)
		this.size = size(array)
		
		stepSize = abs(array(2)-array(1))
		
		this.stepSize = stepSize
		this.isEquallyspaced = .true.
		
		do i=2,this.size
			if( abs( abs(array(i)-array(i-1)) -stepSize) > effTol ) then
				this.isEquallyspaced = .false.
				exit
			end if
		end do
		
		if( allocated(this.data) ) deallocate(this.data)
		allocate( this.data(this.size) )
		
		do i=1,this.size
			this.data( i ) = array( i )
		end do
	end subroutine fromArray
	
	!>
	!! @brief Constructor
	!!
	subroutine fromFile( this, iFileName, column, cComments )
		class(GridBase) :: this
		character(*), intent(in) :: iFileName
		integer, optional, intent(in) :: column
		character(*), optional, intent(in) :: cComments
		
		type(IFStream) :: ifile
		integer :: columnEff
		character(:), allocatable :: cCommentsEff
		type(String) :: buffer
		character(20), allocatable :: tokens(:)
		integer :: i
		real(8), allocatable :: data(:)
		integer :: nData
		real(8) :: effStepSize
		
		cCommentsEff = "#"
		if( present(cComments) ) then
			cCommentsEff = cComments
		end if
		
		columnEff = 1
		if( present(column) ) then
			columnEff = column
		end if
		
		call ifile.init( trim(iFileName) )
		
		!! En el peor de los casos cada
		!! línea es un valor
		allocate( data(ifile.numberOfLines) )
		
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
		allocate( this.data(nData-1) )
		this.data = data(1:nData-1)
		
		deallocate( data )
		call ifile.close()
		
		this.min = minval(this.data)
		this.max = maxval(this.data)
		this.size = size(this.data)
		
		effStepSize = this.data(2)-this.data(1)
		do i=3,size(this.data)
			if( abs(this.data(i)-this.data(i-1)-effStepSize) > 1e-10 ) then
				this.stepSize = -1.0_8
				this.isEquallyspaced = .false.
				return
			end if
		end do
		
		this.stepSize = effStepSize
		this.isEquallyspaced = .true.
	end subroutine fromFile
	
	!>
	!! @brief Copy constructor
	!!
	subroutine copy( this, other )
		class(GridBase), intent(out) :: this
		class(GridBase), intent(in) :: other
		
		this.min = other.min
		this.max = other.max
		this.stepSize = other.stepSize
		this.size = other.size
		this.isEquallyspaced = other.isEquallyspaced
		
		if( allocated(this.data) ) deallocate(this.data)
		allocate( this.data(this.size) )
		
		this.data = other.data
	end subroutine copy
	
	!>
	!! @brief Destructor
	!!
	subroutine destroy( this )
		type(GridBase) :: this
		
		this.min = 0.0_8
		this.max = 0.0_8
		this.size = 0
		this.stepSize = 0.0_8
		this.isEquallyspaced = .false.
		
		if( allocated( this.data ) ) deallocate( this.data )
	end subroutine destroy
	
	!>
	!! @brief
	!!
	function isEqualTo( this, other, tol ) result( output )
		class(GridBase), intent(in) :: this
		class(GridBase), intent(in) :: other
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
			this.size /= other.size .or. &
			this.isEquallyspaced /= other.isEquallyspaced &
		) then
			output = .false.
			return
		end if
		
		if( sum( abs(this.data - other.data)/this.size ) > effTol )then
			output = .false.
			return
		end if
	end function isEqualTo
	
	!>
	!! @brief
	!!
	function addition( this, other ) result( output )
		class(GridBase), intent(in) :: this
		class(GridBase), intent(in) :: other
		type(GridBase) :: output
		
		if( this.size /= other.size ) then
			write(*,*) "## ERROR ## the Grid have not the same size"
			stop
		end if
		
		call output.copy( this )
		output.data = this.data + other.data
	end function addition
	
	!>
	!! @brief
	!!
	function additionFC( this, constant ) result( output )
		class(GridBase), intent(in) :: this
		real(8), intent(in) :: constant
		type(GridBase) :: output
		
		call output.copy( this )
		output.data = this.data+constant
	end function additionFC
	
	!>
	!! @brief
	!!
	function subtraction( this, other ) result( output )
		class(GridBase), intent(in) :: this
		type(GridBase), intent(in) :: other
		type(GridBase) :: output
		
		if( this.size /= other.size ) then
			write(*,*) "## ERROR ## the Grid have not the same size"
			stop
		end if
		
		call output.copy( this )
		output.data = this.data - other.data
	end function subtraction
	
	!>
	!! @brief
	!!
	function subtractionFC( this, constant ) result( output )
		class(GridBase), intent(in) :: this
		real(8), intent(in) :: constant
		type(GridBase) :: output
		
		call output.copy( this )
		output.data = this.data-constant
	end function subtractionFC
	
	!>
	!! @brief
	!!
	function multiplication( this, other ) result( output )
		class(GridBase), intent(in) :: this
		type(GridBase), intent(in) :: other
		type(GridBase) :: output
		
		if( this.size /= other.size ) then
			write(*,*) "## ERROR ## the Grid have not the same size"
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
		class(GridBase), intent(in) :: this
		real(8), intent(in) :: constant
		type(GridBase) :: output
		
		call output.copy( this )
		output.data = this.data*constant
		output.stepSize = this.stepSize*constant
	end function multiplicationFC
	
	!>
	!! @brief
	!!
	function division( this, other ) result( output )
		class(GridBase), intent(in) :: this
		type(GridBase), intent(in) :: other
		type(GridBase) :: output
		
		if( this.size /= other.size ) then
			write(*,*) "## ERROR ## the Grid have not the same size"
			stop
		end if
		
		call output.copy( this )
		output.data = this.data/other.data
	end function division
	
	!>
	!! @brief
	!!
	function divisionFC( this, constant ) result( output )
		class(GridBase), intent(in) :: this
		real(8), intent(in) :: constant
		type(GridBase) :: output
		
		call output.copy( this )
		output.data = this.data/constant
	end function divisionFC
	
	!>
	!! @brief
	!!
	function exponentiation( this, other ) result( output )
		class(GridBase), intent(in) :: this
		type(GridBase), intent(in) :: other
		type(GridBase) :: output
		
		if( this.size /= other.size ) then
			write(*,*) "## ERROR ## the Grid have not the same size"
			stop
		end if
		
		call output.copy( this )
		output.data = this.data**other.data
	end function exponentiation
	
	!>
	!! @brief
	!!
	function exponentiationFC( this, constant ) result( output )
		class(GridBase), intent(in) :: this
		real(8), intent(in) :: constant
		type(GridBase) :: output
		
		call output.copy( this )
		output.data = this.data**constant
	end function exponentiationFC
	
	!>
	!! @brief String representation
	!!
	function str( this ) result( output )
		class(GridBase), intent(in) :: this 
		character(len=200) :: output
		
		integer :: fmt
		character(len=200) :: strBuffer
		
		output = ""
		
		output = trim(output)//"<GridBase:"
		
		output = trim(output)//"min="
		output = trim(output)//trim(adjustl(FString_fromReal(this.min,"(F20.6)")))
		
		output = trim(output)//",max="
		output = trim(output)//trim(adjustl(FString_fromReal(this.max,"(F20.6)")))
		
		if ( this.isEquallyspaced ) then
			output = trim(output)//",isEquallyspaced=T"
			output = trim(output)//",stepSize="
			output = trim(output)//trim(adjustl(FString_fromReal(this.stepSize,"(F20.6)")))
		else
			output = trim(output)//",isEquallyspaced=F"
		end if
		
		output = trim(output)//",size="
		output = trim(output)//trim(adjustl(FString_fromInteger(this.size,"(I20)")))
		
		output = trim(output)//">"
	end function str
	
	!>
	!! @brief
	!!
	subroutine show( this, unit )
		class(GridBase), intent(in) :: this
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
		class(GridBase) :: this
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
		class(GridBase), intent(in) :: this
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
		class(GridBase), intent(in) :: this
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
		
		do i=1,this.size
			write(unitEff,"(e15.7,e15.7)") this.data(i)/unitsEff
		end do
	end subroutine toFStream
	
	!>
	!! @brief
	!!
	function at( this, i ) result( output )
		class(GridBase), intent(in) :: this 
		integer, intent(in) :: i
		real(8) :: output
		
		output = this.data(i)
	end function at
	
	!>
	!! @brief
	!!
	subroutine GridBase_test()
		real(8) :: rMin, rMax
		integer :: gridSize
		type(GridBase) :: rGridBase
		type(GridBase) :: rGridBase2
		
		integer :: i
			
		rMin=0.0_8
		rMax=5.0_8
		gridSize=10
		
		call rGridBase.init( rMin, rMax, gridSize )
		call rGridBase.show()
		
		do i=1,rGridBase.size
			write(*,"(i5,f10.5)") i, rGridBase.data(i)
		end do
		
		write(*,*) "---"
		write(*,*) "Testing copy constructor"
		write(*,*) "---"
		call rGridBase2.copy( rGridBase )
		call rGridBase2.show()
		do i=1,rGridBase2.size
			write(*,"(i5,f10.5)") i, rGridBase2.data(i)
		end do
	end subroutine GridBase_test
	
end module GridBase_

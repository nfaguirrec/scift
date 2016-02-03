!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!  This file is part of scift (Scientific Fortran Tools).
!!  Copyright (C) by authors (2013-2013)
!!  
!!  Authors (alphabetic order):
!!    * Aguirre N.F. (nfaguirrec@gmail.com)  (2013-2013)
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

!*
! @brief
!*
module Table_
	use String_
	use IOStream_
	implicit none
	private
	
	public :: &
		Table_test
	
	type, public :: Table
		integer :: nRows
		integer :: nCols
		type(String), allocatable :: data(:,:)
		
		contains
			generic :: init => initDefault, fromFile
			generic :: assignment(=) => copy
			
			procedure :: initDefault
			procedure :: fromFile
			procedure :: copy
			final :: destroy
			procedure :: str
			procedure :: show
			procedure :: showContent
	end type Table
	
	contains
	
	!*
	! @brief Constructor
	!*
	subroutine initDefault( this )
		class(Table) :: this 
		
	end subroutine initDefault
	
	!*
	! @brief Constructor
	!*
	subroutine fromFile( this, fileName, cComments )
		class(Table) :: this 
		character(*), intent(in) :: fileName
		character(*), optional, intent(in) :: cComments
		
		type(IFStream) :: ifile
		character(:), allocatable :: cCommentsEff
		type(String) :: buffer
		character(100) :: fStrBuffer
		character(20), allocatable :: tokens(:)
		type(String), allocatable :: rawData(:,:)
		integer :: i,j
		type(String), allocatable :: currentRow(:)
		integer :: nRow, nCol
		
		if( present(cComments) ) then
			cCommentsEff = cComments
		else
			cCommentsEff = "#"
		end if
		
		call ifile.init( fileName )
		
		allocate( rawData( ifile.numberOfLines, ifile.minNColumns ) )
		allocate( currentRow(ifile.minNColumns) )
		
		nCol = 1
		do while( .not. ifile.eof() )
			buffer = ifile.readLine( cCommentsEff )
			call buffer.split( tokens, " " )
			
			if( len(trim(buffer.fstr)) > 0 ) then
				nRow = 1
				do i=1,size(tokens)
					if( len(trim(tokens(i))) /= 0 ) then
						read( tokens(i),* ) fStrBuffer
						currentRow(nRow) = trim(fStrBuffer)
						nRow = nRow + 1
					end if
				end do
				
				rawData( nCol, : ) = currentRow
				nCol = nCol + 1
			end if
		end do
		
		allocate( this.data( nCol-1, nRow-1 ) )
		this.data = rawData
		
		this.nRows = nRow-1
		this.nCols = nCol-1
		
		deallocate( currentRow )
		
		call ifile.close()
	end subroutine fromFile
	
	!*
	! @brief Copy constructor
	!*
	subroutine copy( this, other )
		class(Table), intent(out) :: this
		class(Table), intent(in) :: other
		
	end subroutine copy
	
	!*
	! @brief Destructor
	!*
	subroutine destroy( this )
		type(Table) :: this
		
	end subroutine destroy
	
	!*
	! @brief Convert to string
	!*
	function str( this ) result( output )
		class(Table) :: this 
		character(len=200) :: output
		
		integer :: fmt
		character(len=200) :: strBuffer
		
		output = ""
		
		output = trim(output)//"<Table:"
		
		output = trim(output)//"nRows="
		fmt = int(log10(float(this.nRows+1)))+1
		write(strBuffer, "(i<fmt>)") this.nRows
		output = trim(output)//trim(strBuffer)
		
		output = trim(output)//",nCols="
		fmt = int(log10(float(this.nCols+1)))+1
		write(strBuffer, "(i<fmt>)") this.nCols
		output = trim(output)//trim(strBuffer)
		
		output = trim(output)//">"
	end function str
	
	!*
	! @brief Show 
	!*
	subroutine show( this, unit )
		class(Table) :: this
		integer, optional, intent(in) :: unit
		
		integer :: effunit
		
		if( present(unit) ) then
			effunit = unit
		else
			effunit = 6
		end if
		
		write(effunit,"(a)") trim(str(this))
	end subroutine show
	
	!*
	! @brief Show 
	!*
	subroutine showContent( this, unit )
		class(Table) :: this
		integer, optional, intent(in) :: unit
		
		integer :: i, j
		integer :: effunit
		
		if( present(unit) ) then
			effunit = unit
		else
			effunit = 6
		end if
		
		do i=1,size(this.data,dim=1)
			do j=1,size(this.data,dim=2)
				write(effunit,"(A20)",advance='no') this.data(i,j).fstr
			end do
			write(*,*)
		end do
		
	end subroutine showContent
	
	!*
	! @brief Test method
	!*
	subroutine Table_test()
		type(Table) :: table
		
		call table.init( "data/formats/TABLE" )
		call table.show()
		call table.showContent()
	end subroutine Table_test
	
end module Table_

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
			generic :: assignment(=) => copyTable
			
			procedure :: initDefault
			procedure :: fromFile
			procedure :: copyTable
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
	subroutine copyTable( this, other )
		class(Table), intent(out) :: this
		type(Table), intent(in) :: other
		
	end subroutine copyTable
	
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

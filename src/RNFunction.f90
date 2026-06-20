!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                                                                                   !!
!!  This file is part of SciFT project                                               !!
!!  Copyright (c) 2010-2014 Nestor F. Aguirre (nfaguirrec@gmail.com)                 !!
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

module RNFunction_
	use GOptions_
	use Grid_
	use String_
	use IOStream_
	implicit none
	private
	
	public :: &
		RNFunction_checkTypeN1DF, &
		RNFunction_getFormatIdFromFileExtension
		
!>
!! This class use the List template declared into List.h90 file,
!! please take a look to this file for details
!!
#define NFunction_checkTypeN1DF RNFunction_checkTypeN1DF
#define NFunction_getFormatIdFromFileExtension RNFunction_getFormatIdFromFileExtension
#define NFunction RNFunction
#define __TYPE_VALUE__ real(8)
#define __ADD_METHODS__
#define __ID_TYPE__ 0
#include "NFunction.h90"
#undef NFunction
#undef __TYPE_VALUE__
#undef __ADD_METHODS__
#undef __ID_TYPE__
	
	!>
	!! @brief Constructor
	!!
	function fromFStream( stream, columns, cComments, units ) result( this )
		type(RNFunction) :: this
		type(IFStream), intent(in) :: stream
		integer, optional, intent(in) :: columns(:)
		character(*), optional, intent(in) :: cComments
		real(8), optional, intent(in) :: units(2)
		
		integer :: nData, nPoints
		integer, allocatable :: columnsEff(:)
		character(:), allocatable :: cCommentsEff
		character(:), allocatable :: line
		type(String) :: buffer
		character(20), allocatable :: tokens(:)
		real(8), allocatable :: x(:)
		real(8), allocatable :: y(:)
		real(8) :: effUnits(2)
		
		if( present(cComments) ) then
			cCommentsEff = cComments
		else
			cCommentsEff = "#"
		end if
		
		if( present(columns) ) then
			allocate( columnsEff(size(columns)) )
			columnsEff = columns
		else
			allocate( columnsEff(2) )
			columnsEff = [1,2]
		end if
		
		if( present(units) ) then
			effUnits = units
		else
			effUnits = [1.0_8, 1.0_8]
		end if
		
		! El peor de los casos es que todas las lineas sean datos
		allocate( x(stream.numberOfLines) )
		allocate( y(stream.numberOfLines) )
		
		nData = 1
		do while( .not. stream.eof() )
			line = stream.readLine( cCommentsEff )
			
			if( len(line) /= 0 ) then
				call buffer.fromFString( line )
				call buffer.split( tokens, " " )
				
				if( nData == 1 .and. .not. ( size(tokens) >= size(columnsEff) ) ) then
					write(*,*) "### ERROR ### NFunction.fromFStream(): Number of columns in file ("&
									//trim(FString_fromInteger(size(tokens)))&
									//") are not in agree with the parameter columns ("&
									//trim(FString_fromInteger(size(columnsEff)))//"). nFile >= nCols"
					stop
				end if
				
				if( columnsEff(1) <= size(tokens) .and. columnsEff(2) <= size(tokens) ) then
					if( len_trim(tokens(columnsEff(1))) /= 0 .and. len_trim(tokens(columnsEff(2))) /= 0 ) then
						read( tokens(columnsEff(1)),* ) x(nData)
						read( tokens(columnsEff(2)),* ) y(nData)
						nData = nData + 1
					end if
				end if
			end if
		end do
		
		nPoints = nData-1
		call this.xGrid.fromArray( x(1:nPoints) )
		if( allocated(this.fArray) ) deallocate(this.fArray)
		allocate( this.fArray(nPoints) )
		this.fArray = y(1:nPoints)
		call this.setUnits( effUnits )
		
		call this.checkEquallyspaced()
		
		deallocate(x)
		deallocate(y)
		deallocate(columnsEff)
	end function fromFStream
	
	!>
	!! @brief String representation of the object
	!!
	function str( this ) result( output )
		class(RNFunction) :: this 
		character(len=200) :: output
		
		integer :: fmt
		character(len=200) :: strBuffer
		
		output = ""
		
		output = trim(output)//"<RNFunction:"
		
		output = trim(output)//this.xGrid.str()
		
! 		output = trim(output)//",max="
! 		fmt = int(log10(this.max+1.0))+1
! 		write(strBuffer, "(f<fmt+7>.6)") this.max
! 		output = trim(output)//trim(strBuffer)
! 		
! 		output = trim(output)//",h="
! 		fmt = int(log10(this.h+1.0))+1
! 		write(strBuffer, "(f<fmt+7>.6)") this.h
! 		output = trim(output)//trim(strBuffer)
! 		
! 		output = trim(output)//",size="
! 		fmt = int(log10(float(this.size+1)))+1
! 		write(strBuffer, "(i<fmt>)") this.size
! 		output = trim(output)//trim(strBuffer)
		
		output = trim(output)//">"
	end function str
	
end module RNFunction_

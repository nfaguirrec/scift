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

! @todo implementar la selección de la presición para toReal
module String_
	implicit none
	private
	
	character(1), public, parameter :: ENDL = achar(10)
	! @todo Toca 100 porque en algunos casos len(FString_NULL) cambia al llamarla desde diferentes funciones y cuando cambia, cambia a 100
	character(100), public, parameter :: FString_NULL = repeat(""C,100)

! 	http://publib.boulder.ibm.com/infocenter/lnxpcomp/v9v111/index.jsp?topic=/com.ibm.xlf111.linux.doc/xlflr/generic_interface_blocks.htm
! module m
!   type point
!          character(20) label
!          integer x, y
!   contains
!          procedure :: writepoint
!          generic :: write(formatted) => writepoint
!   end type
! 
!   type :: line
!          type(point) :: p1,p2
!   contains
!          procedure :: writeline
!     generic :: write(formatted) => writeline
!   end type
!   contains
!    subroutine writepoint(dtv, unit, iotype, vlist, iostat, iomsg)
!           class(point), intent(in) :: dtv
!           integer, intent(in) :: unit
!           character(*), intent(in) :: iotype
!           integer, intent(in) :: vlist(:)
!           integer, intent(out) :: iostat
!           character(*), intent(inout) :: iomsg
! 
!           write(unit, *, iostat=iostat, iomsg=iomsg) &
!                           trim(dtv%label), ': (', dtv%x, ', ', dtv%y, ')'
!    end subroutine 
! 
!    subroutine writeline(dtv, unit, iotype, vlist, iostat, iomsg)
!           class(line), intent(in) :: dtv
!           integer, intent(in) :: dtv
!           character(*), intent(in) :: iotype
!           integer, intent(in) :: vlist(:)
!           integer, intent(out) :: iostat
!           character(*), intent(inout) :: iomsg
! 
!           real length, delta_x, delta_y
!           delta_x = dtv%p2%x - dtv%p1%x
!           delta_y = dtv%p2%y - dtv%p1%y
!           length = sqrt(delta_x**2 + delta_y**2)
! 
!           write(unit, *, iostat=iostat, iomsg=iomsg) &
!                           'Distance from ', dtv%p1, ' to ', dtv%p2, ' is ', length
!    end subroutine
! end module
! ! 	READ (FORMATTED)	
! 	subroutine my_read_routine_formatted(dtv, unit, iotype, v_list, iostat, iomsg)
! 		integer, intent(in) :: unit ! unit number
! 		! the derived-type value/variable
! 		dtv_type_spec , intent(inout) :: dtv
! 		! the edit descriptor string
! 		character(len=*), intent(in) :: iotype
! 		integer, intent(in) :: v_list(:)
! 		integer, intent(out) :: iostat
! 		character (len=*), intent(inout) :: iomsg
! 	end
! 	
! ! 	READ (UNFORMATTED)	
! 	subroutine my_read_routine_unformatted(dtv, unit, iostat, iomsg)
! 		integer, intent(in) :: unit
! 		! the derived-type value/variable
! 		dtv_type_spec , intent(inout) :: dtv
! 		integer, intent(out) :: iostat
! 		character (len=*), intent(inout) :: iomsg
! 	end
! 	
! ! 	WRITE (FORMATTED)	
! 	subroutine my_write_routine_formatted(dtv, unit, iotype, v_list, iostat, iomsg)
! 		integer, intent(in) :: unit
! 		! the derived-type value/variable
! 		dtv_type_spec , intent(in) :: dtv
! 		! the edit descriptor string
! 		character (len=*), intent(in) :: iotype
! 		integer, intent(in) :: v_list(:)
! 		integer, intent(out) :: iostat
! 		character (len=*), intent(inout) :: iomsg
! 	end
! 	
! ! 	WRITE (UNFORMATTED)	
! 	subroutine my_write_routine_unformatted(dtv, unit, iostat, iomsg)
! 		integer, intent(in) :: unit
! 		! the derived-type value/variable
! 		dtv_type_spec , intent(in) :: dtv
! 		integer, intent(out) :: iostat
! 		character (len=*), intent(inout) :: iomsg
! 	end	

	public :: &
		FString_split, &
		FString_toLogical, &
		FString_toInteger, &
		FString_toReal, &
		FString_toComplex, &
		FString_toString, &
		FString_toIntegerArray, &
		FString_toRealArray, &
		FString_fromString, &
		FString_fromInteger, &
		FString_fromReal, &
		FString_fromLogical, &
		FString_fromIntegerArray, &
		FString_fromRealArray, &
		FString_removeTabs, &
		FString_replace, &
		FString_replaceByRealArr, &
		FString_count, &
		FString_hashKey, &
		FString_isInteger, &
		FString_isNumeric, &
		FString_toUpper, &
		FString_toLower, &
		FString_isNull, &
		FString_removeFileExtension, &
		String_split, &
		String_toLogical, &
		String_toInteger, &
		String_toReal, &
		String_toComplex, &
! 		String_fromInteger, &
! 		String_fromReal, &
		String_test
	
	type, public :: String
		character(:), allocatable :: fstr
		
		contains
			generic :: init => initDefault, fromFString
			generic :: assignment(=) => copy, fromFString
			generic :: operator(+) => add, addFStr
			generic :: operator(<) => lt
			generic :: operator(==) => eq, eqFStr
			generic :: operator(/=) => neq, neqFStr
			generic :: operator(//) => add, addFStr
! 			generic :: write(formatted) => writeString
				
			procedure :: initDefault
			procedure :: fromFString
! 			procedure :: fromInteger
! 			procedure :: fromReal
			procedure :: copy
			final :: destroy
			procedure :: str
			procedure :: show
! 			procedure :: writeString
			procedure :: isEmpty
			procedure :: length
			procedure :: at
			procedure :: add
			procedure :: addFStr
			procedure :: lt
			procedure :: eq
			procedure :: eqFStr
			procedure :: neq
			procedure :: neqFStr
			procedure :: split
			procedure :: toInteger
			procedure :: toReal
			procedure :: toComplex
			procedure :: removeTabs
			procedure :: hashKey
			procedure :: isInteger
			procedure :: isNumeric
			procedure :: toUpper
			procedure :: toLower
			procedure :: replace
			procedure :: replaceByRealArr
			procedure :: removeFileExtension
	end type String
	
	contains
	
	!>
	!! @brief Constructor
	!!
	subroutine initDefault( this )
		class(String) :: this
		
		if( allocated(this.fstr) ) deallocate(this.fstr)
		this.fstr = ""
	end subroutine initDefault
	
	!>
	!! @brief Constructor, from fortran string
	!!
	subroutine fromFString( this, fstr )
		class(String), intent(out) :: this
		character(*), intent(in) :: fstr
		
		if( allocated(this.fstr) ) deallocate(this.fstr)
! 		this.fstr = adjustl(trim(fstr))
		this.fstr = fstr
	end subroutine fromFString
	
	!>
	!! @brief Copy constructor
	!!
	subroutine copy( this, other )
		class(String), intent(inout) :: this
		class(String), intent(in) :: other
		
		if( allocated(this.fstr) ) deallocate(this.fstr)
		this.fstr = other.fstr
	end subroutine copy
	
	!>
	!! @brief Destructor
	!!
	subroutine destroy( this )
		type(String) :: this
		
		if( allocated(this.fstr) ) deallocate(this.fstr)
	end subroutine destroy
	
	!>
	!! @brief Convert to string
	!!
	function str( this ) result( output )
		class(String) :: this 
		character(len=200) :: output
		
		integer :: fmt
		character(len=200) :: strBuffer
		
		output = ""
		
		output = trim(output)//"<String:"
		
		output = trim(output)//this.fstr
		
! 		output = trim(output)//"min="
! 		fmt = int(log10(this.min+1.0))+1
! 		write(strBuffer, "(f<fmt+7>.6)") this.min
! 		output = trim(output)//trim(strBuffer)
! 		
! 		output = trim(output)//",size="
! 		fmt = int(log10(float(this.size+1)))+1
! 		write(strBuffer, "(i<fmt>)") this.size
! 		output = trim(output)//trim(strBuffer)
		
		output = trim(output)//">"
	end function str
	
	!>
	!! @brief Show 
	!!
	subroutine show( this, unit )
		class(String) :: this
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
! 	subroutine writeString( this, unit, iotype, vlist, iostat, iomsg )
! 		class(String), intent(in) :: this
! 		integer, intent(in) :: unit
! 		character(*), intent(in) :: iotype
! 		integer, intent(in) :: vlist(:)
! 		integer, intent(out) :: iostat
! 		character(*), intent(inout) :: iomsg
! 
! 		write(unit, *, iostat=iostat, iomsg=iomsg) trim(this.str())
! 	end subroutine writeString
	!>
	!! @brief 
	!!
	function isEmpty( this ) result( output )
		class(String), intent(in) :: this
		logical :: output
		
		output = merge( .true., .false., len_trim(adjustl(this.fstr)) == 0 )
	end function isEmpty
	
	!>
	!! @brief 
	!!
	function length( this ) result( output )
		class(String), intent(in) :: this
		integer :: output
		
		output = len_trim(this.fstr)
	end function length
	
	!>
	!! @brief 
	!!
	function at( this, pos ) result( output )
		class(String), intent(in) :: this
		integer, intent(in) :: pos
		character :: output
		
		character(:), allocatable :: buffer
		
		buffer = trim(this.fstr)
		
		output = buffer(pos:pos)
	end function at	
	
	!>
	!! @brief 
	!!
	function add( this, other ) result( output )
		class(String), intent(in) :: this
		class(String), intent(in) :: other
		type(String) :: output
		
		output.fstr = this.fstr//other.fstr
	end function add
	
	!>
	!! @brief 
	!!
	function addFStr( this, other ) result( output )
		class(String), intent(in) :: this
		character(*), intent(in) :: other
		type(String) :: output
		
		output.fstr = this.fstr//other
	end function addFStr
	
	!>
	!! @brief 
	!!
	function lt( this, other ) result( output )
		class(String), intent(in) :: this
		class(String), intent(in) :: other
		logical :: output
		
		output = ( this.hashKey() < other.hashKey() )
	end function lt
	
	!>
	!! @brief 
	!!
	function eq( this, other ) result( output )
		class(String), intent(in) :: this
		class(String), intent(in) :: other
		logical :: output
		
		output = ( this.hashKey() == other.hashKey() )
	end function eq
	
	!>
	!! @brief 
	!!
	function eqFStr( this, other ) result( output )
		class(String), intent(in) :: this
		character(*), intent(in) :: other
		logical :: output
		
		output = ( this.hashKey() == FString_hashKey(other) )
	end function eqFStr
	
	!>
	!! @brief 
	!!
	function neq( this, other ) result( output )
		class(String), intent(in) :: this
		class(String), intent(in) :: other
		logical :: output
		
		output = ( this.hashKey() /= other.hashKey() )
	end function neq
	
	!>
	!! @brief 
	!!
	function neqFStr( this, other ) result( output )
		class(String), intent(in) :: this
		character(*), intent(in) :: other
		logical :: output
		
		output = ( this.hashKey() /= FString_hashKey(other) )
	end function neqFStr
	
	!>
	!! @brief 
	!!
	subroutine split( this, tokens, delimiters )
		class(String), intent(in) :: this
		character(*), allocatable, intent(out) :: tokens(:)
		character(*), intent(in) :: delimiters
		
		call FString_split( this.fstr, tokens, delimiters )
	end subroutine split
	
	!>
	!! @brief 
	!!
	subroutine FString_split( str, tokens, delimiters )
		character(*), intent(in) :: str
		character(*), allocatable, intent(out) :: tokens(:)
		character(*), intent(in) :: delimiters
		
		integer, allocatable :: posTokenBegin(:), posTokenEnd(:)
		logical :: isDelimiter, isDelimiterPrev
		integer :: ntokens
		integer :: i, j
		
		if( allocated(tokens) ) then
			deallocate(tokens)
		end if
		
		if( len(trim(str)) == 0 ) then
			allocate( tokens(1) )
			tokens(1) = str
			return
		end if
		
		! En el peor de los casos todos los caracteres son separadores
		allocate( posTokenBegin(len(str)) )
		allocate( posTokenEnd(len(str)) )
		
		posTokenBegin = 0
		posTokenEnd = 0
		
		!! scan
		ntokens = 1
		isDelimiterPrev = .true.
		do i=1,len(str)
! 			write(*,"(3A)",advance="no") str(i:i)
			
			isDelimiter = .false.
			do j=1,len(delimiters)
				if( str(i:i) == delimiters(j:j) ) then
					isDelimiter = .true.
					exit
				end if
			end do
			
			if( isDelimiter .and. .not. isDelimiterPrev ) then
				posTokenEnd( ntokens ) = i-1
				ntokens = ntokens + 1
! 				write(*,*) "    E"
			else if( .not. isDelimiter .and. isDelimiterPrev ) then
				posTokenBegin( ntokens ) = i
! 				write(*,*) "    B"
! 			else
! 				write(*,*) ""
			end if
			
			isDelimiterPrev = isDelimiter
		end do
		
		if( posTokenEnd(ntokens) == 0 .and. .not. isDelimiter ) then
			posTokenEnd( ntokens ) = len( str )
		else
			ntokens = ntokens - 1 
		end if
		
! 		write(*,"(A,<len(str)>I3)") "ntokens = ", ntokens
! 		write(*,"(A,<len(str)>I3)") "  begin = ", posTokenBegin
! 		write(*,"(A,<len(str)>I3)") "    end = ", posTokenEnd
		
		allocate( tokens(ntokens) )
		
		do i=1,ntokens
			tokens(i) = str( posTokenBegin(i):posTokenEnd(i) )
		end do
		
		deallocate( posTokenBegin )
		deallocate( posTokenEnd )
		
	end subroutine FString_split
	
	!>
	!! @brief 
	!!
	function toLogical( this ) result( output )
		class(String), intent(in) :: this
		logical :: output
		
		read( this.fstr, * ) output
	end function toLogical
	
	!>
	!! @brief 
	!!
	function toInteger( this ) result( output )
		class(String), intent(in) :: this
		integer :: output
		
		read( this.fstr, * ) output
	end function toInteger
	
	!>
	!! @brief 
	!!
	function toReal( this ) result( output )
		class(String), intent(in) :: this
		real(8) :: output
		
		read( this.fstr, * ) output
	end function toReal
	
	!>
	!! @brief 
	!!
	function toComplex( this ) result( output )
		class(String), intent(in) :: this
		complex(8) :: output
		
		read( this.fstr, * ) output
	end function toComplex
	
	!>
	!! @brief Replace every occurrence of the character \tab in this string
	!!        for four blank spaces
	!!
	subroutine removeTabs( this )
		class(String) :: this
		
		this = FString_removeTabs( this.fstr )
	end subroutine removeTabs
	
	!>
	!! @brief 
	!!
	function FString_toLogical( str ) result( output )
		character(*), intent(in) :: str
		logical :: output
		
		read( str, * ) output
	end function FString_toLogical
	
	!>
	!! @brief 
	!!
	function FString_toInteger( str ) result( output )
		character(*), intent(in) :: str
		integer :: output
		
		read( str, * ) output
	end function FString_toInteger
	
	!>
	!! @brief 
	!!
	function FString_toReal( str ) result( output )
		character(*), intent(in) :: str
		real(8) :: output
		
		read( str, * ) output
	end function FString_toReal
	
	!>
	!! @brief 
	!!
	function FString_toComplex( str ) result( output )
		character(*), intent(in) :: str
		complex(8) :: output
		
		read( str, * ) output
	end function FString_toComplex
	
	!>
	!! @brief 
	!!
	function FString_toString( str ) result( output )
		character(*), intent(in) :: str
		type(String) :: output
		
		call output.fromFString( str )
	end function FString_toString
	
	!>
	!! @brief 
	!!
	subroutine FString_toIntegerArray( str, output )
		character(*), intent(in) :: str
		integer, allocatable :: output(:)
		
		character(1000), allocatable :: tokens(:)
		character(1000) :: strBuffer
		integer :: i
		
		call FString_split( trim(adjustl(str)), tokens, "()[]" )
		strBuffer = tokens(1)
		call FString_split( strBuffer, tokens, "," )
		
		if(  allocated(output) ) deallocate( output )
		allocate( output(size(tokens)) )
		
		do i=1,size(tokens)
			read( tokens(i), * ) output(i)
		end do
		
		deallocate(tokens)
	end subroutine FString_toIntegerArray
	
	!>
	!! @brief 
	!!
	subroutine FString_toRealArray( str, output )
		character(*), intent(in) :: str
		real(8), allocatable :: output(:)
		
		character(100000), allocatable :: tokens(:)
		character(100000) :: strBuffer
		integer :: i
		
		call FString_split( trim(adjustl(str)), tokens, "()[]" )
		strBuffer = tokens(1)
		
		if( len_trim(strBuffer) == 0 ) then
			deallocate(tokens)
! 			deallocate(strBuffer)
			return
		end if
		
		call FString_split( strBuffer, tokens, "," )
		
		if(  allocated(output) ) deallocate( output )
		allocate( output(size(tokens)) )
		
		do i=1,size(tokens)
			read( tokens(i), * ) output(i)
		end do
		
		deallocate(tokens)
! 		deallocate(strBuffer)
	end subroutine FString_toRealArray
	
	!>
	!! @brief 
	!!
	function FString_fromString( str ) result( output )
		type(String), intent(in) :: str
		character(:), allocatable :: output
		
		output = str.fstr
	end function FString_fromString
	
	!>
	!! @brief 
	!!
	function FString_fromInteger( val, format ) result( output )
		integer, intent(in) :: val
		character(*), optional, intent(in) :: format
		character(1000) :: output
		
		character(1000) :: strBuffer
		
		if( present(format) ) then
			write( strBuffer, format ) val
			output = strBuffer
		else
			write( strBuffer, * ) val
			output = trim(adjustl(strBuffer))
		end if
	end function FString_fromInteger
	
	!>
	!! @brief 
	!!
	function FString_fromReal( val, format ) result( output )
		real(8), intent(in) :: val
		character(*), optional, intent(in) :: format
		character(:), allocatable :: output
		
		character(1000) :: strBuffer
		
		if( present(format) ) then
			write( strBuffer, format ) val
			output = strBuffer
		else
			write( strBuffer, * ) val
			output = trim(adjustl(strBuffer))
		end if
	end function FString_fromReal
	
	!>
	!! @brief 
	!!
	function FString_fromLogical( val, format ) result( output )
		logical, intent(in) :: val
		character(*), optional, intent(in) :: format
		character(:), allocatable :: output
		
		character(1000) :: strBuffer
		
		if( present(format) ) then
			write( strBuffer, format ) val
			output = strBuffer
		else
			write( strBuffer, * ) val
			output = trim(adjustl(strBuffer))
		end if
	end function FString_fromLogical
	
	!>
	!! @brief 
	!!
	function FString_fromIntegerArray( val, format ) result( output )
		integer, intent(in) :: val(:)
		character(*), optional, intent(in) :: format
		character(1000) :: output
		
		character(1000) :: strBuffer
		
		if( present(format) ) then
			write( strBuffer, format ) val
		else
			write( strBuffer, * ) val
		end if
		
		output = "( "//trim(adjustl(strBuffer))//" )"
	end function FString_fromIntegerArray
	
	!>
	!! @brief 
	!!
	function FString_fromRealArray( val, format ) result( output )
		real(8), intent(in) :: val(:)
		character(*), optional, intent(in) :: format
		character(:), allocatable :: output
		
		character(1000) :: strBuffer
		
		if( present(format) ) then
			write( strBuffer, format ) val
		else
			write( strBuffer, * ) val
		end if
		
		output = "( "//trim(adjustl(strBuffer))//" )"
	end function FString_fromRealArray
	
	!>
	!! @brief Replace every occurrence of the character \tab in this string
	!!        for four blank spaces
	!! @todo Hay que ofrecer la opción de seleccionar el tamaño del tab
	!!       por ejemplo subroutine FString_removeTabs( str, tabSize )
	!! @todo Creo que retornar un allocatable es peligroso
	!!
	function FString_removeTabs( str ) result( output )
		character(*), intent(inout) :: str
		character(:), allocatable :: output
		
		output = FString_replace( str, achar(9), "    " )
	end function FString_removeTabs
	
	!>
	!! @brief 
	!!
	function FString_replace( str, before, after, wholeWords, wordSeparators ) result( output )
		character(*), intent(in) :: str
		character(*), intent(in) :: before
		character(*), intent(in) :: after
		logical, optional, intent(in) :: wholeWords
		character(*), optional, intent(in) :: wordSeparators
		character(:), allocatable :: output
		
		integer :: i
		integer :: nMatches
		integer, allocatable :: matchPos(:)
		
		nMatches = FString_count( str, before, matchPos, wholeWords, wordSeparators )
		
		if( nMatches == 0 ) then
			output = str
			return
		end if
		
		output = ""
		do i=1,nMatches+1
			if( i==1 ) then
				output = str(1:matchPos(i)-1)//after
			else if ( i==size(matchPos)+1 ) then
				output = output//str(min(matchPos(i-1)+len(before),len(str)+1):len(str))
			else
				output = output//str(matchPos(i-1)+len(before):matchPos(i)-1)//after
			end if
		end do
		
		deallocate( matchPos )
	end function FString_replace
	
	!>
	!! @brief 
	!!
	function FString_replaceByRealArr( str, varName, varValue, wholeWords, wordSeparators ) result( output )
		character(*), intent(inout) :: str
		character(*), intent(in) :: varName(:)
		real(8), intent(in) :: varValue(:)
		logical, optional, intent(in) :: wholeWords
		character(*), optional, intent(in) :: wordSeparators
		character(:), allocatable :: output
		
		integer :: i
		
		if( size(varName) /= size(varValue) ) then
			write(6,*) "### ERROR ### FString_replaceByRealArr. size(varName) /= size(varValue)"
			stop
		end if
		
		output = str
		do i=1,size(varName)
			output = FString_replace( output, trim(adjustl(varName(i))), FString_fromReal(varValue(i)), wholeWords=.true. )
		end do
	end function FString_replaceByRealArr
	
	!>
	!! @brief 
	!!
	function FString_count( str, ref, matchPos, wholeWords, wordSeparators ) result( nMatches )
		character(*), intent(in) :: str
		character(*), intent(in) :: ref
		integer, allocatable, optional, intent(out) :: matchPos(:)
		logical, optional, intent(in) :: wholeWords
		character(*), optional, intent(in) :: wordSeparators
		integer :: nMatches
		
		logical :: effWholeWords
		character(:), allocatable :: effWordSeparators
		
		integer :: pos
		integer, allocatable :: tmpMatchPos(:)
		integer, allocatable :: tmpMatchWholeWord(:) ! 0 or 1
		character(:), allocatable :: strBuffer
		integer :: i, n
		
		effWholeWords = .false.
		if( present(wholeWords) ) effWholeWords = wholeWords
		
		! wordSeparators": "./\\()\"'-:,.;<>~!@#$%^&*|+=[]{}`~?\\.",
		effWordSeparators = ":;@-.,/_~?&=%+#*()[]{} "//achar(9)
		if( present(wordSeparators) ) effWordSeparators = wordSeparators
		
		strBuffer = str
		
		! En el peor de los casos todos los caracteres son ref
		allocate( tmpMatchPos(len(str)) )
		allocate( tmpMatchWholeWord(len(str)) )
		
		tmpMatchPos = 0
		tmpMatchWholeWord = 0
		
		n = 1
		do while( .true. )
			pos = index( strBuffer, ref )
			
			if( pos == 0 ) exit
			
			if( ( &
				  ( index( effWordSeparators, strBuffer(pos-1:pos-1) ) /= 0 .or. pos-1 == 0 ).and. &
				  index( effWordSeparators, strBuffer(pos+len(ref):pos+len(ref)) ) /= 0 &
			) ) then
				tmpMatchWholeWord(n) = 1
			end if
			
			tmpMatchPos(n) = pos + merge( 0, tmpMatchPos(n-1), n<1 )
			n = n + 1
			
			strBuffer = strBuffer(pos+1:)
		end do
		
		nMatches = n-1
		
		if( present(matchPos) ) then
			if( effWholeWords ) then
				if( allocated(matchPos) ) deallocate( matchPos )
				allocate( matchPos(sum(tmpMatchWholeWord)) )
				
				i = 1
				do n=1,nMatches
					if( tmpMatchWholeWord(n) == 1 ) then
						matchPos(i) = tmpMatchPos(n)
						i = i+1
					end if
				end do
				
				nMatches = sum(tmpMatchWholeWord)
			else
				if( allocated(matchPos) ) deallocate( matchPos )
				allocate( matchPos(nMatches) )
				
				matchPos = tmpMatchPos(1:nMatches)
			end if
		end if
		
		deallocate(tmpMatchPos)
	end function FString_count
	
	!>
	!! @brief 
	!!
! 	function hashKey( this, debug ) result( output )
	function hashKey( this ) result( output )
		class(String), intent(in) :: this
! 		logical, optional, intent(in) :: debug
		integer :: output
		
! 		output = FString_hashKey( this.fstr, debug )
		output = FString_hashKey( this.fstr )
	end function hashKey
	
	!>
	!! @brief 
	!! @todo Al parecer si this=,R,S:0,5 dice que es de tipo numerico
	!!
	function isInteger( this ) result( output )
		class(String), intent(in) :: this
		logical :: output
		
		output = FString_isInteger( this.fstr )
	end function isInteger
	
	!>
	!! @brief
	!! @todo Al parecer si this=,R,S:0,5 dice que es de tipo numerico
	!!
	function isNumeric( this ) result( output )
		class(String), intent(in) :: this
		logical :: output
		
		output = FString_isNumeric( this.fstr )
	end function isNumeric
	
	!>
	!! @brief Returns an uppercase copy of the string.
	!!
	function toUpper( this ) result( output )
		class(String), intent(in) :: this
		type(String) :: output
		
		output = FString_toUpper( this.fstr )
	end function toUpper
	
	!>
	!! @brief Returns a lowercase copy of the string.
	!!
	function toLower( this ) result( output )
		class(String), intent(in) :: this
		type(String) :: output
		
		output = FString_toLower( this.fstr )
	end function toLower
	
	!>
	!! @brief Replaces every occurrence of the string before with the string after
	!!
	subroutine replace( this, before, after, wholeWords, wordSeparators )
		class(String) :: this
		character(*), intent(in) :: before
		character(*), intent(in) :: after
		logical, optional, intent(in) :: wholeWords
		character(*), optional, intent(in) :: wordSeparators
		
		this = FString_replace( this.fstr, before, after, wholeWords, wordSeparators )
	end subroutine replace
	
	!>
	!! @brief 
	!!
	subroutine replaceByRealArr( this, varName, varValue, wholeWords, wordSeparators )
		class(String) :: this
		character(*), allocatable, intent(in) :: varName(:)
		real(8), allocatable, intent(in) :: varValue(:)
		logical, optional, intent(in) :: wholeWords
		character(*), optional, intent(in) :: wordSeparators
		
		this = FString_replaceByRealArr( this.fstr, varName, varValue, wholeWords, wordSeparators )
	end subroutine replaceByRealArr
	
	!>
	!! @brief 
	!!
	function removeFileExtension( this, extension ) result( output )
		class(String), intent(in) :: this
		type(String), optional :: extension
		type(String) :: output
		
		output = FString_removeFileExtension( this.fstr, extension=extension.fstr )
	end function removeFileExtension
	
	!>
	!! @brief 
	!!   Taken from the java.lang.String hash function, it use 32 bits
	!!
! 	function FString_hashKey( str, debug ) result( output )
	function FString_hashKey( str ) result( output )
		character(*), intent(in) :: str
! 		logical, optional, intent(in) :: debug
		integer :: output
		
		integer :: i
		
! 		logical :: effDebug
! 		
! 		effDebug = .false.
! 		if( present(debug) ) effDebug = debug
		
		output = 0
		do i=1,len(str)
! 			if( effDebug ) then
! 				write(0,*) str(i:i), "-->", ichar(str(i:i)), " : ", ichar(str(i:i))*31**(len(str)-i)
! 			end if
			
			output = output + ichar(str(i:i))*31**(len(str)-i)
		end do
		
! 		if( effDebug ) then
! 			write(0,*) "key --> ", output
! 		end if
		
	end function FString_hashKey
	
	!>
	!! @brief 
	!! @todo Al parecer si this=,R,S:0,5 dice que es de tipo numerico
	!!
	function FString_isInteger( str ) result( output )
		character(*), intent(in) :: str
		logical :: output
		
		integer :: realType
		integer :: e
		
		output = .false.
		
		read( str, *, iostat=e ) realType
		
		output = ( e == 0 )
	end function FString_isInteger
	
	!>
	!! @brief 
	!! @todo Al parecer si this=,R,S:0,5 dice que es de tipo numerico
	!!
	function FString_isNumeric( str ) result( output )
		character(*), intent(in) :: str
		logical :: output
		
		real(8) :: realType
		integer :: e
		
		output = .false.
		
		read( str, *, iostat=e ) realType
		
		output = ( e == 0 )
	end function FString_isNumeric
	
	!>
	!! @brief Returns an uppercase copy of the string.
	!! @todo La conversión se hace con una tabla específica, pero hay
	!!       que permitir que la conversión se haga con un LOCALE designado
	!!       @see FString_toLower()
	!!
	function FString_toUpper( str ) result( output )
		character(*), intent(in) :: str
		character(len(str)) :: output
		
		! Translation from awk implementation
		! echo aábcdeéfghiéjklmnoópqrstuúvwxyz | awk '{ print toupper($0) }'
		character(*), parameter :: lc = "¡!¿? aábcdeéfghiíjklmnoópqrstuúvwxyz"
		character(*), parameter :: uc = "¡!¿? AÁBCDEÉFGHIÍJKLMNOÓPQRSTUÚVWXYZ"
		
		integer :: j, k
		
		output = str
		do j=1,len_trim(str)
			k = index( lc, str(j:j) )
			if (k > 0) output(j:j) = uc(k:k)
		end do
	end function FString_toUpper
	
	!>
	!! @brief Returns a lowercase copy of the string.
	!!
	function FString_toLower( str ) result( output )
		character(*), intent(in) :: str
		character(len(str)) :: output
		
		character(*), parameter :: lc = "¡!¿? aábcdeéfghiíjklmnoópqrstuúvwxyz"
		character(*), parameter :: uc = "¡!¿? AÁBCDEÉFGHIÍJKLMNOÓPQRSTUÚVWXYZ"
		
		integer :: j, k
		
		output = str
		do j=1,len_trim(str)
			k = index( uc, str(j:j) )
			if (k > 0) output(j:j) = lc(k:k)
		end do
	end function FString_toLower
	
	!>
	!! @brief Returns a lowercase copy of the string.
	!!
	function FString_isNull( str ) result( output )
		character(*), intent(in) :: str
		logical :: output
		
		output = ( trim(str) == trim(FString_NULL) )
	end function FString_isNull
	
	!>
	!! @brief
	!!
	function FString_removeFileExtension( str, extension ) result( output )
		character(*), intent(in) :: str
		character(:), optional, allocatable :: extension
		character(:), allocatable :: output
		
		integer :: idPos
		
		idPos = scan( str, ".", back=.true. )
		
		output = str(1:idPos-1)
		
		if( present(extension) ) extension = str(idPos:len(str))
	end function FString_removeFileExtension
	
	!>
	!! @brief 
	!!
	function String_toLogical( str ) result( output )
		type(String), intent(in) :: str
		logical :: output
		
		read( str.fstr, * ) output
	end function String_toLogical
	
	!>
	!! @brief 
	!!
	function String_toInteger( str ) result( output )
		type(String), intent(in) :: str
		integer :: output
		
		read( str.fstr, * ) output
	end function String_toInteger
	
	!>
	!! @brief 
	!!
	function String_toReal( str ) result( output )
		type(String), intent(in) :: str
		real(8) :: output
		
		read( str.fstr, * ) output
	end function String_toReal
	
	!>
	!! @brief 
	!!
	function String_toComplex( str ) result( output )
		type(String), intent(in) :: str
		complex(8) :: output
		
		read( str.fstr, * ) output
	end function String_toComplex
	
	!>
	!! @brief 
	!!
	subroutine String_split( str, tokens, delimiters )
		class(String), intent(in) :: str
		character(*), allocatable, intent(out) :: tokens(:)
		character(*), intent(in) :: delimiters
		
		call FString_split( str.fstr, tokens, delimiters )
	end subroutine String_split
	
	!>
	!! @brief Test method
	!!
	subroutine String_test()
		type(String) :: str1
		type(String) :: str2
		type(String) :: str3
		integer :: int1
		real(8) :: real1
		complex(8) :: complex1
		character(100), allocatable :: fstrArray(:)
		integer, allocatable :: intArray(:)
		real(8), allocatable :: realArray(:)
		character(:), allocatable :: fstr1
		character(100), allocatable :: tokens(:)
! 		character(100) :: fstr
		character(:), allocatable :: fstr
		integer :: i
		
		write(*,*)
		write(*,*) "Testing constructors"
		write(*,*) "===================="

		call str1.init( "Hello my friends" )
		str2 = str1
		call str2.show()
		
		str2 = "Hello my friends from asignation operator"
		call str2.show()
		
		fstr = "Hello my friends from fortran string"
		str2 = fstr
		call str2.show()
		
		write(*,*)
		write(*,*) "Testing operators"
		write(*,*) "================="
		
		call str1.show()
		call str2.show()
		str3 = str1+str2
		call str3.show()
		
		str1 = "My friends"
		str1 = str1+" it works"
		call str1.show()
		
		write(*,*)
		write(*,*) "Testing split"
		write(*,*) "============="
		write(*,*) "Original ==> ", str1.fstr
		call str1.split( tokens, " " )
		write(*,*) "Split( ) ==> "
		do i=1,size(tokens)
			write(*,*) i, "    ", trim(tokens(i))
		end do
		
		write(*,*)
		fstr1 = "Hello :my friends: from;?fortran?string"
		write(*,*) "Original   ==> ", fstr1
		call FString_split( fstr1, tokens, ":;?" )
		write(*,*) "Split(:;?) ==> "
		do i=1,size(tokens)
			write(*,*) i, "    ", trim(tokens(i))
		end do
		
		write(*,*)
		fstr1 = "Hello :my friends: from;?fortran?string"
		write(*,*) "Original ==> ", fstr1
		call FString_split( fstr1, tokens, "-" )
		write(*,*) "Split(-) ==> "
		do i=1,size(tokens)
			write(*,*) i, "    ", trim(tokens(i))
		end do
		
		write(*,*)
		fstr1 = ""
		write(*,*) "Original ==> ", fstr1
		call FString_split( fstr1, tokens, "-" )
		write(*,*) "Split(-) ==> "
		do i=1,size(tokens)
			write(*,*) i, "    ", trim(tokens(i))
		end do
		
		write(*,*)
		fstr1 = "------"
		write(*,*) "Original ==> ", fstr1
		call FString_split( fstr1, tokens, "-" )
		write(*,*) "Split(-) ==> "
		do i=1,size(tokens)
			write(*,*) i, "    ", trim(tokens(i))
		end do
		
		deallocate( tokens )
		
		write(*,*)
		write(*,*) "Testing convertion to integer, real and complex"
		write(*,*) "==============================================="
		str1 = "AAABBB"
		call str1.show()
		write(*,*) "isNumeric => ", str1.isNumeric()
		str1 = "12345"
		call str1.show()
		write(*,*) "isNumeric => ", str1.isNumeric()
		write(*,*) "integer   => ", str1.toInteger()
		write(*,*) "   real   => ", str1.toReal()
		write(*,*) "complex   => ", str1.toComplex()
		str1 = "0.12345"
		call str1.show()
		write(*,*) "isNumeric => ", str1.isNumeric()
		write(*,*) "integer   => ", str1.toInteger()
		write(*,*) "   real   => ", str1.toReal()
		write(*,*) "complex   => ", str1.toComplex()
		str1 = "-3.52345"
		call str1.show()
		write(*,*) "isNumeric => ", str1.isNumeric()
		write(*,*) "integer => ", str1.toInteger()
		write(*,*) "   real => ", str1.toReal()
		write(*,*) "complex   => ", str1.toComplex()
		str1 = "(-3.52345,1.7538)"
		call str1.show()
		write(*,*) "isNumeric => ", str1.isNumeric()
		write(*,*) "integer => ", str1.toInteger()
		write(*,*) "   real => ", str1.toReal()
		write(*,*) "complex   => ", str1.toComplex()
		str1 = " ( -3.52345, 2.345, 6.345 )"
		call str1.show()
		write(*,*) "isNumeric => ", str1.isNumeric()
		call FString_toIntegerArray( str1.fstr, intArray )
		write(*,*) "integer => ", intArray
		str1 = " ( -3.52345, 2.345, 6.345 )"
		call str1.show()
		write(*,*) "isNumeric => ", str1.isNumeric()
		call FString_toRealArray( str1.fstr, realArray )
		write(*,*) "     real => ", realArray
		
		if( allocated(intArray) ) deallocate( intArray )
		if( allocated(realArray) ) deallocate( realArray )
		
		write(*,*)
		write(*,*) "Testing convertion from integer and real"
		write(*,*) "======================================"
		int1 = 12345
		write(*,*) "integer => ", trim(FString_fromInteger( int1 ))
		write(*,*) "integer => ", trim(FString_fromInteger( int1, "(I10)" ))
		real1 = -3.52345_8
		write(*,*) "   real => ", trim(FString_fromReal( real1 ))
		write(*,*) "   real => ", trim(FString_fromReal( real1, "(F10.3)" ))
		
		write(*,*)
		write(*,*) "Testing count and replace characters"
		write(*,*) "===================================="
! 		fstr = "maHola"//char(9)//"ma,amigos"//char(9)//char(9)//"del almama"
		fstr = "maHola ma,amigos del almama"
		
		write(*,*) "inicial --"//trim(fstr)//"-- len=", len_trim(fstr)
		write(*,*) "found ", FString_count( fstr, char(9) ), "characters char(9)"
		write(*,*) "found ", FString_count( fstr, "am" ), "characters 'am'"
		write(*,*) "---"//FString_removeTabs( fstr )//"---"
! 		call FString_replace( fstr, achar(9), "   " )
! 		write(*,*) "removeTabs --"//trim(fstr)//"--"
		write(*,*) "---"//FString_replace( fstr, char(9), "XXX" )//"---"
! 		call FString_replace( fstr, "a", "uu" )
		write(*,*) "inicial                  ---"//trim(fstr)//"---"
		write(*,*) "replace    'ma'->'xx'    ---"//FString_replace( fstr, "ma", "xx" )//"---"
		write(*,*) "replace hw 'ma'->'xx'    ---"//FString_replace( fstr, "ma", "xx", wholeWords=.true. )//"---"
		
		write(*,*)
		write(*,*) "Testing replace variables by reals"
		write(*,*) "=================================="
		fstr = "a**2*sin(2*pi/4.0/a**2)+exp(-b*x**2)"
		write(*,"(A,A)") "original => ", fstr
		write(*,"(A,2A10)") "    vars => ", ["a","b"]
		write(*,"(A,2F10.5)") "    vars => ", [3.12345_8,0.09876_8]
		write(*,"(A,A)") " final => ", trim(FString_replaceByRealArr( fstr, ["a","b"], [3.12345_8,0.09876_8] ))
		write(*,*)
		
		fstr = "a**2*sin(2*pi/4.0/a**2)+exp(-b*x**2)"
		write(*,"(A,A)") "original => ", fstr
		allocate(fstrArray(3))
		fstrArray = [ "a", "b", "pi" ]
		write(*,"(A,3A10)") "    vars => ", fstrArray
		allocate(realArray(3))
		realArray = [ 3.12345_8, 0.09876_8, 3.141592_8 ]
		write(*,"(A,3F10.5)") "    vars => ", realArray
		write(*,"(A,A)") " final => ", trim(FString_replaceByRealArr( fstr, fstrArray, realArray ))
		deallocate(fstrArray)
		deallocate(realArray)
		
		write(*,*)
		write(*,*) "Testing FString_NULL"
		write(*,*) "===================="
		str1 = FString_NULL
		write(*,*) "str ==> ", str1.fstr
		write(*,*) "( str1 /= FString_NULL ) ==> ", ( str1 /= FString_NULL )
		write(*,*) "( str1 == FString_NULL ) ==> ", ( str1 == FString_NULL )
		
		write(*,*)
		write(*,*) "Testing Remove extension"
		write(*,*) "========================"
		str1 = "Hola.234-kjsdf.dat"
		write(*,*) "str ==> ", str1.fstr
		str2 = str1.removeFileExtension( extension=str3 )
		write(*,*) "str.removeFileExtension() ==> ", str2.fstr
		write(*,*) "      extension ==> ", str3.fstr
		write(*,*) "FString_removeFileExtension( str ) ==> ", FString_removeFileExtension( str1.fstr )
	end subroutine String_test
	
end module String_

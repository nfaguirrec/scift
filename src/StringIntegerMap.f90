!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                                                                                   !!
!!  This file is part of SciFT project                                               !!
!!  Copyright (c) 2012-2013 Nestor F. Aguirre (nfaguirrec@gmail.com)                 !!
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

module StringIntegerMap_
	use IOStream_
	use String_
	use StringIntegerPair_
	use StringIntegerPairList_, StringIntegerMapIterator => StringIntegerPairListIterator
	implicit none
	private
	
	public :: StringIntegerMapIterator
	
		
#define Map StringIntegerMap
! #define __CLASS_ITEMMAP__ class(String)
#define __CLASS_MAPITERATOR__  type(StringIntegerMapIterator)
#define __TYPE_MAPLIST__       type(StringIntegerPairList)
#define __MAPLIST__       StringIntegerPairList
#define __TYPE_MAPPAIR__       type(StringIntegerPair)
#define __CLASS_MAPPAIR__      class(StringIntegerPair)
#define __MAPPAIR__      StringIntegerPair
! #define __CLASS_MAPKEY__       class(String)
#define __CLASS_MAPVALUE__     integer
#define __TYPE_MAPVALUE__     integer
#define __MAPVALUE__     integer
#define __ADD_METHODS__
#include "Map.h90"
#undef Map
#undef __CLASS_MAPITERATOR__
#undef __TYPE_MAPLIST__
#undef __TYPE_MAPPAIR__
#undef __CLASS_MAPPAIR__
! #undef __CLASS_MAPKEY__
#undef __CLASS_MAPVALUE__
#undef __TYPE_MAPVALUE__
#undef __ADD_METHODS__
	
	!>
	!! @brief Convert to string
	!!
	function str( this, formatted, prefix ) result( output )
		class(StringIntegerMap) :: this 
		character(:), allocatable :: output
		logical, optional :: formatted
		character(*), optional :: prefix
		
		logical :: effFormatted
		character(:), allocatable :: effPrefix
		
		type(StringIntegerMapIterator), pointer :: iter
		type(StringIntegerPair) :: pair
		integer :: fmt
		character(200) :: fstr
		
		effFormatted = .false.
		if( present(formatted) ) effFormatted = formatted
		
		effPrefix = ""
		if( present(prefix) ) effPrefix = prefix
		
		output = ""
		
		if( .not. effFormatted ) then
#define ITEMI(l,v) output = trim(output)//l; write(fstr,*) v; output = trim(output)//trim(adjustl(fstr))
		
			output = trim(output)//"<Map:"
		
			ITEMI( "size=", this%size() )
			
#undef ITEMI
			output = trim(output)//">"
! 		else
! #define LINE(l) output = trim(output)//effPrefix//l//new_line('')
! #define ITEMS(l,v) output = trim(output)//effPrefix//l; write(fstr, "(x,a)") trim(v); output = trim(output)//trim(fstr)//new_line('')
! #define ITEMI(l,v) output = trim(output)//effPrefix//l; write(fstr, "(i10)") v; output = trim(output)//trim(fstr)//new_line('')
! #define ITEMR(l,v) output = trim(output)//effPrefix//l; write(fstr, "(f10.5)") v; output = trim(output)//trim(fstr)//new_line('')
! 
! 			LINE("Map")
! 			LINE("---------")
! ! 			ITEMI( "min=", this%min )
! ! 			ITEMR( ",size=", this%size )
! 			LINE("")
! #undef LINE
! #undef ITEMS
! #undef ITEMI
! #undef ITEMR
		end if
	end function str
	
	!>
	!! Save the data in two column format in a
	!! selected unit
	!!
	subroutine toFStream( this, ofile )
		class(StringIntegerMap) :: this
		type(OFStream), optional, intent(in) :: ofile
		
		integer :: unitEff
		integer :: maxLen
#ifdef __GFORTRAN__
		character(len=100) :: fmtStr
#endif
		
		type(StringIntegerMapIterator), pointer :: iter
		type(StringIntegerPair) :: pair
		
		if( present(ofile) ) then
			unitEff = ofile%unit
		else
			unitEff = IO_STDOUT
		end if
		
		maxLen = 0
		iter => this%begin
		do while( associated(iter) )
			pair = this%pair( iter )
			if( len(pair%first%fstr) > maxLen ) maxLen = len(pair%first%fstr)
			
			iter => iter%next
		end do
		
		write(unitEff,"(a)") "#"//trim(str(this))
		
		iter => this%begin
		do while( associated(iter) )
			pair = this%pair( iter )
#ifdef __GFORTRAN__
			write(fmtStr, "(A,I0,A)") "(A", maxLen, ",I15)"
			write(unitEff,fmtStr) pair%first%fstr, pair%second
#else
			write(unitEff,"(A<maxLen>,I15)") pair%first%fstr, pair%second
#endif
			
			iter => iter%next
		end do
	end subroutine toFStream
	
	subroutine showMyMap( mymap )
		type(StringIntegerMap) :: mymap
		
		type(StringIntegerMapIterator), pointer :: iter
		type(StringIntegerPair) :: pair
		
		iter => mymap%begin
		do while( associated(iter) )
			pair = mymap%pair( iter )
			write(*,"(I20,A15,I5)") pair%first%hashKey(), pair%first%fstr, pair%second
			
			iter => iter%next
		end do
		write(*,*)
	end subroutine

end module StringIntegerMap_

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                                                                                   !!
!!  This file is part of SciFT project                                               !!
!!  Copyright (c) 2012-2014 Nestor F. Aguirre (nfaguirrec@gmail.com)                 !!
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

module StringRealHistogramMap_
	use GOptions_
	use IOStream_
	use String_
	use RealHistogram_
	use StringRealHistogramPair_
	use StringRealHistogramPairList_, StringRealHistogramMapIterator => StringRealHistogramPairListIterator
	implicit none
	private
	
	public :: StringRealHistogramMapIterator
	
	public :: &
		StringRealHistogramMap_test
		
#define Map StringRealHistogramMap
! #define __CLASS_ITEMMAP__ class(String)
#define __CLASS_MAPITERATOR__  class(StringRealHistogramMapIterator)
#define __MAPLIST__            StringRealHistogramPairList
#define __TYPE_MAPLIST__       type(StringRealHistogramPairList)
#define __TYPE_MAPPAIR__       type(StringRealHistogramPair)
#define __CLASS_MAPPAIR__      class(StringRealHistogramPair)
! #define __CLASS_MAPKEY__       class(String)
#define __CLASS_MAPVALUE__     class(RealHistogram)
#define __TYPE_MAPVALUE__     type(RealHistogram)
#define __ADD_METHODS__ \
        generic :: add => addValue, addArray; \
        procedure :: addValue; \
        procedure :: addArray
#include "Map.h90"
#undef __CLASS_MAPITERATOR__
#undef __MAPLIST__
#undef __TYPE_MAPLIST__
#undef __TYPE_MAPPAIR__
#undef __CLASS_MAPPAIR__
! #undef __CLASS_MAPKEY__
#undef __CLASS_MAPVALUE__
#undef __TYPE_MAPVALUE__
#undef __ADD_METHODS__
#undef Map
	
	!>
	!! @brief Convert to string
	!!
	function str( this, formatted, prefix ) result( output )
		class(StringRealHistogramMap) :: this 
		character(:), allocatable :: output
		logical, optional :: formatted
		character(*), optional :: prefix
		
		logical :: effFormatted
		character(:), allocatable :: effPrefix
		
		class(StringRealHistogramMapIterator), pointer :: iter
		type(StringRealHistogramPair) :: pair
		integer :: fmt
		character(200) :: fstr
		
		effFormatted = .false.
		if( present(formatted) ) effFormatted = formatted
		
		effPrefix = ""
		if( present(prefix) ) effPrefix = prefix
		
		output = ""
		
		if( .not. effFormatted ) then
#define RFMT(v) int(log10(max(abs(v),1.0)))+merge(1,2,v>=0)
#define ITEMR(l,v) output = trim(output)//l; fmt = RFMT(v); write(fstr, "(f<fmt+7>.6)") v; output = trim(output)//trim(fstr)
#define ITEMI(l,v) output = trim(output)//l; write(fstr,*) v; output = trim(output)//trim(adjustl(fstr))
		
			output = trim(output)//"<Map:"
			
			ITEMI( "size=", this.size() )
#undef RFMT
#undef ITEMR
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
! ! 			ITEMI( "min=", this.min )
! ! 			ITEMR( ",size=", this.size )
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
		class(StringRealHistogramMap) :: this
		type(OFStream), optional, intent(in) :: ofile
		
		integer :: unitEff
		integer :: maxLen
		
		class(StringRealHistogramMapIterator), pointer :: iter
		type(StringRealHistogramPair) :: pair
		
		if( present(ofile) ) then
			unitEff = ofile.unit
		else
			unitEff = IO_STDOUT
		end if
		
		maxLen = 0
		iter => this.begin
		do while( associated(iter) )
			pair = this.pair( iter )
			if( len(pair.first.fstr) > maxLen ) maxLen = len(pair.first.fstr)
			
			iter => iter.next
		end do
		
		write(unitEff,"(a)") "#"//trim(str(this))
		
		iter => this.begin
		do while( associated(iter) )
			pair = this.pair( iter )
			write(unitEff,"(A<maxLen>,F15.7)") pair.first.fstr, pair.second.str()
			
			iter => iter.next
		end do
	end subroutine toFStream
	
        !>
        !! @brief 
        !!
        subroutine addArray( this, key, array, rule )
                class(StringRealHistogramMap) :: this
                type(String), intent(in) :: key
                real(8), intent(in) :: array(:)
		integer, optional :: rule
                
		class(StringRealHistogramMapIterator), pointer :: ptr
		type(RealHistogram) :: hist
                
		if( this.find( key, ptr ) ) then
			call ptr.data.second.add( array )
		else
			hist = RealHistogram( rule )
			call hist.add( array )
			call this.insert( key, hist )
		end if
        end subroutine addArray
        
        !>
        !! @brief 
        !!
        subroutine addValue( this, key, value, rule )
                class(StringRealHistogramMap) :: this
                type(String), intent(in) :: key
                real(8), intent(in) :: value
		integer, optional :: rule
                
		class(StringRealHistogramMapIterator), pointer :: ptr
		type(RealHistogram) :: hist
                
		if( this.find( key, ptr ) ) then
			call ptr.data.second.add( value )
		else
			hist = RealHistogram( rule )
			call hist.add( value )
			call this.insert( key, hist )
		end if
        end subroutine addValue
	
        !>
        !! @brief 
        !!
	subroutine showMyMap( mymap )
		type(StringRealHistogramMap) :: mymap
		
		class(StringRealHistogramMapIterator), pointer :: iter
		type(StringRealHistogramPair) :: pair
		
		iter => mymap.begin
		do while( associated(iter) )
			pair = mymap.pair( iter )
			write(*,"(I20,A,A15,A)") pair.first.hashKey(), "  ==>  ", pair.first.fstr//", ", pair.second.str()
			
			iter => iter.next
		end do
		write(*,*)
	end subroutine
	
	!>
	!! @brief Test method
	!!
	subroutine StringRealHistogramMap_test()
		use TestUtils_
		type(String) :: str
		type(RealHistogram) :: hist, histAt
		type(StringRealHistogramPair) :: pair
		type(StringRealHistogramMap) :: mymap
		class(StringRealHistogramMapIterator), pointer :: ptr
		
		mymap = StringRealHistogramMap()
		call assert_equal( mymap%size(), 0, "StringRealHistogramMap_test: size empty" )
		
		str = "Ademas"
		hist = RealHistogram( Histogram_STURGES )
		call hist.add( [24.15162_8, 19.56235_8, 27.82564_8, 23.38200_8, 25.19829_8, 25.26511_8, 23.81071_8, 22.70389_8] )
		call mymap.insert( str, hist )
		call assert_equal( mymap%size(), 1, "StringRealHistogramMap_test: size 1" )
		
		histAt = mymap%at( str )
		call assert_equal( histAt%size(), 8, "StringRealHistogramMap_test: histAt size" )
		
		str = "Amor"
		hist = RealHistogram( Histogram_STURGES )
		call hist.add( [24.15162_8, 19.56235_8, 27.82564_8, 23.38200_8, 25.19829_8, 25.26511_8, 23.81071_8] )
		call mymap.insert( str, hist )
		call assert_equal( mymap%size(), 2, "StringRealHistogramMap_test: size 2" )
		
		str = "Amor"
		hist = RealHistogram( Histogram_STURGES )
		call hist.add( [24.15162_8] )
		call mymap.insert( str, hist )
		call assert_equal( mymap%size(), 2, "StringRealHistogramMap_test: size unchanged after duplicate insert" )
		histAt = mymap%at( str )
		call assert_equal( histAt%size(), 1, "StringRealHistogramMap_test: updated size" )
		
		str = "Entonces"
		hist = RealHistogram( Histogram_STURGES )
		call hist.add( [24.15162_8, 19.56235_8, 27.82564_8, 23.38200_8, 25.19829_8, 25.26511_8] )
		call mymap.insert( str, hist )
		
		str = "Corazon"
		hist = RealHistogram( Histogram_STURGES )
		call hist.add( [24.15162_8, 19.56235_8, 27.82564_8, 23.38200_8, 25.19829_8] )
		call mymap.insert( str, hist )
		
		str = "Hola"
		hist = RealHistogram( Histogram_STURGES )
		call hist.add( [24.15162_8, 19.56235_8, 27.82564_8, 23.38200_8] )
		call mymap.insert( str, hist )
		
		str = "Conejo"
		hist = RealHistogram( Histogram_STURGES )
		call hist.add( [24.15162_8, 19.56235_8, 27.82564_8] )
		call mymap.insert( str, hist )
		
		call assert_equal( mymap%size(), 6, "StringRealHistogramMap_test: size 6" )
		
		str = "Corazon"
		call assert_true( mymap.find( str, ptr ), "StringRealHistogramMap_test: find Corazon" )
		pair = mymap.pair( ptr )
		call assert_equal( pair%first%fstr, "Corazon", "StringRealHistogramMap_test: find key Corazon" )
		call assert_equal( pair%second%size(), 5, "StringRealHistogramMap_test: find value size" )
		
		str = "Corazon "
		call assert_true( .not. mymap.find( str, ptr ), "StringRealHistogramMap_test: find Corazon with trailing space" )
		
		str = "Conejo"
		call mymap.erase( str )
		call assert_equal( mymap%size(), 5, "StringRealHistogramMap_test: size after erase Conejo" )
		
		str = "Entonces"
		call mymap.erase( str )
		
		str = "Amor"
		call mymap.erase( str )
		call assert_equal( mymap%size(), 3, "StringRealHistogramMap_test: size after erase others" )
		
		call mymap.clear()
		call assert_equal( mymap%size(), 0, "StringRealHistogramMap_test: size after clear" )
		
		str = "Ademas"
		hist = RealHistogram( Histogram_STURGES )
		call hist.add( [24.15162_8] )
		call mymap.insert( str, hist )
		
		str = "Amor"
		hist = RealHistogram( Histogram_STURGES )
		call hist.add( [24.15162_8, 19.56235_8, 27.82564_8] )
		call mymap.insert( str, hist )
		
		str = "Amor"
		hist = RealHistogram( Histogram_STURGES )
		call hist.add( [24.15162_8, 19.56235_8] )
		call mymap.set( str, hist )
		histAt = mymap%at( str )
		call assert_equal( histAt%size(), 2, "StringRealHistogramMap_test: size after set Amor" )
		
		str = "Hola"
		call mymap.set( str, hist )
		call assert_equal( mymap%size(), 3, "StringRealHistogramMap_test: size after set non-existent Hola" )
		histAt = mymap%at( str )
		call assert_equal( histAt%size(), 2, "StringRealHistogramMap_test: size of Hola" )
		
		call mymap.clear()
		
		str = "Hola"
		call mymap.set( str, hist )
		histAt = mymap%at( str )
		call histAt.add( [0.456_8, 5.9423_8] )
		call mymap.set( str, histAt )
		histAt = mymap%at( str )
		call assert_equal( histAt%size(), 4, "StringRealHistogramMap_test: size after add elements manually" )
		
		str = "Hola"
		call mymap.add( str, [0.456_8, 5.9423_8] )
		histAt = mymap%at( str )
		call assert_equal( histAt%size(), 6, "StringRealHistogramMap_test: size after map.add array" )
		
		str = "Entonces"
		call mymap.add( str, [0.456_8, 5.9423_8] )
		call assert_equal( mymap%size(), 2, "StringRealHistogramMap_test: map size after map.add array to new key" )
		histAt = mymap%at( str )
		call assert_equal( histAt%size(), 2, "StringRealHistogramMap_test: value size after map.add array to new key" )
		
	end subroutine StringRealHistogramMap_test

end module StringRealHistogramMap_

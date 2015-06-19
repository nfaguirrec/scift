!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!  This file is part of scift (Scientific Fortran Tools).
!!  Copyright (C) by authors (2012-2013)
!!  
!!  Authors (alphabetic order):
!!    * Aguirre N.F. (nfaguirrec@gmail.com)  (2012-2014)
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

module StringRealHistogramMap_
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
			unitEff = STDOUT
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
			call hist.init( rule )
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
			call hist.init( rule )
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
		type(String) :: str
		type(RealHistogram) :: hist
		type(StringRealHistogramPair) :: pair
		type(StringRealHistogramMap) :: mymap
		class(StringRealHistogramMapIterator), pointer :: ptr
		logical :: loc
		
		call mymap.init()
		
		str = "Ademas"
		call hist.init( STURGES )
		call hist.add( [24.15162_8, 19.56235_8, 27.82564_8, 23.38200_8, 25.19829_8, 25.26511_8, 23.81071_8, 22.70389_8] )
		call mymap.insert( str, hist )
		call showMyMap( mymap )
		
		str = "Amor"
		call hist.init( STURGES )
		call hist.add( [24.15162_8, 19.56235_8, 27.82564_8, 23.38200_8, 25.19829_8, 25.26511_8, 23.81071_8] )
		call mymap.insert( str, hist )
		call showMyMap( mymap )	
		
		str = "Amor"
		call hist.init( STURGES )
		call hist.add( [24.15162_8] )
		call mymap.insert( str, hist )
		call showMyMap( mymap )	
		
		str = "Entonces"
		call hist.init( STURGES )
		call hist.add( [24.15162_8, 19.56235_8, 27.82564_8, 23.38200_8, 25.19829_8, 25.26511_8] )
		call mymap.insert( str, hist )
		call showMyMap( mymap )	
		
		str = "Corazon"
		call hist.init( STURGES )
		call hist.add( [24.15162_8, 19.56235_8, 27.82564_8, 23.38200_8, 25.19829_8] )
		call mymap.insert( str, hist )
		call showMyMap( mymap )	
		
		str = "Hola"
		call hist.init( STURGES )
		call hist.add( [24.15162_8, 19.56235_8, 27.82564_8, 23.38200_8] )
		call mymap.insert( str, hist )
		call showMyMap( mymap )	
		
		str = "Conejo"
		call hist.init( STURGES )
		call hist.add( [24.15162_8, 19.56235_8, 27.82564_8] )
		call mymap.insert( str, hist )
		call showMyMap( mymap )
			
		call mymap.show()
		
! 		! Entonces -> Amor -> Amor -> Hola -> Ademas -> Conejo
		
		write(*,*) "Buscando 'Corazon'"
		str = "Corazon"
		if( mymap.find( str, ptr ) ) then
			pair = mymap.pair( ptr )
			write(*,*) "   Encontrado asi: ( ", pair.first.fstr, pair.second.str(), " )"
		else
			write(*,*) "   No encontrado"
		end if
		
		write(*,*) "Buscando 'Corazon '"
		str = "Corazon "
		if( mymap.find( str, ptr ) ) then
			pair = mymap.pair( ptr )
			write(*,*) "   Encontrado asi: ( ", pair.first.fstr, pair.second.str(), " )"
		else
			write(*,*) "   No encontrado"
		end if
		
		write(*,*) "Eliminando a 'Conejo'"
		str = "Conejo"
		call mymap.erase( str )
		call showMyMap( mymap )
		
		write(*,*) "Eliminando a 'Entonces'"
		str = "Entonces"
		call mymap.erase( str )
		call showMyMap( mymap )
		
		write(*,*) "Eliminando a 'Amor'"
		str = "Amor"
		call mymap.erase( str )
		call showMyMap( mymap )
		
		write(*,*) "Limpiando el mapa"
		call mymap.clear()
		call showMyMap( mymap )
		
		write(*,*) "Adicionando nuevas cosas"
		
		str = "Ademas"
		call hist.init( STURGES )
		call hist.add( [24.15162_8] )
		call mymap.insert( str, hist )
		call showMyMap( mymap )
		
		str = "Amor"
		call hist.init( STURGES )
		call hist.add( [24.15162_8, 19.56235_8, 27.82564_8] )
		call mymap.insert( str, hist )
		call showMyMap( mymap )
		
		write(*,*) "Cambiando el elemento Amor a size=2"
		str = "Amor"
		call hist.init( STURGES )
		call hist.add( [24.15162_8, 19.56235_8] )
		call mymap.set( str, hist )
		call showMyMap( mymap )
		
		write(*,*) "Cambiando un elemento que no existe Hola a size=2"
		str = "Hola"
		call mymap.set( str, hist )
		call showMyMap( mymap )
		
		write(*,*) "Limpiando el mapa"
		call mymap.clear()
		call showMyMap( mymap )
		
! 		write(*,*) "Cambiando un elemento que no existe HHHHHHH a 56"
! 		str = "HHHHHHH"
! 		call mymap.set( str, 56.0_8 )
! 		call showMyMap( mymap )
! 		
! 		write(*,*) "Insertando un elemento que ya existe HHHHHHH a 60"
! 		str = "HHHHHHH"
! 		call mymap.insert( str, 60.0_8 )
! 		call showMyMap( mymap )
		
		!------------------------------------------------
		! Test propios de Histgram
		write(*,*) "Cambiando un elemento que no existe Hola a size=2 y adicionando un valor a hist"
		str = "Hola"
		call mymap.set( str, hist )
		call showMyMap( mymap )
		hist = mymap.at( str )
		call hist.add( [0.456_8, 5.9423_8] )
		call mymap.set( str, hist )
		call showMyMap( mymap )
		
		write(*,*) "Cambiando un elemento por referencia de size=4 a size=6"
		
		str = "Hola"
		call mymap.add( str, [0.456_8, 5.9423_8] )
		call showMyMap( mymap )
		
		write(*,*) "Cambiando un elemento que no existe Entonces a size=2"
		str = "Entonces"
		call mymap.add( str, [0.456_8, 5.9423_8] )
		call showMyMap( mymap )
		
	end subroutine StringRealHistogramMap_test

end module StringRealHistogramMap_

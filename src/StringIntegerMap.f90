!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!  This file is part of scift (Scientific Fortran Tools).
!!  Copyright (C) by authors (2012-2013)
!!  
!!  Authors (alphabetic order):
!!    * Aguirre N.F. (nfaguirrec@gmail.com)  (2012-2013)
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

module StringIntegerMap_
	use IOStream_
	use String_
	use StringIntegerPair_
	use StringIntegerPairList_, StringIntegerMapIterator => StringIntegerPairListIterator
	implicit none
	private
	
	public :: StringIntegerMapIterator
	
	public :: &
		StringIntegerMap_test
		
#define Map StringIntegerMap
! #define __CLASS_ITEMMAP__ class(String)
#define __CLASS_MAPITERATOR__  class(StringIntegerMapIterator)
#define __TYPE_MAPLIST__       type(StringIntegerPairList)
#define __TYPE_MAPPAIR__       type(StringIntegerPair)
#define __CLASS_MAPPAIR__      class(StringIntegerPair)
! #define __CLASS_MAPKEY__       class(String)
#define __CLASS_MAPVALUE__     integer
#define __TYPE_MAPVALUE__     integer
#define __ADD_METHODS__
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
	
	!*
	! @brief Convert to string
	!*
	function str( this, formatted, prefix ) result( output )
		class(StringIntegerMap) :: this 
		character(:), allocatable :: output
		logical, optional :: formatted
		character(*), optional :: prefix
		
		logical :: effFormatted
		character(:), allocatable :: effPrefix
		
		class(StringIntegerMapIterator), pointer :: iter
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
		
			ITEMI( "size=", this.size() )
			
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
		class(StringIntegerMap) :: this
		type(OFStream), optional, intent(in) :: ofile
		
		integer :: unitEff
		integer :: maxLen
		
		class(StringIntegerMapIterator), pointer :: iter
		type(StringIntegerPair) :: pair
		
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
			write(unitEff,"(A<maxLen>,I15)") pair.first.fstr, pair.second
			
			iter => iter.next
		end do
	end subroutine toFStream
	
	subroutine showMyMap( mymap )
		type(StringIntegerMap) :: mymap
		
		class(StringIntegerMapIterator), pointer :: iter
		type(StringIntegerPair) :: pair
		
		iter => mymap.begin
		do while( associated(iter) )
			pair = mymap.pair( iter )
			write(*,"(I20,A15,I5)") pair.first.hashKey(), pair.first.fstr, pair.second
			
			iter => iter.next
		end do
		write(*,*)
	end subroutine
	
	!*
	! @brief Test method
	!*
	subroutine StringIntegerMap_test()
		type(String) :: str
		type(StringIntegerPair) :: pair
		type(StringIntegerMap) :: mymap
		class(StringIntegerMapIterator), pointer :: ptr
		
		call mymap.init()
		
		str = "Ademas"
		call mymap.insert( str, 45 )
		call showMyMap( mymap )
		
		str = "Amor"
		call mymap.insert( str, 3 )
		call showMyMap( mymap )	
		
		str = "Entonces"
		call mymap.insert( str, 8 )
		call showMyMap( mymap )	
		
		str = "Corazon"
		call mymap.insert( str, 9 )
		call showMyMap( mymap )	
		
		str = "Hola"
		call mymap.insert( str, 4 )
		call showMyMap( mymap )	
		
		str = "Conejo"
		call mymap.insert( str, 24 )
		call showMyMap( mymap )	
		
		call mymap.show()
		
		! Entonces -> Amor -> Amor -> Hola -> Ademas -> Conejo
		
		write(*,*) "Buscando 'Corazon'"
		str = "Corazon"
		if( mymap.find( str, ptr ) ) then
			pair = mymap.pair( ptr )
			write(*,*) "   Encontrado asi: ( ", pair.first.fstr, pair.second, " )"
		else
			write(*,*) "   No encontrado"
		end if
		
		write(*,*) "Buscando 'Corazon '"
		str = "Corazon "
		if( mymap.find( str, ptr ) ) then
			pair = mymap.pair( ptr )
			write(*,*) "   Encontrado asi: ( ", pair.first.fstr, pair.second, " )"
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
		call mymap.insert( str, 45 )
		call showMyMap( mymap )
		
		str = "Amor"
		call mymap.insert( str, 3 )
		call showMyMap( mymap )
		
		write(*,*) "Cambiando el elemento Amor a 8"
		str = "Amor"
		call mymap.set( str, 8 )
		call showMyMap( mymap )
		
		write(*,*) "Cambiando un elemento que no existe Hola a 56"
		str = "Hola"
		call mymap.set( str, 56 )
		call showMyMap( mymap )
		
		write(*,*) "Limpiando el mapa"
		call mymap.clear()
		call showMyMap( mymap )
		
		write(*,*) "Cambiando un elemento que no existe HHHHHHH a 56"
		str = "HHHHHHH"
		call mymap.set( str, 56 )
		call showMyMap( mymap )
		
		write(*,*) "Insertando un elemento que ya existe HHHHHHH a 60"
		str = "HHHHHHH"
		call mymap.insert( str, 60 )
		call showMyMap( mymap )

		
	end subroutine StringIntegerMap_test

end module StringIntegerMap_

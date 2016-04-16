!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!  This file is part of scift (Scientific Fortran Tools).
!!  Copyright (C) by authors (2011-2013)
!!  
!!  Authors (alphabetic order):
!!    * Aguirre N.F. (nfaguirrec@gmail.com)  (2011-2013)
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

module IOStream_
	use String_
	implicit none
	private
	
	!>
	!! @brief Public parameters
	!!
	integer, public, parameter :: IO_STDIN  = 5
	integer, public, parameter :: IO_STDOUT = 6
	integer, public, parameter :: IO_STDERR = 0
	character(19), parameter :: NULL_FILENAME = "@$@NULL_FILENAME@$@"
	
	public :: &
		IOStream_test
	
	!>
	!! @brief private parameters
	!!
	integer :: lastUnit = 9
	
	!>
	!! @brief Base class
	!!
	type, abstract, public :: FStream
		character(255) :: name = NULL_FILENAME
		character(100) :: extension = ""
		integer :: unit = 0
		character(10) :: status = NULL_FILENAME
		character(10) :: access = NULL_FILENAME
		
		contains
			procedure :: init => FStream_init
			procedure :: open => FStream_init
			procedure :: isOpen
			procedure :: close
			procedure :: eof => FStream_eof
			procedure :: show
	end type FStream
	
	!>
	!! @brief Represents the input files
	!!
	type, public, extends ( FStream ) :: IFStream
		integer :: numberOfLines = 0
		integer :: minNColumns = 0
		integer :: maxNColumns = 0
		integer :: currentLine = 0
		
		contains
			procedure :: scan => IFStream_scan
			procedure :: rewind => IFStream_rewind
			procedure :: readLine => IFStream_readLine
			procedure :: peek => IFStream_peek
			procedure :: showContent => IFStream_showContent
	end type IFStream
	
	!>
	!! @brief Represents the output files
	!!
	type, public, extends ( FStream ) :: OFStream
	end type OFStream
	
	contains
	
	!>
	!! @brief Constructor
	!!
	subroutine FStream_init( this, name, append )
		class(FStream) :: this
		character(*), intent(in), optional :: name
		logical, intent(in), optional :: append
		
		logical :: effAppend
		character(20) :: sunit
		integer :: fmt
		
		character(100), allocatable :: tokens(:)
		
		lastUnit = lastUnit + 1
		select case ( lastUnit )
			case ( IO_STDIN )
				lastUnit = lastUnit + 2
			case ( IO_STDOUT )
				lastUnit = lastUnit + 1
		end select
		
		this.unit = lastUnit
		
		if( present(name) ) then
			this.name = name
			
			call FString_split( name, tokens, "." )
			
			if( size( tokens ) > 1 ) then
				this.extension = trim(tokens(size(tokens)))
			end if
			
			deallocate( tokens )
		else
			fmt = int(log10(1.0*lastUnit))+1
			write( sunit, "(i<fmt>)" ) lastUnit
			this.name = "fort."//trim(sunit)
		end if
		
		effAppend = .false.
		if( present(append) ) effAppend = append
		
		if( effAppend ) then
			this.access = "append"
		else
			this.access = "sequential"
		end if
		
		select type( this )
			type is ( IFStream )
				this.status = "old"
				call this.scan()
			type is ( OFStream )
! 				this.status = "new"
! 				this.status = "replace"
				this.status = "unknown"
		end select
		
		open( unit=this.unit, file=trim(adjustl(this.name)), access=this.access, status=this.status )
	end subroutine FStream_init
	
	!>
	!! @brief Destructor
	!!
	function isOpen( this ) result( output )
		class(FStream), intent(in) :: this
		logical :: output
		
		if( this.unit == 0 ) then
			output = .false.
		else
			output = .true.
		end if
	end function isOpen
	
	!>
	!! @brief Destructor
	!!
	subroutine close( this )
		class(FStream) :: this
		
		close( this.unit )
		
		this.name = NULL_FILENAME
		this.extension = ""
		this.unit = 0
		this.status = NULL_FILENAME
		this.access = NULL_FILENAME
	end subroutine close
	
	!>
	!! @brief Checks the end of file bit
	!!
	function FStream_eof( this ) result( output )
		class(FStream) :: this
		logical output
		
		output = eof( this.unit )
	end function FStream_eof
	
	!>
	!! @brief Print to screen the details of this object
	!!
	subroutine show( this )
		class(FStream), intent(in) :: this
		
		integer :: fmt
		
		select type( this )
			type is ( FStream )
				write(IO_STDOUT, "(a)", advance="no") "<FStream:"
			type is ( IFStream )
				write(IO_STDOUT, "(a)", advance="no") "<IFStream:"
			type is ( OFStream )
				write(IO_STDOUT, "(a)", advance="no") "<OFStream:"
		end select
		
		write(IO_STDOUT, "(a)", advance="no") "name="
		write(IO_STDOUT, "(a)", advance="no") trim(this.name)
		
		write(IO_STDOUT, "(a)", advance="no") ",extension="
		write(IO_STDOUT, "(a)", advance="no") trim(this.extension)
		
		write(IO_STDOUT, "(a)", advance="no") ",unit="
		fmt = int(log10(1.0*this.unit))+1
		write(IO_STDOUT, "(i<fmt>)", advance="no") this.unit
		
		select type( this )
			type is ( IFStream )
				write(IO_STDOUT, "(a)", advance="no") ",numberOfLines="
				fmt = int(log10(1.0*this.numberOfLines+1))+1
				write(IO_STDOUT, "(i<fmt>)", advance="no") this.numberOfLines
				
				write(IO_STDOUT, "(a)", advance="no") ",minNColumns="
				fmt = int(log10(1.0*this.minNColumns+1))+1
				write(IO_STDOUT, "(i<fmt>)", advance="no") this.minNColumns
				
				write(IO_STDOUT, "(a)", advance="no") ",maxNColumns="
				fmt = int(log10(1.0*this.maxNColumns+1))+1
				write(IO_STDOUT, "(i<fmt>)", advance="no") this.maxNColumns
		end select

		write(IO_STDOUT, "(a)", advance="no") ">"
		write(IO_STDOUT, *) ""
	end subroutine show
	
	!>
	!! @brief Extract information from the file i.e. numberOfLines
	!!
	subroutine IFStream_scan( this )
		class(IFStream) :: this
		
		integer :: iostat
		character(1000) :: buffer
		type(String) :: strBuffer
		character(100), allocatable :: tokens(:)
		
		open( unit=this.unit, file=this.name, status=this.status, iostat=iostat )
		
		if( iostat /= 0 ) then
			write(IO_STDOUT, *) "### Error ###: The file ( ", trim(this.name), " ) cannot be open"
		else
			this.numberOfLines = -1
			iostat = 1
			this.maxNColumns = -1
			this.minNColumns = 1000
			do while( iostat /= -1 )
				this.numberOfLines = this.numberOfLines + 1
				read( this.unit, "(a)", iostat=iostat ) buffer
				
				call strBuffer.init( trim(buffer) )
				call strBuffer.split( tokens, " " )
				
				if( this.minNColumns > size(tokens) ) then
					this.minNColumns = size(tokens)
				end if
				
				if( this.maxNColumns < size(tokens) ) then
					this.maxNColumns = size(tokens)
				end if
				
				deallocate( tokens )
			end do
		end if
		
		close( unit=this.unit )
	end subroutine IFStream_scan
	
	!>
	!! @brief
	!!
	subroutine IFStream_rewind( this )
		class(IFStream) :: this
		
		rewind( unit=this.unit )
	end subroutine IFStream_rewind
	
	!>
	!! @brief Extract information from the file i.e. numberOfLines
	!! @todo Puede que haya problemas si se llama a otras funciones despues de readLine ya que no se cierra el fichero
	!! @todo El output no debería ser de tipo allocatable ya que queda en manos del usuario llamar el deallocate
	!!
	function IFStream_readLine( this, cComments, peek ) result( output )
		class(IFStream) :: this
		character(*), optional, intent(in) :: cComments
		logical, optional, intent(in) :: peek
		character(:), allocatable :: output
		
		logical :: effPeek
		character(10000) :: buffer
		integer :: posComment
		
		effPeek = .false.
		if( present(peek) ) effPeek = peek
		
		! @todo No estoy seguro que esto funcione para hacer el peek
		if( effPeek ) then
			read(this.unit,'(A)',advance='NO') buffer
		else
			read(this.unit,'(A)') buffer
		end if
		
		if( present(cComments) ) then
			posComment = index( buffer, cComments )
			
			if( posComment /= 0 ) then
				output = trim(buffer(1:posComment-1))
			else
				output = trim(buffer)
			end if
		else
			output = trim(buffer)
		end if
		
		this.currentLine = this.currentLine + 1
	end function IFStream_readLine
	
	!>
	!! @brief Peek next line
	!!
	function IFStream_peek( this, cComments ) result( output )
		class(IFStream) :: this
		character(*), optional, intent(in) :: cComments
		character(:), allocatable :: output
		
		output = this.readLine( cComments, peek=.true. )
	end function IFStream_peek
	
	!>
	!! @brief Extract information from the file i.e. numberOfLines
	!!
	subroutine IFStream_showContent( this )
		class(IFStream) :: this
		
		integer :: iostat
		character(1000) :: buffer
		
		open( unit=this.unit, file=this.name, status=this.status, iostat=iostat )
		
		if( iostat /= 0 ) then
			write(IO_STDOUT, *) "### Error ###: The file ( ", trim(this.name), " ) cannot be open"
		else
			iostat = 1
			do while( iostat /= -1 )
				read( this.unit, "(A)", iostat=iostat ) buffer
				write(*,*) trim(buffer)
			end do
		end if
		
		close( unit=this.unit )
	end subroutine IFStream_showContent
	
! 	subroutine FStream_write( this, data )
! 		class(FStream) :: this
! 		class(*) :: data
! 		
! 		write( unit=this.unit ) data
! 	end subroutine FStream_write
	
	!>
	!! Test method of this class
	!!
	subroutine IOStream_test()
		implicit none
		
		type(IFStream) :: ifile
		type(OFStream) :: ofile
		
		call ifile.init( "data/formats/XYZ" )
		call ifile.show()
		call ifile.showContent()
		
! 		call ofile.init( "output.dat" )
! 	! 	call ofile.write( "Hola amigos" )
! 		write( ofile.unit, * ) "Hola amigos"
! 		call ofile.show()
	end subroutine IOStream_test
	
end module IOStream_

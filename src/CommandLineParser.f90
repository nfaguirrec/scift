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

!>
!! @brief extract parameters from the commands line
!!
!! This class allows extract parameters from the
!! line command
!!
!! for example:
!!     -i file.dat -q 0.159
!!
module CommandLineParser_
	use String_
	implicit none
	private
	
	public :: &
		CommandLineParser_test
	
	type, private :: Option
		character(20) :: name
		character(255) :: description
		logical :: isOptional
		character :: ttype
		character(20) :: value
	end type Option
	
	type, public :: CommandLineParser
		type(String) :: usage
		
		contains
			procedure :: init
			procedure :: copy
			final :: destroy
			procedure :: str
			procedure :: show
			procedure :: get => getString
			procedure :: getString
			procedure :: getInteger
			procedure :: getReal
			procedure :: getLogical
	end type CommandLineParser
	
	contains
	
	!>
	!! @brief Constructor
	!!
	subroutine init( this )
		class(CommandLineParser) :: this 
		
	end subroutine init
	
	!>
	!! @brief Copy constructor
	!!
	subroutine copy( this, other )
		class(CommandLineParser) :: this
		type(CommandLineParser) :: other
		
		this.usage = other.usage
	end subroutine copy
	
	!>
	!! @brief Destructor
	!!
	subroutine destroy( this )
		type(CommandLineParser) :: this
		
	end subroutine destroy
	
	!>
	!! @brief Convert to string
	!!
	function str( this ) result( output )
		class(CommandLineParser) :: this 
		character(len=200) :: output
		
		integer :: fmt
		character(len=200) :: strBuffer
		
		output = ""
		
		output = trim(output)//"<CommandLineParser:"
		
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
		class(CommandLineParser) :: this
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
	!! @brief Retorna el valor de la opción
	!!
	!! Retorna el valor apropiado de la opción
	!! si este ha sido pasado como opción desde
	!! la consola, retornará su valor; de lo
	!! contrario retornará el valor por omisión
	!!
	function getString( this, param, def ) result( output )
		class(CommandLineParser) :: this 
		character(*), intent(in) :: param
		character(*), optional, intent(in) :: def
		type(String) :: output
		
		character(10000) :: effdef
		character(10000) :: buffer
		integer :: i
		
		if( present(def) ) then
			effdef = def
		else
			effdef = "$$EMPTY**$$..@"
		end if
		
		do i=1,command_argument_count()
			call get_command_argument( i, buffer )
			
			if( buffer(1:1) == '-' .and. trim(buffer) == trim(param) .and. i+1<=command_argument_count() ) then
				call get_command_argument( i+1, buffer )
				output = trim(buffer)
				return
			end if
		end do
		
		if( effdef /= "$$EMPTY**$$..@" ) then
			output = trim(effdef)
		else
			write(*,*) "### ERROR ### CommandLineParser: Parameter "//param//" is required"
			
			if( len_trim(this.usage.fstr) > 0 ) then
				write(*,*) ""
				write(*,"(A)") trim(this.usage.fstr)
			end if
			
			stop
		end if
	end function getString
	
	!>
	!! @brief
	!!
	function getInteger( this, param, def ) result( output )
		class(CommandLineParser) :: this 
		character(*), intent(in) :: param
		integer, optional, intent(in) :: def
		integer :: output
		
		character(100) :: buffer
		
		if( present(def) ) then
			write( buffer, * ) def
			output = String_toInteger( this.getString( param, buffer ) )
		else
			output = String_toInteger( this.getString( param ) )
		end if
	end function getInteger
	
	!>
	!! @brief
	!!
	function getReal( this, param, def ) result( output )
		class(CommandLineParser) :: this 
		character(*), intent(in) :: param
		real(8), optional, intent(in) :: def
		real(8) :: output
		
		character(100) :: buffer
		
		if( present(def) ) then
			write( buffer, * ) def
			output = String_toReal( this.getString( param, buffer ) )
		else
			output = String_toReal( this.getString( param ) )
		end if
	end function getReal
	
	!>
	!! @brief
	!!
	function getLogical( this, param, def ) result( output )
		class(CommandLineParser) :: this 
		character(*), intent(in) :: param
		logical, optional, intent(in) :: def
		logical :: output
		
		character(100) :: buffer
		
		if( present(def) ) then
			write( buffer, * ) def
			output = String_toLogical( this.getString( param, buffer ) )
		else
			output = String_toLogical( this.getString( param ) )
		end if
	end function getLogical
	
	!>
	!! @brief Test method
	!!
	subroutine CommandLineParser_test()
		type(CommandLineParser) :: parser
		type(String) :: buffer
		real(8) :: realBuffer
		integer :: intBuffer
		
		buffer = parser.get( "-d" )
		write(*,*) "Parameter -d"
		write(*,*) "------------"
		write(*,*) trim(buffer.fstr)
		write(*,*) buffer.toReal()
		write(*,*) buffer.toInteger()
		
		realBuffer = parser.getReal( "-f", def=-3.0_8 )
		write(*,*) "Parameter -f"
		write(*,*) "------------"
		write(*,*) realBuffer
		
		intBuffer = parser.getInteger( "-g" )
		write(*,*) "Parameter -g"
		write(*,*) "------------"
		write(*,*) intBuffer
	end subroutine CommandLineParser_test
	
end module CommandLineParser_

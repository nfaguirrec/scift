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
		
		this%usage = other%usage
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
! 		fmt = int(log10(this%min+1.0))+1
! 		write(strBuffer, "(f<fmt+7>.6)") this%min
! 		output = trim(output)//trim(strBuffer)
! 		
! 		output = trim(output)//",size="
! 		fmt = int(log10(float(this%size+1)))+1
! 		write(strBuffer, "(i<fmt>)") this%size
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
		
		write(effunit,"(a)") trim(this%str())
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
		
		character(100000) :: effdef
		character(100000) :: buffer
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
			
			if( len_trim(this%usage%fstr) > 0 ) then
				write(*,*) ""
				write(*,"(A)") trim(this%usage%fstr)
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
			output = String_toInteger( this%getString( param, buffer ) )
		else
			output = String_toInteger( this%getString( param ) )
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
			output = String_toReal( this%getString( param, buffer ) )
		else
			output = String_toReal( this%getString( param ) )
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
			output = String_toLogical( this%getString( param, buffer ) )
		else
			output = String_toLogical( this%getString( param ) )
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
		
		buffer = parser%get( "-d" )
		write(*,*) "Parameter -d"
		write(*,*) "------------"
		write(*,*) trim(buffer%fstr)
		write(*,*) buffer%toReal()
		write(*,*) buffer%toInteger()
		
		realBuffer = parser%getReal( "-f", def=-3.0_8 )
		write(*,*) "Parameter -f"
		write(*,*) "------------"
		write(*,*) realBuffer
		
		intBuffer = parser%getInteger( "-g" )
		write(*,*) "Parameter -g"
		write(*,*) "------------"
		write(*,*) intBuffer
	end subroutine CommandLineParser_test
	
end module CommandLineParser_

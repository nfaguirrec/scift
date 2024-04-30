!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                                                                                   !!
!!  This file is part of SciFT project                                               !!
!!  Copyright (c) 2011-2013 Nestor F. Aguirre (nfaguirrec@gmail.com)                 !!
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
!! @brief
!!
module Timer_
	use IFPORT
	implicit none
	private
	
	public :: &
		Timer_test
	
	type, public :: Timer
		character(:), allocatable :: name
		real(8) :: startTime
		real(8) :: elapsetTime
		character(255) :: sDate
		
		contains
			generic :: init => initDefault
			generic :: assignment(=) => copyTimer
			
			procedure :: initDefault
			procedure :: copyTimer
			procedure :: str
			procedure :: show
			procedure :: start
			procedure :: restart => start
			procedure :: elapsed
			procedure :: elapsedSeconds
			procedure :: elapsedMinutes
			procedure :: elapsedHours
			procedure :: startDate
			procedure :: currentDate
	end type Timer
	
	contains
	
	!>
	!! @brief Constructor
	!!
	subroutine initDefault( this, name )
		class(Timer) :: this
		character(*), optional, intent(in) :: name
		
		this.name = ""
		if( present(name) ) this.name = name
		
		call this.start()
	end subroutine initDefault
	
	!>
	!! @brief Copy Constructor
	!!
	subroutine copyTimer( this, other )
		class(Timer), intent(out) :: this
		type(Timer), intent(in) :: other
		
		this.name = other.name
		this.startTime = other.startTime
		this.sDate = other.sDate
		this.elapsetTime = other.elapsetTime
	end subroutine copyTimer
	
	!>
	!! @brief Destructor
	!!
	subroutine destroyTimer( this )
		type(Timer) :: this
		
	end subroutine destroyTimer
	
	!>
	!! @brief Convert to string
	!!
	function str( this, formatted, prefix ) result( output )
		class(Timer) :: this 
		character(:), allocatable :: output
		logical, optional :: formatted
		character(*), optional :: prefix
		
		logical :: effFormatted
		character(:), allocatable :: effPrefix
		
		integer :: fmt
		character(200) :: fstr
		
		effFormatted = .false.
		if( present(formatted) ) effFormatted = formatted
		
		effPrefix = ""
		if( present(prefix) ) effPrefix = prefix
		
		output = ""
		
		if( .not. effFormatted ) then
#define RFMT(v) int(log10(max(abs(v),1.0)))+merge(1,2,v>=0)
#define ITEMS(l,v) output = trim(output)//effPrefix//trim(l)//trim(adjustl(v))
#define ITEMI(l,v) output = trim(output)//l; fmt = RFMT(v); write(fstr, "(i<fmt>)") v; output = trim(output)//trim(fstr)
#define ITEMR(l,v) output = trim(output)//l; fmt = RFMT(v); write(fstr, "(f<fmt+7>.6)") v; output = trim(output)//trim(fstr)
		
			output = trim(output)//"<Timer:"
! 			ITEMI( "min=", this.min )
! 			ITEMR( ",size=", this.size )
#undef RFMT
#undef ITEMS
#undef ITEMI
#undef ITEMR
			output = trim(output)//">"
		else
#define LINE(l) output = trim(output)//effPrefix//l//new_line('')
#define ITEMS(l,v) output = trim(output)//effPrefix//l; write(fstr, "(x,a)") trim(v); output = trim(output)//trim(fstr)//new_line('')
#define ITEMI(l,v) output = trim(output)//effPrefix//l; write(fstr, "(i10)") v; output = trim(output)//trim(fstr)//new_line('')
#define ITEMR(l,v) output = trim(output)//effPrefix//l; write(fstr, "(f10.5)") v; output = trim(output)//trim(fstr)//new_line('')

			LINE("Timer")
			LINE("---------")
! 			ITEMI( "min=", this.min )
! 			ITEMR( ",size=", this.size )
			LINE("")
#undef LINE
#undef ITEMS
#undef ITEMI
#undef ITEMR
		end if
	end function str
	
	!>
	!! @brief Show
	!!
	subroutine show( this, unit, formatted )
		class(Timer) :: this
		integer, optional, intent(in) :: unit
		logical, optional :: formatted
		
		integer :: effunit
		logical :: effFormatted
		
		effFormatted = .false.
		if( present(formatted) ) effFormatted = formatted
		
		effunit = 6
		if( present(unit) ) effunit = unit
		
		write(effunit,"(a)") trim(str(this,effFormatted))
	end subroutine show
	
	!>
	!! @brief
	!!
	subroutine start( this )
		class(Timer), intent(inout) :: this
		
		this.startTime = DCLOCK()
		call FDATE(this.sDate)
		this.elapsetTime = 0.0_8
	end subroutine start
	
	!>
	!! @brief
	!!
	function elapsed( this ) result( output )
		class(Timer), intent(in) :: this
		integer :: output(3)
		
		real(8) :: cTime
		
		cTime = DCLOCK()
		
		output(1) = int( ( cTime - this.startTime )/3600.0_8 )
		output(2) = mod( int( ( cTime - this.startTime )/60.0_8 ), 60 )
		output(3) = mod( int( cTime - this.startTime ), 60 )
	end function elapsed
	
	!>
	!! @brief
	!!
	function elapsedSeconds( this ) result( output )
		class(Timer), intent(in) :: this
		real(8) :: output
		
		output = DCLOCK() - this.startTime
	end function elapsedSeconds
	
	!>
	!! @brief
	!!
	function elapsedMinutes( this ) result( output )
		class(Timer), intent(in) :: this
		real(8) :: output
		
		output = ( DCLOCK() - this.startTime )/60.0_8
	end function elapsedMinutes
	
	!>
	!! @brief
	!!
	function elapsedHours( this ) result( output )
		class(Timer), intent(in) :: this
		real(8) :: output
		
		output = ( DCLOCK() - this.startTime )/3600.0_8
	end function elapsedHours
	
	!>
	!! @brief
	!!
	function startDate( this ) result( output )
		class(Timer), intent(in) :: this
		character(:), allocatable :: output
		
		output = this.sDate
	end function startDate
	
	!>
	!! @brief
	!!
	function currentDate( this ) result( output )
		class(Timer), intent(in) :: this
		character(255) :: output
		
		call FDATE(output)
	end function currentDate
	
	!>
	!! @brief
	!!
	subroutine Timer_test()
		
	end subroutine Timer_test
	
end module Timer_

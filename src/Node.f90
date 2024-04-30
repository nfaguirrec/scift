!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                                                                                   !!
!!  This file is part of SciFT project                                               !!
!!  Copyright (c) 2012-2016 Nestor F. Aguirre (nfaguirrec@gmail.com)                 !!
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

module Node_
	use String_
	implicit none
	private
	
	type, public :: Node
		integer :: id
		character(100) :: label
		real(8) :: weight
		
		contains
			generic :: init => initDefault
			generic :: assignment(=) => copy
			generic :: operator(==) => equal
			generic :: operator(/=) => nequal
			
			procedure :: initDefault
			procedure :: copy
			procedure :: equal
			procedure :: nequal
	end type Node
	
	contains
	
	!>
	!! @brief Constructor
	!!
	subroutine initDefault( this, id, label, weight )
		class(Node) :: this
		integer, optional :: id
		character(*), optional :: label
		real(8), optional :: weight
		
		integer :: effId
		character(100) :: effLabel
		real(8) :: effWeight
		
		effId = 0
		if( present(id) ) effId = id
		
		effLabel = trim(FString_fromInteger(id))
		if( present(label) ) effLabel = label
		
		effWeight = 0.0_8
		if( present(weight) ) effWeight = weight
		
		this.id = effId
		this.label = effLabel
		this.weight = effWeight
	end subroutine initDefault
	
	!>
	!! @brief Copy constructor
	!!
	subroutine copy( this, other )
		class(Node), intent(inout) :: this
		type(Node), intent(in) :: other
		
		this.id = other.id
		this.label = other.label
		this.weight = other.weight
	end subroutine copy

	!>
	!! @brief 
	!!
	function equal( this, other ) result( output )
		class(Node), intent(in) :: this
		class(Node), intent(in) :: other
		logical :: output
		
		write(*,*) "### ERROR ### Node.equal() is not implmented yet"
		stop
! 		output = ( this.sNode == other.sNode .and. this.tNode == other.tNode )
	end function equal
	
	!>
	!! @brief 
	!!
	function nequal( this, other ) result( output )
		class(Node), intent(in) :: this
		class(Node), intent(in) :: other
		logical :: output
		
		output = .not. ( this == other )
	end function nequal
end module Node_

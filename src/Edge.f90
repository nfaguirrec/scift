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

module Edge_
	use String_
	implicit none
	private
	
	type, public :: Edge
		integer :: sNode
		integer :: tNode
		integer :: id
		character(100) :: label
		real(8) :: weight
		
		logical :: directed
		
		contains
			generic :: init => initDefault
			generic :: assignment(=) => copyEdge
			generic :: operator(==) => equal
			generic :: operator(/=) => nequal
			
			procedure :: initDefault
			procedure :: copyEdge
			procedure :: equal
			procedure :: nequal
	end type Edge
	
	contains
	
	!>
	!! @brief Constructor
	!!
	subroutine initDefault( this, sNode, tNode, id, label, weight, directed )
		class(Edge) :: this
		integer :: sNode
		integer :: tNode
		integer, optional :: id
		character(*), optional :: label
		real(8), optional :: weight
		logical, optional :: directed
		
		integer :: effId
		character(100) :: effLabel
		real(8) :: effWeight
		logical :: effDirected
		
		effId = 0
		if( present(id) ) effId = id
		
		effweight = 1.0_8
		if( present(weight) ) effweight = weight
		
		effDirected = .false.
                if( present(directed) ) effDirected = directed
                
                if( effDirected ) then
			effLabel = trim(FString_fromInteger(sNode))//"-->"//trim(FString_fromInteger(tNode))
		else
			if( sNode > tNode ) then
				effLabel = trim(FString_fromInteger(sNode))//"--"//trim(FString_fromInteger(tNode))
			else
				effLabel = trim(FString_fromInteger(tNode))//"--"//trim(FString_fromInteger(sNode))
			end if
		end if
                if( present(label) ) effLabel = label
		
		this.sNode = sNode
		this.tNode = tNode
		this.id = effId
		this.label = effLabel
		this.weight = effweight
	end subroutine initDefault
	
	!>
	!! @brief Copy constructor
	!!
	subroutine copyEdge( this, other )
		class(Edge), intent(inout) :: this
		type(Edge), intent(in) :: other
		
		this.sNode = other.sNode
		this.tNode = other.tNode
		this.id = other.id
		this.label = other.label
		this.weight = other.weight
		this.directed = other.directed
	end subroutine copyEdge

	!>
	!! @brief 
	!!
	function equal( this, other ) result( output )
		class(Edge), intent(in) :: this
		class(Edge), intent(in) :: other
		logical :: output
		
		output = ( this.sNode == other.sNode .and. this.tNode == other.tNode )
	end function equal
	
	!>
	!! @brief 
	!!
	function nequal( this, other ) result( output )
		class(Edge), intent(in) :: this
		class(Edge), intent(in) :: other
		logical :: output
		
		output = .not. ( this == other )
	end function nequal
	
end module Edge_

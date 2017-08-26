!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!  This file is part of scift (Scientific Fortran Tools).
!!  Copyright (C) by authors (2012-2016)
!!  
!!  Authors (alphabetic order):
!!    * Aguirre N.F. (nfaguirrec@gmail.com)  (2016-2016)
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
			generic :: assignment(=) => copy
			generic :: operator(==) => equal
			generic :: operator(/=) => nequal
			
			procedure :: initDefault
			procedure :: copy
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
	subroutine copy( this, other )
		class(Edge), intent(inout) :: this
		class(Edge), intent(in) :: other
		
		this.sNode = other.sNode
		this.tNode = other.tNode
		this.id = other.id
		this.label = other.label
		this.weight = other.weight
		this.directed = other.directed
	end subroutine copy

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

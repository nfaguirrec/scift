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
	implicit none
	private
	
	type, public :: Edge
		integer :: sNode
		integer :: tNode
		integer :: id
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
	end type Edge
	
	contains
	
	!>
	!! @brief Constructor
	!!
	subroutine initDefault( this, sNode, tNode, id, weight )
		class(Edge) :: this
		integer :: sNode
		integer :: tNode
		integer, optional :: id
		real(8), optional :: weight
		
		integer :: effId
		real(8) :: effWeight
		
		effweight = 1.0_8
		if( present(weight) ) effweight = weight
		
		effId = 0
		if( present(id) ) effId = id
		
		this.sNode = sNode
		this.tNode = tNode
		this.weight = effweight
		this.id = effId
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
		this.weight = other.weight
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

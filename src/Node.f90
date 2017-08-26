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
		class(Node), intent(in) :: other
		
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

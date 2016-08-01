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
	implicit none
	private
	
	type, public :: Node
		integer :: id
		real(8) :: weight
		
		contains
			generic :: init => initDefault
			generic :: assignment(=) => copy
			generic :: operator(==) => eq
			
			procedure :: initDefault
			procedure :: copy
			procedure :: eq
	end type Node
	
	contains
	
	!>
	!! @brief Constructor
	!!
	subroutine initDefault( this, id, weight )
		class(Node) :: this
		integer, optional :: id
		real(8), optional :: weight
		
		integer :: effId
		real(8) :: effWeight
		
		effweight = 1.0_8
		if( present(weight) ) effweight = weight
		
		effId = 0
		if( present(id) ) effId = id
		
		this.weight = effweight
		this.id = effId
	end subroutine initDefault
	
	!>
	!! @brief Copy constructor
	!!
	subroutine copy( this, other )
		class(Node), intent(inout) :: this
		class(Node), intent(in) :: other
		
		this.id = other.id
		this.weight = other.weight
	end subroutine copy

	!>
	!! @brief 
	!!
	function eq( this, other ) result( output )
		class(Node), intent(in) :: this
		class(Node), intent(in) :: other
		logical :: output
		
! 		output = ( this.sNode == other.sNode .and. this.tNode == other.tNode )
	end function eq

end module Node_

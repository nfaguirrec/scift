!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!  This file is part of scift (Scientific Fortran Tools).
!!  Copyright (C) by authors (2012-2014)
!!  
!!  Authors (alphabetic order):
!!    * Aguirre N.F. (nfaguirrec@gmail.com)  (2012-2014)
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

module IVector_
	implicit none
	private
	
	public :: &
		IVector_test
	
#define NVector IVector
#define __TYPE_VALUE__ integer
#define __ID_TYPE__ 1
#include "NVector.h90"
#undef __ID_TYPE__
#undef __TYPE_VALUE__
#undef NVector
	
	!>
	!! @brief Test method
	!!
	subroutine IVector_test()
		type(IVector) :: A, B, C
		
		call A.init( 12, 1 )
		call A.show( 6, .true. )
		
		call B.random( 12, type=COLUMN_VECTOR )
		call B.show( 6, .true. )
		
		A = A*2
		call A.show( 6, .true. )
		
		call B.random( 12, type=ROW_VECTOR )
		call B.show( 6, .true. )
	end subroutine IVector_test

end module IVector_
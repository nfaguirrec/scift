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

module RVector_
	implicit none
	private
	
	public :: &
		RVector_test
	
#define NVector RVector
#define __TYPE_VALUE__ real(8)
#define __ID_TYPE__ 2
#include "NVector.h90"
#undef __ID_TYPE__
#undef __TYPE_VALUE__
#undef NVector
	
	!>
	!! @brief Test method
	!!
	subroutine RVector_test()
		type(RVector) :: A, B, C
		
		call A.init( 12, 1.0_8 )
		call A.show( 6, .true. )
		
		call B.random( 12, type=COLUMN_VECTOR )
		call B.show( 6, .true. )
		
		A = A*2.0_8
		call A.show( 6, .true. )
		
		call B.random( 12, type=ROW_VECTOR )
		call B.show( 6, .true. )
	end subroutine RVector_test

end module RVector_
!*
! @brief
!*
module IterativeAlgorithm_
	implicit none
	private
	
	type, abstract, public :: IterativeAlgorithm
		integer :: step
		integer :: maxIter
		
		contains
			procedure :: iterate
	end type IterativeAlgorithm
	
! http://www.extremeoptimization.com/Documentation/Reference/Extreme.Mathematics.Algorithms.IterativeAlgorithm_Members.aspx
	
	contains
	
	!*
	! @brief Show 
	!*
	subroutine iterate( this )
		implicit none
		class(IterativeAlgorithm) :: this
		
	end subroutine iterate
	
end module IterativeAlgorithm_

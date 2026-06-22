program test_Math
    use Math_
    use TestUtils_
    use ieee_arithmetic   ! http://fortranwiki%org/fortran/show/ieee_arithmetic
    implicit none
		real(8), allocatable :: rArray(:)
		integer, allocatable :: iArray(:)
		integer, allocatable :: indexes(:)
		integer, allocatable :: iComb(:), iCombOld(:,:)
		character(10), allocatable :: strArray(:), sComb(:)
		integer, allocatable :: MyiArray(:)
		real(8) :: x
		
		integer :: i, j, k, l
		integer :: nFrag
		integer :: ssum
		real(8) :: zero
		real(8) :: negativeOne
		
		call assert_equal_real( Gamma(0.5_8), 1.772453850905516_8, 1e-10_8, "Math_test: Gamma(0.5)" )
		call assert_equal_real( log_Gamma(0.5_8), 0.572364942924700_8, 1e-10_8, "Math_test: log_Gamma(0.5)" )
		call assert_equal_real( Gamma(8.0_8), 5040.0_8, 1e-10_8, "Math_test: Gamma(8.0)" )
		call assert_equal( Math_fact(7), 5040, "Math_test: Math_fact(7)" )
		call assert_equal( int(Math_comb(7,2)), 21, "Math_test: Math_comb(7,2)" )
		call assert_equal( int(Math_multisetNumber(7,2)), 28, "Math_test: Math_multisetNumber(7,2)" )
		
		allocate( strArray(3) )
		strArray = ["a", "b", "c"]
		
		call Math_multisets( 3, 3, iCombOld, myStrConstrain )
		call assert_equal( size(iCombOld, dim=1), 6, "Math_test: size of str multisets" )
		deallocate( strArray )
		deallocate( iCombOld )
		
		allocate( MyiArray(3) )
		MyiArray = [1, 3, 5]
		
		call assert_equal( int(Math_multisetNumber(3,3)), 10, "Math_test: Math_multisetNumber(3,3)" )
		call Math_multisets( 3, 3, iCombOld, myIntConstrain )
		call assert_equal( size(iCombOld, dim=1), 4, "Math_test: size of int multisets" )
		
		deallocate( MyiArray )
		deallocate( iCombOld )
		
		allocate( strArray(8) )
		strArray = [character(4) :: "H", "C", "CH", "C2", "H2", "C2H", "CH2", "C2H2"]
		
		allocate( MyiArray(8) )
		MyiArray = [1, 6, 7, 12, 2, 13, 8, 14]
		
		k=1
		do nFrag=1,size(strArray)
			call Math_multisets( size(strArray), nFrag, iCombOld, myMassConstrain )
			
			do i=1,size(iCombOld,dim=1)
				ssum = 0
				do j=1,nFrag
					ssum = ssum + MyiArray( iCombOld(i,j) )
				end do
				call assert_true( ssum <= 14, "Math_test: mass constraint <= 14" )
				k=k+1
			end do
		end do
		
		deallocate( strArray )
		deallocate( MyiArray )
		deallocate( iCombOld )
		
		zero = 0.0_8
		negativeOne = -1.0_8
		call assert_true( .not. Math_isNaN( 2.0_8 ), "Math_test: isNaN(2.0)" )
		call assert_true( Math_isNaN( sqrt(negativeOne) ), "Math_test: isNaN(sqrt(-1))" )
		call assert_true( .not. Math_isInf( 1.0d56 ), "Math_test: isInf(1.0d56)" )
		call assert_true( Math_isInf( 1.0_8/zero ), "Math_test: isInf(1/0)" )
		call assert_true( Math_isInf( 1.0_8/zero + 10.0_8 ), "Math_test: isInf(1/0+10)" )
		call assert_true( Math_isInf( 1.0_8/zero - 10.0_8 ), "Math_test: isInf(1/0-10)" )
		

    contains

    	
    	!>
    	!! This is only necessary for Math_test method
    	!!
    	function myStrConstrain( multisetPositions, current ) result( output )
    		integer, allocatable, intent(in) :: multisetPositions(:)
    		integer, intent(in) :: current
    		logical :: output
    		
    		integer :: i, j
    		
    		if( current == size(multisetPositions) ) then
    			!----------------------------------------------
    			! ¿ El multiset encontrado es correcto ?
    			!----------------------------------------------
    			output = .false.
    			
    			do i=1,size(multisetPositions)
    ! 				do j=1,size(multisetPositions)
    ! 					if( multisetPositions(i) == 1 .and. multisetPositions(j) == 3 ) then ! Que la cadena tenga una "a" y una "c"
    ! 						output = .true.
    ! 						return
    ! 					end if
    
    					if( multisetPositions(i) == 1 ) then ! Que la cadena tenga una "a" en cualquier posición
    						output = .true.
    						return
    					end if
    ! 				end do
    			end do
    		else
    			!----------------------------------------------
    			! ¿ El camino recorrido es incorrecto ?
    			!----------------------------------------------
    			output = .false. ! La solución solo se puede saber hasta el final
    		end if
    		
    		return
    	end function myStrConstrain

    	
    	!>
    	!! This is only necessary for RandomUtils_test method
    	!!
    	function myIntConstrainDebug( multisetPositions, current ) result( output )
    		integer, allocatable, intent(in) :: multisetPositions(:)
    		integer, intent(in) :: current
    		logical :: output
    		
    		integer :: i
    		integer :: ssum
    		
    		write(*,"(A)", advance="no") ">>>"
    		
    		ssum = 0
    		do i=1,current
    			ssum = ssum + MyiArray( multisetPositions(i) )
    			
    			write(*,"(I1,A,I1,A)", advance="no") MyiArray( multisetPositions(i) ), "(", multisetPositions(i), ")"
    		end do
    		
    		if( current == size(multisetPositions) ) then
    			!----------------------------------------------
    			! ¿ El multiset encontrado es correcto ?
    			!----------------------------------------------
    			write(*,"(A,I3,I5)", advance="no") "<<<", ssum, current
    			
    			output = .false.
    			if( ssum <= 8 ) then ! Que la suma no exceda 8
    				write(*,"(A)") "   OK"
    				output = .true.
    			else
    				write(*,"(A)") "   Failed"
    			end if
    			
    		else
    			!----------------------------------------------
    			! ¿ El camino recorrido es incorrecto ?
    			!----------------------------------------------
    			write(*,"(A,I3,I5)", advance="no") "<<<", ssum
    			
    			output = .true.
    			if( ssum <= 8 ) then ! Que la suma no exceda 8
    				write(*,"(A)") "   right way"
    				output = .false.
    			else
    				write(*,"(A)") "   bad way"
    			end if
    			
    		end if
    		
    		return
    	end function myIntConstrainDebug

    	
    	!>
    	!! This is only necessary for RandomUtils_test method
    	!!
    	function myIntConstrain( multisetPositions, current ) result( output )
    		integer, allocatable, intent(in) :: multisetPositions(:)
    		integer, intent(in) :: current
    		logical :: output
    		
    		integer :: i
    		integer :: ssum
    		
    		ssum = 0
    		do i=1,current
    			ssum = ssum + MyiArray( multisetPositions(i) )
    		end do
    		
    		if( current == size(multisetPositions) ) then
    			!----------------------------------------------
    			! ¿ El multiset encontrado es correcto ?
    			!----------------------------------------------
    			output = .false.
    			if( ssum <= 8 ) then ! Que la suma no exceda 8
    				output = .true.
    			end if
    		else
    			!----------------------------------------------
    			! ¿ El camino recorrido es incorrecto ?
    			!----------------------------------------------
    			output = .true.
    			if( ssum <= 8 ) then ! Que la suma no exceda 8
    				output = .false.
    			end if
    		end if
    		
    		return
    	end function myIntConstrain

    	
    	!>
    	!! This is only necessary for RandomUtils_test method
    	!!
    	function myMassConstrain( multisetPositions, current ) result( output )
    		integer, allocatable, intent(in) :: multisetPositions(:)
    		integer, intent(in) :: current
    		logical :: output
    		
    		integer :: i
    		integer :: ssum
    		
    		ssum = 0
    		do i=1,current
    			ssum = ssum + MyiArray( multisetPositions(i) )
    		end do
    		
    		if( current == size(multisetPositions) ) then
    			!----------------------------------------------
    			! ¿ El multiset encontrado es correcto ?
    			!----------------------------------------------
    			output = .false.
    			if( ssum <= MyiArray(size(MyiArray)) ) then
    				output = .true.
    			end if
    		else
    			!----------------------------------------------
    			! ¿ El camino recorrido es incorrecto ?
    			!----------------------------------------------
    			output = .true.
    			if( ssum <= MyiArray(size(MyiArray)) ) then
    				output = .false.
    			end if
    		end if
    		
    		return
    	end function myMassConstrain

end program test_Math

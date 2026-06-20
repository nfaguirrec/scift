program test_RandomUtils
    use RandomUtils_
    use TestUtils_
    use Math_
    implicit none
		integer, allocatable :: iArray(:), iComb(:), MyiArray(:)
		character(10), allocatable :: strArray(:), sComb(:)
		integer :: i, rValInt
		real(8) :: rValReal
		logical :: has_a
		
		! 1. Shuffling test
		allocate( iArray(5) )
		iArray = [ 1, 2, 3, 4, 5 ]
		call RandomUtils_randomizeVector( iArray )
		call assert_equal( size(iArray), 5, "RandomUtils_test: randomized vector size" )
		call assert_equal( sum(iArray), 15, "RandomUtils_test: randomized vector sum" )
		call assert_equal( sum(iArray**2), 55, "RandomUtils_test: randomized vector sum of squares" )
		deallocate( iArray )
		
		! 2. Uniform random distribution test
		do i=1,100
			rValReal = RandomUtils_uniform( [1.0_8, 7.5_8] )
			call assert_true( rValReal >= 1.0_8 .and. rValReal <= 7.5_8, "RandomUtils_test: uniform real range" )
			
			rValInt = RandomUtils_uniform( [1, 7] )
			call assert_true( rValInt >= 1 .and. rValInt <= 7, "RandomUtils_test: uniform int range" )
		end do
		
		! 3. Random string multiset test
		allocate( strArray(7) )
		strArray = [ "a", "b", "c", "d", "e", "f", "g" ]
		call RandomUtils_randomMultiset( strArray, 3, sComb, myStrConstrain )
		
		has_a = .false.
		do i=1,3
			if ( trim(sComb(i)) == "a" ) has_a = .true.
		end do
		call assert_true( has_a, "RandomUtils_test: string multiset contains a" )
		deallocate( strArray )
		deallocate( sComb )
		
		! 4. Random integer multiset test
		allocate( MyiArray(3) )
		MyiArray = [ 1, 3, 5 ]
		call RandomUtils_randomMultiset( MyiArray, 3, iComb, myIntConstrain )
		call assert_true( sum(iComb) <= 8, "RandomUtils_test: integer multiset sum" )
		
		deallocate( MyiArray )
		deallocate( iComb )

    contains

    	
    	!>
    	!! This is only necessary for RandomUtils_test method
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

end program test_RandomUtils

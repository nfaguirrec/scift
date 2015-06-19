!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!  This file is part of scift (Scientific Fortran Tools).
!!  Copyright (C) by authors (2011-2014)
!!  
!!  Authors (alphabetic order):
!!    * Aguirre N.F. (nfaguirrec@gmail.com)  (2011-2014)
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

!>
!! @brief
!!
module RandomUtils_
	use Math_
	implicit none
	private
	
	public :: &
		RandomUtils_init, &
		RandomUtils_randomizeVector, &
		RandomUtils_uniform, &
		RandomUtils_randomMultiset, &
		RandomUtils_randomWalkerStep, &
		RandomUtils_test
		
	interface RandomUtils_randomizeVector
		module procedure RandomUtils_iRandomizeVector
		module procedure RandomUtils_rRandomizeVector
	end interface RandomUtils_randomizeVector
	
	interface RandomUtils_uniform
		module procedure RandomUtils_rUniform
		module procedure RandomUtils_iUniform
	end interface RandomUtils_uniform
		
	interface RandomUtils_randomMultiset
		module procedure RandomUtils_iRandomMultiset
		module procedure RandomUtils_sRandomMultiset
	end interface RandomUtils_randomMultiset
	
	interface
		function prototypeMultisetConstraint( multisetPositions, current ) result( output )
			integer, allocatable, intent(in) :: multisetPositions(:)
			integer, intent(in) :: current
			logical :: output
		end function prototypeMultisetConstraint
	end interface
		
	integer, allocatable :: kMultiCombChosen(:)
	integer(8) :: kMultiCombIterator = -1
	logical :: kMultiCombFirstTime = .true.
	
	logical :: randFirstTime = .true. !<- Used in order to stablish the seed in random procedures
	integer, allocatable :: ids(:)
	procedure(prototypeMultisetConstraint), pointer, private :: MSConstraint !<- This is neccesary only for RandomUtils_randomMultiset
	logical :: randomMultisetLocated = .false.
	
	integer, allocatable :: MyiArray(:) !<- This is only neccesary for test method
		
	contains
	
	!>
	!! Initialization of random numbers
	!!
	subroutine RandomUtils_init()
		integer :: i, n, clock
		integer, dimension(:), allocatable :: seed
		real(8) :: rBuffer
		
		if( randFirstTime ) then
			call random_seed(size = n)
			allocate(seed(n))
			call system_clock(count=clock)
			seed = clock + 37 * (/ (i - 1, i = 1, n) /)
			call random_seed(put = seed)
			deallocate(seed)
			
			call random_number( rBuffer ) !<- Desechamos el primer número de la serie, ya que parece que es muy regular respecto al tiempo
			
			randFirstTime = .false.
		end if
	end subroutine RandomUtils_init
	
	!>
	!! Shuffles Vector with Fisher-Yates shuffling
	!!
	subroutine RandomUtils_iRandomizeVector( arr )
		integer, allocatable, intent(inout) :: arr(:)
		
		integer :: nElems
		integer :: i, s
		integer :: temp
		real(8) :: rBuffer
		
		nElems = size(arr)
		
		if( randFirstTime ) then
			call RandomUtils_init()
			randFirstTime = .false.
		end if
		
		if( nElems > 1 ) then
			i = nElems
			do while( i > 1 )
				call random_number( rBuffer )
				s = int(rBuffer*i)+1
				
				temp = arr(s)
				arr(s) = arr(i)
				arr(i) = temp
				
				i = i-1
			end do
		end if
	end subroutine RandomUtils_iRandomizeVector
	
	!>
	!! Shuffles Vector with Fisher-Yates shuffling
	!!
	subroutine RandomUtils_rRandomizeVector( arr )
		real(8), allocatable, intent(inout) :: arr(:)
		
		integer, allocatable :: pos(:)
		integer :: i
		
		allocate( pos(size(arr)) )
		
		pos = (/ ( i, i = 1, size(arr) ) /)
		
		call RandomUtils_iRandomizeVector( pos )
		arr = (/ ( arr(pos(i)), i = 1, size(arr)) /)
		
		deallocate( pos )
	end subroutine RandomUtils_rRandomizeVector
	
	!>
	!! @brief Build a random sample using an uniform distribution
	!! 
	!! @param[out] sample In this array will will store the generated random numbers
	!!                    it should be allocated in advance
	!!
	function RandomUtils_rUniform( range ) result( output )
		real(8), intent(in) :: range(2)
		real(8) :: output
		
		call RandomUtils_init()
		call random_number( output )
		
		output = range(1) + output*abs(range(2)-range(1))
	end function RandomUtils_rUniform
	
	!>
	!! @brief Build a random sample using an uniform distribution
	!! 
	!! @param[out] sample In this array will will store the generated random numbers
	!!                    it should be allocated in advance
	!!
	function RandomUtils_iUniform( range ) result( output )
		integer, intent(in) :: range(2)
		integer :: output
		
		real(8) :: rBuffer
		
		call RandomUtils_init()
		call random_number( rBuffer )
		
		output = int(rBuffer*(range(2)+1-range(1)))+range(1)
	end function RandomUtils_iUniform
	
	!>
	!! @brief Chooses the first one combination with repetitions which
	!!        satisfy the constraint. This implementation uses a
	!!        tree-like algorithm with random browsing.
	!! 
	!! @param[in] nElems Number of elements
	!! @param[in] sGroups size of the groups
	!! @param[out] randomItem random multiset
	!! 
	!! example: if S={0,1,3,2} then the combination of these elements in groups of two elements
	!!          which satisfy the constraint e_i + e_j = 3 are:
	!!     1)   0,3
	!!     2)   2,1
	recursive subroutine randomMultisetBase( nElems, sGroups, randomItem, nChosen, at )
		integer, intent(in) :: nElems
		integer, intent(in) :: sGroups
		integer, allocatable, intent(inout) :: randomItem(:)
		integer, optional, intent(in) :: nChosen
		integer, optional, intent(in) :: at
		
		integer :: effNChosen
		integer :: effAt
		
		integer :: i
		integer :: counter
		logical :: badWay
		
		effNChosen = 1
		if( present(nChosen) ) effNChosen = nChosen
		
		effAt = 1
		if( present(at) ) effAt = at
		
		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		! When this point is reached, the current combination
		! is the kMultiCombChosen vector
		if ( effNChosen == sGroups+1 ) then
			randomItem(:) = kMultiCombChosen(:)
			
			kMultiCombIterator = kMultiCombIterator + 1
			randomMultisetLocated = MSConstraint( kMultiCombChosen, effNChosen-1 )
			
			return
		else if( effNChosen > 1 ) then
			badWay = MSConstraint( kMultiCombChosen, effNChosen-1 )
			
			if( badWay ) return
		end if
			
		if( .not. randomMultisetLocated ) then
			!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
			! Build all possibilities for each element in the item array
			do i=effAt,nElems
				kMultiCombChosen(effNChosen) = ids(i)
				
				call randomMultisetBase( nElems, sGroups, randomItem, effNChosen + 1, i )
				
				if( randomMultisetLocated ) return
			end do
		end if
		
		return
	end subroutine randomMultisetBase
	
	!>
	!! @brief
	!!
	subroutine RandomUtils_sRandomMultiset( elems, sGroups, randomItem, constrainFunction )
		character(10), allocatable, intent(in) :: elems(:) ! @todo Hay que cambiar character(10) por character(:) allocatable o algo así
		integer, intent(in) :: sGroups
		character(10), allocatable, intent(inout) :: randomItem(:)
		procedure(prototypeMultisetConstraint) :: constrainFunction
		
		integer :: i, nElems
		integer, allocatable :: randomItemBase(:)
		
		nElems = size(elems)
		
		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		! It initializes the global parameters
		if( allocated(kMultiCombChosen) ) deallocate( kMultiCombChosen )
		kMultiCombIterator = 1
		
		allocate( kMultiCombChosen(sGroups) )
		kMultiCombChosen = 0
		
		allocate( randomItemBase( sGroups ) )
		randomItemBase = 0
		
		allocate( ids(nElems) )
		
		do i=1,nElems
			ids(i) = i
		end do
		
		call RandomUtils_randomizeVector( ids )
		
		randomMultisetLocated = .false.
		MSConstraint => constrainFunction
		
		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		! It searches the random multiset
		call randomMultisetBase( size(elems), sGroups, randomItemBase )
		
		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		! It establish the original values for the global parameters
		kMultiCombIterator = -1
		deallocate( kMultiCombChosen )
		deallocate( ids )
		MSConstraint => null()
		
		if( .not. randomMultisetLocated ) then
			write(6,"(A)") "### ERROR ### RandomUtils_randomMultiset: It can't be located a multiset with chosen constraint"
			write(6,"(A)") "                                   Don't forget that the constraint don't must be dependent of order of elements"
			stop
		end if
		
		randomMultisetLocated = .false.
		
		if( allocated(randomItem) ) deallocate(randomItem)
		allocate( randomItem(sGroups) )
		
		do i=1,sGroups
			randomItem(i) = elems( randomItemBase(i) )
		end do
		
		deallocate( randomItemBase )
	end subroutine RandomUtils_sRandomMultiset
	
	!>
	!! @brief
	!!
	subroutine RandomUtils_iRandomMultiset( elems, sGroups, randomItem, constrainFunction )
		integer, allocatable, intent(in) :: elems(:)
		integer, intent(in) :: sGroups
		integer, allocatable, intent(inout) :: randomItem(:)
		procedure(prototypeMultisetConstraint) :: constrainFunction
		
		integer :: i, nElems
		integer, allocatable :: randomItemBase(:)
		
		nElems = size(elems)
		
		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		! It initializes the global parameters
		if( allocated(kMultiCombChosen) ) deallocate( kMultiCombChosen )
		kMultiCombIterator = 1
		
		allocate( kMultiCombChosen(sGroups) )
		kMultiCombChosen = 0
		
		allocate( randomItemBase( sGroups ) )
		randomItemBase = 0
		
		allocate( ids(nElems) )
		
		do i=1,nElems
			ids(i) = i
		end do
		
		call RandomUtils_randomizeVector( ids )
		
		randomMultisetLocated = .false.
		MSConstraint => constrainFunction
		
		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		! It searches the random multiset
		call randomMultisetBase( size(elems), sGroups, randomItemBase )
		
		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		! It establish the original values for the global parameters
		kMultiCombIterator = -1
		deallocate( kMultiCombChosen )
		deallocate( ids )
		MSConstraint => null()
		
		if( .not. randomMultisetLocated ) then
			write(6,"(A)") "### ERROR ### RandomUtils_randomMultiset: It can't be located a multiset with chosen constraint"
			write(6,"(A)") "                                   Don't forget that the constraint don't must be dependent of order of elements"
			stop
		end if
		
		randomMultisetLocated = .false.
		
		if( allocated(randomItem) ) deallocate(randomItem)
		allocate( randomItem(sGroups) )
		
		do i=1,sGroups
			randomItem(i) = elems( randomItemBase(i) )
		end do
		
		deallocate( randomItemBase )
	end subroutine RandomUtils_iRandomMultiset
	
	!>
	!! @brief
	!!
	subroutine RandomUtils_randomWalkerStep( beginPos, endPos, stepSize, stepSizeVec )
		real(8), intent(in) :: beginPos(*)
		real(8), intent(in) :: endPos(*)
		real(8), intent(in), optional :: stepSize
		real(8), intent(in), optional :: stepSizeVec(*)
		
! 		real(8), intent(in), optional :: stepSize
! 		real(8), intent(in), optional :: stepSizeVec(*)
		
! 		nElems = size(elems)

	end subroutine RandomUtils_randomWalkerStep
	
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
	
	!>
	!! @brief Test method
	!!
	subroutine RandomUtils_test()
		integer, allocatable :: iArray(:), iComb(:)
		character(10), allocatable :: strArray(:), sComb(:)
		real(8), allocatable :: rArray(:), rArray2(:)
		
		integer, allocatable :: items(:,:)
		
		integer :: i, j
		
		write(*,*) ""
		write(*,*) "Random walkers"
		write(*,*) "=============="
		
		allocate( rArray(9) )
		rArray = [ 0.456, 1.156, 2.126, 3.126, 4.126, 5.126, 6.126, 7.126, 8.126, 9.126 ]
		
		write(*,*) ""
		write(*,*) "Randomize vectors"
		write(*,*) "================="
		
		allocate( iArray(5) )
		
		iArray = [ 1, 2, 3, 4, 5 ]
		write(*,"(A,5I3,A)") "  original vector = [", iArray, " ]"
		call RandomUtils_randomizeVector( iArray )
		write(*,"(A,5I3,A)") "randomized vector = [", iArray, " ]"
		
		deallocate( iArray )
		
		write(*,*) ""
		write(*,*) "Uniform distribution"
		write(*,*) "===================="
		write(*,*) ""
		write(*,*) "RandomUtils_uniform( [1.0_8,7.5_8] )"
		do i=1,10
			write(*,"(I5,F10.5)") i, RandomUtils_uniform( [1.0_8,7.5_8] )
		end do
		write(*,*) ""
		write(*,*) "RandomUtils_uniform( [1,7] )"
		do i=1,10
			write(*,"(I5,I5)") i, RandomUtils_uniform( [1,7] )
		end do
		
		write(*,*) ""
		write(*,*) "Random string multiset"
		write(*,*) "======================"

		allocate( strArray(7) )
		strArray = [ "a", "b", "c", "d", "e", "f", "g" ]
		
		write(*,*) "set = [ 'a', 'b', 'c', 'd', 'e', 'f', 'g' ]"
		write(*,*)
		
		call RandomUtils_randomMultiset( strArray, 3, sComb, myStrConstrain )
		write(*,*) ""
		write(*,*) "The constraint is: The multiset must have the letter a in any position"
		write(*,*) "RandomUtils_randomMultiset( nElems, 3 ) = ", ( trim(sComb(i)), i=1,3 )
		
		!#######################################################
		! To check using a histogram
! 		write(*,*) ""
! 		write(*,"(A)", advance="no") "Generation of 1000000 random multiset with the same constraint ... "
! 		
! 		open( 10, file="strSalida" )
! 		do i=1,1000000
! 			call RandomUtils_randomMultiset( strArray, 3, sComb, myStrConstrain )
! 			write(10,*) ( trim(sComb(j)), j=1,3 )
! 		end do
! 		close( 10 )
! 		
! 		write(*,*) "OK"
! 		write(*,*) ""
! 		write(*,*) "You can check it using the following awk command:"
! #define cc achar(34)//achar(34)
! 		write(*,"(A)") "awk 'BEGIN{nData=0}{n=split($1,arr,"//cc//"); asort(arr); s="//cc//"; for(i=1;i<=n;i++){s=s"//cc//"arr[i]}; if(s in map){ map[s] = map[s]+1 }else{ map[s]=1 }; nData+=1}END{ for(k in map) print k, map[k]/nData }' strSalida"
! #undef cc
		!#######################################################
		
		deallocate( strArray )
		deallocate( sComb )
		
		write(*,*) ""
		write(*,*) "Random integer multiset"
		write(*,*) "======================="

		allocate( MyiArray(3) )
		MyiArray = [ 1, 3, 5 ]
		
		write(*,*) "set = [ 1, 3, 5 ]"
		write(*,*)
		
		write(*,*) ""
		write(*,*) "The constraint is: The sum of all elements cannot be greater than 8"
! 		call RandomUtils_RandomMultiset( MyiArray, 3, iComb, myIntConstrainDebug, sorted=.false. )
		call RandomUtils_RandomMultiset( MyiArray, 3, iComb, myIntConstrainDebug )
		write(*,"(A,<size(iComb)>I)") "RandomUtils_randomMultiset( nElems, 3, sorted=.false. ) = ", ( iComb(i), i=1,size(iComb) )
! 		call RandomUtils_RandomMultiset( MyiArray, 3, iComb, myIntConstrainDebug, sorted=.true. )
		call RandomUtils_RandomMultiset( MyiArray, 3, iComb, myIntConstrainDebug )
		write(*,"(A,<size(iComb)>I)") "RandomUtils_randomMultiset( nElems, 3, sorted=.true. ) = ", ( iComb(i), i=1,size(iComb) )
		
		deallocate(MyiArray)
		
		!#######################################################
		! To check using a histogram
		write(*,*) ""
! 		allocate( MyiArray(7) )
! 		MyiArray = [ 1, 3, 5, 7, 2, 4, 6 ]
! 		write(*,*) "set = [ 1, 3, 5, 7, 2, 4, 6 ]"
		allocate( MyiArray(3) )
		MyiArray = [ 1, 3, 5 ]
		write(*,*) "set = [ 1, 3, 5 ]"
		
		write(*,*) "The constraint is: The sum of all elements cannot be greater than 8"
		write(*,"(A)", advance="no") "Generation of 2000000 random unsorted multiset"
		
		open( 10, file="intSalida" )
		do i=1,2000000
! 			call RandomUtils_randomMultiset( MyiArray, 3, iComb, myIntConstrain, sorted=.false. )
			call RandomUtils_randomMultiset( MyiArray, 3, iComb, myIntConstrain )
			write(10,"(3I1)") ( iComb(j), j=1,3 )
! 			call RandomUtils_randomMultiset( MyiArray, 2, iComb, myIntConstrain )
! 			write(10,"(3I1)") ( iComb(j), j=1,2 )
		end do
		close( 10 )
		
		write(*,*) "OK"
		write(*,*) ""
		write(*,*) "You can check it using the following awk command:"
#define cc achar(34)//achar(34)
		write(*,"(A)") "awk 'BEGIN{nData=0}{n=split($1,arr,"//cc//"); asort(arr); s="//cc//"; for(i=1;i<=n;i++){s=s"//cc//"arr[i]}; if(s in map){ map[s] = map[s]+1 }else{ map[s]=1 }; nData+=1}END{ for(k in map) print k, map[k]/nData }' intSalida"
! awk 'BEGIN{nData=0}{s=$1; if(s in map){ map[s] = map[s]+1 }else{ map[s]=1 }; nData++}END{ for(k in map) print k, map[k]/nData }' intSalida | sort
#undef cc
		
		write(*,*) "The constraint is: The sum of all elements cannot be greater than 8"
		write(*,"(A)", advance="no") "Generation of 2000000 random sorted multiset with the same constraint ... "
		
		open( 10, file="intSalidaSorted" )
		do i=1,2000000
! 			call RandomUtils_randomMultiset( MyiArray, 3, iComb, myIntConstrain, sorted=.true. )
			call RandomUtils_randomMultiset( MyiArray, 3, iComb, myIntConstrain )
			write(10,"(3I1)") ( iComb(j), j=1,3 )
! 			call RandomUtils_randomMultiset( MyiArray, 2, iComb, myIntConstrain, sorted=.true. )
! 			write(10,"(3I1)") ( iComb(j), j=1,2 )
		end do
		close( 10 )
		
		write(*,*) "OK"
		write(*,*) ""
		write(*,*) "You can check it using the following awk command:"
#define cc achar(34)//achar(34)
		write(*,"(A)") "awk 'BEGIN{nData=0}{n=split($1,arr,"//cc//"); asort(arr); s="//cc//"; for(i=1;i<=n;i++){s=s"//cc//"arr[i]}; if(s in map){ map[s] = map[s]+1 }else{ map[s]=1 }; nData+=1}END{ for(k in map) print k, map[k]/nData }' intSalidaSorted"
		write(*,"(A)") "or"
		write(*,"(A)") "awk 'BEGIN{nData=0}{s=$1; if(s in map){ map[s] = map[s]+1 }else{ map[s]=1 }; nData+=1}END{ for(k in map) print k, map[k]/nData }' intSalidaSorted"
! awk 'BEGIN{nData=0}{s=$1; if(s in map){ map[s] = map[s]+1 }else{ map[s]=1 }; nData++}END{ for(k in map) print k, map[k]/nData }' intSalidaSorted | sort
#undef cc
		!#######################################################
		
! 		call Math_multisets( size(MyiArray), 3, items  )
! 		
! 		j=1
! 		do i=1,size(items,dim=1)
! 			if( sum(MyiArray( items(i,:) )) < 8  ) then
! 				write(*,"(I5,A,<size(items,dim=2)>I1)") j, " --> ", MyiArray( items(i,:) )
! 				j = j+1
! 			end if
! 		end do
! 		
! 		deallocate( items )
		
		deallocate(MyiArray)
		deallocate( iComb )

	end subroutine RandomUtils_test
	
end module RandomUtils_

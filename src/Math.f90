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

module Math_
	use ieee_arithmetic   ! http://fortranwiki.org/fortran/show/ieee_arithmetic
	implicit none
	private
	
	real(8), public, parameter :: Math_PI = acos(-1.0_8)
	real(8), public, parameter :: Math_INF = 1.0_8/0.0_8
! 	real(8), public, parameter :: Math_INF = 99999999999999999.0_8
	real(8), public, parameter :: Math_NAN = 0.0_8/0.0_8
	complex(8), public, parameter :: Math_I = cmplx(0.0_8,1.0_8)
	
	integer, public, parameter :: Math_IINF = huge(0)
	
	public :: &
		Math_wigner3j, &
		Math_cart2Spher, &
		Math_spher2Cart, &
		Math_sort, &
		Math_fact, &
		Math_comb, &
		Math_multisetNumber, &
		Math_multisets, &
		Math_isOdd, &
		Math_isEven, &
		Math_fixPrecision, &
		Math_erfinv, &
		Math_ustep, &
		Math_ubox, &
		Math_sinc, &
		Math_nsinc, &
		Math_gaussian, &
		Math_lorentzian, &
		Math_erfTophat, &
		Math_flatTopWindow, &
		Math_average, &
		Math_stdev, &
		Math_skewness, &
		Math_floorDivision, &
		Math_isNaN, &
		Math_isInf, &
		Math_isIInf, &
		Math_dotProduct, &
		Math_crossProduct, &
		Math_pointLineDistance, &
		Math_test
		
	interface Math_sort
		module procedure Math_isort
		module procedure Math_rsort
	end interface Math_sort
	
	interface Math_floorDivision
		module procedure Math_iFloorDivision
		module procedure Math_rFloorDivision
	end interface Math_floorDivision
	
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
	
	integer, allocatable :: ids(:)
	procedure(prototypeMultisetConstraint), pointer, private :: MSConstraint !<- This is neccesary only for Math_multisets
	logical :: isMultisetLocated = .false.
	
	integer, allocatable :: MyiArray(:) !<- This is only neccesary for test method
	
	real(8), private :: erfinv_a3 = -0.140543331_8
	real(8), private :: erfinv_a2 = 0.914624893_8
	real(8), private :: erfinv_a1 = -1.645349621_8
	real(8), private :: erfinv_a0 = 0.886226899_8

	real(8), private :: erfinv_b4 = 0.012229801_8
	real(8), private :: erfinv_b3 = -0.329097515_8
	real(8), private :: erfinv_b2 = 1.442710462_8
	real(8), private :: erfinv_b1 = -2.118377725_8
	real(8), private :: erfinv_b0 = 1.0_8

	real(8), private :: erfinv_c3 = 1.641345311_8
	real(8), private :: erfinv_c2 = 3.429567803_8
	real(8), private :: erfinv_c1 = -1.62490649_8
	real(8), private :: erfinv_c0 = -1.970840454_8

	real(8), private :: erfinv_d2 = 1.637067800_8
	real(8), private :: erfinv_d1 = 3.543889200_8
	real(8), private :: erfinv_d0 = 1.0_8
	
	contains
	
	!>
	!! This function calculates the value of a Wigner-3j symbol.
	!! The equation used is equation 1.5 in "The 3j and 6j symbols" by
	!! Manuel Rotenber, R. Bivins, N.Metropolis  and John K. Wooten, JR
	!! 1959, The Technology Press, Massachusett.
	!!
	!! The function has been tested for all sets, where j1d=j2d=5/2
	!! and j3d={0, 2, 4}, and m1d+m2d+m3d=0
	!! It has been tested to give zero for some vanishing terms
	!! If you should want to use it for higher js than the ones I have tested
	!! you might have to do something to avoid overflow in the expression with
	!! the many factorials
	!!
	function Math_wigner3j( j1, j2, j3, m1, m2, m3 ) result (w3j)
		real(8), intent(in) :: j1,j2,j3,m1,m2,m3
		real(8) :: w3j
		
		integer:: i
		real(8) :: j1d,j2d,j3d,m1d,m2d,m3d
		real(8) :: k,kmin,kmax,ksum,divisor,sign
		real(8), dimension(0:200) ::lnfac
		
		j1d = real(j1,8)
		j2d = real(j2,8)
		j3d = real(j3,8)
		m1d = real(m1,8)
		m2d = real(m2,8)
		m3d = real(m3,8)
		
		lnfac(0)=0.d0
		lnfac(1)=0.d0
		do i=2,200
			lnfac(i)=lnfac(i-1)+log(REAL(i))
		end do
		
		kmax = min(j1d+j2d-j3d, j1d-m1d,j2d+m2d)
		kmin = max(0.d0, j2d-j3d-m1d, j1d-j3d+m2d)
		
		ksum=0.
		do k = kmin, kmax
			divisor=exp(lnfac(nint(k))+lnfac(nint(j1d+j2d-j3d-k))+ &
				lnfac(nint(j1d-m1d-k))+lnfac(nint(j2d+m2d-k))+ &
				lnfac(nint(j3d-j2d+m1d+k))+lnfac(nint(j3d-j1d-m2d+k)))
			ksum=ksum+minusonetothe(int(k))/divisor
		end do !k
		
		w3j= minusonetothe(nint(j1d-j2d-m3d))*sqrt(  &
				exp(lnfac(nint(j1d+j2d-j3d))+lnfac(nint(j1d-j2d+j3d))+&
				lnfac(nint(-j1d+j2d+j3d))+&
				lnfac(nint(j1d+m1d))+lnfac(nint(j1d-m1d))+&
				lnfac(nint(j2d+m2d))+lnfac(nint(j2d-m2d))+&
				lnfac(nint(j3d+m3d))+lnfac(nint(j3d-m3d))-&
				lnfac(nint(j1d+j2d+j3d+1))))*ksum
	end function Math_wigner3j
	
	!>
	!! @brief Converts cartesian coordinates to spherical coordinates
	!! @param   vec = [ x, y, z ]
	!! @return rvec = [ r, theta, phi ]
	!!
	function Math_cart2Spher( vec ) result( rvec )
		real(8), intent(in) :: vec(3)
		real(8) :: rvec(3)
		
		real(8) :: r
		
		r = sqrt(sum(vec**2))
		rvec = [ r, acos(vec(3)/r), atan2(vec(2),vec(1)) ]
	end function Math_cart2Spher
	
	!>
	!! @brief Converts spherical coordinates to cartesian coordinates
	!! @param rvec = [ r, theta, phi ]
	!! @return vec = [ x, y, z ]
	!!
	function Math_spher2Cart( rvec ) result( vec )
		real(8), intent(in) :: rvec(3)
		real(8) :: vec(3)
		
		vec = [ rvec(1)*sin(rvec(2))*cos(rvec(3)), rvec(1)*sin(rvec(2))*sin(rvec(3)), rvec(1)*cos(rvec(2)) ]
	end function Math_spher2Cart
	
	!>
	!! @brief
	!!
	function minusonetothe(j) result (sgn)
		integer :: i,j
		real*8 :: sgn
		
		i=j
		if (j<=0) i=-j
		if (mod(i,2)==0) then
			sgn=1
		else
			sgn=-1
		endif
	end function minusonetothe
	
	!>
	!! indexing array so that array(indexes(j)), j=1..n is in
	!! ascending numerical order.
	!! method is heapsort, see also subroutine hpsort.
	!! taken from numerical recipies, p 233.
	!!
	subroutine Math_isort( array, indexes )
		integer, allocatable, intent(in) :: array(:)
		integer, allocatable, intent(inout) :: indexes(:)
		
		integer :: i, j, l
		integer :: n
		integer :: id, ir
		real(8) :: value
		
		if( .not. allocated(array) ) then
			write(*,*) "Error in Math_sort, array not allocated"
			stop
		end if
		
		if( .not. allocated(indexes) ) then
			write(*,*) "Error in Math_sort, indexes not allocated"
			stop
		end if
		
		if( size(array) /= size(indexes) ) then
			write(*,*) "Error in Math_sort, array and indexes have different size"
			stop
		end if
		
		n = size(array)
		
		do j=1,n
			indexes(j)=j
		end do
		
		if( n == 1 ) return
		
		l=n/2+1
		ir=n
		
		do while( .true. )
			if( l > 1 ) then
				l = l-1
				id = indexes(l)
				value = array(id)
			else
				id=indexes(ir)
				value=array(id)
				indexes(ir)=indexes(1)
				ir=ir-1
				
				if(ir == 1) then
					indexes(1)=id
					return
				end if
			end if
			
			i = l
			j = 2*l
			
			do while( j <= ir )
				if( j < ir ) then
					if( array(indexes(j)) < array(indexes(j+1)) ) then
						j=j+1
					end if
				end if
				
				if( value < array(indexes(j)) ) then
					indexes(i)=indexes(j)
					i=j
					j=2*j
				else
					j=ir+1
				end if
			end do
			
			indexes(i)=id
		end do
	end subroutine Math_isort
	
	!>
	!! indexing array so that array(indexes(j)), j=1..n is in
	!! ascending numerical order.
	!! method is heapsort, see also subroutine hpsort.
	!! taken from numerical recipies, p 233.
	!!
	subroutine Math_rsort( array, indexes )
		real(8), allocatable, intent(in) :: array(:)
		integer, allocatable, intent(inout) :: indexes(:)
		
		integer :: i, j, l
		integer :: n
		integer :: id, ir
		real(8) :: value
		
		if( .not. allocated(array) ) then
			write(*,*) "Error in Math_sort, array not allocated"
			stop
		end if
		
		if( .not. allocated(indexes) ) then
			write(*,*) "Error in Math_sort, indexes not allocated"
			stop
		end if
		
		if( size(array) /= size(indexes) ) then
			write(*,*) "Error in Math_sort, array and indexes have different size"
			stop
		end if
		
		n = size(array)
		
		do j=1,n
			indexes(j)=j
		end do
		
		if( n == 1 ) return
		
		l=n/2+1
		ir=n
		
		do while( .true. )
			if( l > 1 ) then
				l = l-1
				id = indexes(l)
				value = array(id)
			else
				id=indexes(ir)
				value=array(id)
				indexes(ir)=indexes(1)
				ir=ir-1
				
				if(ir == 1) then
					indexes(1)=id
					return
				end if
			end if
			
			i = l
			j = 2*l
			
			do while( j <= ir )
				if( j < ir ) then
					if( array(indexes(j)) < array(indexes(j+1)) ) then
						j=j+1
					end if
				end if
				
				if( value < array(indexes(j)) ) then
					indexes(i)=indexes(j)
					i=j
					j=2*j
				else
					j=ir+1
				end if
			end do
			
			indexes(i)=id
		end do
	end subroutine Math_rsort
	
	!>
	!! @brief Returns the factorial of n
	!! 
	pure function Math_fact( n ) result( output )
		integer, intent(in) :: n
		integer :: output
		
		integer :: i
		
		output = 1
		do i=n,1,-1
			output = output*i
		end do
	end function Math_fact
	
	!>
	!!
	!! taken from http://orion.math.iastate.edu/burkardt/f_src/combo/combo.html
	!! COMB computes the combinatorial coefficient C(N,K).
	!!
	!!    Real arithmetic is used, and C(N,K) is computed directly, via
	!!    Gamma functions, rather than recursively.
	!!
	!!    C(N,K) is the number of distinct combinations of K objects
	!!    chosen from a set of N distinct objects.  A combination is
	!!    like a set, in that order does not matter.
	!!
	!!  Examples:
	!!    The number of combinations of 2 things chosen from 5 is 10.
	!!
	!!    C(5,2) = ( 5 * 4 * 3 * 2 * 1 ) / ( ( 3 * 2 * 1 ) * ( 2 * 1 ) ) = 10.
	!!
	!!    The actual combinations may be represented as:
	!!
	!!      (1,2), (1,3), (1,4), (1,5), (2,3),
	!!      (2,4), (2,5), (3,4), (3,5), (4,5).
	!!
	!!  Formula:
	!!    C(N,K) = N! / ( (N-K)! * K! )
	!!
	!!  Modified:
	!!    16 June 1999
	!!
	!!  Author:
	!!    John Burkardt
	!!
	!!  @param[in]      n integer N, the value of N.
	!!  @param[in]      m integer K, the value of K.
	!!  @output    output real CNK, the value of C(N,K)
	!!
	pure function Math_comb( n, k ) result( output )
		integer, intent(in) :: n, k
		integer(8) :: output
		
		real(8) :: arg
		real(8) :: cnk
		real(8) :: fack
		real(8) :: facn
		real(8) :: facnmk
		
		if ( n < 0 ) then
			cnk = 0.0E+00
		else if ( k == 0 ) then
			cnk = 1.0E+00
		else if ( k == 1 ) then
			cnk = real( n )
		else if ( k > 1 .and. k < n-1 ) then
			arg = real( n + 1 )
			facn = log(gamma( arg ))
			
			arg = real( k + 1 )
			fack = log(gamma( arg ))
			
			arg = real( n - k + 1 )
			facnmk = log(gamma( arg ))
			
			cnk = anint( exp ( facn - fack - facnmk ) )
		else if ( k == n-1 ) then
			cnk = real( n )
		else if ( k == n ) then
			cnk = 1.0E+00
		else
			cnk = 0.0E+00
		end if
		
		output = int(cnk,8)
	end function Math_comb
	
	!>
	!! @brief Number of combinations with repetitions
	!!
	!! The number of ways to sample sGroups elements from a set of nElems elements allowing for duplicates
	!!
	function Math_multisetNumber( nElems, sGroups ) result ( output )
		integer, intent(in) :: nElems
		integer, intent(in) :: sGroups
		integer :: output
		
		output = Math_comb( nElems+sGroups-1, sGroups )
	end function Math_multisetNumber
	
	!>
	!! @brief Builds the combinations with repetitions
	!!
	!! A sGroups-combination with repetitions, or sGroups-multicombination, or multiset of size sGroups
	!! from a set S is given by a sequence of sGroups not necessarily distinct elements of S,
	!! where order is not taken into account.
	!!
	!! @param[in] nElems Number of elements
	!! @param[in] sGroups size of the groups
	!! @param[out] items final groups
	!!
	!! example: if S={a,b,c} then the combination of these elements in groups of two elements are
	!!     1)   aa
	!!     2)   ab
	!!     3)   ac
	!!     4)   bb
	!!     5)   bc
	!!     6)   cc
	recursive subroutine Math_multisets( nElems, sGroups, items, constrainFunction )
		integer, intent(in) :: nElems
		integer, intent(in) :: sGroups
		integer, allocatable, intent(inout) :: items(:,:)
		procedure(prototypeMultisetConstraint), optional :: constrainFunction
		
		integer, allocatable :: itemsBase(:,:)
		integer :: i
		
		if( allocated(kMultiCombChosen) ) deallocate( kMultiCombChosen )
		
		kMultiCombIterator = 1
		
		allocate( kMultiCombChosen(sGroups) )
		kMultiCombChosen = 0
		
		if( allocated(itemsBase) ) deallocate( itemsBase )
		allocate( itemsBase( Math_multisetNumber( nElems, sGroups ), sGroups ) )
		itemsBase = 0
		MSConstraint => constrainFunction
		
		call multisetsBase( nElems, sGroups, itemsBase )
		
		if( kMultiCombIterator-1 < 1 ) then
			write(6,"(A)") "### ERROR ### Math_multisets: There is not multisets with chosen constraint"
			write(6,"(A)") "                              Don't forget that the constraint don't must be dependent of order of elements"
			stop
		end if
		
		if( allocated(items) ) deallocate(items)
		allocate( items(kMultiCombIterator-1,sGroups) )
		
		do i=1,size(items,dim=1)
			items(i,:) = itemsBase(i,:)
		end do
		
		deallocate(itemsBase)
		MSConstraint => null()
		kMultiCombIterator = -1
		deallocate( kMultiCombChosen )
	end subroutine Math_multisets
	
	!>
	!! @brief Internal function
	!!
	recursive subroutine multisetsBase( nElems, sGroups, items, nChosen, at )
		integer, intent(in) :: nElems
		integer, intent(in) :: sGroups
		integer, allocatable, intent(inout) :: items(:,:)
		integer, optional, intent(in) :: nChosen
		integer, optional, intent(in) :: at
		
		integer :: effNChosen
		integer :: effAt
		
		logical :: isMultisetOK, isBadWay
		integer :: i
		integer :: counter
		
		effNChosen = 1
		if( present(nChosen) ) effNChosen = nChosen
		
		effAt = 1
		if( present(at) ) effAt = at
		
		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		! When this point is reached, the current combination
		! is the kMultiCombChosen vector
		if ( effNChosen == sGroups+1 ) then
			if( associated(MSConstraint) ) then
				isMultisetOK = MSConstraint( kMultiCombChosen, effNChosen-1 )
			else
				isMultisetOK = .true.
			end if
			
			if( isMultisetOK ) then
				items( kMultiCombIterator, : ) = kMultiCombChosen(:)
				kMultiCombIterator = kMultiCombIterator + 1
			end if
			
			return
		else if( effNChosen > 1 ) then
			if( associated(MSConstraint) ) then
				isBadWay = MSConstraint( kMultiCombChosen, effNChosen-1 )
			else
				isBadWay = .false.
			end if
			
			if( isBadWay ) return
		end if

		
		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		! Build all possibilities for each element in the item array
		do i=effAt,nElems
			kMultiCombChosen(effNChosen) = i
			
			call multisetsBase( nElems, sGroups, items, effNChosen+1, i )
		end do
		
		return
	end subroutine multisetsBase
	
	!>
	!! @brief
	!!
	function Math_isOdd( n ) result( output )
		integer, intent(in) :: n
		logical :: output
		
		output = ( mod(n,2)>0 )
	end function Math_isOdd
	
	!>
	!! @brief
	!!
	function Math_isEven( n ) result( output )
		integer, intent(in) :: n
		logical :: output
		
		output = ( mod(n,2) == 0 )
	end function Math_isEven
	
	!>
	!! @brief
	!!
	function Math_fixPrecision( x, prec ) result( output )
		real(8), intent(in) :: x
		integer, intent(in) :: prec
		real(8) :: output
		
		output = real(ceiling(x*10**prec),8)/10**prec
	end function Math_fixPrecision
	
	!>
	!! @brief
	!! @todo La sección comentada puede hacer falta
	!! Tomada de https://www.google.es/search?q=error+%237768%3A+This+operation+on+this+data+type+is+currently+inaccurate.&oq=error+%237768%3A+This+operation+on+this+data+type+is+currently+inaccurate.&aqs=chrome..69i57.363j0j4&client=ubuntu-browser&sourceid=chrome&es_sm=91&ie=UTF-8
	!!
	function Math_erfinv( x_in ) result( r )
		real(8), intent(in) :: x_in
		real(8) :: r
		
		real(8) :: x, x2, y
		integer :: sign_x
		
		x = x_in

		if( x < -1.0_8 .or. x > 1_8 ) then
! 			r = Math_NAN
			r = sqrt(-1.0_8)
			return
		end if
		
		if( x == 0.0_8 ) then
			r = 0.0_8
			return
		end if
		
		if( x > 0.0_8 ) then
			sign_x = 1
		else
			sign_x = -1
			x = -x
		end if

		if( x <= 0.7_8 ) then
			x2 = x*x
			r = x*(((erfinv_a3 * x2 + erfinv_a2) * x2 + erfinv_a1) * x2 + erfinv_a0)
! 			r = r/(((erfinv_b4 * x2 + erfinv_b3) * x2 + erfinv_b2) * x2 + erfinv_b1)*x2 + erfinv_b0
		else
			y = sqrt (-log ((1.0_8 - x) /2.0_8))
			r = (((erfinv_c3 * y + erfinv_c2) * y + erfinv_c1) * y + erfinv_c0)
			r = r/((erfinv_d2 * y + erfinv_d1) * y + erfinv_d0)
		end if
		
		r = r * sign_x
		x = x * sign_x
		
		r = r - (erf(r) - x) / (2.0_8/sqrt(Math_PI)*exp(-r*r))
		r = r - (erf(r) - x) / (2.0_8/sqrt(Math_PI)*exp(-r*r))
	end function Math_erfinv
	
	!>
	!! @brief
	!!
	pure function Math_ustep( x ) result( output )
		real(8), intent(in) :: x
		real(8) :: output
		
		output = 0.5_8*( 1.0_8 + tanh( 1.0d12*x ) )
	end function Math_ustep
	
	!>
	!! @brief
	!!
	pure function Math_ubox( x ) result( output )
		real(8), intent(in) :: x
		real(8) :: output
		
		output = Math_ustep( x+0.5_8 ) - Math_ustep( x-0.5_8 )
	end function Math_ubox
	
	!>
	!! @brief Unnormalized sinc function
	!!
	pure function Math_sinc( x ) result( output )
		real(8), intent(in) :: x
		real(8) :: output
		
		output = sin(x)/x
	end function Math_sinc
	
	!>
	!! @brief Normalized sinc function
	!!
	pure function Math_nsinc( x ) result( output )
		real(8), intent(in) :: x
		real(8) :: output
		
		output = sin(Math_PI*x)/Math_PI/x
	end function Math_nsinc
	
	!>
	!! @brief Gaussian function
	!!
	pure function Math_gaussian( x, x0, I0, FWHM  ) result( output )
		real(8), intent(in) :: x, x0, I0, FWHM
		real(8) :: output
		
		real(8) :: width
		
		width = FWHM/2.35482_8
		output = I0*exp( -(x-x0)**2/2.0_8/width**2 )
	end function Math_gaussian
	
	!>
	!! @brief Gaussian lorentzian
	!!
	pure function Math_lorentzian( x, x0, I0, FWHM  ) result( output )
		real(8), intent(in) :: x, x0, I0, FWHM
		real(8) :: output
		
		real(8) :: width
		
		width = 0.5_8*FWHM
		output = I0/( 1.0_8+((x-x0)/width)**2 )
	end function Math_lorentzian
	
	!>
	!! @brief
	!! @input t Independent variable (time)
	!! @input t0 Position of its center
	!! @input w tophat width of the pulse
	!! @input w turn-on/turn-off time
	!!
	function Math_erfTophat( t, t0, w, dw ) result( output )
		real(8) :: t
		real(8) :: t0
		real(8) :: w
		real(8) :: dw
		real(8) :: output
		
		output = 0.0_8
		if ( t>t0-w/2-dw .and. t<=t0-w/2 ) then
			output = 0.5*erf( 4.0*(t-(t0-w/2-dw/2))/dw )+0.5
		else if ( t>t0-w/2 .and. t<=t0+w/2 ) then
			output = 1.0
		else if ( t>t0+w/2 .and. t<t0+w/2+dw ) then
			output = 0.5-0.5*erf( 4.0*(t-(t0+w/2+dw/2))/dw )
		end if
	end function Math_erfTophat
	
	!>
	!! @brief
	!! @input t Independent variable (time)
	!! @input t0 Position of its center
	!! @input w width of the window
	!!
	function Math_flatTopWindow( t, t0, w ) result( output )
		real(8) :: t
		real(8) :: t0
		real(8) :: w
		real(8) :: output
		
		output = 0.0_8
		if ( t>=t0-w/2 .and. t<=t0+w/2 ) then
			output = 1d-9*( 215578947.0 + 416631580.0*cos(2.0*Math_PI*(t-t0)/w) + 277263158.0*cos(4.0*Math_PI*(t-t0)/w)\
					+83578947.0*cos(6.0*Math_PI*(t-t0)/w) + 6947368.0*cos(8.0*Math_PI*(t-t0)/w) )
		end if
	end function Math_flatTopWindow
	
	!>
	!! @brief
	!!
	function Math_average( array ) result( output )
		real(8), allocatable, intent(in) :: array(:)
		real(8) :: output
		
		integer :: i
		real(8) :: ssum
		
		ssum = 0.0_8
		do i=1,size(array)
			ssum = ssum + array(i)
		end do
		
		output = ssum/real(size(array),8)
	end function Math_average
	
	!>
	!! @brief
	!!
	function Math_stdev( array, aver ) result( output )
		real(8), allocatable, intent(in) :: array(:)
		real(8), optional, intent(in) :: aver
		real(8) :: output
		
		real(8) :: effAver
		
		integer :: i
		real(8) :: ssum
		
		if( present(aver) ) then
			effAver = aver
		else
			effAver = Math_average( array )
		end if
		
		ssum = 0.0_8
		do i=1,size(array)
			ssum = ssum + ( array(i) - effAver )**2
		end do
		
		output = sqrt( ssum/real(size(array),8) )
	end function Math_stdev
	
	!>
	!! @brief
	!!
	function Math_skewness( array ) result( output )
		real(8), allocatable, intent(in) :: array(:)
		real(8) :: output
		
		integer :: i
		real(8) :: aver, stdev, ssum
		
		aver = Math_average( array )
		stdev = Math_stdev( array, aver )
		
		ssum = 0.0_8
		do i=1,size(array)
			ssum = ssum + ( array(i) - aver )**3
		end do
		
		output = ssum/real(size(array),8)/stdev**3
		
		! Esto ocurre si stdev es cero, entonces es una delta de Dirac
		! y por lo tanto simetrica, por eso he puesto output=0
		if( Math_isNaN(output) ) output = 0.0_8
	end function Math_skewness
	
	!>
	!! @brief
	!!
	pure function Math_rFloorDivision( x, y ) result( output )
		real(8), intent(in) :: x
		real(8), intent(in) :: y
		real(8) :: output
		
		output = floor( x/y )
	end function Math_rFloorDivision
	
	!>
	!! @brief
	!!
	pure function Math_iFloorDivision( x, y ) result( output )
		integer, intent(in) :: x
		integer, intent(in) :: y
		real(8) :: output
		
		output = floor( real(x,8)/real(y,8) )
	end function Math_iFloorDivision
	
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
	
	!>
	!! @brief The result has the value true if the value of x is NaN; otherwise, false.
	!!
	function Math_isNaN( x ) result( output )
		real(8), intent(in) :: x
		logical :: output
		
		output = IEEE_IS_NAN( x )
	end function Math_isNaN
	
	!>
	!! @brief The result has the value true if the value of x is Inf; otherwise, false.
	!!
	function Math_isInf( x ) result( output )
		real(8), intent(in) :: x
		logical :: output
		
		output = .not. IEEE_IS_FINITE( x )
	end function Math_isInf
	
	!>
	!! @brief The result has the value true if the value of x is Inf; otherwise, false.
	!!
	function Math_isIInf( x ) result( output )
		integer, intent(in) :: x
		logical :: output
		
		output = ( huge(0) == x )
	end function Math_isIInf
	
	!>
	!! @brief Returns the scalar product of two vectors (r1,r2),
	!!        which must have the same length (same number of elements).
	!!
	function Math_dotProduct( r1, r2 ) result( output )
		real(8) :: r1(3), r2(3)
		real(8) :: output
		
		output = DOT_PRODUCT( r1, r2 )
	end function Math_dotProduct
	
	!>
	!! @brief Returns the cross product of two vectors (r1,r2),
	!!        which must have the same length (same number of elements).
	!!
	function Math_crossProduct( r1, r2 ) result( output )
		real(8) :: r1(3), r2(3)
		real(8) :: output(3)
		
		output(1) = r1(2)*r2(3) - r1(3)*r2(2)
		output(2) = r1(3)*r2(1) - r1(1)*r2(3)
		output(3) = r1(1)*r2(2) - r1(2)*r2(1)
	end function Math_crossProduct
	
	!>
	!! @brief Returns the distance from the point r0 to the line specified by
	!!        two points r1 and r2 lying on it. Only 3D case is available.
	!!
	function Math_pointLineDistance( r0, r1, r2 ) result( output )
		real(8) :: r0(3), r1(3), r2(3)
		real(8) :: output
		
		output = norm2(Math_crossProduct( r0-r1, r0-r2 ))/norm2(r2-r1)
	end function Math_pointLineDistance
	
	!>
	!! @brief Test method
	!!
	subroutine Math_test()
		real(8), allocatable :: rArray(:)
		integer, allocatable :: iArray(:)
		integer, allocatable :: indexes(:)
		integer, allocatable :: iComb(:), iCombOld(:,:)
		character(10), allocatable :: strArray(:), sComb(:)
! 		character(10), allocatable :: comb(:)
		real(8) :: x
		
		integer :: i, j, k, l
		integer :: nFrag
		
! 		write(*,*) ""
! 		write(*,*) "Sorting vectors"
! 		write(*,*) "==============="
! 		allocate( rArray(19) )
! 		allocate( indexes(19) )
! 		
! 		rArray = [ -33.89007, -33.89007, -35.21007, -35.42677, -35.90699, &
! 			  -33.67374, -34.15396, -33.27902, -33.27902, -34.04338, &
! 			  -34.04338, -31.81726, -31.81726, -33.49329, -33.97351, &
! 			  -32.91849, -33.39871, -33.24224, -33.24224 ]
! 			  
! 		call Math_sort( rArray, indexes )
! 		
! 		write(*,"(5X,2A15)") "original", "sorted"
! 		write(*,"(5X,2A15)") "--------", "------"
! 		do i=1,size(rArray)
! 			write(*,"(I5,2F15.5)") i, rArray(i), rArray( indexes(i) )
! 		end do
! 		
! 		allocate( iArray(19) )
! 		
! 		iArray = [ -35, -33, -35, -35, -31, &
! 			     -32, -34, -33, -34, -32, &
! 			     -39, -31, -31, -31, -33, &
! 			     -31, -33, -33, -30 ]
! 			     
! 		call Math_sort( iArray, indexes )
! 		
! 		write(*,"(A)") ""
! 		write(*,"(5X,2A15)") "original", "sorted"
! 		write(*,"(5X,2A15)") "--------", "------"
! 		do i=1,size(rArray)
! 			write(*,"(I5,2I15)") i, iArray(i), iArray( indexes(i) )
! 		end do
! 			  
! 		deallocate( rArray )
! 		deallocate( iArray )
! 		deallocate( indexes )
! 		
! 		write(*,*) ""
! 		write(*,*) "3j symbols"
! 		write(*,*) "=========="
! 		
! 		write(*,*) "(3/2  0  3/2;    0  0    0) = ", Math_wigner3j( 1.5_8, 0.0_8, 1.5_8, 0.0_8, 0.0_8, 0.0_8 )
! 		write(*,*) "(3/2  0  3/2;  1/2  0 -1/2) = ", Math_wigner3j( 1.5_8, 0.0_8, 1.5_8, 0.5_8, 0.0_8,-0.5_8 )
! 		write(*,*) "(3/2  0  3/2; -1/2  0  1/2) = ", Math_wigner3j( 1.5_8, 0.0_8, 1.5_8,-0.5_8, 0.0_8, 0.5_8 )
! 		write(*,*) "(3/2  0  3/2;  3/2  0 -3/2) = ", Math_wigner3j( 1.5_8, 0.0_8, 1.5_8, 1.5_8, 0.0_8,-1.5_8 )
! 		write(*,*) "(3/2  0  3/2; -3/2  0  3/2) = ", Math_wigner3j( 1.5_8, 0.0_8, 1.5_8,-1.5_8, 0.0_8, 1.5_8 )
		
		write(*,*) ""
		write(*,*) "Combinatorial"
		write(*,*) "============="
		
		write(*,*) "Gamma(0.5) = ", Gamma(0.5_8)
		write(*,*) "log_Gamma(0.5) = ", log_Gamma(0.5_8)
		write(*,*) "Gamma(7+1) = ", Gamma(8.0_8)
		write(*,*) "Math_fact(7) = ", Math_fact(7)
		write(*,*) "Math_comb(7,2) = ", Math_comb(7,2)
		write(*,*) "Math_multisetNumber(7,2) = ", Math_multisetNumber( 7, 2 )
		write(*,*) ""
				
! 		deallocate( comb )
		!--------------------------------------------------------
		
		allocate( strArray(3) )
		strArray = ["a", "b", "c"]
		
		write(*,*)
		write(*,"(A,3A2,A)") " set = { ", strArray, "}"
		write(*,*) "Math_multisetNumber( nElems, 3 ) = "
		call Math_multisets( 3, 3, iCombOld, myStrConstrain )
		
		do i=1,size(iCombOld,dim=1)
			write(*,"(I5,A)", advance="no") i, ")   "
			do j=1,3
				write(*,"(A)", advance="no") trim(strArray( iCombOld(i,j) ))
			end do
			write(*,*) ""
		end do
		
		deallocate( strArray )
		deallocate( iCombOld )
		
		!-----------------------------------------------------------------
		allocate( MyiArray(3) )
		MyiArray = [1, 3, 5]
		
		write(*,*)
		write(*,"(A,3I2,A)") " set = { ", MyiArray, " }"
		write(*,*) "Math_multisetNumber( nElems, 3 ) = ", Math_multisetNumber( 3, 3 )
		call Math_multisets( 3, 3, iCombOld, myIntConstrain )
! 		call Math_multisets( 3, 3, iCombOld, myIntConstrainDebug )
		
		do i=1,size(iCombOld,dim=1)
			write(*,"(I5,A)", advance="no") i, ")   "
			do j=1,3
				write(*,"(I1)", advance="no") MyiArray( iCombOld(i,j) )
			end do
			write(*,*) ""
		end do
		
		deallocate( MyiArray )
		deallocate( iCombOld )

		!--------------------------------------------------------
		
		allocate( strArray(8) )
		strArray = ["H", "C", "CH", "C2", "H2", "C2H", "CH2", "C2H2"]
		
		allocate( MyiArray(8) )
		MyiArray = [1, 6, 7, 12, 2, 13, 8, 14]
		
		write(*,*)
		write(*,"(A,<size(strArray)>A5,A)") " set = { ", strArray, " }"
		
		k=1
		do nFrag=1,size(strArray)
			call Math_multisets( size(strArray), nFrag, iCombOld, myMassConstrain )
			
			do i=1,size(iCombOld,dim=1)
				write(*,"(I5,A)", advance="no") k, ")   "
				do j=1,nFrag-1
					write(*,"(A)", advance="no") trim(strArray( iCombOld(i,j) ))//"+"
				end do
				write(*,"(A)", advance="no") trim(strArray( iCombOld(i,nFrag) ))
				write(*,*) ""
				
				k=k+1
			end do
		end do
		
		deallocate( strArray )
		deallocate( iCombOld )
		
		!--------------------------------------------------------
		
! 		x = -0.99999_8
! 		do while( x < 1.0_8 )
! 			write(*,*) x, Math_erfinv(x)
! 			x = x+0.001_8
! 		end do
		
! 		write(*,"(3F15.6)") Math_erfinv(0.5_8), Math_erfinv(0.33_8), Math_erfinv(-1.0_8/3.0_8)
! 		write(*,"(3F15.6)") 0.4769362762, 0.3013321461, -0.3045701942
! 		write(*,"(3F15.6)") Math_erfinv(0.5_8)-0.4769362762, Math_erfinv(0.33_8)-0.3013321461, Math_erfinv(-1.0_8/3.0_8)-(-0.3045701942)

		write(*,*) ""
		write(*,*) "Testing IEEE support"
		write(*,*) "===================="
		write(*,*) "isNaN( 2.0 ) = ", Math_isNaN( 2.0_8 )
		write(*,*) "isNaN( 1.0d500 ) = ", Math_isNaN( 1.0d500 )
		write(*,*) "isNaN( sqrt(-1.0_8) ) = ", Math_isNaN( sqrt(-1.0_8) )
		write(*,*) "1.0_8/Math_INF = ", 1.0_8/Math_INF
		write(*,"(A,F10.5)") "Math_INF = ", Math_INF
		write(*,*) "isInf( 1.0d56 ) = ", Math_isInf( 1.0d56 )
		write(*,*) "isInf( 1.0_8/0.0_8 ) = ", Math_isInf( 1.0_8/0.0_8 )
		write(*,*) "isInf( 1.0_8/0.0_8+10.0_8 ) = ", Math_isInf( 1.0_8/0.0_8+10.0_8 )
		write(*,*) "isInf( 1.0_8/0.0_8-10.0_8 ) = ", Math_isInf( 1.0_8/0.0_8-10.0_8 )
		
	end subroutine Math_test
	
end module Math_


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                                                                                   !!
!!  This file is part of SciFT project                                               !!
!!  Copyright (c) 2011-2014 Nestor F. Aguirre (nfaguirrec@gmail.com)                 !!
!!                                                                                   !!
!!  Redistribution and use in source and binary forms, with or without               !!
!!  modification, are permitted provided that the following conditions are met:      !!
!!                                                                                   !!
!!  1. Redistributions of source code must retain the above copyright notice, this   !!
!!     list of conditions and the following disclaimer.                              !!
!!  2. Redistributions in binary form must reproduce the above copyright notice,     !!
!!     this list of conditions and the following disclaimer in the documentation     !!
!!     and/or other materials provided with the distribution.                        !!
!!  3. Neither the name of the copyright holders nor the names of its contributors   !!
!!     may be used to endorse or promote products derived from this software         !!
!!     without specific prior written permission.                                    !!
!!                                                                                   !!
!!  The copyright holders provide no reassurances that the source code provided      !!
!!  does not infringe any patent, copyright, or any other intellectual property      !!
!!  rights of third parties.  The copyright holders disclaim any liability to any    !!
!!  recipient for claims brought against recipient by any third party for            !!
!!  infringement of that parties intellectual property rights.                       !!
!!                                                                                   !!
!!  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND  !!
!!  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED    !!
!!  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE           !!
!!  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR  !!
!!  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES   !!
!!  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;     !!
!!  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND      !!
!!  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT       !!
!!  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS    !!
!!  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                     !!
!!                                                                                   !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!>
!! http://docs.python.org/2/library/random.html
!! @brief
!!
module RandomSampler_
	use UnitsConverter_
	use Math_
	use RandomUtils_
	implicit none
	private
	
	public :: &
		RandomSampler_test
		
	real(8), allocatable, private :: extraParamsDist(:)
	
	type, public :: RandomSampler
		integer :: nDim
		real(8), allocatable :: range(:,:)
		
		contains
			procedure :: init
			procedure :: copy
			final :: destroy
			procedure :: str
			procedure :: show
			
			procedure :: setRange
			procedure :: buildSample
			
			procedure :: uniform
			procedure :: uniformEllipsoid
			procedure :: normal
	end type RandomSampler
	
	interface
		function prototypeFunction( x ) result( output )
				real(8), intent(in) :: x(:)
				real(8) :: output
		end function prototypeFunction
	end interface
	
	contains
	
	!>
	!! @brief Constructor
	!!
	subroutine init( this, nDim )
		class(RandomSampler) :: this 
		integer :: nDim
		
		integer :: i, n, clock
		integer, dimension(:), allocatable :: seed
		
		this%nDim = nDim
		
		if( allocated(this%range) ) deallocate( this%range )
		if( allocated(extraParamsDist) ) deallocate( extraParamsDist )
		
		allocate( this%range(nDim,2) )
		allocate( extraParamsDist(nDim) )
		
		do i=1,nDim
			this%range(i,1) = 0.0_8
			this%range(i,2) = 1.0_8
		end do
		
		call RandomUtils_init()
	end subroutine init
	
	!>
	!! @brief Copy constructor
	!!
	subroutine copy( this, other )
		class(RandomSampler) :: this
		type(RandomSampler), intent(in) :: other

	end subroutine copy
	
	!>
	!! @brief Destructor
	!!
	subroutine destroy( this )
		type(RandomSampler) :: this
		
		if( allocated(this%range) ) deallocate( this%range )
		if( allocated(extraParamsDist) ) deallocate( extraParamsDist )
	end subroutine destroy
	
	!>
	!! @brief Convert to string
	!!
	function str( this ) result( output )
		class(RandomSampler) :: this 
		character(len=200) :: output
		
		integer :: fmt
		character(len=200) :: strBuffer
		
		output = ""
		
		output = trim(output)//"<RandomSampler:"
		
!! 		output = trim(output)//"min="
!! 		fmt = int(log10(this%min+1.0))+1
!! 		write(strBuffer, "(f<fmt+7>.6)") this%min
!! 		output = trim(output)//trim(strBuffer)
!! 		
!! 		output = trim(output)//",size="
!! 		fmt = int(log10(float(this%size+1)))+1
!! 		write(strBuffer, "(i<fmt>)") this%size
!! 		output = trim(output)//trim(strBuffer)
		
		output = trim(output)//">"
	end function str
	
	!>
	!! @brief Show 
	!!
	subroutine show( this, unit )
		class(RandomSampler) :: this
		integer, optional, intent(in) :: unit
		
		integer :: effunit
		
		if( present(unit) ) then
			effunit = unit
		else
			effunit = 6
		end if
		
		write(effunit,"(a)") trim(str(this))
	end subroutine show

	!>
	!! @brief Selects the range
	!!
	subroutine setRange( this, dim, range )
		class(RandomSampler) :: this
		integer :: dim
		real(8) :: range(2)
		
		this%range(dim,1) = range(1) ! min
		this%range(dim,2) = range(2) ! max
	end subroutine setRange
	
	!>
	!! @brief Build a random sample
	!! @todo Este es el algoritmmo de Metropolis, hay que dar la posibilidad
	!!       de usar cualquier algoritmo por seleccion, abajo esta es de
	!!       aceptacion-rechazo aunque esta en C++ hay que modificarlo
	!!
	subroutine buildSample( this, sample, targetDist, delta, rMin )
		class(RandomSampler) :: this
		real(8) :: sample(:,:)
		procedure(prototypeFunction) :: targetDist
		real(8), optional :: delta
		real(8), optional :: rMin
		
		real(8) :: deltaEff
		real(8) :: rMinEff
		integer :: nData
		real(8) :: randNumber
		real(8) :: probability
		real(8) :: distance
		real(8), allocatable :: rCurrent(:)
		real(8), allocatable :: rNew(:)
		logical :: outOfRange
		integer :: i
		
		if( size(sample,dim=1) /= size(this%range,dim=1) ) then
			stop "## Error ## ... in subroutine RandomSampler:buildSample(), size(sample) != size(range)"
		end if
		
		deltaEff = sum( abs(this%range(:,2)-this%range(:,1)) )/(this%nDim*10.0_8)
		if( present(delta) ) deltaEff = delta
		
		rMinEff = 0.0_8
		if( present(rMin) ) rMinEff = rMin
		
		if( deltaEff < rMinEff ) then
			stop "## Error ## ... in subroutine RandomSampler:buildSample(), delta<rMin"
		end if
		
		allocate( rCurrent(this%nDim) )
		allocate( rNew(this%nDim) )
		
! 		do i=1,this%nDim
! 			! Numero actual aleatorio en el intervalo [min,max]
! 			call random_number( randNumber )
! 			rCurrent(i) = this%range(i,1)+randNumber*abs( this%range(i,2)-this%range(i,1) ) ;
! 		end do
		rCurrent = 0.0_8
		
		nData = 1
		sample(:,nData) = 0.0_8
		
		nData = 2
		do while( nData <= size(sample,dim=2) )
			
			do while( .true. )
				
				outOfRange = .false.
				
				do i=1,3
				
					call random_number( randNumber )
					rNew(i) = rCurrent(i)+(2.0*randNumber-1.0_8)*deltaEff
				
					if ( rNew(i) < this%range(i,1) .or. rNew(i) > this%range(i,2) ) then
						outOfRange = .true.
						exit
					end if
				end do
				
				if( sqrt(sum(rNew**2)) < 5.0_8 ) then
					outOfRange = .false.
				end if
				
				if( outOfRange ) then
					cycle
				end if
					
				do i=1,nData-1
					distance = sqrt(sum((rNew(:)-sample(:,i))**2))
					if ( distance <= rMinEff ) then
						outOfRange = .true.
						exit
					end if
				end do

				if( .not. outOfRange ) then
					exit
				end if
			
			end do
			
			probability = targetDist(rNew)/targetDist(rCurrent)
			
			call random_number( randNumber )
			if( probability > 1.0 .or. randNumber < probability ) then
				rCurrent = rNew
				sample(:,nData) = rCurrent
				nData = nData + 1
! 				write(*,"(F5.1,A,I4,A,I4,A)") (100.0_8*real(nData-1,8)/real(size(sample,dim=2),8)), "%  (", nData-1, "/", size(sample,dim=2), ")"
			end if
			
		end do

	end subroutine buildSample
	
! 	void RandomSampler::runAcceptedReject()
! 	{
! 		double randomX ;
! 		double randomY ;
! 		
! 		srand( time(NULL) );
! 		
! 		generatedData = 0 ;
! 		while( data%size() < requiredData ){
! 			
! 			// Numero aleatorio en el intervalo [min,max]
! 			randomX = min+RAND*fabs(max-min) ;
! 			
! 			// Numero aleatorio en el intervalo [0,G(x)]
! 			randomY = RAND*proposalDistribution( randomX ) ;
! 			
! 			if( randomY < targetDistribution( randomX ) )
! 				data.push_back( randomX ) ;
! 			
! 			generatedData++ ;
! 			
! 		}
! 	}
	
	!>
	!! @brief Build a random sample using an uniform distribution
	!!
	!! @param[out] sample In this array will will store the generated random numbers
	!!                    it should be allocated in advance
	!!
	subroutine uniform( this, sample, A )
		class(RandomSampler) :: this
		real(8) :: sample(:,:)
		real(8), optional, intent(in) :: A(:,:)
		
		integer :: i, j, nPoints, nDim
		
		real(8), allocatable :: eVec(:,:)
		real(8), allocatable :: eVal(:)
		real(8), allocatable :: workSpace(:)
		integer :: info
		
		if( size(sample,dim=1) /= size(this%range,dim=1) ) then
			stop "## Error ## ... in subroutine RandomSampler:uniform(), size(sample) != size(range)"
		end if
		
		nDim = size(sample,dim=1)
		nPoints = size(sample,dim=2)
		
		if( .not. present(A) ) then
			call random_number( sample )
			
			do i=1,nDim
				sample(i,:) = this%range(i,1) + sample(i,:)*abs(this%range(i,2)-this%range(i,1))
			end do
		else
			allocate( eVec( nDim, nDim ) )
			allocate( eVal( nDim ) )
			allocate( workSpace( 3*nDim-1 ) )
			
			eVec = A
			
			call dsyev( 'V', 'L', nDim, eVec, nDim, eVal, workSpace, 3*nDim-1, info )
			
			if ( info /= 0 ) then
				write(*,*) "### ERROR ### RandomSampler.normal: eigen values matrix failed"
				stop
			end if
			
			eVal(:) = 1.0_8/sqrt(eVal(:))
			
			call random_number( sample )
			
			do i=1,nPoints
				sample(:,i) = -1.0_8 + 2.0_8*sample(:,i)
				sample(:,i) = matmul(eVec,eVal(:)*sample(:,i)) !+ aver(:)
			end do
			
			deallocate( eVec )
			deallocate( eVal )
			deallocate( workSpace )
		end if
	end subroutine uniform
	
	!>
	!! @brief Build a random sample using an uniform distribution
	!!
	!! @param[out] sample In this array will will store the generated random numbers
	!!                    it should be allocated in advance
	!!
	subroutine uniformEllipsoid( this, sample, A )
		class(RandomSampler) :: this
		real(8) :: sample(:,:)
		real(8), optional, intent(in) :: A(:,:)
		
		integer :: i, j, nPoints, nDim
		
		real(8), allocatable :: eVec(:,:)
		real(8), allocatable :: eVal(:)
		real(8), allocatable :: workSpace(:)
		integer :: info
		
		if( size(sample,dim=1) /= size(this%range,dim=1) ) then
			stop "## Error ## ... in subroutine RandomSampler:uniform(), size(sample) != size(range)"
		end if
		
		nDim = size(sample,dim=1)
		nPoints = size(sample,dim=2)
		
		if( .not. present(A) ) then
			call random_number( sample )
			
			do i=1,size(sample,dim=1)
				sample(i,:) = this%range(i,1) + sample(i,:)*abs(this%range(i,2)-this%range(i,1))
			end do
		else
			allocate( eVec( nDim, nDim ) )
			allocate( eVal( nDim ) )
			allocate( workSpace( 3*nDim-1 ) )
			
			eVec = A
			
			call dsyev( 'V', 'L', nDim, eVec, nDim, eVal, workSpace, 3*nDim-1, info )
			
			if ( info /= 0 ) then
				write(*,*) "### ERROR ### RandomSampler.normal: eigen values matrix failed"
				stop
			end if
			
			eVal(:) = 1.0_8/sqrt(eVal(:))
			
			call random_number( sample )
				
			do i=1,nPoints
				
				do while( .true. )
					sample(:,i) = -1.0_8 + 2.0_8*sample(:,i)
					
					if( sum(sample(:,i)**2) < 1.0_8 ) then
						exit
					else
						call random_number( sample(:,i) )
					end if
				end do
			end do
			
			do i=1,nPoints
				sample(:,i) = matmul(eVec,eVal(:)*sample(:,i)) !+ aver(:)
			end do
			
			deallocate( eVec )
			deallocate( eVal )
			deallocate( workSpace )
		end if
	end subroutine uniformEllipsoid
	
	!>
	!! @brief Build a random sample using an normal distribution
	!!
	!! @param[out] sample In this array will will store the generated random numbers
	!!                    it should be allocated in advance
	!!
	subroutine normal( this, sample, stdev, cov, aver )
		class(RandomSampler) :: this
		real(8) :: sample(:,:)
		real(8), optional, intent(in) :: stdev(:)
		real(8), optional, intent(in) :: cov(:,:)
		real(8), optional, intent(in) :: aver(:)
		
		integer :: i, nPoints, nDim
		real(8), allocatable :: randNumber(:,:,:)
		
		real(8), allocatable :: eVec(:,:)
		real(8), allocatable :: eVal(:)
		real(8), allocatable :: workSpace(:)
		integer :: info
		
		if( size(sample,dim=1) /= size(this%range,dim=1) ) then
			stop "## Error ## ... in subroutine RandomSampler:normal(), size(sample) != size(range)"
		end if
		
		nDim = size(sample,dim=1)
		nPoints = size(sample,dim=2)
		
		allocate(randNumber(2,nDim,nPoints))
		
		call random_number( randNumber )
		
		if( present(stdev) .and. present(aver) ) then
			do i=1,nPoints
				sample(:,i) = stdev(:)*sqrt(-2.0_8*log(randNumber(1,:,i)))*cos(2.0_8*Math_PI*randNumber(2,:,i))+aver(:)
			end do
		else if( present(cov) .and. present(aver) ) then
		
			allocate( eVec( nDim, nDim ) )
			allocate( eVal( nDim ) )
			allocate( workSpace( 3*nDim-1 ) )
			
			eVec = cov
			
			call dsyev( 'V', 'L', nDim, eVec, nDim, eVal, workSpace, 3*nDim-1, info )
			
			if ( info /= 0 ) then
				write(*,*) "### ERROR ### RandomSampler.normal: eigen values matrix failed"
				stop
			end if
			
			write(*,*) "eVec"
			write(*,*) eVec(:,1)
			write(*,*) eVec(:,2)
			write(*,*) eVec(:,3)
			write(*,*) "eVal"
			write(*,*) eVal(:)
			
			do i=1,nPoints
				sample(:,i) = sqrt(-2.0_8*log(randNumber(1,:,i)))*cos(2.0_8*Math_PI*randNumber(2,:,i))
				sample(:,i) = matmul(eVec,eVal(:)*sample(:,i)) + aver(:)
			end do
			
			deallocate( eVec )
			deallocate( eVal )
			deallocate( workSpace )
		else
			do i=1,nPoints
				sample(:,i) = sqrt(-2.0_8*log(randNumber(1,:,i)))*cos(2.0_8*Math_PI*randNumber(2,:,i))
			end do
		end if
		
		deallocate(randNumber)
	end subroutine normal
	
	!>
	!! @brief Build a random sample using an uniform distribution
	!!
	!! @param[out] sample In this array will will store the generated random numbers
	!!                    it should be allocated in advance
	!!
! 	subroutine uniformSphere( this, sample, cov )
! 		class(RandomSampler) :: this
! 		real(8) :: sample(:,:)
! 		real(8), optional, intent(in) :: cov(:,:)
! 		
! 		integer :: i, nPoints, nDim
! 		
! 		real(8), allocatable :: sampleBase(:,:)
! 		integer :: i
! 		real(8) :: rnd
! 		integer :: nDim, nPoints
! 		real(8) :: cov(3,3)
! 		
! 		nDim = size(sample,dim=1)
! 		nPoints = size(sample,dim=2)
! 			
! 		allocate( sampleBase(nDim+1,nPoints) )
! 		
! 		call this%normal( sampleBase, cov=cov, aver=[0.0_8,0.0_8] )
! 		
! 		do i=1,nPoints
! ! 			write(10,'(<nDim>F10.5)') sample(:,i)
! 			sample(:,i) = sample(:,i)/norm2(sample(:,i))
! 			
! 			write(10,'(2F10.5,F10.5,L)') sample(1,i), sample(3,i), sqrt(sample(1,i)**2+sample(3,i)**2), ( sqrt(sample(1,i)**2+sample(3,i)**2) < 1.0_8 )
! ! 			write(10,'(4F10.5)') sample(1,i)/norm2(sample(:,i)), sample(2,i)/norm2(sample(:,i))
! 		end do
! 		write(10,*) ""
! 		write(10,*) ""
! 	end subroutine uniformSphere
	
	!>
	!! This is neccesary only for RandomSampler_test()
	!!
	function dist( r ) result( output )
		real(8), intent(in) :: r(:)
		real(8) :: output
		
		real(8) :: pi = acos(-1.0_8)
		
		output = sqrt(50.0_8/pi)*exp(-50.0_8*sum((r-0.5_8)**2) )
	end function dist

	!>
	!! @brief Test method
	!!
	subroutine RandomSampler_test()
		type(RandomSampler) :: rs
		real(8), allocatable :: sample(:,:)
		integer :: i, j
		real(8) :: rnd
		integer :: nDim, nPoints
		real(8) :: a, b, theta, Rot(2,2), Sigma(2,2), Amatrix(2,2)
		
		open( 10,file="salida" )
		
		nDim = 2
		nPoints = 10000
		allocate( sample(nDim,nPoints) )
		call rs%init( nDim )
		
! 		call rs%setRange( 1, [1.0_8,2.0_8] )
! 		call rs%setRange( 2, [2.0_8,3.0_8] )
! 		call rs.uniform( sample )

! 		call rs%setRange( 1, [-10.0_8,10.0_8] )
! 		call rs%setRange( 2, [-10.0_8,10.0_8] )
! 		call rs.normal( sample, stdev=[1.0_8,2.0_8], aver=[4.0_8,4.0_8] )
			
			
! 		call rs%setRange( 1, [-1.0_8,1.0_8] )
! 		call rs%setRange( 2, [-1.0_8,1.0_8] )
		
		a = 1.0_8
		b = 2.0_8
		theta = -Math_PI/4.0_8
! 		theta = 0.0_8
		
		Sigma(1,:) = [ 1.0_8/a, 0.0_8 ]
		Sigma(2,:) = [ 0.0_8, 1.0_8/b ]
		
		Rot(1,:) = [ cos(theta),-sin(theta) ]
		Rot(2,:) = [ sin(theta), cos(theta) ]
		
		Amatrix = matmul(Rot,matmul(Sigma**2,transpose(Rot)))
		
		do i=1,2
			do j=1,2
				write(*,"(F10.5)",advance="no") Amatrix(i,j)
			end do
			write(*,*) ""
		end do
! 		call rs.normal( sample, cov=cov, aver=[0.0_8,0.0_8] )
		call rs.uniformEllipsoid( sample, A=Amatrix )
		
		write(10,*) "# Uniform distribution"
		do i=1,nPoints
			write(10,'(<nDim>F10.5)') sample(:,i)
		end do
		write(10,*) ""
		write(10,*) ""
		
! 		call rs%init( nDim=3 )
! ! 		call rs%setRange( 1, [0.0_8,10.0_8] )
! ! 		call rs%setRange( 2, [0.0_8,10.0_8] )
! ! 		call rs%setRange( 3, [0.0_8,10.0_8] )
! 		call rs.buildSample( sample, dist )
! 		
! 		write(10,*) "# XXXX distribution"
! 		do i=1,size(sample,dim=2)
! 			write(10,'(3F10.5)') sample(1,i), sample(2,i), sample(3,i)
! 		end do
		
		close(10)
		
		deallocate( sample )
	end subroutine RandomSampler_test
	
end module RandomSampler_

program test_RandomSampler
    use RandomSampler_
    use TestUtils_
    use UnitsConverter_
    use Math_
    use RandomUtils_
    implicit none
		type(RandomSampler) :: rs
		real(8), allocatable :: sample(:,:)
		integer :: i
		integer :: nDim, nPoints
		real(8) :: a, b, theta, Rot(2,2), Sigma(2,2), Amatrix(2,2)
		real(8) :: val
		
		nDim = 2
		nPoints = 1000
		allocate( sample(nDim,nPoints) )
		call rs%init( nDim )
		
		a = 1.0_8
		b = 2.0_8
		theta = -Math_PI/4.0_8
		
		Sigma(1,:) = [ 1.0_8/a, 0.0_8 ]
		Sigma(2,:) = [ 0.0_8, 1.0_8/b ]
		
		Rot(1,:) = [ cos(theta),-sin(theta) ]
		Rot(2,:) = [ sin(theta), cos(theta) ]
		
		Amatrix = matmul(Rot,matmul(Sigma**2,transpose(Rot)))
		
		call rs%uniformEllipsoid( sample, A=Amatrix )
		
		do i=1,nPoints
			val = dot_product(sample(:,i), matmul(Amatrix, sample(:,i)))
			call assert_true( val <= 1.0_8 + 1e-12_8, "RandomSampler_test: point outside ellipsoid" )
		end do
		
		deallocate( sample )

    contains

    	
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

end program test_RandomSampler

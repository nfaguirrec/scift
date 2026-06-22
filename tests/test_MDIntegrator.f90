program test_MDIntegrator
    use MDIntegrator_
    use TestUtils_
    implicit none
		integer :: nParticles
		integer :: nDimensions
		real(8), allocatable :: positions(:,:)
		real(8), allocatable :: velocities(:,:)
		real(8), allocatable :: accelerations(:,:)
		
		type(MDIntegrator) :: solver
		real(8) :: t0
		real(8) :: dt
		
		real(8) :: omega, T
		
		omega = 0.057_8
		T = 2.0_8*3.1415926_8/omega
		
		nParticles = 1
		nDimensions = 1
		allocate( positions(nDimensions,nParticles) )
		allocate( velocities(nDimensions,nParticles) )
		allocate( accelerations(nDimensions,nParticles) )
		dt = 0.01_8
		
		t0 = 0.0_8
		
		call solver%init( positions, velocities, accelerations )
		
		positions = 0.0_8
		velocities = 0.0_8
		accelerations = 0.0_8
		solver%time = t0
		solver%timeStep = 0.01_8
		
		do while( .true. )
			if( solver%time > T ) exit
			call solver%iterate( computeForcesTest )
		end do
		
		call assert_equal_real( solver%time, 110.24000_8, 1e-5_8, "MDIntegrator_test: final time" )
		call assert_equal_real( positions(1,1), 89.454615_8, 1e-5_8, "MDIntegrator_test: final position" )
		
		deallocate( positions )
		deallocate( velocities )
		deallocate( accelerations )
		
end program test_MDIntegrator

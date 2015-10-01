!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!  This file is part of scift (Scientific Fortran Tools).
!!  Copyright (C) by authors (2011-2015)
!!  
!!  Authors (alphabetic order):
!!    * Aguirre N.F. (nfaguirrec@gmail.com)  (2011-2015)
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
module MDIntegrator_
	implicit none
	private
	
	enum, BIND(c)
		enumerator :: VELOCITY_VERLET = 1
		enumerator :: BEEMAN = 2
		enumerator :: BEEMAN_PREDCORR = 3
		enumerator :: BEEMAN_PREDCORR_AM = 4
	end enum
	
	public :: &
		MDIntegrator_charIDtoID, &
		MDIntegrator_test
	
	type, public :: MDIntegrator
		real(8) :: time
		real(8) :: timeStep
		integer :: maxIter
		integer :: ttype
		
		real(8), pointer :: positions(:,:)
		real(8), pointer :: velocities(:,:)
		real(8), pointer :: accelerations(:,:)
		real(8), pointer :: forces(:,:)
		
		integer :: step
		
		contains
			procedure :: init
			final :: destroy
			
			procedure :: nParticles
			procedure :: nDimensions
			procedure :: iterate
	end type MDIntegrator
	
	real(8), allocatable :: posBufferT0(:,:)
	real(8), allocatable :: accelBufferT0(:,:)
	real(8), allocatable :: accelBufferTm1(:,:)
	
	interface
		subroutine prototypeComputeForces( t, positions, velocities, accelerations, forces )
			real(8), intent(in) :: t
			real(8), target, intent(in) :: positions(:,:)
			real(8), target, intent(in) :: velocities(:,:)
			real(8), target :: accelerations(:,:)
			real(8), target, intent(in) :: forces(:,:)
		end subroutine prototypeComputeForces
	end interface
	
	contains
	
	!>
	!! @brief Default constructor
	!!
	subroutine init( this, positions, velocities, accelerations, forces )
		class(MDIntegrator) :: this
		real(8), target :: positions(:,:)
		real(8), target :: velocities(:,:)
		real(8), target :: accelerations(:,:)
		real(8), target :: forces(:,:)
		real(8) :: time0
		real(8) :: timeStep
		
		this.positions => positions
		this.velocities => velocities
		this.accelerations => accelerations
		this.forces => forces
		
		this.time = 0.0_8
		this.timeStep = 1e-3
		this.maxIter = 1000
		this.step = 0
		this.ttype = VELOCITY_VERLET
		
		if( allocated(posBufferT0) ) deallocate(posBufferT0)
		if( allocated(accelBufferT0) ) deallocate(accelBufferT0)
		if( allocated(accelBufferTm1) ) deallocate(accelBufferTm1)
		
		allocate( posBufferT0(this.nDimensions(),this.nParticles()) )
		allocate( accelBufferT0(this.nDimensions(),this.nParticles()) )
		allocate( accelBufferTm1(this.nDimensions(),this.nParticles()) )
		
		posBufferT0 = 0.0_8
		accelBufferT0 = 0.0_8
		accelBufferTm1 = 0.0_8
	end subroutine init
	
	!>
	!! @brief Destructor
	!!
	subroutine destroy( this )
		type(MDIntegrator) :: this
		
		nullify( this.positions )
		nullify( this.velocities )
		nullify( this.accelerations )
		nullify( this.forces )
		
		this.time = 0.0_8
		this.timeStep = 0.0_8
		this.maxIter = 0
		this.step = 0
		
		deallocate( posBufferT0 )
		deallocate( accelBufferT0 )
		deallocate( accelBufferTm1 )
	end subroutine destroy
	
	!>
	!! @brief
	!!
	subroutine control( this, maxIter, timeStep )
		class(MDIntegrator) :: this
		real(8), intent(in) :: maxIter
		real(8), intent(in) :: timeStep
		
	end subroutine control
	
	!>
	!! @brief
	!!
	function nParticles( this ) result( output )
		class(MDIntegrator) :: this
		integer :: output
		
		output = size(this.positions,dim=2)
	end function nParticles
	
	!>
	!! @brief
	!!
	function nDimensions( this ) result( output )
		class(MDIntegrator) :: this
		integer :: output
		
		output = size(this.positions,dim=1)
	end function nDimensions
	
	!>
	!! @brief
	!!
	subroutine iterate( this, computeForces )
		class(MDIntegrator) :: this
		procedure(prototypeComputeForces) :: computeForces
		
#define x0 posBufferT0
#define t this.time
#define x this.positions
#define v this.velocities
#define a this.accelerations
#define F this.forces
#define a0 accelBufferT0
#define am1 accelBufferTm1
#define dt this.timeStep
		
		select case( this.ttype )
			case( VELOCITY_VERLET )
				x = x + v*dt + 0.5_8*a*dt*dt
				a0 = a
				call computeForces( t, x, v, a, F ) ! a <--
				v = v + 0.5_8*(a0+a)*dt
				
			case( BEEMAN )
				x = x + v*dt + (1.0_8/6.0_8)*( 4.0_8*a0 - am1 )*dt*dt
				a0 = a
				call computeForces( t, x, v, a, F ) ! a <--
				v = v + (1.0_8/6.0_8)*( 2.0_8*a + 5.0_8*a0 - am1 )*dt
				am1 = a0
				
			case( BEEMAN_PREDCORR )
				x0 = x
				x = x + v*dt + (1.0_8/6.0_8)*( 4.0_8*a0 - am1 )*dt*dt
				a0 = a
				call computeForces( t, x, v, a, F ) ! a <--
				x = x0 + v*dt + (1.0_8/6.0_8)*( a + 2.0_8*a0 )*dt*dt
				v = ( x - x0 )/dt + (1.0_8/6.0_8)*( 2.0_8*a + a0 )*dt
				am1 = a0
				
			case( BEEMAN_PREDCORR_AM )
				x0 = x
				x = x + v*dt + (1.0_8/6.0_8)*( 4.0_8*a0 - am1 )*dt*dt
				a0 = a
				call computeForces( t, x, v, a, F ) ! a <--
				x = x0 + v*dt + (1.0_8/6.0_8)*( a + 2.0_8*a0 )*dt*dt
				v = v + (1.0_8/12.0_8)*( 5.0_8*a + 8.0_8*a0 - am1 )*dt
				am1 = a0
				
		end select

#undef x0
#undef t
#undef x
#undef v
#undef a
#undef F
#undef a0
#undef am1
#undef dt
		
		this.step = this.step + 1
		this.time = this.time + this.timeStep
	end subroutine iterate
	
	!>
	!! @brief Converts the character ID to integer ID
	!!
	function MDIntegrator_charIDtoID( charID ) result( id )
		character(*), intent(in) :: charID
		integer :: id
		
		select case( charID )
			case( "VelocityVerlet" )
				id = VELOCITY_VERLET
			case( "VV" )
				id = VELOCITY_VERLET
			case( "Beeman" )
				id = BEEMAN
			case( "B0" )
				id = BEEMAN
			case( "BeemanPredCorr" )
				id = BEEMAN_PREDCORR
			case( "B1" )
				id = BEEMAN_PREDCORR
			case( "BeemanPredCorrAM" )
				id = BEEMAN_PREDCORR_AM
			case( "B2" )
				id = BEEMAN_PREDCORR_AM
		end select
	end function MDIntegrator_charIDtoID
	
	!>
	!! @brief
	!!
	subroutine computeForcesTest( t, r, v, a, F )
		real(8), intent(in) :: t
		real(8), target, intent(in) :: r(:,:)
		real(8), target, intent(in) :: v(:,:)
		real(8), target :: a(:,:)
		real(8), target, intent(in) :: F(:,:)
		
		real(8) :: E0 = 1.0_8
		real(8) :: omega = 1.0_8
		
! 		F = F - dV( r )
! 		F = F + 1.0_8/r**2 - E0*cos(omega*t)
! 		a = F/1.0_8
! 		a = 1.0_8/(r+1.0_8)**2 - E0*cos(omega*t)
		a = - E0*cos(omega*t)
		
	end subroutine computeForcesTest
	
	!>
	!! @brief Test method
	!!
	subroutine MDIntegrator_test()
		integer :: nParticles
		integer :: nDimensions
		real(8), allocatable :: positions(:,:)
		real(8), allocatable :: velocities(:,:)
		real(8), allocatable :: accelerations(:,:)
		real(8), allocatable :: forces(:,:)
		
		type(MDIntegrator) :: solver
		real(8) :: t0
		real(8) :: dt
		integer :: step
		
		real(8) :: r0
		
		nParticles = 1
		nDimensions = 1
		allocate( positions(nDimensions,nParticles) )
		allocate( velocities(nDimensions,nParticles) )
		allocate( accelerations(nDimensions,nParticles) )
		allocate( forces(nDimensions,nParticles) )
		dt = 0.01_8
		
		t0 = 0.0_8
		do while( .true. )
			if( t0 > 3.1415926_8/2.0_8 ) exit
			
			call solver.init( positions, velocities, accelerations, forces )
			
			positions = 0.0_8
			velocities = 0.0_8
			accelerations = 0.0_8
			forces = 0.0_8
			solver.time = t0
			solver.timeStep = 0.01_8
			
			do while( .true. )
				if( solver.time > 10.0_8 ) exit
				write(*,"(2F20.5)") solver.time, positions
				
				call solver.iterate( computeForcesTest )
			end do
			
			write(*,*) ""
			write(*,*) ""
			
			t0 = t0 + 0.1_8
		end do
		
	end subroutine MDIntegrator_test

end module MDIntegrator_

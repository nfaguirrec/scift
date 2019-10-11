!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                                                                                   !!
!!  This file is part of SciFT project                                               !!
!!  Copyright (c) 2011-2015 Nestor F. Aguirre (nfaguirrec@gmail.com)                 !!
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
		subroutine prototypeComputeForces( t, positions, velocities, accelerations )
			real(8), intent(in) :: t
			real(8), target, intent(in) :: positions(:,:)
			real(8), target, intent(in) :: velocities(:,:)
			real(8), target :: accelerations(:,:)
		end subroutine prototypeComputeForces
	end interface
	
	contains
	
	!>
	!! @brief Default constructor
	!!
	subroutine init( this, positions, velocities, accelerations )
		class(MDIntegrator) :: this
		real(8), target :: positions(:,:)
		real(8), target :: velocities(:,:)
		real(8), target :: accelerations(:,:)
		real(8) :: time0
		real(8) :: timeStep
		
		this%positions => positions
		this%velocities => velocities
		this%accelerations => accelerations
		
		this%time = 0.0_8
		this%timeStep = 1e-3
		this%maxIter = 1000
		this%step = 0
		this%ttype = VELOCITY_VERLET
		
		if( allocated(posBufferT0) ) deallocate(posBufferT0)
		if( allocated(accelBufferT0) ) deallocate(accelBufferT0)
		if( allocated(accelBufferTm1) ) deallocate(accelBufferTm1)
		
		allocate( posBufferT0(this%nDimensions(),this%nParticles()) )
		allocate( accelBufferT0(this%nDimensions(),this%nParticles()) )
		allocate( accelBufferTm1(this%nDimensions(),this%nParticles()) )
		
		posBufferT0 = 0.0_8
		accelBufferT0 = 0.0_8
		accelBufferTm1 = 0.0_8
	end subroutine init
	
	!>
	!! @brief Destructor
	!!
	subroutine destroy( this )
		type(MDIntegrator) :: this
		
		nullify( this%positions )
		nullify( this%velocities )
		nullify( this%accelerations )
		
		this%time = 0.0_8
		this%timeStep = 0.0_8
		this%maxIter = 0
		this%step = 0
		
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
		
		output = size(this%positions,dim=2)
	end function nParticles
	
	!>
	!! @brief
	!!
	function nDimensions( this ) result( output )
		class(MDIntegrator) :: this
		integer :: output
		
		output = size(this%positions,dim=1)
	end function nDimensions
	
	!>
	!! @brief
	!!
	subroutine iterate( this, computeForces )
		class(MDIntegrator) :: this
		procedure(prototypeComputeForces) :: computeForces
		
#define x0 posBufferT0
#define t this%time
#define x this%positions
#define v this%velocities
#define a this%accelerations
#define a0 accelBufferT0
#define am1 accelBufferTm1
#define dt this%timeStep
		
		select case( this%ttype )
			case( VELOCITY_VERLET )
				x = x + v*dt + 0.5_8*a*dt*dt
				a0 = a
				call computeForces( t, x, v, a ) ! a <--
				v = v + 0.5_8*(a0+a)*dt
				
			case( BEEMAN )
				x = x + v*dt + (1.0_8/6.0_8)*( 4.0_8*a0 - am1 )*dt*dt
				a0 = a
				call computeForces( t, x, v, a ) ! a <--
				v = v + (1.0_8/6.0_8)*( 2.0_8*a + 5.0_8*a0 - am1 )*dt
				am1 = a0
				
			case( BEEMAN_PREDCORR )
				x0 = x
				x = x + v*dt + (1.0_8/6.0_8)*( 4.0_8*a0 - am1 )*dt*dt
				a0 = a
				call computeForces( t, x, v, a ) ! a <--
				x = x0 + v*dt + (1.0_8/6.0_8)*( a + 2.0_8*a0 )*dt*dt
				v = ( x - x0 )/dt + (1.0_8/6.0_8)*( 2.0_8*a + a0 )*dt
				am1 = a0
				
			case( BEEMAN_PREDCORR_AM )
				x0 = x
				x = x + v*dt + (1.0_8/6.0_8)*( 4.0_8*a0 - am1 )*dt*dt
				a0 = a
				call computeForces( t, x, v, a ) ! a <--
				x = x0 + v*dt + (1.0_8/6.0_8)*( a + 2.0_8*a0 )*dt*dt
				v = v + (1.0_8/12.0_8)*( 5.0_8*a + 8.0_8*a0 - am1 )*dt
				am1 = a0
				
		end select

#undef x0
#undef t
#undef x
#undef v
#undef a
#undef a0
#undef am1
#undef dt
		
		this%step = this%step + 1
		this%time = this%time + this%timeStep
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
	subroutine computeForcesTest( t, r, v, a )
		real(8), intent(in) :: t
		real(8), target, intent(in) :: r(:,:)
		real(8), target, intent(in) :: v(:,:)
		real(8), target :: a(:,:)
		
		real(8) :: E0 = 0.3_8
		real(8) :: omega = 0.057_8
		
! 		F = F - dV( r )
! 		F = F + 1.0_8/r**2 - E0*cos(omega*t)
! 		a = F/1.0_8
! 		diff(-1/sqrt(2+r**2),r,1);
		a = - r*(r**2+2.0_8)**((-3.0_8)/2.0_8) - E0*cos(omega*t)
! 		a = - E0*cos(omega*t)
		
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
		
		type(MDIntegrator) :: solver
		real(8) :: t0
		real(8) :: dt
		integer :: step
		
		real(8) :: r0
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
		do while( .true. )
			if( t0 > 0.2_8*T ) exit
			
			call solver%init( positions, velocities, accelerations )
			
			positions = 0.0_8
			velocities = 0.0_8
			accelerations = 0.0_8
			solver.time = t0
			solver.timeStep = 0.01_8
			
			do while( .true. )
				if( solver.time > T ) exit
				write(*,"(2F20.5)") solver.time, positions
				
				call solver.iterate( computeForcesTest )
			end do
			
			write(*,*) ""
			write(*,*) ""
			
			t0 = t0 + 0.02_8*T
		end do
		
	end subroutine MDIntegrator_test

end module MDIntegrator_

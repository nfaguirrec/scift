!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                                                                                   !!
!!  This file is part of SciFT project                                               !!
!!  Copyright (c) 2011-2013 Nestor F. Aguirre (nfaguirrec@gmail.com)                 !!
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

! module Unit_
! 	implicit none
! 	private
! 	
! 	type, public :: Unit
! 		character(*) :: name
! 		character(*) :: symbol
! 		character(*) :: category
! 		real(8) :: value
! 	end type Unit
! 	
! end module Unit_

! Esto debe ser una clase singleton
! y entre lo que contendrá será
! el sistema de unidades con el que
! actualmente se está trabajando
! module SystemOfUnits_
! 	implicit none
! 	private
! 	
! 	type, public :: SystemOfUnits
! 		real(8) :: mass
! 	end type SystemOfUnits
! 	
! end module SystemOfUnits_

! module CM1Units_
! 	implicit none
! 	private
! 	
! 	!!
! 	! Mass   => amu
! 	! Lenght => Å
! 	! Time   => 9.14294598 10⁻¹¹ s
! 	! Energy => cm⁻¹
! 	!!
! 	real(8), public, parameter :: amu = 1.0_8
! 	real(8), public, parameter :: angs = 1.0_8
! 	real(8), public, parameter :: t_cm1 = 1.0_8
! 	real(8), public, parameter :: cm1 = 1.0_8
! 	real(8), public, parameter :: g = 1.0_8/1.660538782e-24
! 	real(8), public, parameter :: kg = 1.0_8/1.660538782e-27
! 	real(8), public, parameter :: m_au = 1.0_8/1822.88853_8
! 	real(8), public, parameter :: e_au = 1.0_8/4.55633538e-6
! 	real(8), public, parameter :: Eh = e_au
! 	real(8), public, parameter :: Ha = e_au
! end module CM1Units_

!!! Será renombrada UnitsManager
module UnitsConverter_
	use GOptions_
	use String_
! 	use StringRealPair_
! 	use StringRealMap_
	implicit none
	private
	
	public :: &
		sUnit, &
		UnitsConverter_test
	
	! -----------------
	! Esto no esta implementado
	! -----------------
	! Mass   => amu
	! Lenght => Å
	! Time   => 9.14294598 10⁻¹¹ s
	! Energy => cm⁻¹
	
	! -----------------
	! Atomic units
	! -----------------
	! Mass   => m_au
	! Lenght => bohr
	! Time   => t_au
	! Energy => Ha
	
! 	real(8), private :: LENGHT = 1.0_8
! 	real(8), private :: MASS = 1.0_8
! 	real(8), private :: ACTION = 1.0_8
! 	real(8), private :: CHARGE = 1.0_8
	
	integer, private, parameter :: UnitsConverter_NUNITS = 19
	
	character(10), private, parameter :: UnitsConverter_NAME(UnitsConverter_NUNITS) = [ &
		"amu", &
		"angs", &
		"eV", &
		"cm1", &
		"ryd", &
		"kcalMol", &
		"kJMol", &
		"bohr", &
		"g", &
		"kg", &
		"au", &
		"Eh", &
		"Ha", &
		"deg", &
		"kelvin", &
		"fs", &
		"ps", &
		"nm", &
		"microm" &
	]
	
	real(8), private, parameter :: UnitsConverter_VALUE(UnitsConverter_NUNITS) = [ &
		1822.88853_8, &                            ! amu
		1.0_8/0.52917726_8, &                      ! angs
		1.0_8/27.211396132_8, &                    ! eV
		1.0_8/219474.63068_8, &                    ! cm1
		1.0_8/2.0_8, &                             ! ryd
		1.0_8/627.50956_8, &                       ! kcalMol
		1.0_8/2625.49962_8, &                      ! kJMol
		1.0_8, &                                   ! bohr
		1.0_8/1.660538782e-24, &                   ! g    <<<<<<<<<<< Verificar
		1.0_8/1.660538782e-27, &                   ! kg   <<<<<<<<<<< Verificar
		1.0_8, &                                   ! au
		1.0_8, &                                   ! Eh
		1.0_8, &                                   ! Ha
		acos(-1.0_8)/180.0_8, &                    ! deg
		1.0/3.1577464d5, &                         ! kelvin
! 		1.0/0.02418884326505_8 &                   ! fs
		1.0d-15*1.0/2.418884326505d-17, &          ! fs
		1.0d-12*1.0/2.418884326505d-17, &          ! ps
		10.0_8/0.52917726_8, &                     ! nm
		1.0d3*10.0_8/0.52917726_8 &                ! microm
	]

	real(8), public, parameter :: amu       =   UnitsConverter_VALUE(1)
	real(8), public, parameter :: angs      =   UnitsConverter_VALUE(2)
	real(8), public, parameter :: eV        =   UnitsConverter_VALUE(3)
	real(8), public, parameter :: cm1       =   UnitsConverter_VALUE(4)
	real(8), public, parameter :: ryd       =   UnitsConverter_VALUE(5)
	real(8), public, parameter :: kcalMol   =   UnitsConverter_VALUE(6)
	real(8), public, parameter :: kJMol     =   UnitsConverter_VALUE(7)
	real(8), public, parameter :: bohr      =   UnitsConverter_VALUE(8)
	real(8), public, parameter :: g         =   UnitsConverter_VALUE(9)
	real(8), public, parameter :: kg        =   UnitsConverter_VALUE(10)
	real(8), public, parameter :: au        =   UnitsConverter_VALUE(11)
	real(8), public, parameter :: Eh        =   UnitsConverter_VALUE(12)
	real(8), public, parameter :: Ha        =   UnitsConverter_VALUE(13)
	real(8), public, parameter :: deg       =   UnitsConverter_VALUE(14)
	real(8), public, parameter :: kelvin    =   UnitsConverter_VALUE(15)
	real(8), public, parameter :: fs        =   UnitsConverter_VALUE(16)
	real(8), public, parameter :: ps        =   UnitsConverter_VALUE(17)
	real(8), public, parameter :: nm        =   UnitsConverter_VALUE(18)
	real(8), public, parameter :: microm    =   UnitsConverter_VALUE(19)
	
	contains
	
	!>
	!! @brief Constructor
	!!
	function sUnit( name ) result( output )
		character(*) :: name
		real(8) :: output
		
		integer :: i
		
		do i=1,UnitsConverter_NUNITS
			if ( trim(name) == trim(UnitsConverter_NAME(i)) ) then
				output = UnitsConverter_VALUE(i)
				return
			end if
		end do
		
		if( len_trim(name) > 0 ) call GOptions_warning( "Unknown unit ("//trim(name)//")", "UnitsConverter.sUnit()" )
		
		output = 1.0_8
	end function sUnit
	
	!>
	!! @brief Test method
	!!
	subroutine UnitsConverter_test()
! 		write(*,*) "         Lenght = ", 0.528e-8*cm
! 		write(*,*) "       Velocity = ", 2.18e8*cm/sec
		write(*,*) "         Energy = ", 27.21*eV
! 		write(*,*) "           Time = ", 24.2*asec
		write(*,*) "           Mass = ", 9.10938188e-31*kg
! 		write(*,*) "      Frequency = ", 4.13e16/sec
! 		write(*,*) " Electric field = ", 5.14e9*Volt/cm
! 		write(*,*) " Magnetic field = ", 2.35e5*Tesla
! 		write(*,*) "      Frequency = ", 6.6e-7*eV/GHz
! 		write(*,*) "Laser Intensity = ", 3.51e16*Wcm2
		write(*,*) ""
		write(*,*) "Lennard Jones units (argon)"
! 		write(*,*) "     Temperature = ", 120*K/LJ_T
! 		write(*,*) "Angular momentum = ", 0.02944*LJ_j
	end subroutine UnitsConverter_test
	
end module UnitsConverter_

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

!>
!! @brief Test program
!!
program main
	use UnitsConverter_
	use Math_
	use String_
	use RandomUtils_
	use Matrix_
	use CommandLineParser_
	use Atom_
	use Molecule_
	implicit none
	
	type(CommandLineParser) :: parser
	
	type(Molecule) :: mol
	real(8) :: totalVelocity(3)
	real(8), allocatable :: velocities(:,:)
	real(8) :: totalLinearMomentum(3)
	real(8) :: totalAngularMomentum(3)
	real(8) :: normL
	
	real(8) :: inertiaMoment
	real(8) :: angularVelocity(3)
	
	type(String) :: iFileName
	real(8) :: userEnergy
	integer :: userNTrials
	integer :: userSeed
	real(8) :: userTol
	type(String) :: oXYZFileName
	type(String) :: oVXYZFileName
	type(String) :: oSHARCFileName
	
	logical :: debug
	real(8) :: energy
	integer :: i, k, m
	
	
	if( command_argument_count() < 4 ) then
		write(*,"(A)") "NAME"
		write(*,"(A)") "       molecule.vrandom - Generates a set of random velocity vectors"
		write(*,"(A)") ""
		write(*,"(A)") "SYNOPSIS"
		write(*,"(A)") "       molecule.vrandom [ OPTIONS ] -i file -E energy"
		write(*,"(A)") ""
		write(*,"(A)") "DESCRIPTION"
		write(*,"(A)") "       Generates a set of random velocity vectors which they satisfy the constraints P=0 and L=0 for a given kinetic energy E."
		write(*,"(A)") ""
! 		write(*,"(A)") "AUTHOR"
! 		write(*,"(A)") "       Written by Nestor F. Aguirre"
! 		write(*,"(A)") ""
		write(*,"(A)") "OPTIONS"
		write(*,"(A)") "       molecule.vrandom accepts the following options."
		write(*,"(A)") ""
		write(*,"(A)") "       -i file"
		write(*,"(A)") "              Read the molecular geometry from the xyz file format"
		write(*,"(A)") ""
		write(*,"(A)") "       -E energy"
		write(*,"(A)") "              Assign the value of the kinetic energy E in hartrees."
		write(*,"(A)") "              Velocities are generated such E = sum_{i=1}^n 0.5*m_i*v_i**2"
		write(*,"(A)") ""
		write(*,"(A)") "       -s seed"
		write(*,"(A)") "              Establish the random seed"
		write(*,"(A)") ""
		write(*,"(A)") "       -t tolerance"
		write(*,"(A)") "              Tolerance for P=0 and L=0"
		write(*,"(A)") ""
		write(*,"(A)") "       -o file"
		write(*,"(A)") "              Saves the centered and oriented geometry according to the main inertia moments. The geometry will saved"
		write(*,"(A)") "              in a file format according with its extension. Some supported extensions are: .xyz and .molden."
		write(*,"(A)") ""
		write(*,"(A)") "       -v file"
		write(*,"(A)") "              Saves random velocities following the xyz file format"
		write(*,"(A)") ""
		write(*,"(A)") "       -d debug"
		write(*,"(A)") "              Turn on debugging output"
		write(*,"(A)") ""
		write(*,"(A)") "       -sharc file"
		write(*,"(A)") "              Random velocities and molecular geometry will be saved in SHARC format file"
		write(*,"(A)") "              ( see http://sharc-md.org )"
		write(*,"(A)") ""
		stop
	end if
	
	iFileName = parser.getString( "-i" )
	userEnergy = parser.getReal( "-E" )
! 	userNTrials = parser.getInteger( "-n", def=10000 )
	userSeed = parser.getInteger( "-s", def=-1 )
	userTol = parser.getReal( "-t", def=1d-10 )
	debug = parser.getLogical( "-d", def=.false. )
	oXYZFileName = parser.getString( "-o", def=FString_NULL )
	oVXYZFileName = parser.getString( "-v", def=FString_NULL )
	oSHARCFileName = parser.getString( "-sharc", def=FString_NULL )
	
	if( debug ) then
		write(*,*) ""
		write(*,*) "Input file = ", trim(iFileName.fstr)
		write(*,*) "    energy = ", userEnergy
! 		write(*,*) "    trials = ", userNTrials
		write(*,*) "      seed = ", userSeed
		write(*,*) " tolerance = ", userTol
		write(*,*) "     debug = ", debug
		
		if( oXYZFileName /= FString_NULL ) &
			write(*,*) "Output geometry file = ", trim(oXYZFileName.fstr)
		if( oVXYZFileName /= FString_NULL ) &
			write(*,*) "Output velocity file = ", trim(oVXYZFileName.fstr)
		if( oSHARCFileName /= FString_NULL ) &
			write(*,*) "Output file = ", trim(oSHARCFileName.fstr)
		write(*,*) ""
	end if
	
	mol = Molecule( iFileName.fstr )
	call mol.orient()
	
	allocate( velocities(mol.nAtoms(),3) )
	
	call initRandomNumbers( userSeed )
	
	call randomVelocities( 10000, userEnergy )
	call updateTotalMomentum()
	if( debug ) call showVelocities()
	
	if( debug ) then
		write(*,*) ""
		write(*,"(A20,2A20)") "iter", "LinearMomentum", "AngularMomentum"
		write(*,"(A20,2A20)") "----", "--------------", "---------------"
		write(*,"(I20,2F20.10)") 0, norm2(totalLinearMomentum), norm2(totalAngularMomentum)
	end if
	
	m=1
	do while( .true. )
! 		!--------------------------------------------
! 		
! 		totalVelocity = totalLinearMomentum/mol.mass()
! 
! 		do i=1,mol.nAtoms()
! 			velocities(i,:) = velocities(i,:) - totalVelocity(:)
! 		end do
! 		
! 		! Escala las velocidades
! 		energy = getEnergy()
! 		do i=1,mol.nAtoms()
! 			velocities(i,:) = velocities(i,:)*sqrt(userEnergy/energy)
! 		end do
! 		
! 		call updateTotalMomentum()
! 		if( debug ) call showVelocities()
! 		
! 		if( debug ) write(*,"(I20,2F20.10)") m, norm2(totalLinearMomentum), norm2(totalAngularMomentum)
! 		m=m+1
! 		
! 		!--------------------------------------------
! 		
! 		do i=1,mol.nAtoms()
! 			angularVelocity = crossProduct( mol.atoms(i).r, velocities(i,:) )/sum(mol.atoms(i).r**2)
! 			velocities(i,:) = velocities(i,:) - crossProduct( angularVelocity, mol.atoms(i).r )
! 		end do
! 		
! 		! Escala las velocidades
! 		energy = getEnergy()
! 		do i=1,mol.nAtoms()
! 			velocities(i,:) = velocities(i,:)*sqrt(userEnergy/energy)
! 		end do
! 		
! 		call updateTotalMomentum()
! 		if( debug ) call showVelocities()
! 		
! 		if( debug ) write(*,"(I20,2F20.10)") m, norm2(totalLinearMomentum), norm2(totalAngularMomentum)
! 		m=m+1
! 		
! 		!--------------------------------------------
		
		
		!--------------------------------------------
		
		do i=1,mol.nAtoms()
			angularVelocity = crossProduct( mol.atoms(i).r, velocities(i,:) )/sum(mol.atoms(i).r**2)
			velocities(i,:) = velocities(i,:) - crossProduct( angularVelocity, mol.atoms(i).r )
		end do
		
		! Escala las velocidades
		energy = getEnergy()
		do i=1,mol.nAtoms()
			velocities(i,:) = velocities(i,:)*sqrt(userEnergy/energy)
		end do
		
		call updateTotalMomentum()
		if( debug ) call showVelocities()
		
		if( debug ) write(*,"(I20,2F20.10)") m, norm2(totalLinearMomentum), norm2(totalAngularMomentum)
		m=m+1
		
		!--------------------------------------------
		
		totalVelocity = totalLinearMomentum/mol.mass()

		do i=1,mol.nAtoms()
			velocities(i,:) = velocities(i,:) - totalVelocity(:)
		end do
		
		! Escala las velocidades
		energy = getEnergy()
		do i=1,mol.nAtoms()
			velocities(i,:) = velocities(i,:)*sqrt(userEnergy/energy)
		end do
		
		call updateTotalMomentum()
		if( debug ) call showVelocities()
		
		if( debug ) write(*,"(I20,2F20.10)") m, norm2(totalLinearMomentum), norm2(totalAngularMomentum)
		m=m+1
		
		!--------------------------------------------
		
		if( norm2(totalLinearMomentum) < userTol .and. norm2(totalAngularMomentum) < userTol ) exit
	end do
	
	if( debug ) then
		write(*,*) ""
		write(*,*) "**************************************"
		write(*,*) " SUMMARY"
		write(*,*) "**************************************"
		call showVelocities()
		
		write(*,*) ""
		write(*,*) "**************************************"
		write(*,*) " GEOMETRY"
		write(*,*) "**************************************"
		call mol.save()
	end if
	
	if( oXYZFileName /= FString_NULL ) then
		call mol.save( oXYZFileName.fstr )
	end if
	
	if( oVXYZFileName /= FString_NULL ) then
		open( 1, file=oVXYZFileName.fstr )
		write(1,*) mol.nAtoms()
		write(1,"(A)") "Generated velocities by molecule.vrandom from "//trim(mol.name)
		do i=1,mol.nAtoms()
			write(1,"(A5,3F20.8)") trim(mol.atoms(i).symbol), velocities(i,:)
		end do
		write(1,"(A)") ""
		close(1)
	end if
	
	if( oSHARCFileName /= FString_NULL ) then
		open( 1, file=oSHARCFileName.fstr )
		
		do i=1,mol.nAtoms()
			write(1,"(A5,F5.1,3F15.8,F15.5)") trim(mol.atoms(i).symbol), mol.atoms(i).atomicNumber(), mol.atoms(i).r, mol.atoms(i).mass()/amu
		end do
		
		write(1,"(A)") ""
		
		do i=1,mol.nAtoms()
			write(1,"(3F20.8)") velocities(i,:)
		end do
		
		close(1)
	end if
	
	deallocate( velocities )
	
	contains
	
	!>
	!! @brief 
	!!
	subroutine initRandomNumbers( seed )
		integer, intent(in) :: seed
		
		integer :: clock
		integer, allocatable :: seedVec(:)
		real(8) :: randNumber
		integer :: n

		if( seed == -1 ) then
			call random_seed(size = n)
			allocate(seedVec(n))
			call system_clock(count=clock)
			seedVec = clock + 37 * (/ (i - 1, i = 1, n) /)
			call random_seed(put = seedVec)
			deallocate(seedVec)
			
			call random_number( randNumber ) !<- Desechamos el primer número de la serie, ya que parece que es muy regular respecto al tiempo
		else
			call random_seed(size = n)
			allocate(seedVec(n))
			seedVec = seed + 37 * (/ (i - 1, i = 1, n) /)
			call random_seed(put = seedVec)
			deallocate(seedVec)
			
			call random_number( randNumber ) !<- Desechamos el primer número de la serie, ya que parece que es muy regular respecto al tiempo
		end if
	end subroutine initRandomNumbers
	
	!>
	!! @brief 
	!!
	subroutine randomVelocities( nTrials, totalEnergy )
		integer, intent(in) :: nTrials
		real(8), intent(in) :: totalEnergy
		
		real(8), allocatable :: energies(:)
		real(8) :: randNumber
		real(8) :: r, theta, phi
		integer :: n
		
		allocate( energies(mol.nAtoms()) )
		
! 		do n=1,nTrials
! 			energies = 0.0_8
! 			do i=1,mol.nAtoms()-1
! 				call random_number( randNumber ) ! [0,1]
! 				energies(i) = randNumber*totalEnergy
! 			end do
! 			
! 			if( sum(energies(1:mol.nAtoms()-1)) < totalEnergy ) then
! 				exit
! 			end if
! 		end do
! 		energies( mol.nAtoms() ) = totalEnergy-sum(energies(1:mol.nAtoms()-1))

		energies = 0.0_8
		do i=1,mol.nAtoms()
			call random_number( randNumber ) ! [0,1]
			energies(i) = randNumber*( totalEnergy - merge( sum(energies(1:i-1)), 0.0_8, i>1 ) )
		end do
		
		call RandomUtils_randomizeVector( energies )
		
		if( debug ) then
			write(*,*) ""
			write(*,*) " Energies"
			write(*,*) "----------"
			write(*,*) ""
! 			write(*,*) "nTrials = ", n-1
			write(*,*) "totalEnergy = ", totalEnergy
			write(*,*) ""
			
! 			write(*,*) "masas"
			do i=1,mol.nAtoms()
				write(*,"(5X,I3,A10,F20.5,F10.5)") i, trim(mol.atoms(i).symbol), mol.atoms(i).mass(), energies(i)
! 				write(*,*) trim(mol.atoms(i).symbol), mol.atoms(i).mass()
			end do
			
			write(*,"(8X,30X,A10)") "----------"
			write(*,"(8X,30X,F10.5)") sum(energies)
			write(*,"(8X,A)") ""
		end if
		
		do i=1,mol.nAtoms()
			r =sqrt( 2.0_8*energies(i)/mol.atoms(i).mass() )
			
			call random_number( randNumber ) ! [0,1]
			theta = randNumber*Math_PI
			
			call random_number( randNumber ) ! [0,1]
			phi = randNumber*2.0_8*Math_PI
			
			velocities(i,1) = r*cos(phi)*sin(theta)
			velocities(i,2) = r*sin(phi)*sin(theta)
			velocities(i,3) = r*cos(theta)
		end do
		
		deallocate( energies )
	end subroutine randomVelocities
	
	!>
	!! @brief 
	!!
	function getEnergy() result ( output )
		real(8) :: output
		
		output = 0.0_8
		do i=1,mol.nAtoms()
			output = output + sum(0.5_8*mol.atoms(i).mass()*velocities(i,:)**2)
		end do
	end function getEnergy
	
	!>
	!! @brief 
	!!
	function crossProduct( a, b ) result( c )
		real(8), intent(in) :: a(3)
		real(8), intent(in) :: b(3)
		real(8) :: c(3)
		
		c = [ a(2)*b(3)-a(3)*b(2), a(3)*b(1)-a(1)*b(3), a(1)*b(2)-a(2)*b(1) ]
	end function crossProduct
	
	!>
	!! @brief 
	!!
	subroutine updateTotalMomentum()
		totalLinearMomentum = 0.0_8
		do i=1,mol.nAtoms()
			totalLinearMomentum = totalLinearMomentum + velocities(i,:)*mol.atoms(i).mass()
		end do
		
		totalAngularMomentum = 0.0_8
		do i=1,mol.nAtoms()
			totalAngularMomentum = totalAngularMomentum + crossProduct( mol.atoms(i).r, mol.atoms(i).mass()*velocities(i,:) )
		end do
	end subroutine updateTotalMomentum
	
	!>
	!! @brief 
	!!
	subroutine showVelocities()
		real(8) :: ssum(3)
		
		write(*,*) ""
		write(*,*) " Velocities"
		write(*,*) "------------"
		write(*,"(5X,A5,3A15)") "sym","x", "y", "z"
		
		ssum = 0.0_8
		do i=1,mol.nAtoms()
			write(*,"(5X,A5,3F15.5)") trim(mol.atoms(i).symbol), velocities(i,:)
			ssum = ssum + velocities(i,:)
		end do
		write(*,"(5X,5X,3A15)") "---------", "---------", "---------"
		write(*,"(5X,5X,3F15.5)") ssum
		write(*,*) ""
		write(*,*) " Total linear momentum"
		write(*,*) "-----------------------"
		write(*,"(5X,3A15,A20)") "x", "y", "z", "|P|"
		write(*,"(5X,3F15.5,F20.5)") totalLinearMomentum, norm2(totalLinearMomentum)
		
		write(*,*) ""
		write(*,*) " Total angular momentum"
		write(*,*) "------------------------"
		write(*,"(5X,3A15,A20)") "x", "y", "z", "|L|"
		write(*,"(5X,3F15.5,F20.5)") totalAngularMomentum, norm2(totalAngularMomentum)
		
		write(*,*) ""
		write(*,*) " Energy = ", getEnergy()
		
	end subroutine showVelocities

end program main

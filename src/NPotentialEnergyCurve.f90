!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                                                                                   !!
!!  This file is part of SciFT project                                               !!
!!  Copyright (c) 2010-2013 Nestor F. Aguirre (nfaguirrec@gmail.com)                 !!
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

module NPotentialEnergyCurve_
	use UnitsConverter_
	use Grid_
	use RNFunction_
	use Spline_
	use Morse_
	use ThrularNumerovMethod_
	implicit none
	private 
	
	public :: &
		NPotentialEnergyCurve_test
		
	type, public, extends ( RNFunction ) :: NPotentialEnergyCurve
		type(RNFunction) :: rawData
		
		contains
			generic :: init => initDefault
			procedure :: initDefault
			procedure :: parent
! 			procedure :: destroy
! 			procedure :: str
! 			procedure :: show
			procedure :: run
	end type NPotentialEnergyCurve
	
	interface
		function prototypeFunction( x ) result( output )
				real(8), intent(in) :: x
				real(8) :: output
		end function prototypeFunction
	end interface
	
	procedure(prototypefunction) :: longrange
	procedure(prototypefunction) :: veryshortrange
	
	! límites de la grid inicial
	real(8) :: firstRawData(2)
	real(8) :: lastRawData(2)
		
	contains
	
	subroutine initDefault( this, rawData )
		implicit none
		class(NPotentialEnergyCurve) :: this
		type(RNFunction), intent(in) :: rawData
		
		this.rawData = rawData
	end subroutine initDefault
	
	function parent( this ) result( output )
		implicit none
		class(NPotentialEnergyCurve) :: this
		type(RNFunction) :: output
		
		call output.fromGridArray( this.xGrid, this.fArray )
	end function parent
	
	subroutine run( this, rGrid, longRangeF, veryShortRangeF )
		class(NPotentialEnergyCurve) :: this
		type(Grid), intent(in) :: rGrid
		procedure(prototypeFunction), optional :: longRangeF
		procedure(prototypeFunction), optional :: veryShortRangeF
		
		type(Spline) :: spline
		real(8) :: xi
		real(8), allocatable :: y(:)
		integer :: i
		
		call spline.init( this.rawData )
		allocate( y(rGrid.nPoints) )
		
		firstRawData = [ this.rawData.xGrid.data(1), this.rawData.fArray(1) ]
		lastRawData = [ this.rawData.xGrid.data(this.rawData.nPoints()), this.rawData.fArray(this.rawData.nPoints()) ]
		
		do i=1,rGrid.nPoints
		
			xi = rGrid.data(i)
			
			if( xi < firstRawData(1) ) then
				if( present(veryShortRangeF) ) then
					y(i) = veryShortRangeF( xi )
				else
					y(i) = veryShortRangeDefault( xi )
				end if
			else if( xi >= firstRawData(1) .and. xi<= lastRawData(1) ) then
				y(i) = spline.evaluate(xi)
			else if( xi > lastRawData(1) ) then
				if( present(longRangeF) ) then
					y(i) = longRangeF( xi )
				else
					write(*,*) "@@ WARNING @@ Long range default function is not right implemented"
					y(i) = longRangeDefault( xi )
				end if
			end if
			
		end do
		
		call this.fromGridArray( rGrid, y )
	end subroutine run
	
	!>
	!! This is neccesary only for NPotentialEnergyCurve_test()
	!!
	function shortRangeDefault( x ) result( output )
		real(8), intent(in) :: x
		real(8) :: output
		
		real(8) :: Re, we, wexe, rMass
		real(8) :: De, alpha
		
		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		!! Cl2 X¹Σg+ 
		!! NIST Standard Reference Data Program
		!! -------------------------------------
		!! re    = 1.9879 Å
		!! ωe    = 559.72 cm⁻¹
		!! ωexe  = 2.675 cm⁻¹
		!! m(Cl) = 35.4257 amu
		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		Re = 1.9879_8*angs
		we = 559.72_8*cm1
		wexe = 2.675_8*cm1
		rMass = 0.5_8*35.4257_8*amu
		
		De = 0.25_8*we**2.0_8/wexe
		alpha = we*sqrt(0.5*rMass/De)
		
		output = De*( exp(2.0_8*alpha*(Re-x)) &
				- 2.0_8*exp(alpha*(Re-x)) )
	end function shortRangeDefault
	
	!>
	!! This is neccesary only for NPotentialEnergyCurve_test()
	!!
	function veryShortRangeDefault( x ) result( output )
		real(8), intent(in) :: x
		real(8) :: output
		
		output = shortRangeDefault( 2.5_8 )
	end function veryShortRangeDefault
	
	!>
	!! This is neccesary only for NPotentialEnergyCurve_test()
	!!
	function longRangeDefault( x ) result( output )
		real(8), intent(in) :: x
		real(8) :: output
		
		real(8) :: c6 = -3.66827_8
		real(8) :: c8 = 3.65743_8
		
		output = sign(1.0,c6)*(c6/x)**6.0_8+sign(1.0,c8)*(c8/x)**8.0_8
	end function longRangeDefault
	
	!>
	!! @todo Hay que revisar que el valor del estado ligado es correcto
	!!
	subroutine NPotentialEnergyCurve_test()
		implicit none
		type(Grid) :: rGrid
		type(Grid) :: finalGrid
		type(RNFunction) :: rawCurve
		type(NPotentialEnergyCurve) :: pECurve
		type(ThrularNumerovMethod) :: solver
		
		integer :: i
		real(8) :: rMass = 30.0_8*0.5_8*35.4257_8*amu
		
		call rGrid.fromFile( "data/formats/GRID2D" )
! 		call rGrid.init( 2.5_8, 8.0_8, size=20 )
		call rGrid.show()
		
		call rawCurve.fromFunction( rGrid, shortRangeDefault )
		call rawCurve.save("rawCurve")
		
		call finalGrid.init( 1.0_8, 1000.0_8, nPoints=100000 )
		
		call pECurve.init( rawCurve )
		call pECurve.run( finalGrid, longRangeDefault, veryShortRangeDefault )
		call pECurve.show()
		call pECurve.save("pECurve")
		call pECurve.setUnits( [angs,cm1] )
		
		call solver.init( pECurve, rMass=rMass )
		call solver.run()
		
		do i=1,solver.nStates
				write(*,"(i5,f20.10)") i, solver.eigenValues(i)/cm1
		end do
		
! 		call solver.eigenfunction(1).save( "solverWF1", units=[angs,1.0_8] )
		
		call solver.destroy()
	end subroutine NPotentialEnergyCurve_test
	
end module NPotentialEnergyCurve_

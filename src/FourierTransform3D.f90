!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                                                                                   !!
!!  This file is part of SciFT project                                               !!
!!  Copyright (c) 2015-2015 Nestor F. Aguirre (nfaguirrec@gmail.com)                 !!
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

module FourierTransform3D_
	use FFTW3_
	use GOptions_
	use Math_
	use Grid3D_
	use CNFunction3D_
	use RNFunction3D_
	use RandomUtils_
	use FourierTransform_
	use FourierTransform2D_
	implicit none
	private
	
	public :: &
		FourierTransform3D_omegaGrid, &
		FourierTransform3D_xyzGrid, &
		FourierTransform3D_phase, &
		FourierTransform3D_iphase, &
		FourierTransform3D_shift, &
		FourierTransform3D_ishift, &
		FourierTransform3D_plan, &
		FourierTransform3D_execute, &
		FourierTransform3D_destroyPlan, &
		FourierTransform3D_dft, &
		FourierTransform3D_idft, &
		FourierTransform3D_fft, &
		FourierTransform3D_ifft, &
		FourierTransform3D_nft, &
		FourierTransform3D_inft, &
		FourierTransform3D_test
		
	type, public :: FourierTransform3D
		class(CNFunction3D), pointer, private :: iFunc
		integer(8), private :: planF, planB
		
		integer :: nPoints(3)
		type(Grid3D) :: x
		type(Grid3D) :: omega
		
		contains
			procedure :: init
			final :: destroy
			procedure :: str
			procedure :: show
			procedure :: execute
! 			procedure :: filter
	end type FourierTransform3D
	
    interface FourierTransform3D_omegaGrid
		module procedure FourierTransform3D_omegaGridFromData
		module procedure FourierTransform3D_omegaGridFromXGrid
    end interface FourierTransform3D_omegaGrid

    interface FourierTransform3D_xyzGrid
		module procedure FourierTransform3D_xyzGridFromData
		module procedure FourierTransform3D_xyzGridFromOmegaGrid
    end interface FourierTransform3D_xyzGrid

    interface FourierTransform3D_phase
        module procedure FourierTransform3D_phase_realArray
        module procedure FourierTransform3D_phase_complexArray
        module procedure FourierTransform3D_phase_RNFunction3D
        module procedure FourierTransform3D_phase_CNFunction3D
    end interface FourierTransform3D_phase
    
    interface FourierTransform3D_iphase
        module procedure FourierTransform3D_iphase_realArray
        module procedure FourierTransform3D_iphase_complexArray
        module procedure FourierTransform3D_iphase_CNFunction3D
    end interface FourierTransform3D_iphase
	
    interface FourierTransform3D_shift
        module procedure FourierTransform3D_shift_Grid
        module procedure FourierTransform3D_shift_RNFunction3D
        module procedure FourierTransform3D_shift_CNFunction3D
    end interface FourierTransform3D_shift

    interface FourierTransform3D_ishift
        module procedure FourierTransform3D_ishift_Grid
        module procedure FourierTransform3D_ishift_CNFunction3D
    end interface FourierTransform3D_ishift

    interface FourierTransform3D_plan
        module procedure FourierTransform3D_plan_Array
        module procedure FourierTransform3D_plan_CNFunction3D
    end interface FourierTransform3D_plan
    
    interface FourierTransform3D_dft
        module procedure FourierTransform3D_dft_CArray
    end interface FourierTransform3D_dft
    
    interface FourierTransform3D_idft
        module procedure FourierTransform3D_idft_Array
    end interface FourierTransform3D_idft

    interface FourierTransform3D_fft
        module procedure FourierTransform3D_fft_CNFunction3D
    end interface FourierTransform3D_fft

    interface FourierTransform3D_ifft
        module procedure FourierTransform3D_ifft_CNFunction3D
    end interface FourierTransform3D_ifft

    interface FourierTransform3D_nft
        module procedure FourierTransform3D_nft_CNFunction3D
    end interface FourierTransform3D_nft

    interface FourierTransform3D_inft
        module procedure FourierTransform3D_inft_CNFunction3D
    end interface FourierTransform3D_inft
	
	contains
	
	!>
	!! @brief Constructor
	!! @todo En algunas ocasiones es importante hacer la transfomada de no todos los puntos
	!!
	subroutine init( this, iFunc, domain, oFunc )
		class(FourierTransform3D) :: this 
		class(CNFunction3D), target, intent(in) :: iFunc
		integer(8), intent(in), optional :: domain
		class(CNFunction3D), target, optional, intent(in) :: oFunc
		
		integer(8) :: effDomain
		
		integer :: nx, ny, nz
		
		if( associated(this%iFunc) ) nullify(this%iFunc)
		this%iFunc => iFunc
		this%nPoints = iFunc%nPoints() ! << Hay que cambiar para hacer la transformada de algunos puntos
		
		nx = this%nPoints(1)
		ny = this%nPoints(2)
		nz = this%nPoints(3)
		
		effDomain = FourierTransform_SPATIAL_DOMAIN
		if( present(domain) ) effDomain = domain
		
		if( present(oFunc) ) call GOptions_error( "oFunc parameter is not implamented yet", "FourierTransform3D%init()" )
		
		call dfftw_plan_dft_3d( this%planF, nx, ny, nz, this%iFunc%fArray, this%iFunc%fArray, FFTW_FORWARD, FFTW_ESTIMATE )
		call dfftw_plan_dft_3d( this%planB, nx, ny, nz, this%iFunc%fArray, this%iFunc%fArray, FFTW_BACKWARD, FFTW_ESTIMATE )
			
		if( effDomain == FourierTransform_SPATIAL_DOMAIN ) then
			
			this%x = this%iFunc%xyzGrid ! << Hay que cambiar para hacer la transformada de algunos puntos
			this%omega = FourierTransform3D_omegaGrid( this%x )
			
		else if( effDomain == FourierTransform_FREQUENCY_DOMAIN ) then
			
			call GOptions_warning( "FourierTransform_FREQUENCY_DOMAIN have not been tested yet", "FourierTransform%init()" )
			this%omega = this%iFunc%xyzGrid ! << Hay que cambiar para hacer la transformada de algunos puntos
			this%x = FourierTransform3D_xyzGrid( this%omega )
			
		end if
	end subroutine init
	
	!>
	!! @brief Destructor
	!!
	subroutine destroy( this )
		type(FourierTransform3D) :: this
		
		! Si los activo el programa produce:
		! 	forrtl: error (76): Abort trap signal
! 		call dfftw_destroy_plan( this%planF )
! 		call dfftw_destroy_plan( this%planB )
		
		nullify(this%iFunc)
		
		this%nPoints = -1
	end subroutine destroy
	
	!>
	!! @brief
	!!
	function str( this ) result( output )
		class(FourierTransform3D) :: this 
		character(len=200) :: output
		
		integer :: fmt
		character(len=200) :: strBuffer
		
		output = ""
		
		output = trim(output)//"<FourierTransform3D:"
		
! 		output = trim(output)//"min="
! 		fmt = int(log10(this%min+1.0))+1
! 		write(strBuffer, "(f<fmt+7>.6)") this%min
! 		output = trim(output)//trim(strBuffer)
! 		
! 		output = trim(output)//",max="
! 		fmt = int(log10(this%max+1.0))+1
! 		write(strBuffer, "(f<fmt+7>.6)") this%max
! 		output = trim(output)//trim(strBuffer)
! 		
! 		output = trim(output)//",h="
! 		fmt = int(log10(this%h+1.0))+1
! 		write(strBuffer, "(f<fmt+7>.6)") this%h
! 		output = trim(output)//trim(strBuffer)
! 		
! 		output = trim(output)//",size="
! 		fmt = int(log10(float(this%size+1)))+1
! 		write(strBuffer, "(i<fmt>)") this%size
! 		output = trim(output)//trim(strBuffer)
		
		output = trim(output)//">"
	end function str
	
	!>
	!! @brief
	!!
	subroutine show( this, unit )
		class(FourierTransform3D) :: this
		integer, optional, intent(in) :: unit
		
		integer :: effunit
		
		if( present(unit) ) then
			effunit = unit
		else
			effunit = 6
		end if
		
		write(effunit,"(a)") trim(this%str())
	end subroutine show
	
	!>
	!! @brief
	!!
	subroutine execute( this, sgn, sync, shift )
		class(FourierTransform3D) :: this
		integer, intent(in) :: sgn
		logical, optional, intent(in) :: sync
		logical, optional, intent(in) :: shift
		
		logical :: effSync
		logical :: effShift
		
		effSync = .false.
		if( present(sync) ) effSync = sync
		
		effShift = .false.
		if( present(shift) ) effShift = shift
		
		if( sgn == FourierTransform_FORWARD ) then
		
			call dfftw_execute( this%planF )
			
			if( effSync ) then
				this%iFunc%xyzGrid = this%omega
				
				if( effShift ) then
					call FourierTransform3D_phase( this%iFunc )
					call FourierTransform3D_shift( this%iFunc )
				end if
			end if
			
		else if ( sgn == FourierTransform_BACKWARD ) then
		
			if( effSync ) then
				if( effShift ) then
					call FourierTransform3D_ishift( this%iFunc )
					call FourierTransform3D_phase( this%iFunc )
				end if
			end if
			
			call dfftw_execute( this%planB )
			
			if( effSync ) then
				this%iFunc%xyzGrid = this%x
			end if
			
			this%iFunc = this%iFunc/real(this%nPoints(1)*this%nPoints(2)*this%nPoints(3),8)
			
		else
			call GOptions_error( "Bad value for sgn", "FourierTransform3D.execute()" )
		end if
	end subroutine execute
	
	!>
	!! @brief
	!!
	function FourierTransform3D_omegaGridFromData( n, h, order ) result( output )
		integer, intent(in) :: n(3)
		real(8), optional, intent(in) :: h(3)
		integer, optional, intent(in) :: order
		type(Grid3D) :: output
		
		integer :: i
		
		do i=1,3
			output%component(i) = FourierTransform_omegaGrid( n(i), h(i), order=order )
		end do
	end function FourierTransform3D_omegaGridFromData
	
	!>
	!! @brief
	!!
	function FourierTransform3D_omegaGridFromXGrid( xyzGrid, order ) result( output )
		type(Grid3D), intent(in) :: xyzGrid
		integer, optional, intent(in) :: order
		type(Grid3D) :: output
		
		output = FourierTransform3D_omegaGridFromData( xyzGrid%nPointsVec(), xyzGrid%stepSize(), order=order )
	end function FourierTransform3D_omegaGridFromXGrid
	
	!>
	!! @brief
	!!
	function FourierTransform3D_xyzGridFromData( n, h, order ) result( output )
		integer, intent(in) :: n(3)
		real(8), intent(in) :: h(3)
		integer, optional, intent(in) :: order
		type(Grid3D) :: output
		
		integer :: i
		
		do i=1,3
			output%component(i) = FourierTransform_xGrid( n(i), h(i), order=order )
		end do
	end function FourierTransform3D_xyzGridFromData
	
	!>
	!! @brief
	!!
	function FourierTransform3D_xyzGridFromOmegaGrid( omegaGrid, order ) result( output )
		type(Grid3D), intent(in) :: omegaGrid
		integer, optional, intent(in) :: order
		type(Grid3D) :: output
		
		real(8) :: dx(3)
		
		! dx = 2*pi/n/dp
		dx = 2.0_8*MATH_PI/real(omegaGrid%nPoints(),8)/omegaGrid%stepSize()
		
		output = FourierTransform3D_xyzGridFromData( omegaGrid%nPoints(), dx, order=order )
	end function FourierTransform3D_xyzGridFromOmegaGrid
	
	!>
	!! @brief
	!!
	subroutine FourierTransform3D_phase_realArray( iArray, oArray )
		real(8) :: iArray(:)
		real(8), optional :: oArray(:)
		
! 		integer :: i, j, n, p0
! 		
! 		n = size( iArray )
! 		p0 = floor( n/2.0_8 )+2
! 		
! 		i = 1
! 		do j=p0,n
! 			if( present(oArray) ) then
! 				oArray(i) = iArray(i)*(-1.0_8)**j
! 			else
! 				iArray(i) = iArray(i)*(-1.0_8)**j
! 			end if
! 			
! 			i = i+1
! 		end do
! 		
! 		do j=1,p0-1
! 			if( present(oArray) ) then
! 				oArray(i) = iArray(i)*(-1.0_8)**j
! 			else
! 				iArray(i) = iArray(i)*(-1.0_8)**j
! 			end if
! 			
! 			i = i+1
! 		end do
	end subroutine FourierTransform3D_phase_realArray
	
	!>
	!! @brief
	!!
	subroutine FourierTransform3D_phase_complexArray( iArray, oArray )
		complex(8) :: iArray(:)
		complex(8), optional :: oArray(:)
		
! 		integer :: i, j, n, p0
! 		
! 		n = size( iArray )
! 		p0 = floor( n/2.0_8 )+2
! 		
! 		i = 1
! 		do j=p0,n
! 			if( present(oArray) ) then
! 				oArray(i) = iArray(i)*(-1.0_8)**j
! 			else
! 				iArray(i) = iArray(i)*(-1.0_8)**j
! 			end if
! 			
! 			i = i+1
! 		end do
! 		
! 		do j=1,p0-1
! 			if( present(oArray) ) then
! 				oArray(i) = iArray(i)*(-1.0_8)**j
! 			else
! 				iArray(i) = iArray(i)*(-1.0_8)**j
! 			end if
! 			
! 			i = i+1
! 		end do
	end subroutine FourierTransform3D_phase_complexArray
	
	!>
	!! @brief
	!!
	subroutine FourierTransform3D_iphase_realArray( iArray, oArray )
		real(8) :: iArray(:)
		real(8), optional :: oArray(:)
		
! 		integer :: i, j, n, p2
! 		
! 		n = size( iArray )
! 		p2 = n-floor(n/2.0_8)
! 		
! 		i = 1
! 		do j=p2,n
! 			if( present(oArray) ) then
! 				oArray(i) = iArray(i)*(-1.0_8)**(j-1)
! 			else
! 				iArray(i) = iArray(i)*(-1.0_8)**(j-1)
! 			end if
! 			
! 			i = i+1
! 		end do
! 		
! 		do j=1,p2-1
! 			if( present(oArray) ) then
! 				oArray(i) = iArray(i)*(-1.0_8)**(j-1)
! 			else
! 				iArray(i) = iArray(i)*(-1.0_8)**(j-1)
! 			end if
! 			
! 			i = i+1
! 		end do
	end subroutine FourierTransform3D_iphase_realArray
	
	!>
	!! @brief
	!!
	subroutine FourierTransform3D_iphase_complexArray( iArray, oArray )
		complex(8) :: iArray(:)
		complex(8), optional :: oArray(:)
		
! 		integer :: i, j, n, p2
! 		
! 		n = size( iArray )
! 		p2 = n-floor(n/2.0_8)
! 		
! 		i = 1
! 		do j=p2,n
! 			if( present(oArray) ) then
! 				oArray(i) = iArray(i)*(-1.0_8)**(j-1)
! 			else
! 				iArray(i) = iArray(i)*(-1.0_8)**(j-1)
! 			end if
! 			
! 			i = i+1
! 		end do
! 		
! 		do j=1,p2-1
! 			if( present(oArray) ) then
! 				oArray(i) = iArray(i)*(-1.0_8)**(j-1)
! 			else
! 				iArray(i) = iArray(i)*(-1.0_8)**(j-1)
! 			end if
! 			
! 			i = i+1
! 		end do
	end subroutine FourierTransform3D_iphase_complexArray
	
	!>
	!! @brief
	!!
	subroutine FourierTransform3D_phase_RNFunction3D( iFunc, oFunc )
		class(RNFunction3D) :: iFunc
		class(RNFunction3D), optional :: oFunc
		
		integer :: i
		
		if( iFunc%isEquallyspaced() ) then
			call GOptions_warning( &
				"Grid should not be equally spaced", &
				"FourierTransform3D_phase_RNFunction3D()" &
			)
		end if
		
		if( present(oFunc) ) then
			oFunc = iFunc
			
			do i=1,iFunc%nPoints(1)
				call FourierTransform2D_phase( oFunc%fArray(i,:,:) )
			end do
			
			do i=1,iFunc%nPoints(2)
				call FourierTransform2D_phase( oFunc%fArray(:,i,:) )
			end do
			
			do i=1,iFunc%nPoints(3)
				call FourierTransform2D_phase( oFunc%fArray(:,:,i) )
			end do
		else
			do i=1,iFunc%nPoints(1)
				call FourierTransform2D_phase( iFunc%fArray(i,:,:) )
			end do
			
			do i=1,iFunc%nPoints(2)
				call FourierTransform2D_phase( iFunc%fArray(:,i,:) )
			end do
			
			do i=1,iFunc%nPoints(3)
				call FourierTransform2D_phase( iFunc%fArray(:,:,i) )
			end do
		end if
	end subroutine FourierTransform3D_phase_RNFunction3D
	
	!>
	!! @brief
	!!
	subroutine FourierTransform3D_phase_CNFunction3D( iFunc, oFunc )
		class(CNFunction3D) :: iFunc
		class(CNFunction3D), optional :: oFunc
		
		integer :: i
		
		if( iFunc%isEquallyspaced() ) then
			call GOptions_warning( &
				"Grid should not be equally spaced", &
				"FourierTransform3D_phase_CNFunction3D()" &
			)
		end if
		
		if( present(oFunc) ) then
			oFunc = iFunc
			
			do i=1,iFunc%nPoints(1)
				call FourierTransform2D_phase( oFunc%fArray(i,:,:) )
			end do
			
			do i=1,iFunc%nPoints(2)
				call FourierTransform2D_phase( oFunc%fArray(:,i,:) )
			end do
			
			do i=1,iFunc%nPoints(3)
				call FourierTransform2D_phase( oFunc%fArray(:,:,i) )
			end do
		else
			do i=1,iFunc%nPoints(1)
				call FourierTransform2D_phase( iFunc%fArray(i,:,:) )
			end do
			
			do i=1,iFunc%nPoints(2)
				call FourierTransform2D_phase( iFunc%fArray(:,i,:) )
			end do
			
			do i=1,iFunc%nPoints(3)
				call FourierTransform2D_phase( iFunc%fArray(:,:,i) )
			end do
		end if
	end subroutine FourierTransform3D_phase_CNFunction3D
	
	!>
	!! @brief
	!!
	subroutine FourierTransform3D_iphase_CNFunction3D( iFunc, oFunc )
		class(CNFunction3D) :: iFunc
		class(CNFunction3D), optional :: oFunc
		
! 		if( .not. iFunc%isEquallyspaced() ) then
! 			call GOptions_warning( &
! 				"Grid should be equally spaced", &
! 				"FourierTransform3D_ishift_Grid()" &
! 			)
! 		end if
! 		
! 		if( present(oFunc) ) then
! 			oFunc = iFunc
! 			
! 			call FourierTransform3D_iphase_complexArray( oFunc%yArray )
! 		else
! 			call FourierTransform3D_iphase_complexArray( iFunc%yArray )
! 		end if
	end subroutine FourierTransform3D_iphase_CNFunction3D
	
	!>
	!! @brief
	!!
	subroutine FourierTransform3D_shift_Grid( iGrid, oGrid )
		class(Grid3D) :: iGrid
		class(Grid3D), optional :: oGrid
		
		integer :: i
		
		if( iGrid%isEquallyspaced() ) then
			call GOptions_warning( &
				"Grid should not be equally spaced", &
				"FourierTransform3D_shift_Grid()" &
			)
		end if
		
		if( present(oGrid) ) then
			oGrid = iGrid
			
			do i=1,3
				call FourierTransform_shift( oGrid%component(i) )
			end do
		else
			do i=1,3
				call FourierTransform_shift( iGrid%component(i) )
			end do
		end if
		
	end subroutine FourierTransform3D_shift_Grid
	
	!>
	!! @brief
	!!
	subroutine FourierTransform3D_ishift_Grid( iGrid, oGrid )
		class(Grid3D) :: iGrid
		class(Grid3D), optional :: oGrid
		
		integer :: i
		
		if( .not. iGrid%isEquallyspaced() ) then
			call GOptions_warning( &
				"Grid should be equally spaced", &
				"FourierTransform3D_ishift_Grid()" &
			)
		end if
		
		if( present(oGrid) ) then
			oGrid = iGrid
			
			do i=1,3
				call FourierTransform_ishift( oGrid%component(i) )
			end do
		else
			do i=1,3
				call FourierTransform_ishift( iGrid%component(i) )
			end do
		end if
		
	end subroutine FourierTransform3D_ishift_Grid
	
	!>
	!! @brief
	!!
	subroutine FourierTransform3D_shift_RNFunction3D( iFunc, oFunc )
		class(RNFunction3D) :: iFunc
		class(RNFunction3D), optional :: oFunc
		
		integer :: i
		
		if( iFunc%isEquallyspaced() ) then
			call GOptions_warning( &
				"Grid should not be equally spaced", &
				"FourierTransform3D_shift_Grid()" &
			)
		end if
		
		if( present(oFunc) ) then
			oFunc = iFunc
			
			do i=1,iFunc%nPoints(1)
				call FourierTransform2D_shift( oFunc%fArray(i,:,:) )
			end do
			
			do i=1,iFunc%nPoints(2)
				call FourierTransform2D_shift( oFunc%fArray(:,i,:) )
			end do
			
			do i=1,iFunc%nPoints(3)
				call FourierTransform2D_shift( oFunc%fArray(:,:,i) )
			end do
			
			call FourierTransform3D_shift( oFunc%xyzGrid )
		else
			do i=1,iFunc%nPoints(1)
				call FourierTransform2D_shift( iFunc%fArray(i,:,:) )
			end do
			
			do i=1,iFunc%nPoints(2)
				call FourierTransform2D_shift( iFunc%fArray(:,i,:) )
			end do
			
			do i=1,iFunc%nPoints(3)
				call FourierTransform2D_shift( iFunc%fArray(:,:,i) )
			end do
			
			call FourierTransform3D_shift( iFunc%xyzGrid )
		end if
		
	end subroutine FourierTransform3D_shift_RNFunction3D
	
	!>
	!! @brief
	!!
	subroutine FourierTransform3D_shift_CNFunction3D( iFunc, oFunc )
		class(CNFunction3D) :: iFunc
		class(CNFunction3D), optional :: oFunc
		
		integer :: i
		
		if( iFunc%isEquallyspaced() ) then
			call GOptions_warning( &
				"Grid should not be equally spaced", &
				"FourierTransform3D_shift_Grid()" &
			)
		end if
		
		if( present(oFunc) ) then
			oFunc = iFunc
			
			do i=1,iFunc%nPoints(1)
				call FourierTransform2D_shift( oFunc%fArray(i,:,:) )
			end do
			
			do i=1,iFunc%nPoints(2)
				call FourierTransform2D_shift( oFunc%fArray(:,i,:) )
			end do
			
			do i=1,iFunc%nPoints(3)
				call FourierTransform2D_shift( oFunc%fArray(:,:,i) )
			end do
			
			call FourierTransform3D_shift( oFunc%xyzGrid )
		else
			do i=1,iFunc%nPoints(1)
				call FourierTransform2D_shift( iFunc%fArray(i,:,:) )
			end do
			
			do i=1,iFunc%nPoints(2)
				call FourierTransform2D_shift( iFunc%fArray(:,i,:) )
			end do
			
			do i=1,iFunc%nPoints(3)
				call FourierTransform2D_shift( iFunc%fArray(:,:,i) )
			end do
			
			call FourierTransform3D_shift( iFunc%xyzGrid )
		end if
		
	end subroutine FourierTransform3D_shift_CNFunction3D
	
	!>
	!! @brief
	!!
	subroutine FourierTransform3D_ishift_CNFunction3D( iFunc, oFunc )
		class(CNFunction3D) :: iFunc
		class(CNFunction3D), optional :: oFunc
		
		integer :: i
		
		if( .not. iFunc%isEquallyspaced() ) then
			call GOptions_warning( &
				"Grid should be equally spaced", &
				"FourierTransform3D_ishift_Grid()" &
			)
		end if
		
		if( present(oFunc) ) then
			oFunc = iFunc
			
			do i=1,iFunc%nPoints(1)
				call FourierTransform2D_ishift( oFunc%fArray(i,:,:) )
			end do
			
			do i=1,iFunc%nPoints(2)
				call FourierTransform2D_ishift( oFunc%fArray(:,i,:) )
			end do
			
			do i=1,iFunc%nPoints(3)
				call FourierTransform2D_ishift( oFunc%fArray(:,:,i) )
			end do
			
			call FourierTransform3D_ishift( oFunc%xyzGrid )
		else
			do i=1,iFunc%nPoints(1)
				call FourierTransform2D_ishift( iFunc%fArray(i,:,:) )
			end do
			
			do i=1,iFunc%nPoints(2)
				call FourierTransform2D_ishift( iFunc%fArray(:,i,:) )
			end do
			
			do i=1,iFunc%nPoints(3)
				call FourierTransform2D_ishift( iFunc%fArray(:,:,i) )
			end do
			
			call FourierTransform3D_ishift( iFunc%xyzGrid )
		end if
		
	end subroutine FourierTransform3D_ishift_CNFunction3D
	
	!>
	!! @brief
	!!
	function FourierTransform3D_plan_Array( iArray, sgn, oArray ) result( plan )
		complex(8) :: iArray(:,:,:)
		integer, intent(in) :: sgn
		complex(8), optional :: oArray(:,:,:)
		integer(8) :: plan
		
		if( present(oArray) ) then
			call dfftw_plan_dft_3d( plan, size(iArray,dim=1), size(iArray,dim=2), size(iArray,dim=3), iArray, oArray, sgn, FFTW_ESTIMATE )
		else
			call dfftw_plan_dft_3d( plan, size(iArray,dim=1), size(iArray,dim=2), size(iArray,dim=3), iArray, iArray, sgn, FFTW_ESTIMATE )
		end if
	end function FourierTransform3D_plan_Array
	
	!>
	!! @brief
	!!
	function FourierTransform3D_plan_CNFunction3D( iFunc, sgn, oFunc ) result( plan )
		type(CNFunction3D) :: iFunc
		integer, intent(in) :: sgn
		type(CNFunction3D), optional :: oFunc
		integer(8) :: plan
		
		if( present(oFunc) ) then
			call dfftw_plan_dft_3d( plan, iFunc%nPoints(1), iFunc%nPoints(2), iFunc%nPoints(3), iFunc%fArray, oFunc%fArray, sgn, FFTW_ESTIMATE )
		else
			call dfftw_plan_dft_3d( plan, iFunc%nPoints(1), iFunc%nPoints(2), iFunc%nPoints(3), iFunc%fArray, iFunc%fArray, sgn, FFTW_ESTIMATE )
		end if
	end function FourierTransform3D_plan_CNFunction3D
	
	!>
	!! 
	!!
	subroutine FourierTransform3D_execute( plan )
		integer(8), intent(in) :: plan
		
		call dfftw_execute( plan )
	end subroutine FourierTransform3D_execute
	
	!>
	!! 
	!!
	subroutine FourierTransform3D_destroyPlan( plan )
		integer(8), intent(in) :: plan
		
		call dfftw_destroy_plan( plan )
	end subroutine FourierTransform3D_destroyPlan
	
	!>
	!! 
	!!
	subroutine FourierTransform3D_dft_CArray( iArray, oArray, sgn )
		complex(8) :: iArray(:,:,:)
		complex(8), optional :: oArray(:,:,:)
		integer, optional :: sgn
		
		integer :: effSgn
		
		integer(8) :: plan
		
		effSgn = FourierTransform_FORWARD
		if( present(sgn) ) effSgn = sgn
		
		if( present(oArray) ) then
			if( size(iArray) /= size(oArray) ) &
				call GOptions_error( "iArray and oArray have not the same size", "FourierTransform3D_dft_CArray" )
			
			call dfftw_plan_dft_3d( plan, size(iArray,dim=1), size(iArray,dim=2), size(iArray,dim=3), iArray, oArray, effSgn, FFTW_ESTIMATE )
			call dfftw_execute( plan )
		else
			call dfftw_plan_dft_3d( plan, size(iArray,dim=1), size(iArray,dim=2), size(iArray,dim=3), iArray, iArray, effSgn, FFTW_ESTIMATE )
			call dfftw_execute( plan )
		end if
		
		call dfftw_destroy_plan( plan )
	end subroutine FourierTransform3D_dft_CArray
	
	!>
	!! 
	!!
	subroutine FourierTransform3D_idft_Array( iArray, oArray, sgn )
		complex(8) :: iArray(:)
		complex(8), optional :: oArray(:)
		integer, optional :: sgn
		
		stop "FourierTransform3D_idft_Array is not implemented"
! 		integer :: effSgn
! 		
! 		integer(8) :: plan
! 		
! 		effSgn = FourierTransform3D_BACKWARD
! 		if( present(sgn) ) effSgn = sgn
! 		
! 		if( present(oArray) ) then
! 			if( size(iArray) /= size(oArray) ) &
! 				call GOptions_error( "iArray and oArray have not the same size", "FourierTransform3D_idft_Array" )
! 			
! 			call dfftw_plan_dft_1d( plan, size(iArray), iArray, oArray, effSgn, FourierTransform3DW_ESTIMATE )
! 			call dfftw_execute( plan )
! 			
! 			oArray = oArray/real( size(iArray), 8 )
! 		else
! 			call dfftw_plan_dft_1d( plan, size(iArray), iArray, iArray, effSgn, FourierTransform3DW_ESTIMATE )
! 			call dfftw_execute( plan )
! 			
! 			iArray = iArray/real( size(iArray), 8 )
! 		end if
! 		
! 		call dfftw_destroy_plan( plan )
	end subroutine FourierTransform3D_idft_Array
	
	!>
	!! 
	!!
	function FourierTransform3D_fft_CNFunction3D( iFunc, sgn ) result( oFunc )
		type(CNFunction3D) :: iFunc
		integer, optional :: sgn
		type(CNFunction3D) :: oFunc
		
		integer :: n(3)
		real(8) :: dx(3), dp(3)
		type(Grid3D) :: xyzGrid
		
		! @todo Check for checkEquallyspaced
		n = iFunc%nPoints()
		dx = iFunc%xyzGrid%stepSize()
		dp = 2.0_8*Math_PI/dx/real(n,8)
		
		oFunc = iFunc
		call FourierTransform3D_dft( iFunc%fArray, oFunc%fArray, sgn=FourierTransform_FORWARD )
		oFunc%xyzGrid = FourierTransform3D_omegaGrid( n, dx, order=FourierTransform_SORDER )
		
		call FourierTransform3D_phase( oFunc )
		call FourierTransform3D_shift( oFunc )
		
		oFunc = oFunc*dx(1)*dx(2)*dx(3)/sqrt(2.0_8*Math_PI)**3
	end function FourierTransform3D_fft_CNFunction3D
	
	!>
	!! 
	!!
	function FourierTransform3D_ifft_CNFunction3D( iFunc, sgn ) result( oFunc )
		type(CNFunction3D) :: iFunc
		integer, optional :: sgn
		type(CNFunction3D) :: oFunc
		
		integer :: n(3)
		real(8) :: dx(3), dp(3)
		type(Grid3D) :: xyzGrid
		
		integer :: i
		
		! @todo Check for checkEquallyspaced
		n = iFunc%nPoints()
		dp = iFunc%xyzGrid%stepSize()
		dx = 2.0_8*Math_PI/dp/real(n,8)
		
		oFunc = iFunc
		
		call FourierTransform3D_ishift( oFunc )
		call FourierTransform3D_phase( oFunc )
		
		call FourierTransform3D_dft( oFunc%fArray, oFunc%fArray, sgn=FourierTransform_BACKWARD )
		oFunc%xyzGrid = FourierTransform3D_xyzGrid( n, dx, order=FourierTransform_NORDER )
		
		oFunc = oFunc*dp(1)*dp(2)*dp(3)/sqrt(2.0_8*Math_PI)**3
	end function FourierTransform3D_ifft_CNFunction3D
	
	!>
	!! @brief
	!!
	function FourierTransform3D_nft_CNFunction3D( iFunc, sgn ) result( oFunc )
		type(CNFunction3D), intent(in) :: iFunc
		integer, optional, intent(in) :: sgn
		type(CNFunction3D) :: oFunc
		
		integer :: effSgn
		
		integer :: iw, jw, kw, ix, jx, kx, n(3)
		real(8) :: dx(3), dp(3)
		type(Grid3D) :: xyzGrid
		
		effSgn = FourierTransform_FORWARD
		if( present(sgn) ) effSgn = sgn
		
		! @todo Check for checkEquallyspaced
		n = iFunc%nPoints()
		dx = iFunc%xyzGrid%stepSize()
		dp = 2.0_8*Math_PI/dx/real(n,8)
		
		oFunc = iFunc
		oFunc%xyzGrid = FourierTransform3D_omegaGrid( n, dx, order=FourierTransform_NORDER )
		call oFunc%xyzGrid%show()
		
		do kw=1,n(3); do jw=1,n(2); do iw=1,n(1)
			oFunc%fArray(iw,jw,kw) = 0.0_8
			do kx=1,n(3); do jx=1,n(2); do ix=1,n(1)
				oFunc%fArray(iw,jw,kw) = oFunc%fArray(iw,jw,kw) &
					+ iFunc%fArray(ix,jx,kx)*exp( real(effSgn,8)*Math_I*sum( iFunc%xyzGrid%at(ix,jx,kx)*oFunc%xyzGrid%at(iw,jw,kw) ) )
			end do; end do; end do
		end do; end do; end do
		
		oFunc = oFunc*dx(1)*dx(2)*dx(3)/(2.0_8*Math_PI)
	end function FourierTransform3D_nft_CNFunction3D

	!>
	!! @brief
	!!
	function FourierTransform3D_inft_CNFunction3D( iFunc, sgn ) result( oFunc )
		type(CNFunction3D), intent(in) :: iFunc
		integer, optional, intent(in) :: sgn
		type(CNFunction3D) :: oFunc
		
		integer :: effSgn
		
		integer :: iw, jw, kw, ix, jx, kx, n(3)
		real(8) :: dx(3), dp(3)
		type(Grid3D) :: xyzGrid
		
		effSgn = FourierTransform_BACKWARD
		if( present(sgn) ) effSgn = sgn
		
		! @todo Check for checkEquallyspaced
		n = iFunc%nPoints()
		dp = iFunc%xyzGrid%stepSize()
		dx = 2.0_8*Math_PI/dp/real(n,8)
		
		oFunc = iFunc
		oFunc%xyzGrid = FourierTransform3D_xyzGrid( n, dx, order=FourierTransform_NORDER )
		
		do kw=1,n(3); do jw=1,n(2); do iw=1,n(1)
			oFunc%fArray(iw,jw,kw) = 0.0_8
			do kx=1,n(3); do jx=1,n(2); do ix=1,n(1)
				oFunc%fArray(iw,jw,kw) = oFunc%fArray(iw,jw,kw) &
					+ iFunc%fArray(ix,jx,kx)*exp( real(effSgn,8)*Math_I*sum( iFunc%xyzGrid%at(ix,jx,kx)*oFunc%xyzGrid%at(iw,jw,kw) ) )
			end do; end do; end do
		end do; end do; end do
		
		oFunc = oFunc*dp(1)*dp(2)*dp(3)/(2.0_8*Math_PI)
	end function FourierTransform3D_inft_CNFunction3D
	
	!>
	!! This is neccesary only for FourierTransform3D_test()
	!! Maxima:
	!!   f(x) := exp(-0.5*x**2);
	!!
	function funcGaussian( x, y, z ) result( output )
		real(8), intent(in) :: x, y, z
		complex(8) :: output
		
		real(8) :: alpha
		
		alpha = 5.0_8
		output = exp(-alpha*( x**2 + y**2 + z**2 ))
	end function funcGaussian
	
	!>
	!! This is neccesary only for FourierTransform3D_test()
	!! Maxima:
	!!   f(x) := exp(-0.5*x**2);
	!!
	!! La convencion de normalizacion de acuerdo con wikipedia es:
    !!   Fourier transform unitary, angular frequency
	!!
	function funcFGaussian( x ) result( output )
		real(8), intent(in) :: x
		complex(8) :: output
		
		real(8) :: alpha
		
		alpha = 0.5_8
! 		output = exp(-x**2/4.0_8/alpha)/sqrt(2.0_8*alpha)
		output = exp(-(x-5.0_8)**2/4.0_8/alpha)/sqrt(2.0_8*alpha) + 0.4_8*exp(-(x+2.0_8)**2/10.0_8/alpha)/sqrt(2.0_8*alpha)*sin(2.0*x) &
			+ 0.4_8*Math_I*exp(-(x-2.0_8)**2/10.0_8/alpha)/sqrt(2.0_8*alpha)*cos(2.0*x)
	end function funcFGaussian
	
	!>
	!! This is neccesary only for FourierTransform3D_test()
	!!
	function funcRectangular( x, y, z ) result( output )
		real(8), intent(in) :: x, y, z
		complex(8) :: output
		
		real(8) :: a1, a2, a3
		
		a1 = 0.5_8
		a2 = 1.0_8
		a3 = 1.5_8
		
		output = Math_ubox( a1*x )*Math_ubox( a2*y )*Math_ubox( a3*z )
	end function funcRectangular
	
	!>
	!! This is neccesary only for FourierTransform3D_test()
	!!
	function FfuncRectangular( w1, w2, w3 ) result( output )
		real(8), intent(in) :: w1, w2, w3
		complex(8) :: output
		
		real(8) :: a1, a2, a3
		
		a1 = 0.5_8
		a2 = 1.0_8
		a3 = 1.5_8
		
		output = Math_nsinc( w1/(2.0_8*Math_PI*a1) ) &
					*Math_nsinc( w2/(2.0_8*Math_PI*a2) ) &
					*Math_nsinc( w3/(2.0_8*Math_PI*a3) )&
					/sqrt(2.0_8*Math_PI)**3/a1/a2/a3
	end function FfuncRectangular
	
	!>
	!! @bief test
	!!
	subroutine FourierTransform3D_test()
		type(Grid3D) :: xyzGrid, omegaGrid
		type(CNFunction3D) :: funcA, dFuncA, fftFuncA, fftDFuncA
		type(CNFunction3D) :: funcB, dFuncB, fftFuncB, fftDFuncB
		type(CNFunction3D) :: funcAB, dFuncAB, fftFuncAB, fftDFuncAB
		type(RNFunction3D) :: aSpectrum, pSpectrum
		type(FourierTransform3D) :: fft
		real(8) :: exactValue
		real(8) :: value
		integer :: i, j
		real(8), allocatable :: array(:,:,:)
		complex(8), allocatable :: cArray(:,:,:)
		integer(8) :: planF, planB
		character :: cBuffer
		
		write(*,*) "----------------------------------"
		write(*,*) " Verifing omega Grid constructors "
		write(*,*) "----------------------------------"
		
		xyzGrid = FourierTransform3D_omegaGridFromData( [10,11,12], [0.1_8,0.2_8,0.3_8], FourierTransform_NORDER )
		call xyzGrid%show()
		call xyzGrid%save()
		
		xyzGrid = FourierTransform3D_omegaGridFromData( [10,11,12], [0.1_8,0.2_8,0.3_8], FourierTransform_SORDER )
		call xyzGrid%show()
		call xyzGrid%save()
		
		write(*,*) "-------------------------------"
		write(*,*) " Verifing xy Grid constructors "
		write(*,*) "-------------------------------"
		
		xyzGrid = FourierTransform3D_xyzGridFromData( [10,11,12], [0.1_8,0.2_8,0.3_8], FourierTransform_NORDER )
		call xyzGrid%show()
		call xyzGrid%save()
		
		xyzGrid = FourierTransform3D_xyzGridFromData( [10,11,12], [0.1_8,0.2_8,0.3_8], FourierTransform_SORDER )
		call xyzGrid%show()
		call xyzGrid%save()
		
		return
		call GOptions_doYouWantToContinue()
		
! 		write(*,*) "-------------------------------------------"
! 		write(*,*) " FourierTransform3D of a gaussian function "
! 		write(*,*) "-------------------------------------------"
! 		
! 		call xyzGrid%init( [-2.5_8,-2.5_8], [2.5_8,2.5_8], size=[90,110] )
! 		call funcA%init( xyzGrid, funcRectangular )
! 		write(*,"(A)") "input (.func) = "
! 		call funcA%show()
! 		call funcA%save( ".func", format=BLKS_FORMAT )
! 		
! 		funcB = FourierTransform3D_fft( funcA )
! 		write(*,"(A)") "exact (.Ffunc) = "
! 		call funcB%show()
! 		call funcB%save( ".Ffunc", format=BLKS_FORMAT )
! 		
! 		call xyzGrid%init( [-54.677675_8,-67.241507_8], [55.920349_8,68.486720_8], size=[90,110] )
! 		call funcB%init( xyzGrid, FfuncRectangular )
! ! 		funcB = FourierTransform3D_nft( funcA )
! 		write(*,"(A)") "exact (.Fexact) = "
! 		call funcB%show()
! 		call funcB%save( ".Fexact", format=BLKS_FORMAT )
! 		
! ! 		call system( "echo splot \"//achar(34)//".func\"//achar(34)//" u 1:2:3 w l, \"//achar(34)//"\"//achar(34)//" u 1:2:4 w l | gnuplot -p" )
! ! 		call system( "echo splot \"//achar(34)//".Ffunc\"//achar(34)//" u 1:2:3 w l, \"//achar(34)//"\"//achar(34)//" u 1:2:4 w l | gnuplot -p" )
! ! 		call system( "echo splot \"//achar(34)//".Fexact\"//achar(34)//" u 1:2:3 w l, \"//achar(34)//"\"//achar(34)//" u 1:2:4 w l | gnuplot -p" )
! 		call system( "echo splot \"//achar(34)//".func\"//achar(34)//" u 1:2:3 w l | gnuplot -p" )
! 		call system( "echo splot \"//achar(34)//".Ffunc\"//achar(34)//" u 1:2:3 w l | gnuplot -p" )
! 		call system( "echo splot \"//achar(34)//".Fexact\"//achar(34)//" u 1:2:3 w l | gnuplot -p" )
! 		call GOptions_doYouWantToContinue()
! 		call system( "rm .func .Ffunc .Fexact" )
		
! 		write(*,*) "-----------------------------"
! 		write(*,*) " iFourierTransform3D of a gaussian function"
! 		write(*,*) "-----------------------------"
! 		
! 		call xyzGrid%init( [-2.5_8,-2.5_8], [2.5_8,2.5_8], size=[90,110] )
! 		call funcA%init( xyzGrid, funcRectangular )
! 		funcA = FourierTransform3D_nft( funcA )
! 		write(*,"(A)") "input (.Ffunc) = "
! 		call funcA%show()
! 		call funcA%save( ".Ffunc", format=BLKS_FORMAT )
! 		
! 		write(*,"(A)") "iFourierTransform3D (.func) = "
! 		funcB = funcA
! 		funcB = FourierTransform3D_ifft( funcB )
! 		call funcB%show()
! 		call funcB%save( ".func", format=BLKS_FORMAT )
! 		
! 		funcB = FourierTransform3D_inft( funcA )
! 		write(*,"(A)") "exact (.exact) = "
! 		call funcB%show()
! 		call funcB%save( ".exact", format=BLKS_FORMAT )
! 		
! ! 		call system( "echo splot \"//achar(34)//".Ffunc\"//achar(34)//" u 1:2:3 w l, \"//achar(34)//"\"//achar(34)//" u 1:2:4 w l | gnuplot -p" )
! ! 		call system( "echo splot \"//achar(34)//".func\"//achar(34)//" u 1:2:3 w l, \"//achar(34)//"\"//achar(34)//" u 1:2:4 w l | gnuplot -p" )
! ! 		call system( "echo splot \"//achar(34)//".exact\"//achar(34)//" u 1:2:3 w l, \"//achar(34)//"\"//achar(34)//" u 1:2:4 w l | gnuplot -p" )
! 		call system( "echo splot \"//achar(34)//".Ffunc\"//achar(34)//" u 1:2:3 w l | gnuplot -p" )
! 		call system( "echo splot \"//achar(34)//".func\"//achar(34)//" u 1:2:3 w l | gnuplot -p" )
! 		call system( "echo splot \"//achar(34)//".exact\"//achar(34)//" u 1:2:3 w l | gnuplot -p" )
! 		call GOptions_doYouWantToContinue()
! 		call system( "rm .Ffunc .func .exact" )
! 		
! 		write(*,*) "-------------------------------"
! 		write(*,*) " iFourierTransform3D( FourierTransform3D(func) ). CNFunction3D "
! 		write(*,*) "-------------------------------"
! 		
! 		call xyzGrid%init( [-2.5_8,-2.5_8], [2.5_8,2.5_8], size=[90,110] )
! 		call funcA%init( xyzGrid, funcRectangular )
! 
! 		write(*,"(A)") "input (.func) = "
! 		call funcA%show()
! 		call funcA%save( ".func", format=BLKS_FORMAT )
! 		
! 		write(*,"(A)") "FourierTransform3D (.Ffunc) = "
! 		funcB = FourierTransform3D_fft( funcA )
! 		call funcB%show()
! 		call funcB%save( ".Ffunc", format=BLKS_FORMAT )
! 		
! 		write(*,"(A)") "iFourierTransform3D (.iFFfunc) = "
! 		funcA = FourierTransform3D_ifft( funcB )
! 		call funcA%show()
! 		call funcA%save( ".iFFfunc", format=BLKS_FORMAT )
! 		
! ! 		call system( "echo splot \"//achar(34)//".func\"//achar(34)//" u 1:2:3 w l, \"//achar(34)//"\"//achar(34)//" u 1:2:4 w l | gnuplot -p" )
! ! 		call system( "echo splot \"//achar(34)//".Ffunc\"//achar(34)//" u 1:2:3 w l, \"//achar(34)//"\"//achar(34)//" u 1:2:4 w l | gnuplot -p" )
! ! 		call system( "echo splot \"//achar(34)//".iFFfunc\"//achar(34)//" u 1:2:3 w l, \"//achar(34)//"\"//achar(34)//" u 1:2:4 w l | gnuplot -p" )
! 		call system( "echo splot \"//achar(34)//".func\"//achar(34)//" u 1:2:3 w l | gnuplot -p" )
! 		call system( "echo splot \"//achar(34)//".Ffunc\"//achar(34)//" u 1:2:3 w l | gnuplot -p" )
! 		call system( "echo splot \"//achar(34)//".iFFfunc\"//achar(34)//" u 1:2:3 w l | gnuplot -p" )
! 		call GOptions_doYouWantToContinue()
! 		call system( "rm .func .Ffunc .iFFfunc" )
		
! 		write(*,*) "--------------------------------"
! 		write(*,*) " FourierTransform3D( iFourierTransform3D(Ffunc) ). CNFunction3D "
! 		write(*,*) "--------------------------------"
! 		
! 		call xyzGrid%init( [-2.5_8,-2.5_8], [2.5_8,2.5_8], size=[90,110] )
! 		call funcA%init( xyzGrid, funcRectangular )
! 		funcA = FourierTransform3D_nft( funcA )
! 
! 		write(*,"(A)") "input (.Ffunc) = "
! 		call funcA%show()
! 		call funcA%save( ".Ffunc", format=BLKS_FORMAT )
! 		
! 		write(*,"(A)") "iFourierTransform3D (.iFfunc) = "
! 		funcA = FourierTransform3D_ifft( funcA )
! 		call funcA%show()
! 		call funcA%save( ".iFfunc", format=BLKS_FORMAT )
! 		
! 		write(*,"(A)") "FourierTransform3D (.FiFfunc) = "
! 		funcA = FourierTransform3D_fft( funcA )
! 		call funcA%show()
! 		call funcA%save( ".FiFfunc", format=BLKS_FORMAT )
! 		
! 		call system( "echo splot \"//achar(34)//".Ffunc\"//achar(34)//" u 1:2:3 w l | gnuplot -p" )
! 		call system( "echo splot \"//achar(34)//".iFfunc\"//achar(34)//" u 1:2:3 w l | gnuplot -p" )
! 		call system( "echo splot \"//achar(34)//".FiFfunc\"//achar(34)//" u 1:2:3 w l | gnuplot -p" )
! 		call GOptions_doYouWantToContinue()
! 		call system( "rm .Ffunc .iFfunc .FiFfunc" )
		
		write(*,*) "-----------------------------------------------------"
		write(*,*) " Verifing FourierTransform3D CNFunction3D with plans "
		write(*,*) "-----------------------------------------------------"
		
		call xyzGrid%init( [-2.5_8,-2.5_8,-2.5_8], [2.5_8,2.5_8,2.5_8], size=[90,110,200] )
		call funcA%init( xyzGrid, funcRectangular )
		write(*,"(A)") "input (.func) = "
		call funcA%show()
		call funcA%save( "func.cube", format=AUTO_FORMAT )
		
		planF = FourierTransform3D_plan( funcA, FourierTransform_FORWARD )
		planB = FourierTransform3D_plan( funcA, FourierTransform_BACKWARD )
		
		write(*,"(A)") "FourierTransform3D (.Ffunc) = "
		call FourierTransform3D_execute( planF )
		funcA%xyzGrid = FourierTransform3D_omegaGrid( funcA%xyzGrid, order=FourierTransform_SORDER )
! 		call FourierTransform3D_phase( funcA )
		call FourierTransform3D_shift( funcA )
		call funcA%show()
! 		call funcA%save( ".Ffunc", format=BLKS_FORMAT )
		call funcA%save( "Ffunc.cube", format=AUTO_FORMAT )
		call funcA%save( "Ffunc.n3df", format=AUTO_FORMAT )
		stop
		write(*,"(A)") "iFourierTransform3D (.iFFfunc) = "
		call FourierTransform3D_ishift( funcA )
		call FourierTransform3D_phase( funcA )
		call FourierTransform3D_execute( planB )
		funcA%xyzGrid = FourierTransform3D_xyzGrid( funcA%xyzGrid, order=FourierTransform_NORDER )
		funcA = funcA/real( funcA%nPoints(1)*funcA%nPoints(2), 8 )
		call funcA%show()
		call funcA%save( ".iFFfunc", format=BLKS_FORMAT )
		
		call system( "echo splot \"//achar(34)//".func\"//achar(34)//" u 1:2:3 w l | gnuplot -p" )
		call system( "echo splot \"//achar(34)//".Ffunc\"//achar(34)//" u 1:2:3 w l | gnuplot -p" )
		call system( "echo splot \"//achar(34)//".iFFfunc\"//achar(34)//" u 1:2:3 w l | gnuplot -p" )
		call GOptions_doYouWantToContinue()
		call system( "rm .func .Ffunc .iFFfunc" )

		call FourierTransform3D_destroyPlan( planF )
		call FourierTransform3D_destroyPlan( planB )
		
! 		write(*,*) "------------------------------------------"
! 		write(*,*) " iFourierTransform3D( FourierTransform3D(func) ). CNFunction3D with plans"
! 		write(*,*) "------------------------------------------"
! 		
! 		call xyzGrid%init( [-2.5_8,-2.5_8], [2.5_8,2.5_8], size=[90,110] )
! 		call funcA%init( xyzGrid, funcRectangular )
! 		write(*,"(A)") "input (.func) = "
! 		call funcA%show()
! 		call funcA%save( ".func", format=BLKS_FORMAT )
! 		
! 		planF = FourierTransform3D_plan( funcA, FourierTransform_FORWARD )
! 		planB = FourierTransform3D_plan( funcA, FourierTransform_BACKWARD )
! 		
! 		call FourierTransform3D_execute( planF )
! 		call FourierTransform3D_execute( planB )
! 		funcA = funcA/real( funcA%nPoints(1)*funcA%nPoints(2), 8 )
! 		
! 		call funcA%show()
! 		call funcA%save( ".iFFfunc", format=BLKS_FORMAT )
! 		
! 		call system( "echo splot \"//achar(34)//".func\"//achar(34)//" u 1:2:3 w l | gnuplot -p" )
! 		call system( "echo splot \"//achar(34)//".iFFfunc\"//achar(34)//" u 1:2:3 w l | gnuplot -p" )
! 		call GOptions_doYouWantToContinue()
! 		call system( "rm .func .iFFfunc" )
! 
! 		call FourierTransform3D_destroyPlan( planF )
! 		call FourierTransform3D_destroyPlan( planB )
		
		write(*,*) "-----------------------------------------"
		write(*,*) " Verifing FourierTransform3D CNFunction3D oriented object"
		write(*,*) "-----------------------------------------"
		
		call xyzGrid%init( [-2.5_8,-2.5_8,-2.5_8], [2.5_8,2.5_8,2.5_8], size=[90,110,130] )
		call funcA%init( xyzGrid, funcRectangular )
		write(*,"(A)") "input (.func) = "
		call funcA%show()
		call funcA%save( ".func", format=BLKS_FORMAT )
		
		call fft%init( funcA, FourierTransform_SPATIAL_DOMAIN )
		
		call fft.execute( FourierTransform_FORWARD, sync=.true., shift=.true. )
		write(*,"(A)") "FourierTransform3D (.Ffunc) = "
		call funcA%show()
		call funcA%save( ".Ffunc", format=BLKS_FORMAT )
		
		call fft.execute( FourierTransform_BACKWARD, sync=.true., shift=.true. )
		write(*,"(A)") "iFourierTransform3D (.iFFfunc) = "
		call funcA%show()
		call funcA%save( ".iFFfunc", format=BLKS_FORMAT )
		
		call system( "echo splot \"//achar(34)//".func\"//achar(34)//" u 1:2:3 w l | gnuplot -p" )
		call system( "echo splot \"//achar(34)//".Ffunc\"//achar(34)//" u 1:2:3 w l | gnuplot -p" )
		call system( "echo splot \"//achar(34)//".iFFfunc\"//achar(34)//" u 1:2:3 w l | gnuplot -p" )
		call GOptions_doYouWantToContinue()
		call system( "rm .func .Ffunc .iFFfunc" )
		
! 		write(*,*) "--------------------------------------"
! 		write(*,*) " Second derivative via FourierTransform3D with plans"
! 		write(*,*) "--------------------------------------"
! 		
! 		call xyzGrid%init( -3.0_8*Math_PI, 3.0_8*Math_PI, 1001 )
! 		omegaGrid = FourierTransform3D_omegaGrid( xyzGrid )
! 		call funcA%init( xyzGrid, funcTest )
! 		write(*,"(A)") "input (.func) = "
! 		call funcA%show()
! 		call funcA%save(".func")
! 		
! 		planF = FourierTransform3D_plan( funcA, FourierTransform3D_FORWARD )
! 		planB = FourierTransform3D_plan( funcA, FourierTransform3D_BACKWARD )
! 		
! 		call FourierTransform3D_execute( planF )
! 		
! 		funcA%yArray = ( Math_I*fft.omega )**2*funcA%yArray
! 		
! 		write(*,"(A)") "iFourierTransform3D (.dfunc) = "
! 		call FourierTransform3D_execute( planB )
! 		funcA = funcA/real( funcA%nPoints(), 8 )
! 		call funcA%show()
! 		call funcA%save(".dfunc")
! 		
! 		call funcB%init( xyzGrid, d2funcTest )
! 		write(*,"(A)") "exact (.exact) = "
! 		call funcB%show()
! 		call funcB%save(".exact")
! 		
! ! 		call system( "echo plot \"//achar(34)//".func\"//achar(34)//" u 1:2 w l lw 1.5, \"//achar(34)//"\"//achar(34)//" u 1:3 w l lw 1.5 | gnuplot -p" )
! ! 		call system( "echo plot \"//achar(34)//".dfunc\"//achar(34)//" u 1:2 w l lw 1.5, \"//achar(34)//"\"//achar(34)//" u 1:3 w l lw 1.5 | gnuplot -p" )
! ! 		call system( "echo plot \"//achar(34)//".exact\"//achar(34)//" u 1:2 w l lw 1.5, \"//achar(34)//"\"//achar(34)//" u 1:3 w l lw 1.5 | gnuplot -p" )
! ! 		call GOptions_doYouWantToContinue()
! 		call system( "rm .func .dfunc .exact" )
! 
! 		call FourierTransform3D_destroyPlan( planF )
! 		call FourierTransform3D_destroyPlan( planB )
! 
! 		write(*,*) "------------------------------------------------------"
! 		write(*,*) " Second derivative via FourierTransform3D CNFunction3D oriented object"
! 		write(*,*) "------------------------------------------------------"
! 		
! 		call xyzGrid%init( -3.0_8*Math_PI, 3.0_8*Math_PI, 1001 )
! 		call funcA%init( xyzGrid, funcTest )
! 		write(*,"(A)") "input (.func) = "
! 		call funcA%show()
! 		call funcA%save(".func")
! 		
! 		call fft%init( funcA, FourierTransform_SPATIAL_DOMAIN )
! 		
! 		call fft.execute( FourierTransform3D_FORWARD )
! 		
! 		funcA%yArray = ( Math_I*fft.omega )**2*funcA%yArray
! 		
! 		call fft.execute( FourierTransform3D_BACKWARD )
! 		
! 		write(*,"(A)") "iFourierTransform3D (.dfunc) = "
! 		call funcA%show()
! 		call funcA%save(".dfunc")
! 		
! 		call funcB%init( xyzGrid, d2funcTest )
! 		write(*,"(A)") "exact (.exact) = "
! 		call funcB%show()
! 		call funcB%save(".exact")
! 		
! ! 		call system( "echo plot \"//achar(34)//".func\"//achar(34)//" u 1:2 w l lw 1.5, \"//achar(34)//"\"//achar(34)//" u 1:3 w l lw 1.5 | gnuplot -p" )
! ! 		call system( "echo plot \"//achar(34)//".dfunc\"//achar(34)//" u 1:2 w l lw 1.5, \"//achar(34)//"\"//achar(34)//" u 1:3 w l lw 1.5 | gnuplot -p" )
! ! 		call system( "echo plot \"//achar(34)//".exact\"//achar(34)//" u 1:2 w l lw 1.5, \"//achar(34)//"\"//achar(34)//" u 1:3 w l lw 1.5 | gnuplot -p" )
! ! 		call GOptions_doYouWantToContinue()
! 		call system( "rm .func .dfunc .exact" )
! 		
! 		write(*,*) "----------"
! 		write(*,*) " Spectrum "
! 		write(*,*) "----------"
! 		
! ! 		call xyzGrid%init( -3.0_8*Math_PI, 3.0_8*Math_PI, 1001 )
! 		call xyzGrid%init( -15.0_8, 15.0_8, 1001 )
! 		call funcA%init( xyzGrid, funcTest )
! 		write(*,"(A)") "input (.func) = "
! 		call funcA%show()
! 		call funcA%save(".func")
! 		
! 		aSpectrum = FourierTransform3D_powerSpectrum( funcA )
! 		
! 		write(*,"(A)") "aspec (.asfunc) = "
! 		call aSpectrum%show()
! 		call aSpectrum%save(".asfunc")
! 		
! 		pSpectrum = FourierTransform3D_phaseSpectrum( funcA )
! 		
! 		write(*,"(A)") "pspec (.psfunc) = "
! 		call pSpectrum%show()
! 		call pSpectrum%save(".psfunc")
! 		
! 		call system( "echo plot \"//achar(34)//".func\"//achar(34)//" u 1:2 w l lw 1.5, \"//achar(34)//"\"//achar(34)//" u 1:3 w l lw 1.5 | gnuplot -p" )
! 		call system( "echo plot \"//achar(34)//".asfunc\"//achar(34)//" u 1:2 w l lw 1.5 | gnuplot -p" )
! 		call system( "echo plot \"//achar(34)//".psfunc\"//achar(34)//" u 1:2 w l lw 1.5 | gnuplot -p" )
! 		call GOptions_doYouWantToContinue()
! 		call system( "rm .func .asfunc .psfunc" )
! 		
! 		write(*,*) "--------------------------------------------------------"
! 		write(*,*) " Filtering by amplitude. FourierTransform3D CNFunction3D oriented object"
! 		write(*,*) "--------------------------------------------------------"
! 		
! 		call xyzGrid%init( -3.0_8*Math_PI, 3.0_8*Math_PI, 1001 )
! 		call funcA%init( xyzGrid, funcTestWithNoise )
! 		write(*,"(A)") "input (.func) = "
! 		call funcA%show()
! 		call funcA%save(".func")
! 		
! 		call fft%init( funcA, FourierTransform_SPATIAL_DOMAIN )
! 		
! 		call fft.execute( FourierTransform3D_FORWARD )
! 		
! 		where( abs( funcA%yArray ) <= 20.0_8 ) funcA%yArray = 0.0_8
! 		
! 		call fft.execute( FourierTransform3D_BACKWARD )
! 		
! 		write(*,"(A)") "iFourierTransform3D (.ffunc) = "
! 		call funcA%show()
! 		call funcA%save(".ffunc")
! 		
! ! 		call system( "echo plot \"//achar(34)//".func\"//achar(34)//" u 1:2 w l lw 1.5, \"//achar(34)//"\"//achar(34)//" u 1:3 w l lw 1.5 | gnuplot -p" )
! ! 		call system( "echo plot \"//achar(34)//".ffunc\"//achar(34)//" u 1:2 w l lw 1.5, \"//achar(34)//"\"//achar(34)//" u 1:3 w l lw 1.5 | gnuplot -p" )
! ! 		call GOptions_doYouWantToContinue()
! 		call system( "rm .func .ffunc" )
! 		
! 		write(*,*) "--------------------------------------------------------"
! 		write(*,*) " Filtering by frequency. FourierTransform3D CNFunction3D oriented object"
! 		write(*,*) "--------------------------------------------------------"
! 		
! 		call xyzGrid%init( -3.0_8*Math_PI, 3.0_8*Math_PI, 1001 )
! 		call funcA%init( xyzGrid, funcTestWithNoise )
! 		write(*,"(A)") "input (.func) = "
! 		call funcA%show()
! 		call funcA%save(".func")
! 		
! 		call fft%init( funcA, FourierTransform_SPATIAL_DOMAIN )
! 		
! 		call fft.execute( FourierTransform3D_FORWARD )
! 		
! 		do i=1,funcA%nPoints()
! 			if( abs( fft.omega(i) ) > 10.0_8 ) then
! 				funcA%yArray(i) = 0.0_8
! 			end if
! 		end do
! 		
! 		call fft.execute( FourierTransform3D_BACKWARD )
! 		
! 		write(*,"(A)") "iFourierTransform3D (.ffunc) = "
! 		call funcA%show()
! 		call funcA%save(".ffunc")
! 		
! ! 		call system( "echo plot \"//achar(34)//".func\"//achar(34)//" u 1:2 w l lw 1.5, \"//achar(34)//"\"//achar(34)//" u 1:3 w l lw 1.5 | gnuplot -p" )
! ! 		call system( "echo plot \"//achar(34)//".ffunc\"//achar(34)//" u 1:2 w l lw 1.5, \"//achar(34)//"\"//achar(34)//" u 1:3 w l lw 1.5 | gnuplot -p" )
! ! 		call GOptions_doYouWantToContinue()
! 		call system( "rm .func .ffunc" )
		
	end subroutine FourierTransform3D_test
	
end module FourierTransform3D_

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                                                                                   !!
!!  This file is part of SciFT project                                               !!
!!  Copyright (c) 2013-2015 Nestor F. Aguirre (nfaguirrec@gmail.com)                 !!
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

module FourierTransform2D_
	use FFTW3_
	use GOptions_
	use Math_
	use Grid2D_
	use CNFunction2D_
	use RNFunction2D_
	use RandomUtils_
	use FourierTransform_
	implicit none
	private
	
	public :: &
		FourierTransform2D_omegaGrid, &
		FourierTransform2D_xyGrid, &
		FourierTransform2D_phase, &
		FourierTransform2D_iphase, &
		FourierTransform2D_shift, &
		FourierTransform2D_ishift, &
		FourierTransform2D_plan, &
		FourierTransform2D_execute, &
		FourierTransform2D_destroyPlan, &
		FourierTransform2D_dft, &
		FourierTransform2D_idft, &
		FourierTransform2D_fft, &
		FourierTransform2D_ifft, &
		FourierTransform2D_nft, &
		FourierTransform2D_inft, &
		FourierTransform2D_test
		
	type, public :: FourierTransform2D
		class(CNFunction2D), pointer, private :: iFunc
		integer(8), private :: planF, planB
		
		integer :: nPoints(2)
		type(Grid2D) :: x
		type(Grid2D) :: omega
		
		contains
			procedure :: init
			final :: destroy
			procedure :: str
			procedure :: show
			procedure :: execute
! 			procedure :: filter
	end type FourierTransform2D
	
    interface FourierTransform2D_omegaGrid
		module procedure FourierTransform2D_omegaGridFromData
		module procedure FourierTransform2D_omegaGridFromXGrid
    end interface FourierTransform2D_omegaGrid

    interface FourierTransform2D_xyGrid
		module procedure FourierTransform2D_xyGridFromData
		module procedure FourierTransform2D_xyGridFromOmegaGrid
    end interface FourierTransform2D_xyGrid

    interface FourierTransform2D_phase
        module procedure FourierTransform2D_phase_realArray
        module procedure FourierTransform2D_phase_complexArray
        module procedure FourierTransform2D_phase_RNFunction2D
        module procedure FourierTransform2D_phase_CNFunction2D
    end interface FourierTransform2D_phase
    
    interface FourierTransform2D_iphase
        module procedure FourierTransform2D_iphase_realArray
        module procedure FourierTransform2D_iphase_complexArray
        module procedure FourierTransform2D_iphase_CNFunction2D
    end interface FourierTransform2D_iphase

    interface FourierTransform2D_shift
        module procedure FourierTransform2D_shift_realArray
        module procedure FourierTransform2D_shift_complexArray
        module procedure FourierTransform2D_shift_Grid
        module procedure FourierTransform2D_shift_RNFunction2D
        module procedure FourierTransform2D_shift_CNFunction2D
    end interface FourierTransform2D_shift

    interface FourierTransform2D_ishift
        module procedure FourierTransform2D_ishift_realArray
        module procedure FourierTransform2D_ishift_complexArray
        module procedure FourierTransform2D_ishift_Grid
        module procedure FourierTransform2D_ishift_CNFunction2D
    end interface FourierTransform2D_ishift

    interface FourierTransform2D_plan
        module procedure FourierTransform2D_plan_Array
        module procedure FourierTransform2D_plan_CNFunction2D
    end interface FourierTransform2D_plan
    
    interface FourierTransform2D_dft
        module procedure FourierTransform2D_dft_CArray
    end interface FourierTransform2D_dft
    
    interface FourierTransform2D_idft
        module procedure FourierTransform2D_idft_Array
    end interface FourierTransform2D_idft

    interface FourierTransform2D_fft
        module procedure FourierTransform2D_fft_CNFunction2D
    end interface FourierTransform2D_fft

    interface FourierTransform2D_ifft
        module procedure FourierTransform2D_ifft_CNFunction2D
    end interface FourierTransform2D_ifft

    interface FourierTransform2D_nft
        module procedure FourierTransform2D_nft_CNFunction2D
    end interface FourierTransform2D_nft

    interface FourierTransform2D_inft
        module procedure FourierTransform2D_inft_CNFunction2D
    end interface FourierTransform2D_inft
    
	contains
	
	!>
	!! @brief Constructor
	!! @todo En algunas ocasiones es importante hacer la transfomada de no todos los puntos
	!!
	subroutine init( this, iFunc, domain, oFunc )
		class(FourierTransform2D) :: this 
		class(CNFunction2D), target, intent(in) :: iFunc
		integer(8), intent(in), optional :: domain
		class(CNFunction2D), target, optional, intent(in) :: oFunc
		
		integer(8) :: effDomain
		
		integer :: nx, ny
		
		if( associated(this.iFunc) ) nullify(this.iFunc)
		this.iFunc => iFunc
		this.nPoints = iFunc.nPoints() ! << Hay que cambiar para hacer la transformada de algunos puntos
		
		nx = this.nPoints(1)
		ny = this.nPoints(2)
		
		effDomain = FourierTransform_SPATIAL_DOMAIN
		if( present(domain) ) effDomain = domain
		
		if( present(oFunc) ) call GOptions_error( "oFunc parameter is not implamented yet", "FourierTransform2D.init()" )
		
		call dfftw_plan_dft_2d( this.planF, nx, ny, this.iFunc.fArray, this.iFunc.fArray, FFTW_FORWARD, FFTW_ESTIMATE )
		call dfftw_plan_dft_2d( this.planB, nx, ny, this.iFunc.fArray, this.iFunc.fArray, FFTW_BACKWARD, FFTW_ESTIMATE )
			
		if( effDomain == FourierTransform_SPATIAL_DOMAIN ) then
			
			this.x = this.iFunc.xyGrid ! << Hay que cambiar para hacer la transformada de algunos puntos
			this.omega = FourierTransform2D_omegaGrid( this.x )
			
		else if( effDomain == FourierTransform_FREQUENCY_DOMAIN ) then
			
			call GOptions_warning( "FourierTransform_FREQUENCY_DOMAIN have not been tested yet", "FourierTransform.init()" )
			this.omega = this.iFunc.xyGrid ! << Hay que cambiar para hacer la transformada de algunos puntos
			this.x = FourierTransform2D_xyGrid( this.omega )
			
		end if
	end subroutine init
	
	!>
	!! @brief Destructor
	!!
	subroutine destroy( this )
		type(FourierTransform2D) :: this
		
		! Si los activo el programa produce:
		! 	forrtl: error (76): Abort trap signal
! 		call dfftw_destroy_plan( this.planF )
! 		call dfftw_destroy_plan( this.planB )
		
		nullify(this.iFunc)
		
		this.nPoints = -1
	end subroutine destroy
	
	!>
	!! @brief
	!!
	function str( this ) result( output )
		class(FourierTransform2D) :: this 
		character(len=200) :: output
		
		integer :: fmt
		character(len=200) :: strBuffer
		
		output = ""
		
		output = trim(output)//"<FourierTransform2D:"
		
! 		output = trim(output)//"min="
! 		fmt = int(log10(this.min+1.0))+1
! 		write(strBuffer, "(f<fmt+7>.6)") this.min
! 		output = trim(output)//trim(strBuffer)
! 		
! 		output = trim(output)//",max="
! 		fmt = int(log10(this.max+1.0))+1
! 		write(strBuffer, "(f<fmt+7>.6)") this.max
! 		output = trim(output)//trim(strBuffer)
! 		
! 		output = trim(output)//",h="
! 		fmt = int(log10(this.h+1.0))+1
! 		write(strBuffer, "(f<fmt+7>.6)") this.h
! 		output = trim(output)//trim(strBuffer)
! 		
! 		output = trim(output)//",size="
! 		fmt = int(log10(float(this.size+1)))+1
! 		write(strBuffer, "(i<fmt>)") this.size
! 		output = trim(output)//trim(strBuffer)
		
		output = trim(output)//">"
	end function str
	
	!>
	!! @brief
	!!
	subroutine show( this, unit )
		class(FourierTransform2D) :: this
		integer, optional, intent(in) :: unit
		
		integer :: effunit
		
		if( present(unit) ) then
			effunit = unit
		else
			effunit = 6
		end if
		
		write(effunit,"(a)") trim(this.str())
	end subroutine show
	
	!>
	!! @brief
	!!
	subroutine execute( this, sgn, sync, shift )
		class(FourierTransform2D) :: this
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
		
			call dfftw_execute( this.planF )
			
			if( effSync ) then
				this.iFunc.xyGrid = this.omega
				
				if( effShift ) then
					call FourierTransform2D_phase( this.iFunc )
					call FourierTransform2D_shift( this.iFunc )
				end if
			end if
			
		else if ( sgn == FourierTransform_BACKWARD ) then
		
			if( effSync ) then
				if( effShift ) then
					call FourierTransform2D_ishift( this.iFunc )
					call FourierTransform2D_phase( this.iFunc )
				end if
			end if
			
			call dfftw_execute( this.planB )
			
			if( effSync ) then
				this.iFunc.xyGrid = this.x
			end if
			
			this.iFunc = this.iFunc/real(this.nPoints(1)*this.nPoints(2),8)
			
		else
			call GOptions_error( "Bad value for sgn", "FourierTransform2D.execute()" )
		end if
	end subroutine execute
	
	!>
	!! @brief
	!!
	function FourierTransform2D_omegaGridFromData( n, h, order ) result( output )
		integer, intent(in) :: n(2)
		real(8), optional, intent(in) :: h(2)
		integer, optional, intent(in) :: order
		type(Grid2D) :: output
		
		integer :: i
		
		do i=1,2
			output.component(i) = FourierTransform_omegaGrid( n(i), h(i), order=order )
		end do
	end function FourierTransform2D_omegaGridFromData
	
	!>
	!! @brief
	!!
	function FourierTransform2D_omegaGridFromXGrid( xyGrid, order ) result( output )
		type(Grid2D), intent(in) :: xyGrid
		integer, optional, intent(in) :: order
		type(Grid2D) :: output
		
		output = FourierTransform2D_omegaGridFromData( xyGrid.nPointsVec(), xyGrid.stepSize(), order=order )
	end function FourierTransform2D_omegaGridFromXGrid
	
	!>
	!! @brief
	!!
	function FourierTransform2D_xyGridFromData( n, h, order ) result( output )
		integer, intent(in) :: n(2)
		real(8), intent(in) :: h(2)
		integer, optional, intent(in) :: order
		type(Grid2D) :: output
		
		integer :: i
		
		do i=1,2
			output.component(i) = FourierTransform_xGrid( n(i), h(i), order=order )
		end do
	end function FourierTransform2D_xyGridFromData
	
	!>
	!! @brief
	!!
	function FourierTransform2D_xyGridFromOmegaGrid( omegaGrid, order ) result( output )
		type(Grid2D), intent(in) :: omegaGrid
		integer, optional, intent(in) :: order
		type(Grid2D) :: output
		
		real(8) :: dx(2)
		
		! dx = 2*pi/n/dp
		dx = 2.0_8*MATH_PI/real(omegaGrid.nPoints(),8)/omegaGrid.stepSize()
		
		output = FourierTransform2D_xyGridFromData( omegaGrid.nPoints(), dx, order=order )
	end function FourierTransform2D_xyGridFromOmegaGrid
	
	!>
	!! @brief
	!!
	subroutine FourierTransform2D_phase_realArray( iArray, oArray )
		real(8) :: iArray(:,:)
		real(8), optional :: oArray(:,:)
		
		integer :: i
		
		do i=1,size(iArray,dim=1)
			call FourierTransform_phase( iArray(i,:), oArray(i,:) )
		end do
		
		do i=1,size(iArray,dim=2)
			call FourierTransform_phase( iArray(:,i), oArray(:,i) )
		end do
	end subroutine FourierTransform2D_phase_realArray
	
	!>
	!! @brief
	!!
	subroutine FourierTransform2D_phase_complexArray( iArray, oArray )
		complex(8) :: iArray(:,:)
		complex(8), optional :: oArray(:,:)
		
		integer :: i
		
		if( present(oArray) ) then
			do i=1,size(iArray,dim=1)
				call FourierTransform_phase( iArray(i,:), oArray(i,:) )
			end do
			
			do i=1,size(iArray,dim=2)
				call FourierTransform_phase( iArray(:,i), oArray(:,i) )
			end do
		else
			do i=1,size(iArray,dim=1)
				call FourierTransform_phase( iArray(i,:) )
			end do
			
			do i=1,size(iArray,dim=2)
				call FourierTransform_phase( iArray(:,i) )
			end do
		end if
	end subroutine FourierTransform2D_phase_complexArray
	
	!>
	!! @brief
	!!
	subroutine FourierTransform2D_iphase_realArray( iArray, oArray )
		real(8) :: iArray(:)
		real(8), optional :: oArray(:)
		
		stop "FourierTransform2D_iphase_realArray is not implemented"
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
	end subroutine FourierTransform2D_iphase_realArray
	
	!>
	!! @brief
	!! @deprecated Este metodo será eliminado si no se utiliza Mon Mar  9 16:43:34 CET 2015
	!! @todo Este metodo será eliminado si no se utiliza Mon Mar  9 16:43:34 CET 2015
	!!
	subroutine FourierTransform2D_iphase_complexArray( iArray, oArray )
		complex(8) :: iArray(:)
		complex(8), optional :: oArray(:)
		
		stop "FourierTransform2D_iphase_complexArray is not implemented"
	end subroutine FourierTransform2D_iphase_complexArray
	
	!>
	!! @brief
	!!
	subroutine FourierTransform2D_phase_RNFunction2D( iFunc, oFunc )
		class(RNFunction2D) :: iFunc
		class(RNFunction2D), optional :: oFunc
		
		integer :: i
		
		if( iFunc.isEquallyspaced() ) then
			call GOptions_warning( &
				"Grid should not be equally spaced", &
				"FourierTransform2D_phase_RNFunction2D()" &
			)
		end if
		
		if( present(oFunc) ) then
			oFunc = iFunc
			
			do i=1,iFunc.nPoints(1)
				call FourierTransform_phase( oFunc.fArray(i,:) )
			end do
			
			do i=1,iFunc.nPoints(2)
				call FourierTransform_phase( oFunc.fArray(:,i) )
			end do
		else
			do i=1,iFunc.nPoints(1)
				call FourierTransform_phase( iFunc.fArray(i,:) )
			end do
			
			do i=1,iFunc.nPoints(2)
				call FourierTransform_phase( iFunc.fArray(:,i) )
			end do
		end if
	end subroutine FourierTransform2D_phase_RNFunction2D
	
	!>
	!! @brief
	!!
	subroutine FourierTransform2D_phase_CNFunction2D( iFunc, oFunc )
		class(CNFunction2D) :: iFunc
		class(CNFunction2D), optional :: oFunc
		
		integer :: i
		
		if( iFunc.isEquallyspaced() ) then
			call GOptions_warning( &
				"Grid should not be equally spaced", &
				"FourierTransform2D_phase_CNFunction2D()" &
			)
		end if
		
		if( present(oFunc) ) then
			oFunc = iFunc
			
			do i=1,iFunc.nPoints(1)
				call FourierTransform_phase( oFunc.fArray(i,:) )
			end do
			
			do i=1,iFunc.nPoints(2)
				call FourierTransform_phase( oFunc.fArray(:,i) )
			end do
		else
			do i=1,iFunc.nPoints(1)
				call FourierTransform_phase( iFunc.fArray(i,:) )
			end do
			
			do i=1,iFunc.nPoints(2)
				call FourierTransform_phase( iFunc.fArray(:,i) )
			end do
		end if
	end subroutine FourierTransform2D_phase_CNFunction2D
	
	!>
	!! @brief
	!! @deprecated Este metodo será eliminado si no se utiliza Mon Mar  9 16:43:34 CET 2015
	!! @todo Este metodo será eliminado si no se utiliza Mon Mar  9 16:43:34 CET 2015
	!!
	subroutine FourierTransform2D_iphase_CNFunction2D( iFunc, oFunc )
		class(CNFunction2D) :: iFunc
		class(CNFunction2D), optional :: oFunc
		
		stop "FourierTransform2D_shift_RNFunction2D is not implemented"
	end subroutine FourierTransform2D_iphase_CNFunction2D
	
	!>
	!! @brief
	!!
	subroutine FourierTransform2D_shift_realArray( iArray, oArray )
		real(8) :: iArray(:,:)
		real(8), optional :: oArray(:,:)
		
		integer :: i
		
		if( present(oArray) ) then
			oArray = iArray
			
			do i=1,size(iArray,dim=1)
				call FourierTransform_shift( oArray(i,:) )
			end do
			
			do i=1,size(iArray,dim=2)
				call FourierTransform_shift( oArray(:,i) )
			end do
		else
			do i=1,size(iArray,dim=1)
				call FourierTransform_shift( iArray(i,:) )
			end do
			
			do i=1,size(iArray,dim=2)
				call FourierTransform_shift( iArray(:,i) )
			end do
		end if
	end subroutine FourierTransform2D_shift_realArray
	
	!>
	!! @brief
	!!
	subroutine FourierTransform2D_shift_complexArray( iArray, oArray )
		complex(8) :: iArray(:,:)
		complex(8), optional :: oArray(:,:)
		
		integer :: i
		
		if( present(oArray) ) then
			oArray = iArray
			
			do i=1,size(iArray,dim=1)
				call FourierTransform_shift( oArray(i,:) )
			end do
			
			do i=1,size(iArray,dim=2)
				call FourierTransform_shift( oArray(:,i) )
			end do
		else
			do i=1,size(iArray,dim=1)
				call FourierTransform_shift( iArray(i,:) )
			end do
			
			do i=1,size(iArray,dim=2)
				call FourierTransform_shift( iArray(:,i) )
			end do
		end if
	end subroutine FourierTransform2D_shift_complexArray
	
	!>
	!! @brief
	!!
	subroutine FourierTransform2D_ishift_realArray( iArray, oArray )
		real(8) :: iArray(:,:)
		real(8), optional :: oArray(:,:)
		
		integer :: i
		
		if( present(oArray) ) then
			oArray = iArray
			
			do i=1,size(iArray,dim=1)
				call FourierTransform_ishift( oArray(i,:) )
			end do
			
			do i=1,size(iArray,dim=2)
				call FourierTransform_ishift( oArray(:,i) )
			end do
		else
			do i=1,size(iArray,dim=1)
				call FourierTransform_ishift( iArray(i,:) )
			end do
			
			do i=1,size(iArray,dim=2)
				call FourierTransform_ishift( iArray(:,i) )
			end do
		end if
	end subroutine FourierTransform2D_ishift_realArray
	
	!>
	!! @brief
	!!
	subroutine FourierTransform2D_ishift_complexArray( iArray, oArray )
		complex(8) :: iArray(:,:)
		complex(8), optional :: oArray(:,:)
		
		integer :: i
		
		if( present(oArray) ) then
			oArray = iArray
			
			do i=1,size(iArray,dim=1)
				call FourierTransform_ishift( oArray(i,:) )
			end do
			
			do i=1,size(iArray,dim=2)
				call FourierTransform_ishift( oArray(:,i) )
			end do
		else
			do i=1,size(iArray,dim=1)
				call FourierTransform_ishift( iArray(i,:) )
			end do
			
			do i=1,size(iArray,dim=2)
				call FourierTransform_ishift( iArray(:,i) )
			end do
		end if
	end subroutine FourierTransform2D_ishift_complexArray
	
	!>
	!! @brief
	!!
	subroutine FourierTransform2D_shift_Grid( iGrid, oGrid )
		class(Grid2D) :: iGrid
		class(Grid2D), optional :: oGrid
		
		integer :: i
		
		if( iGrid.isEquallyspaced() ) then
			call GOptions_warning( &
				"Grid should not be equally spaced", &
				"FourierTransform2D_shift_Grid()" &
			)
		end if
		
		if( present(oGrid) ) then
			oGrid = iGrid
			
			do i=1,2
				call FourierTransform_shift( oGrid.component(i) )
			end do
		else
			do i=1,2
				call FourierTransform_shift( iGrid.component(i) )
			end do
		end if
		
	end subroutine FourierTransform2D_shift_Grid
	
	!>
	!! @brief
	!!
	subroutine FourierTransform2D_ishift_Grid( iGrid, oGrid )
		class(Grid2D) :: iGrid
		class(Grid2D), optional :: oGrid
		
		integer :: i
		
		if( .not. iGrid.isEquallyspaced() ) then
			call GOptions_warning( &
				"Grid should be equally spaced", &
				"FourierTransform2D_ishift_Grid()" &
			)
		end if
		
		if( present(oGrid) ) then
			oGrid = iGrid
			
			do i=1,2
				call FourierTransform_ishift( oGrid.component(i) )
			end do
		else
			do i=1,2
				call FourierTransform_ishift( iGrid.component(i) )
			end do
		end if
		
	end subroutine FourierTransform2D_ishift_Grid
	
	!>
	!! @brief
	!!
	subroutine FourierTransform2D_shift_RNFunction2D( iFunc, oFunc )
		class(RNFunction2D) :: iFunc
		class(RNFunction2D), optional :: oFunc
		
		stop "FourierTransform2D_shift_RNFunction2D is not implemented"
		if( iFunc.isEquallyspaced() ) then
			call GOptions_warning( &
				"Grid should not be equally spaced", &
				"FourierTransform2D_shift_Grid()" &
			)
		end if
		
		if( present(oFunc) ) then
			oFunc = iFunc
			
			call FourierTransform2D_shift_realArray( oFunc.fArray )
			call FourierTransform2D_shift( oFunc.xyGrid )
			call oFunc.xyGrid.checkEquallyspaced()
		else
			call FourierTransform2D_shift_realArray( iFunc.fArray )
			call FourierTransform2D_shift( iFunc.xyGrid )
			call iFunc.xyGrid.checkEquallyspaced()
		end if
		
	end subroutine FourierTransform2D_shift_RNFunction2D
	
	!>
	!! @brief
	!!
	subroutine FourierTransform2D_shift_CNFunction2D( iFunc, oFunc )
		class(CNFunction2D) :: iFunc
		class(CNFunction2D), optional :: oFunc
		
		integer :: i
		
		if( iFunc.isEquallyspaced() ) then
			call GOptions_warning( &
				"Grid should not be equally spaced", &
				"FourierTransform2D_shift_Grid()" &
			)
		end if
		
		if( present(oFunc) ) then
			oFunc = iFunc
			
			do i=1,iFunc.nPoints(1)
				call FourierTransform_shift( oFunc.fArray(i,:) )
			end do
			
			do i=1,iFunc.nPoints(2)
				call FourierTransform_shift( oFunc.fArray(:,i) )
			end do
			
			call FourierTransform2D_shift( oFunc.xyGrid )
		else
			do i=1,iFunc.nPoints(1)
				call FourierTransform_shift( iFunc.fArray(i,:) )
			end do
			
			do i=1,iFunc.nPoints(2)
				call FourierTransform_shift( iFunc.fArray(:,i) )
			end do
			
			call FourierTransform2D_shift( iFunc.xyGrid )
		end if
		
	end subroutine FourierTransform2D_shift_CNFunction2D
	
	!>
	!! @brief
	!!
	subroutine FourierTransform2D_ishift_CNFunction2D( iFunc, oFunc )
		class(CNFunction2D) :: iFunc
		class(CNFunction2D), optional :: oFunc
		
		integer :: i
		
		if( .not. iFunc.isEquallyspaced() ) then
			call GOptions_warning( &
				"Grid should be equally spaced", &
				"FourierTransform2D_ishift_Grid()" &
			)
		end if
		
		if( present(oFunc) ) then
			oFunc = iFunc
			
			do i=1,iFunc.nPoints(1)
				call FourierTransform_ishift( oFunc.fArray(i,:) )
			end do
			
			do i=1,iFunc.nPoints(2)
				call FourierTransform_ishift( oFunc.fArray(:,i) )
			end do
			
			call FourierTransform2D_ishift( oFunc.xyGrid )
		else
			do i=1,iFunc.nPoints(1)
				call FourierTransform_ishift( iFunc.fArray(i,:) )
			end do
			
			do i=1,iFunc.nPoints(2)
				call FourierTransform_ishift( iFunc.fArray(:,i) )
			end do
			
			call FourierTransform2D_ishift( iFunc.xyGrid )
		end if
		
	end subroutine FourierTransform2D_ishift_CNFunction2D
	
	!>
	!! @brief
	!!
	function FourierTransform2D_plan_Array( iArray, sgn, oArray ) result( plan )
		complex(8) :: iArray(:,:)
		integer, intent(in) :: sgn
		complex(8), optional :: oArray(:,:)
		integer(8) :: plan
		
		if( present(oArray) ) then
			call dfftw_plan_dft_2d( plan, size(iArray), iArray, oArray, sgn, FFTW_ESTIMATE )
		else
			call dfftw_plan_dft_2d( plan, size(iArray), iArray, iArray, sgn, FFTW_ESTIMATE )
		end if
	end function FourierTransform2D_plan_Array
	
	!>
	!! @brief
	!!
	function FourierTransform2D_plan_CNFunction2D( iFunc, sgn, oFunc ) result( plan )
		type(CNFunction2D) :: iFunc
		integer, intent(in) :: sgn
		type(CNFunction2D), optional :: oFunc
		integer(8) :: plan
		
		if( present(oFunc) ) then
			call dfftw_plan_dft_2d( plan, iFunc.nPoints(1), iFunc.nPoints(2), iFunc.fArray, oFunc.fArray, sgn, FFTW_ESTIMATE )
		else
			call dfftw_plan_dft_2d( plan, iFunc.nPoints(1), iFunc.nPoints(2), iFunc.fArray, iFunc.fArray, sgn, FFTW_ESTIMATE )
		end if
	end function FourierTransform2D_plan_CNFunction2D
	
	!>
	!! 
	!!
	subroutine FourierTransform2D_execute( plan )
		integer(8), intent(in) :: plan
		
		call dfftw_execute( plan )
	end subroutine FourierTransform2D_execute
	
	!>
	!! 
	!!
	subroutine FourierTransform2D_destroyPlan( plan )
		integer(8), intent(in) :: plan
		
		call dfftw_destroy_plan( plan )
	end subroutine FourierTransform2D_destroyPlan
	
	!>
	!! 
	!!
	subroutine FourierTransform2D_dft_CArray( iArray, oArray, sgn )
		complex(8) :: iArray(:,:)
		complex(8), optional :: oArray(:,:)
		integer, optional :: sgn
		
		integer :: effSgn
		
		integer(8) :: plan
		
		effSgn = FourierTransform_FORWARD
		if( present(sgn) ) effSgn = sgn
		
		if( present(oArray) ) then
			if( size(iArray) /= size(oArray) ) &
				call GOptions_error( "iArray and oArray have not the same size", "FourierTransform2D_dft_CArray" )
			
			call dfftw_plan_dft_2d( plan, size(iArray,dim=1), size(iArray,dim=2), iArray, oArray, effSgn, FFTW_ESTIMATE )
			call dfftw_execute( plan )
		else
			call dfftw_plan_dft_2d( plan, size(iArray,dim=1), size(iArray,dim=2), iArray, iArray, effSgn, FFTW_ESTIMATE )
			call dfftw_execute( plan )
		end if
		
		call dfftw_destroy_plan( plan )
	end subroutine FourierTransform2D_dft_CArray
	
	!>
	!! 
	!!
	subroutine FourierTransform2D_idft_Array( iArray, oArray, sgn )
		complex(8) :: iArray(:)
		complex(8), optional :: oArray(:)
		integer, optional :: sgn
		
		
		stop "FourierTransform2D_idft_Array is not implemented"
! 		integer :: effSgn
! 		
! 		integer(8) :: plan
! 		
! 		effSgn = FourierTransform2D_BACKWARD
! 		if( present(sgn) ) effSgn = sgn
! 		
! 		if( present(oArray) ) then
! 			if( size(iArray) /= size(oArray) ) &
! 				call GOptions_error( "iArray and oArray have not the same size", "FourierTransform2D_idft_Array" )
! 			
! 			call dfftw_plan_dft_1d( plan, size(iArray), iArray, oArray, effSgn, FourierTransform2DW_ESTIMATE )
! 			call dfftw_execute( plan )
! 			
! 			oArray = oArray/real( size(iArray), 8 )
! 		else
! 			call dfftw_plan_dft_1d( plan, size(iArray), iArray, iArray, effSgn, FourierTransform2DW_ESTIMATE )
! 			call dfftw_execute( plan )
! 			
! 			iArray = iArray/real( size(iArray), 8 )
! 		end if
! 		
! 		call dfftw_destroy_plan( plan )
	end subroutine FourierTransform2D_idft_Array
	
	!>
	!! 
	!!
	function FourierTransform2D_fft_CNFunction2D( iFunc, sgn ) result( oFunc )
		type(CNFunction2D), intent(in) :: iFunc
		integer, optional :: sgn
		type(CNFunction2D) :: oFunc
		
		integer :: n(2)
		real(8) :: dx(2), dp(2)
		type(Grid2D) :: xyGrid
		
		! @todo Check for checkEquallyspaced
		n = iFunc.nPoints()
		dx = iFunc.xyGrid.stepSize()
		dp = 2.0_8*Math_PI/dx/real(n,8)
		
		oFunc = iFunc
		call FourierTransform2D_dft( iFunc.fArray, oFunc.fArray, sgn=sgn )
		oFunc.xyGrid = FourierTransform2D_omegaGrid( n, dx, order=FourierTransform_SORDER )
		
		call FourierTransform2D_phase( oFunc )
		call FourierTransform2D_shift( oFunc )
		
		oFunc = oFunc*dx(1)*dx(2)/(2.0_8*Math_PI)
	end function FourierTransform2D_fft_CNFunction2D
	
	!>
	!! 
	!!
	function FourierTransform2D_ifft_CNFunction2D( iFunc, sgn ) result( oFunc )
		type(CNFunction2D) :: iFunc
		integer, optional :: sgn
		type(CNFunction2D) :: oFunc
		
		integer :: n(2)
		real(8) :: dx(2), dp(2)
		type(Grid2D) :: xyGrid
		
		integer :: i
		
		! @todo Check for checkEquallyspaced
		n = iFunc.nPoints()
		dp = iFunc.xyGrid.stepSize()
		dx = 2.0_8*Math_PI/dp/real(n,8)
		
		oFunc = iFunc
		
		call FourierTransform2D_ishift( oFunc )
		call FourierTransform2D_phase( oFunc )
		
		call FourierTransform2D_dft( oFunc.fArray, oFunc.fArray, sgn=FourierTransform_BACKWARD )
		oFunc.xyGrid = FourierTransform2D_xyGrid( n, dx, order=FourierTransform_NORDER )
		
		oFunc = oFunc*dp(1)*dp(2)/(2.0_8*Math_PI)
	end function FourierTransform2D_ifft_CNFunction2D
	
	!>
	!! @brief
	!!
	function FourierTransform2D_nft_CNFunction2D( iFunc, sgn ) result( oFunc )
		type(CNFunction2D), intent(in) :: iFunc
		integer, optional, intent(in) :: sgn
		type(CNFunction2D) :: oFunc
		
		integer :: effSgn
		
		integer :: i, j, k, l, n(2)
		real(8) :: dx(2), dp(2)
		type(Grid2D) :: xyGrid
		
		effSgn = FourierTransform_FORWARD
		if( present(sgn) ) effSgn = sgn
		
		! @todo Check for checkEquallyspaced
		n = iFunc.nPoints()
		dx = iFunc.xyGrid.stepSize()
		dp = 2.0_8*Math_PI/dx/real(n,8)
		
		oFunc = iFunc
		oFunc.xyGrid = FourierTransform2D_omegaGrid( n, dx, order=FourierTransform_NORDER )
		call oFunc.xyGrid.show()
		
		do j=1,n(2); do i=1,n(1)
			oFunc.fArray(i,j) = 0.0_8
			do l=1,n(2); do k=1,n(1)
				oFunc.fArray(i,j) = oFunc.fArray(i,j) &
					+ iFunc.fArray(k,l)*exp( real(effSgn,8)*Math_I*sum( iFunc.xyGrid.at(k,l)*oFunc.xyGrid.at(i,j) ) )
			end do; end do
		end do; end do
		
		oFunc = oFunc*dx(1)*dx(2)/(2.0_8*Math_PI)
	end function FourierTransform2D_nft_CNFunction2D

	!>
	!! @brief
	!!
	function FourierTransform2D_inft_CNFunction2D( iFunc, sgn ) result( oFunc )
		type(CNFunction2D), intent(in) :: iFunc
		integer, optional, intent(in) :: sgn
		type(CNFunction2D) :: oFunc
		
		integer :: effSgn
		
		integer :: i, j, k, l, n(2)
		real(8) :: dx(2), dp(2)
		type(Grid2D) :: xyGrid
		
		effSgn = FourierTransform_BACKWARD
		if( present(sgn) ) effSgn = sgn
		
		! @todo Check for checkEquallyspaced
		n = iFunc.nPoints()
		dp = iFunc.xyGrid.stepSize()
		dx = 2.0_8*Math_PI/dp/real(n,8)
		
		oFunc = iFunc
		oFunc.xyGrid = FourierTransform2D_xyGrid( n, dx, order=FourierTransform_NORDER )
		
		do j=1,n(2); do i=1,n(1)
			oFunc.fArray(i,j) = 0.0_8
			do l=1,n(2); do k=1,n(1)
				oFunc.fArray(i,j) = oFunc.fArray(i,j) &
					+ iFunc.fArray(k,l)*exp( real(effSgn,8)*Math_I*sum( iFunc.xyGrid.at(k,l)*oFunc.xyGrid.at(i,j) ) )
			end do; end do
		end do; end do
		
		oFunc = oFunc*dp(1)*dp(2)/(2.0_8*Math_PI)
	end function FourierTransform2D_inft_CNFunction2D
	
	!>
	!! This is neccesary only for FourierTransform2D_test()
	!!
	function funcRectangular( x, y ) result( output )
		real(8), intent(in) :: x, y
		complex(8) :: output
		
		real(8) :: a1, a2
		
		a1 = 0.5_8
		a2 = 1.0_8
		
		output = Math_ubox( a1*x )*Math_ubox( a2*y )
	end function funcRectangular
	
	!>
	!! This is neccesary only for FourierTransform2D_test()
	!!
	function FfuncRectangular( omega1, omega2 ) result( output )
		real(8), intent(in) :: omega1, omega2
		complex(8) :: output
		
		real(8) :: a1, a2
		
		a1 = 0.5_8
		a2 = 1.0_8
		
		output = Math_nsinc( omega1/(2.0_8*Math_PI*a1) )*Math_nsinc( omega2/(2.0_8*Math_PI*a2) )/(2.0_8*Math_PI)/a1/a2
	end function FfuncRectangular
	
	!>
	!! @bief test
	!!
	subroutine FourierTransform2D_test()
		type(Grid2D) :: xyGrid, omegaGrid
		type(CNFunction2D) :: funcA, dFuncA, fftFuncA, fftDFuncA
		type(CNFunction2D) :: funcB, dFuncB, fftFuncB, fftDFuncB
		type(CNFunction2D) :: funcAB, dFuncAB, fftFuncAB, fftDFuncAB
		type(RNFunction2D) :: aSpectrum, pSpectrum
		type(FourierTransform2D) :: fft
		real(8) :: exactValue
		real(8) :: value
		integer :: i, j
		real(8), allocatable :: array(:,:)
		complex(8), allocatable :: cArray(:,:)
		integer(8) :: planF, planB
		character :: cBuffer
		
		write(*,*) "----------------------------------"
		write(*,*) " Verifing omega Grid constructors "
		write(*,*) "----------------------------------"
		
		xyGrid = FourierTransform2D_omegaGridFromData( [10,11], [0.1_8,0.2_8], FourierTransform_NORDER )
		call xyGrid.show()
		call xyGrid.save()
		
		xyGrid = FourierTransform2D_omegaGridFromData( [10,11], [0.1_8,0.2_8], FourierTransform_SORDER )
		call xyGrid.show()
		call xyGrid.save()
		
		write(*,*) "-------------------------------"
		write(*,*) " Verifing xy Grid constructors "
		write(*,*) "-------------------------------"
		
		xyGrid = FourierTransform2D_xyGridFromData( [10,11], [0.1_8,0.2_8], FourierTransform_NORDER )
		call xyGrid.show()
		call xyGrid.save()
		
		xyGrid = FourierTransform2D_xyGridFromData( [10,11], [0.1_8,0.2_8], FourierTransform_SORDER )
		call xyGrid.show()
		call xyGrid.save()
		
		return
		call GOptions_doYouWantToContinue()
		
! 		write(*,*) "-------------------------------------------"
! 		write(*,*) " FourierTransform2D of a gaussian function "
! 		write(*,*) "-------------------------------------------"
! 		
! 		call xyGrid.init( [-2.5_8,-2.5_8], [2.5_8,2.5_8], size=[90,110] )
! 		call funcA.init( xyGrid, funcRectangular )
! 		write(*,"(A)") "input (.func) = "
! 		call funcA.show()
! 		call funcA.save( ".func", format=BLKS_FORMAT )
! 		
! 		funcB = FourierTransform2D_fft( funcA )
! 		write(*,"(A)") "exact (.Ffunc) = "
! 		call funcB.show()
! 		call funcB.save( ".Ffunc", format=BLKS_FORMAT )
! 		
! 		call xyGrid.init( [-54.677675_8,-67.241507_8], [55.920349_8,68.486720_8], size=[90,110] )
! 		call funcB.init( xyGrid, FfuncRectangular )
! ! 		funcB = FourierTransform2D_nft( funcA )
! 		write(*,"(A)") "exact (.Fexact) = "
! 		call funcB.show()
! 		call funcB.save( ".Fexact", format=BLKS_FORMAT )
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
! 		write(*,*) " iFourierTransform2D of a gaussian function"
! 		write(*,*) "-----------------------------"
! 		
! 		call xyGrid.init( [-2.5_8,-2.5_8], [2.5_8,2.5_8], size=[90,110] )
! 		call funcA.init( xyGrid, funcRectangular )
! 		funcA = FourierTransform2D_nft( funcA )
! 		write(*,"(A)") "input (.Ffunc) = "
! 		call funcA.show()
! 		call funcA.save( ".Ffunc", format=BLKS_FORMAT )
! 		
! 		write(*,"(A)") "iFourierTransform2D (.func) = "
! 		funcB = funcA
! 		funcB = FourierTransform2D_ifft( funcB )
! 		call funcB.show()
! 		call funcB.save( ".func", format=BLKS_FORMAT )
! 		
! 		funcB = FourierTransform2D_inft( funcA )
! 		write(*,"(A)") "exact (.exact) = "
! 		call funcB.show()
! 		call funcB.save( ".exact", format=BLKS_FORMAT )
! 		
! ! 		call system( "echo splot \"//achar(34)//".Ffunc\"//achar(34)//" u 1:2:3 w l, \"//achar(34)//"\"//achar(34)//" u 1:2:4 w l | gnuplot -p" )
! ! 		call system( "echo splot \"//achar(34)//".func\"//achar(34)//" u 1:2:3 w l, \"//achar(34)//"\"//achar(34)//" u 1:2:4 w l | gnuplot -p" )
! ! 		call system( "echo splot \"//achar(34)//".exact\"//achar(34)//" u 1:2:3 w l, \"//achar(34)//"\"//achar(34)//" u 1:2:4 w l | gnuplot -p" )
! 		call system( "echo splot \"//achar(34)//".Ffunc\"//achar(34)//" u 1:2:3 w l | gnuplot -p" )
! 		call system( "echo splot \"//achar(34)//".func\"//achar(34)//" u 1:2:3 w l | gnuplot -p" )
! 		call system( "echo splot \"//achar(34)//".exact\"//achar(34)//" u 1:2:3 w l | gnuplot -p" )
! 		call GOptions_doYouWantToContinue()
! 		call system( "rm .Ffunc .func .exact" )
		
! 		write(*,*) "-------------------------------"
! 		write(*,*) " iFourierTransform2D( FourierTransform2D(func) ). CNFunction2D "
! 		write(*,*) "-------------------------------"
! 		
! 		call xyGrid.init( [-2.5_8,-2.5_8], [2.5_8,2.5_8], size=[90,110] )
! 		call funcA.init( xyGrid, funcRectangular )
! 
! 		write(*,"(A)") "input (.func) = "
! 		call funcA.show()
! 		call funcA.save( ".func", format=BLKS_FORMAT )
! 		
! 		write(*,"(A)") "FourierTransform2D (.Ffunc) = "
! 		funcB = FourierTransform2D_fft( funcA )
! 		call funcB.show()
! 		call funcB.save( ".Ffunc", format=BLKS_FORMAT )
! 		
! 		write(*,"(A)") "iFourierTransform2D (.iFFfunc) = "
! 		funcA = FourierTransform2D_ifft( funcB )
! 		call funcA.show()
! 		call funcA.save( ".iFFfunc", format=BLKS_FORMAT )
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
! 		write(*,*) " FourierTransform2D( iFourierTransform2D(Ffunc) ). CNFunction2D "
! 		write(*,*) "--------------------------------"
! 		
! 		call xyGrid.init( [-2.5_8,-2.5_8], [2.5_8,2.5_8], size=[90,110] )
! 		call funcA.init( xyGrid, funcRectangular )
! 		funcA = FourierTransform2D_nft( funcA )
! 
! 		write(*,"(A)") "input (.Ffunc) = "
! 		call funcA.show()
! 		call funcA.save( ".Ffunc", format=BLKS_FORMAT )
! 		
! 		write(*,"(A)") "iFourierTransform2D (.iFfunc) = "
! 		funcA = FourierTransform2D_ifft( funcA )
! 		call funcA.show()
! 		call funcA.save( ".iFfunc", format=BLKS_FORMAT )
! 		
! 		write(*,"(A)") "FourierTransform2D (.FiFfunc) = "
! 		funcA = FourierTransform2D_fft( funcA )
! 		call funcA.show()
! 		call funcA.save( ".FiFfunc", format=BLKS_FORMAT )
! 		
! 		call system( "echo splot \"//achar(34)//".Ffunc\"//achar(34)//" u 1:2:3 w l | gnuplot -p" )
! 		call system( "echo splot \"//achar(34)//".iFfunc\"//achar(34)//" u 1:2:3 w l | gnuplot -p" )
! 		call system( "echo splot \"//achar(34)//".FiFfunc\"//achar(34)//" u 1:2:3 w l | gnuplot -p" )
! 		call GOptions_doYouWantToContinue()
! 		call system( "rm .Ffunc .iFfunc .FiFfunc" )
		
		write(*,*) "-----------------------------------------------------"
		write(*,*) " Verifing FourierTransform2D CNFunction2D with plans "
		write(*,*) "-----------------------------------------------------"
		
		call xyGrid.init( [-2.5_8,-2.5_8], [2.5_8,2.5_8], size=[90,110] )
		call funcA.init( xyGrid, funcRectangular )
		write(*,"(A)") "input (.func) = "
		call funcA.show()
		call funcA.save( ".func", format=BLKS_FORMAT )
		
		planF = FourierTransform2D_plan( funcA, FourierTransform_FORWARD )
		planB = FourierTransform2D_plan( funcA, FourierTransform_BACKWARD )
		
		write(*,"(A)") "FourierTransform2D (.Ffunc) = "
		call FourierTransform2D_execute( planF )
		funcA.xyGrid = FourierTransform2D_omegaGrid( funcA.xyGrid, order=FourierTransform_SORDER )
		call FourierTransform2D_phase( funcA )
		call FourierTransform2D_shift( funcA )
		write(*,*) "Entonces 0", xyGrid.stepSize(1)*xyGrid.stepSize(2)/(2.0_8*Math_PI)
		value = xyGrid.stepSize(1)*xyGrid.stepSize(2)/(2.0_8*Math_PI)
		funcA = funcA*value
		write(*,*) "Entonces 1"
		call funcA.show()
		call funcA.save( ".Ffunc", format=BLKS_FORMAT )
		
! 		write(*,"(A)") "iFourierTransform2D (.iFFfunc) = "
! 		call FourierTransform2D_ishift( funcA )
! 		call FourierTransform2D_phase( funcA )
! 		call FourierTransform2D_execute( planB )
! 		funcA.xyGrid = FourierTransform2D_xyGrid( funcA.xyGrid, order=FourierTransform_NORDER )
! 		funcA = funcA/real( funcA.nPoints(1)*funcA.nPoints(2), 8 )
! 		call funcA.show()
! 		call funcA.save( ".iFFfunc", format=BLKS_FORMAT )
		
		call system( "echo splot \"//achar(34)//".func\"//achar(34)//" u 1:2:3 w l | gnuplot -p" )
		call system( "echo splot \"//achar(34)//".Ffunc\"//achar(34)//" u 1:2:3 w l | gnuplot -p" )
! 		call system( "echo splot \"//achar(34)//".iFFfunc\"//achar(34)//" u 1:2:3 w l | gnuplot -p" )
		call GOptions_doYouWantToContinue()
		call system( "rm .func .Ffunc .iFFfunc" )

		call FourierTransform2D_destroyPlan( planF )
		call FourierTransform2D_destroyPlan( planB )
		
! 		write(*,*) "------------------------------------------"
! 		write(*,*) " iFourierTransform2D( FourierTransform2D(func) ). CNFunction2D with plans"
! 		write(*,*) "------------------------------------------"
! 		
! 		call xyGrid.init( [-2.5_8,-2.5_8], [2.5_8,2.5_8], size=[90,110] )
! 		call funcA.init( xyGrid, funcRectangular )
! 		write(*,"(A)") "input (.func) = "
! 		call funcA.show()
! 		call funcA.save( ".func", format=BLKS_FORMAT )
! 		
! 		planF = FourierTransform2D_plan( funcA, FourierTransform_FORWARD )
! 		planB = FourierTransform2D_plan( funcA, FourierTransform_BACKWARD )
! 		
! 		call FourierTransform2D_execute( planF )
! 		call FourierTransform2D_execute( planB )
! 		funcA = funcA/real( funcA.nPoints(1)*funcA.nPoints(2), 8 )
! 		
! 		call funcA.show()
! 		call funcA.save( ".iFFfunc", format=BLKS_FORMAT )
! 		
! 		call system( "echo splot \"//achar(34)//".func\"//achar(34)//" u 1:2:3 w l | gnuplot -p" )
! 		call system( "echo splot \"//achar(34)//".iFFfunc\"//achar(34)//" u 1:2:3 w l | gnuplot -p" )
! 		call GOptions_doYouWantToContinue()
! 		call system( "rm .func .iFFfunc" )
! 
! 		call FourierTransform2D_destroyPlan( planF )
! 		call FourierTransform2D_destroyPlan( planB )
		
		write(*,*) "-----------------------------------------"
		write(*,*) " Verifing FourierTransform2D CNFunction2D oriented object"
		write(*,*) "-----------------------------------------"
		
		call xyGrid.init( [-2.5_8,-2.5_8], [2.5_8,2.5_8], size=[90,110] )
		call funcA.init( xyGrid, funcRectangular )
		write(*,"(A)") "input (.func) = "
		call funcA.show()
		call funcA.save( ".func", format=BLKS_FORMAT )
		
		call fft.init( funcA, FourierTransform_SPATIAL_DOMAIN )
		
		call fft.execute( FourierTransform_FORWARD, sync=.true., shift=.true. )
		write(*,"(A)") "FourierTransform2D (.Ffunc) = "
		call funcA.show()
		call funcA.save( ".Ffunc", format=BLKS_FORMAT )
		
		call fft.execute( FourierTransform_BACKWARD, sync=.true., shift=.true. )
		write(*,"(A)") "iFourierTransform2D (.iFFfunc) = "
		call funcA.show()
		call funcA.save( ".iFFfunc", format=BLKS_FORMAT )
		
		call system( "echo splot \"//achar(34)//".func\"//achar(34)//" u 1:2:3 w l | gnuplot -p" )
		call system( "echo splot \"//achar(34)//".Ffunc\"//achar(34)//" u 1:2:3 w l | gnuplot -p" )
		call system( "echo splot \"//achar(34)//".iFFfunc\"//achar(34)//" u 1:2:3 w l | gnuplot -p" )
		call GOptions_doYouWantToContinue()
		call system( "rm .func .Ffunc .iFFfunc" )
		
! 		write(*,*) "--------------------------------------"
! 		write(*,*) " Second derivative via FourierTransform2D with plans"
! 		write(*,*) "--------------------------------------"
! 		
! 		call xyGrid.init( -3.0_8*Math_PI, 3.0_8*Math_PI, 1001 )
! 		omegaGrid = FourierTransform2D_omegaGrid( xyGrid )
! 		call funcA.init( xyGrid, funcTest )
! 		write(*,"(A)") "input (.func) = "
! 		call funcA.show()
! 		call funcA.save(".func")
! 		
! 		planF = FourierTransform2D_plan( funcA, FourierTransform2D_FORWARD )
! 		planB = FourierTransform2D_plan( funcA, FourierTransform2D_BACKWARD )
! 		
! 		call FourierTransform2D_execute( planF )
! 		
! 		funcA.yArray = ( Math_I*fft.omega )**2*funcA.yArray
! 		
! 		write(*,"(A)") "iFourierTransform2D (.dfunc) = "
! 		call FourierTransform2D_execute( planB )
! 		funcA = funcA/real( funcA.nPoints(), 8 )
! 		call funcA.show()
! 		call funcA.save(".dfunc")
! 		
! 		call funcB.init( xyGrid, d2funcTest )
! 		write(*,"(A)") "exact (.exact) = "
! 		call funcB.show()
! 		call funcB.save(".exact")
! 		
! ! 		call system( "echo plot \"//achar(34)//".func\"//achar(34)//" u 1:2 w l lw 1.5, \"//achar(34)//"\"//achar(34)//" u 1:3 w l lw 1.5 | gnuplot -p" )
! ! 		call system( "echo plot \"//achar(34)//".dfunc\"//achar(34)//" u 1:2 w l lw 1.5, \"//achar(34)//"\"//achar(34)//" u 1:3 w l lw 1.5 | gnuplot -p" )
! ! 		call system( "echo plot \"//achar(34)//".exact\"//achar(34)//" u 1:2 w l lw 1.5, \"//achar(34)//"\"//achar(34)//" u 1:3 w l lw 1.5 | gnuplot -p" )
! ! 		call GOptions_doYouWantToContinue()
! 		call system( "rm .func .dfunc .exact" )
! 
! 		call FourierTransform2D_destroyPlan( planF )
! 		call FourierTransform2D_destroyPlan( planB )
! 
! 		write(*,*) "------------------------------------------------------"
! 		write(*,*) " Second derivative via FourierTransform2D CNFunction2D oriented object"
! 		write(*,*) "------------------------------------------------------"
! 		
! 		call xyGrid.init( -3.0_8*Math_PI, 3.0_8*Math_PI, 1001 )
! 		call funcA.init( xyGrid, funcTest )
! 		write(*,"(A)") "input (.func) = "
! 		call funcA.show()
! 		call funcA.save(".func")
! 		
! 		call fft.init( funcA, FourierTransform_SPATIAL_DOMAIN )
! 		
! 		call fft.execute( FourierTransform2D_FORWARD )
! 		
! 		funcA.yArray = ( Math_I*fft.omega )**2*funcA.yArray
! 		
! 		call fft.execute( FourierTransform2D_BACKWARD )
! 		
! 		write(*,"(A)") "iFourierTransform2D (.dfunc) = "
! 		call funcA.show()
! 		call funcA.save(".dfunc")
! 		
! 		call funcB.init( xyGrid, d2funcTest )
! 		write(*,"(A)") "exact (.exact) = "
! 		call funcB.show()
! 		call funcB.save(".exact")
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
! ! 		call xyGrid.init( -3.0_8*Math_PI, 3.0_8*Math_PI, 1001 )
! 		call xyGrid.init( -15.0_8, 15.0_8, 1001 )
! 		call funcA.init( xyGrid, funcTest )
! 		write(*,"(A)") "input (.func) = "
! 		call funcA.show()
! 		call funcA.save(".func")
! 		
! 		aSpectrum = FourierTransform2D_powerSpectrum( funcA )
! 		
! 		write(*,"(A)") "aspec (.asfunc) = "
! 		call aSpectrum.show()
! 		call aSpectrum.save(".asfunc")
! 		
! 		pSpectrum = FourierTransform2D_phaseSpectrum( funcA )
! 		
! 		write(*,"(A)") "pspec (.psfunc) = "
! 		call pSpectrum.show()
! 		call pSpectrum.save(".psfunc")
! 		
! 		call system( "echo plot \"//achar(34)//".func\"//achar(34)//" u 1:2 w l lw 1.5, \"//achar(34)//"\"//achar(34)//" u 1:3 w l lw 1.5 | gnuplot -p" )
! 		call system( "echo plot \"//achar(34)//".asfunc\"//achar(34)//" u 1:2 w l lw 1.5 | gnuplot -p" )
! 		call system( "echo plot \"//achar(34)//".psfunc\"//achar(34)//" u 1:2 w l lw 1.5 | gnuplot -p" )
! 		call GOptions_doYouWantToContinue()
! 		call system( "rm .func .asfunc .psfunc" )
! 		
! 		write(*,*) "--------------------------------------------------------"
! 		write(*,*) " Filtering by amplitude. FourierTransform2D CNFunction2D oriented object"
! 		write(*,*) "--------------------------------------------------------"
! 		
! 		call xyGrid.init( -3.0_8*Math_PI, 3.0_8*Math_PI, 1001 )
! 		call funcA.init( xyGrid, funcTestWithNoise )
! 		write(*,"(A)") "input (.func) = "
! 		call funcA.show()
! 		call funcA.save(".func")
! 		
! 		call fft.init( funcA, FourierTransform_SPATIAL_DOMAIN )
! 		
! 		call fft.execute( FourierTransform2D_FORWARD )
! 		
! 		where( abs( funcA.yArray ) <= 20.0_8 ) funcA.yArray = 0.0_8
! 		
! 		call fft.execute( FourierTransform2D_BACKWARD )
! 		
! 		write(*,"(A)") "iFourierTransform2D (.ffunc) = "
! 		call funcA.show()
! 		call funcA.save(".ffunc")
! 		
! ! 		call system( "echo plot \"//achar(34)//".func\"//achar(34)//" u 1:2 w l lw 1.5, \"//achar(34)//"\"//achar(34)//" u 1:3 w l lw 1.5 | gnuplot -p" )
! ! 		call system( "echo plot \"//achar(34)//".ffunc\"//achar(34)//" u 1:2 w l lw 1.5, \"//achar(34)//"\"//achar(34)//" u 1:3 w l lw 1.5 | gnuplot -p" )
! ! 		call GOptions_doYouWantToContinue()
! 		call system( "rm .func .ffunc" )
! 		
! 		write(*,*) "--------------------------------------------------------"
! 		write(*,*) " Filtering by frequency. FourierTransform2D CNFunction2D oriented object"
! 		write(*,*) "--------------------------------------------------------"
! 		
! 		call xyGrid.init( -3.0_8*Math_PI, 3.0_8*Math_PI, 1001 )
! 		call funcA.init( xyGrid, funcTestWithNoise )
! 		write(*,"(A)") "input (.func) = "
! 		call funcA.show()
! 		call funcA.save(".func")
! 		
! 		call fft.init( funcA, FourierTransform_SPATIAL_DOMAIN )
! 		
! 		call fft.execute( FourierTransform2D_FORWARD )
! 		
! 		do i=1,funcA.nPoints()
! 			if( abs( fft.omega(i) ) > 10.0_8 ) then
! 				funcA.yArray(i) = 0.0_8
! 			end if
! 		end do
! 		
! 		call fft.execute( FourierTransform2D_BACKWARD )
! 		
! 		write(*,"(A)") "iFourierTransform2D (.ffunc) = "
! 		call funcA.show()
! 		call funcA.save(".ffunc")
! 		
! ! 		call system( "echo plot \"//achar(34)//".func\"//achar(34)//" u 1:2 w l lw 1.5, \"//achar(34)//"\"//achar(34)//" u 1:3 w l lw 1.5 | gnuplot -p" )
! ! 		call system( "echo plot \"//achar(34)//".ffunc\"//achar(34)//" u 1:2 w l lw 1.5, \"//achar(34)//"\"//achar(34)//" u 1:3 w l lw 1.5 | gnuplot -p" )
! ! 		call GOptions_doYouWantToContinue()
! 		call system( "rm .func .ffunc" )
		
	end subroutine FourierTransform2D_test
	
end module FourierTransform2D_

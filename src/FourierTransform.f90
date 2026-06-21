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

!!
!! Taken from: http://www%cs%otago%ac%nz/cosc453/student_tutorials/fourier_analysis%pdf
!!
!! In most of the textbooks, this form of the
!! Fourier transform is written as
!!        F(u) = |F(u)|exp(i\theta(u))        (5)
!! but it’s essentially the same thing.
!!
!! There are some words that we use frequently when talking about Fourier
!! transform. The magnitude |F(u)|from equation 5 is called the Fourier spectrum
!! of f(x) and \theta(u) is phase angle. The square of the spectrum,
!! |F(u)|^2 = R(u)^2+I(u)^2 is often denoted as P(u) and is called the
!! power spectrum of f(x).
!!
!! The term spectral density is also commonly used to denote the power spectrum.
!! The Fourier spectrum is often plotted against values of ❩
!! . The Fourier spectrum
!! is useful because it can be easily plotted against ❩
!! on a piece of paper. (See figure
!! 4 for an example of the Fourier spectrum.) Note that XXXX ’s themselves are hard
!! to plot against ❩
!! on the 2-D plane because they are complex numbers

module FourierTransform_
! 	use, intrinsic :: ISO_C_Binding!, only: C_Ptr
	use GOptions_
	use String_
	use Math_
	use Grid_
	use CNFunction_
	use RNFunction_
	use RandomUtils_
	use FFTW3_
	implicit none
	private
	
	public :: &
		FourierTransform_omegaArray, &
		FourierTransform_omegaGrid, &
		FourierTransform_xGrid, &
		FourierTransform_phase, &
		FourierTransform_iphase, &
		FourierTransform_shift, &
		FourierTransform_ishift, &
		FourierTransform_plan, &
		FourierTransform_execute, &
		FourierTransform_destroyPlan, &
		FourierTransform_dft, &
		FourierTransform_idft, &
		FourierTransform_fft, &
		FourierTransform_ifft, &
		FourierTransform_spectrum, &
		FourierTransform_realPartSpectrum, &
		FourierTransform_imagPartSpectrum, &
		FourierTransform_phaseSpectrum, &
		FourierTransform_powerSpectrum, &
		FourierTransform_nft, &
		FourierTransform_inft, &
		FourierTransform_filterByAmplitude, &
		FourierTransform_filterByFrequency, &
		FourierTransform_derivate
		
	type, public :: FourierTransform
		class(CNFunction), pointer, private :: iFunc => null()
#ifdef __GFORTRAN__
		type(C_Ptr), private :: planF, planB
#else
		integer(8), private :: planF, planB
#endif
		
		integer :: nPoints
		type(Grid) :: x
		type(Grid) :: omega
		
		contains
			procedure :: init
			final :: destroy
			procedure :: str
			procedure :: show
			procedure :: execute
! 			procedure :: filter

			! Esto hay que probarlo para la siguiente versión del compilador
			! ya que en ifort es necesario tener un objeto instanciado para poder
			! acceder a estas funciones, sin embergo en otros compiladores ya está arreglado
! 			generic :: FourierTransform_omegaGrid => FourierTransform_omegaGridFromData, FourierTransform_omegaGridFromXGrid
! 			procedure, NOPASS :: FourierTransform_omegaGridFromData
! 			procedure, NOPASS :: FourierTransform_omegaGridFromXGrid
! 
! 			generic :: FourierTransform_shift => FourierTransform_shift_realArray, FourierTransform_shift_complexArray, FourierTransform_shift_Grid, 
! FourierTransform_shift_CNFunction
! 			procedure, NOPASS :: FourierTransform_shift_realArray
! 			procedure, NOPASS :: FourierTransform_shift_complexArray
! 			procedure, NOPASS :: FourierTransform_shift_Grid
! 			procedure, NOPASS :: FourierTransform_shift_CNFunction
! 
! 			generic :: FourierTransform_ishift => FourierTransform_ishift_realArray, FourierTransform_ishift_complexArray, FourierTransform_ishift_Grid, 
! FourierTransform_ishift_CNFunction
! 			procedure, NOPASS :: FourierTransform_ishift_realArray
! 			procedure, NOPASS :: FourierTransform_ishift_complexArray
! 			procedure, NOPASS :: FourierTransform_ishift_Grid
! 			procedure, NOPASS :: FourierTransform_ishift_CNFunction
! 
! 			generic :: FourierTransform_plan => FourierTransform_plan_Array, FourierTransform_plan_CNFunction
! 			procedure, NOPASS :: FourierTransform_plan_Array
! 			procedure, NOPASS :: FourierTransform_plan_CNFunction
! 
! 			generic :: FourierTransform_fft => FourierTransform_dft_CArray, FourierTransform_fft_CNFunction
! 			procedure, NOPASS :: FourierTransform_dft_CArray
! 			procedure, NOPASS :: FourierTransform_fft_CNFunction
! 
! 			generic :: FourierTransform_ifft => FourierTransform_idft_Array, FourierTransform_ifft_CNFunction
! 			procedure, NOPASS :: FourierTransform_idft_Array
! 			procedure, NOPASS :: FourierTransform_ifft_CNFunction
	end type FourierTransform
	
	type, public :: FourierTransform_Window
		integer :: type = FourierTransform_WINDOW_NONE
		real(8) :: param = -1.0_8 !< Este parametro no pode ser menor que cero
		logical :: centered = .true.
		character(100) :: oFile  = FString_NULL !< @todo Por alguna razon no funciona si pongo character(100) por ejemplo
	end type FourierTransform_Window
	
    interface FourierTransform_omegaGrid
		module procedure FourierTransform_omegaGridFromData
		module procedure FourierTransform_omegaGridFromXGrid
    end interface FourierTransform_omegaGrid

    interface FourierTransform_xGrid
		module procedure FourierTransform_xGridFromData
		module procedure FourierTransform_xGridFromOmegaGrid
    end interface FourierTransform_xGrid

    interface FourierTransform_phase
        module procedure FourierTransform_phase_realArray
        module procedure FourierTransform_phase_complexArray
        module procedure FourierTransform_phase_RNFunction
        module procedure FourierTransform_phase_CNFunction
    end interface FourierTransform_phase
    
    interface FourierTransform_iphase
        module procedure FourierTransform_iphase_realArray
        module procedure FourierTransform_iphase_complexArray
        module procedure FourierTransform_iphase_CNFunction
    end interface FourierTransform_iphase
	
    interface FourierTransform_shift
        module procedure FourierTransform_shift_realArray
        module procedure FourierTransform_shift_complexArray
        module procedure FourierTransform_shift_Grid
        module procedure FourierTransform_shift_RNFunction
        module procedure FourierTransform_shift_CNFunction
    end interface FourierTransform_shift

    interface FourierTransform_ishift
        module procedure FourierTransform_ishift_realArray
        module procedure FourierTransform_ishift_complexArray
        module procedure FourierTransform_ishift_Grid
        module procedure FourierTransform_ishift_CNFunction
    end interface FourierTransform_ishift

    interface FourierTransform_plan
        module procedure FourierTransform_plan_Array
        module procedure FourierTransform_plan_CNFunction
    end interface FourierTransform_plan
    
    interface FourierTransform_dft
        module procedure FourierTransform_dft_CArray
    end interface FourierTransform_dft
    
    interface FourierTransform_idft
        module procedure FourierTransform_idft_Array
    end interface FourierTransform_idft

    interface FourierTransform_fft
        module procedure FourierTransform_fft_CNFunction
    end interface FourierTransform_fft
    
    interface FourierTransform_ifft
        module procedure FourierTransform_ifft_CNFunction
    end interface FourierTransform_ifft

    interface FourierTransform_spectrum
        module procedure FourierTransform_spectrumC
        module procedure FourierTransform_spectrumR
    end interface FourierTransform_spectrum
    
    interface FourierTransform_realPartSpectrum
        module procedure FourierTransform_realPartSpectrumC
        module procedure FourierTransform_realPartSpectrumR
    end interface FourierTransform_realPartSpectrum
    
    interface FourierTransform_imagPartSpectrum
        module procedure FourierTransform_imagPartSpectrumC
        module procedure FourierTransform_imagPartSpectrumR
    end interface FourierTransform_imagPartSpectrum
    
    interface FourierTransform_powerSpectrum
        module procedure FourierTransform_powerSpectrumC
        module procedure FourierTransform_powerSpectrumR
    end interface FourierTransform_powerSpectrum

    interface FourierTransform_phaseSpectrum
        module procedure FourierTransform_phaseSpectrumC
        module procedure FourierTransform_phaseSpectrumR
    end interface FourierTransform_phaseSpectrum

    interface FourierTransform_nft
        module procedure FourierTransform_nft_CNFunction
    end interface FourierTransform_nft

    interface FourierTransform_inft
        module procedure FourierTransform_inft_CNFunction
    end interface FourierTransform_inft
	
    interface FourierTransform_derivate
        module procedure FourierTransform_derivate_CNFunction
    end interface FourierTransform_derivate
	
	contains
	
	!>
	!! @brief Constructor
	!! @todo En algunas ocasiones es importante hacer la transfomrda de no todos los puntos
	!!
	subroutine init( this, iFunc, domain, oFunc )
		class(FourierTransform) :: this 
		class(CNFunction), target, intent(in) :: iFunc
		integer(8), intent(in), optional :: domain
		class(CNFunction), target, optional, intent(in) :: oFunc
		
		integer(8) :: effDomain
		
		
		effDomain = FourierTransform_SPATIAL_DOMAIN
		if( present(domain) ) effDomain = domain
		
		if( present(oFunc) ) call GOptions_error( "oFunc parameter is not implamented yet", "FourierTransform.init()" )
		
		! casos: 1) asociado a iFunc Target, 2) asociado a un target diferente, 3) no asociado
		if( .not. associated( this%iFunc, target=iFunc ) ) then
			if( associated(this%iFunc) ) then
				call dfftw_destroy_plan( this%planF )
				call dfftw_destroy_plan( this%planB )
				
				nullify(this%iFunc)
			end if
			
			this%iFunc => iFunc
			this%nPoints = iFunc%nPoints() ! << Hay que cambiar para hacer la transformada de algunos puntos
			
			call dfftw_plan_dft_1d( this%planF, this%nPoints, this%iFunc%fArray, this%iFunc%fArray, FFTW_FORWARD, FFTW_ESTIMATE )
			call dfftw_plan_dft_1d( this%planB, this%nPoints, this%iFunc%fArray, this%iFunc%fArray, FFTW_BACKWARD, FFTW_ESTIMATE )
			
			if( effDomain == FourierTransform_SPATIAL_DOMAIN ) then
				
				this%x = this%iFunc%xGrid ! << Hay que cambiar para hacer la transformada de algunos puntos
				this%omega = FourierTransform_omegaGrid( this%x )
				
			else if( effDomain == FourierTransform_FREQUENCY_DOMAIN ) then
				
				call GOptions_warning( "FourierTransform_FREQUENCY_DOMAIN have not been tested yet", "FourierTransform.init()" )
				this%omega = this%iFunc%xGrid ! << Hay que cambiar para hacer la transformada de algunos puntos
				this%x = FourierTransform_xGrid( this%omega )
			
			end if
		end if
		
	end subroutine init
	
	!>
	!! @brief Destructor
	!!
	subroutine destroy( this )
		type(FourierTransform) :: this
		
		if( associated(this%iFunc) ) then
			call dfftw_destroy_plan( this%planF )
			call dfftw_destroy_plan( this%planB )
			
			nullify(this%iFunc)
		end if
		
		this%nPoints = -1
	end subroutine destroy
	
	!>
	!! @brief
	!!
	function str( this ) result( output )
		class(FourierTransform) :: this 
		character(len=200) :: output
		
		integer :: fmt
		character(len=200) :: strBuffer
		
		output = ""
		
		output = trim(output)//"<FourierTransform:"
		
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
		class(FourierTransform) :: this
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
		class(FourierTransform) :: this
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
			
			! call dfftw_execute( this%planF ) << @todo Es un error conocido del compilador de intel. ver http://www%fftw%org/doc/Plan-execution-in-Fortran%html
			call dfftw_execute_dft( this%planF, this%iFunc%fArray, this%iFunc%fArray )
			
			if( effSync ) then
				this%iFunc%xGrid = this%omega
				
				if( effShift ) then
					call FourierTransform_phase( this%iFunc )
					call FourierTransform_shift( this%iFunc )
				end if
			end if
			
		else if ( sgn == FourierTransform_BACKWARD ) then
		
			if( effSync ) then
				if( effShift ) then
					call FourierTransform_ishift( this%iFunc )
					call FourierTransform_phase( this%iFunc )
				end if
			end if
			
			! call dfftw_execute( this%planB ) << @todo Es un error conocido del compilador de intel. ver http://www%fftw%org/doc/Plan-execution-in-Fortran%html
			call dfftw_execute_dft( this%planB, this%iFunc%fArray, this%iFunc%fArray )
			
			if( effSync ) then
				this%iFunc%xGrid = this%x
			end if
			
			this%iFunc = this%iFunc/real(this%nPoints,8)
			
		else
			call GOptions_error( "Bad value for sgn", "FourierTransform.execute()" )
		end if
	end subroutine execute
	
	!>
	!! @brief Return the Discrete Fourier Transform sample frequencies. ( see: numpy%fft%fftfreq )
	!! 
	!! The returned float array f contains the frequency bin centers in cycles per unit of the sample spacing (with zero at the start).
	!! For instance, if the sample spacing is in seconds, then the frequency unit is cycles/second.
	!! 
	!! Given a window length n and a sample spacing d:
	!! 
	!! f = [0, 1, ..., ceil(n/2),  -ceil(n/2)+1, ..., -1] / (d*n)   if n is even
	!! f = [0, 1, ..., ceil(n/2),    -ceil(n/2), ..., -1] / (d*n)   if n is odd
	!!
	!! @param n : int. Window length.
	!! @param h : scalar, optional. Sample spacing (inverse of the sampling rate). Defaults to 1.
	!! @returns:	f : ndarray. Array of length n containing the sample frequencies.
	!!
	subroutine FourierTransform_omegaArray( array, n, h, order )
		real(8) :: array(:)
		integer, intent(in) :: n
		real(8), optional, intent(in) :: h
		integer, optional, intent(in) :: order
		
		real(8) :: effH
		integer :: effOrder
		
		integer :: i
		real(8) :: dp
		
		effH = 1.0_8
		if( present(h) ) effH = h
		
		effOrder = FourierTransform_SORDER
		if( present(order) ) effOrder = order
		
		if( n /= size( array ) ) &
			call GOptions_error( "size(array) /= n", "FourierTransform_omegaArray" )
			
		dp = 2.0_8*MATH_PI/effH/real(n,8)
		
		do i=1,floor(n/2.0_8)+1
			array(i) = real(i-1,8)*dp
		end do
		
		do i=floor(n/2.0_8)+2,n
			array(i) = -real(n-i+1,8)*dp
		end do
		
		if( effOrder == FourierTransform_NORDER ) then
			call FourierTransform_shift_realArray( array )
		end if
	end subroutine FourierTransform_omegaArray
	
	!>
	!! @brief
	!!
	function FourierTransform_omegaGridFromData( n, h, order ) result( output )
		integer, intent(in) :: n
		real(8), optional, intent(in) :: h
		integer, optional, intent(in) :: order
		type(Grid) :: output
		
		real(8), allocatable :: array(:)
		
		allocate( array( n ) )
		
		call FourierTransform_omegaArray( array, n, h, order=order )
		call output%init( array )
		
		deallocate( array )
	end function FourierTransform_omegaGridFromData
	
	!>
	!! @brief
	!!
	function FourierTransform_omegaGridFromXGrid( xGrid, order ) result( output )
		type(Grid), intent(in) :: xGrid
		integer, optional, intent(in) :: order
		type(Grid) :: output
		
		output = FourierTransform_omegaGridFromData( xGrid%nPoints, xGrid%stepSize, order=order )
	end function FourierTransform_omegaGridFromXGrid
	
	!>
	!! @brief
	!!
	subroutine FourierTransform_xArray( array, n, h, order )
		real(8) :: array(:)
		integer, intent(in) :: n
		real(8), optional, intent(in) :: h
		integer, optional, intent(in) :: order
		
		real(8) :: effH
		integer :: effOrder
		
		integer :: i
		real(8) :: dx
		
		effH = 1.0_8
		if( present(h) ) effH = h
		
		effOrder = FourierTransform_SORDER
		if( present(order) ) effOrder = order
		
		if( n /= size( array ) ) &
			call GOptions_error( "size(array) /= n", "FourierTransform_xArray" )
			
		dx = h
		
		do i=1,floor(n/2.0_8)+1
			array(i) = real(i-1,8)*dx
		end do
		
		do i=floor(n/2.0_8)+2,n
			array(i) = -real(n-i+1,8)*dx
		end do
		
		if( effOrder == FourierTransform_NORDER ) then
			call FourierTransform_shift_realArray( array )
		end if
	end subroutine FourierTransform_xArray
	
	!>
	!! @brief
	!!
	function FourierTransform_xGridFromData( n, h, order ) result( output )
		integer, intent(in) :: n
		real(8), intent(in) :: h
		integer, optional, intent(in) :: order
		type(Grid) :: output
		
		real(8), allocatable :: array(:)
		
		allocate( array( n ) )
		
		call FourierTransform_xArray( array, n, h, order=order )
		call output%init( array )
		
		deallocate( array )
	end function FourierTransform_xGridFromData
	
	!>
	!! @brief
	!!
	function FourierTransform_xGridFromOmegaGrid( omegaGrid, order ) result( output )
		type(Grid), intent(in) :: omegaGrid
		integer, optional, intent(in) :: order
		type(Grid) :: output
		
		real(8) :: dx
		
		! dx = 2*pi/n/dp
		dx = 2.0_8*MATH_PI/real(omegaGrid%nPoints,8)/omegaGrid%stepSize
		
		output = FourierTransform_xGridFromData( omegaGrid%nPoints, dx, order=order )
	end function FourierTransform_xGridFromOmegaGrid
	
	!>
	!! @brief
	!!
	subroutine FourierTransform_phase_realArray( iArray, oArray )
		real(8) :: iArray(:)
		real(8), optional :: oArray(:)
		
		integer :: i, j, n, p0
		
		n = size( iArray )
		p0 = floor( n/2.0_8 )+2
		
		i = 1
		do j=p0,n
			if( present(oArray) ) then
				oArray(i) = iArray(i)*(-1.0_8)**j
			else
				iArray(i) = iArray(i)*(-1.0_8)**j
			end if
			
			i = i+1
		end do
		
		do j=1,p0-1
			if( present(oArray) ) then
				oArray(i) = iArray(i)*(-1.0_8)**j
			else
				iArray(i) = iArray(i)*(-1.0_8)**j
			end if
			
			i = i+1
		end do
	end subroutine FourierTransform_phase_realArray
	
	!>
	!! @brief
	!!
	subroutine FourierTransform_phase_complexArray( iArray, oArray )
		complex(8) :: iArray(:)
		complex(8), optional :: oArray(:)
		
		integer :: i, j, n, p0
		
		n = size( iArray )
		p0 = floor( n/2.0_8 )+2
		
		i = 1
		do j=p0,n
			if( present(oArray) ) then
				oArray(i) = iArray(i)*(-1.0_8)**j
			else
				iArray(i) = iArray(i)*(-1.0_8)**j
			end if
			
			i = i+1
		end do
		
		do j=1,p0-1
			if( present(oArray) ) then
				oArray(i) = iArray(i)*(-1.0_8)**j
			else
				iArray(i) = iArray(i)*(-1.0_8)**j
			end if
			
			i = i+1
		end do
	end subroutine FourierTransform_phase_complexArray
	
	!>
	!! @brief
	!! @deprecated Este metodo será eliminado si no se utiliza Mon Mar  9 16:43:34 CET 2015
	!! @todo Este metodo será eliminado si no se utiliza Mon Mar  9 16:43:34 CET 2015
	!!
	subroutine FourierTransform_iphase_realArray( iArray, oArray )
		real(8) :: iArray(:)
		real(8), optional :: oArray(:)
		
		integer :: i, j, n, p2
		
		n = size( iArray )
		p2 = n-floor(n/2.0_8)
		
		i = 1
		do j=p2,n
			if( present(oArray) ) then
				oArray(i) = iArray(i)*(-1.0_8)**(j-1)
			else
				iArray(i) = iArray(i)*(-1.0_8)**(j-1)
			end if
			
			i = i+1
		end do
		
		do j=1,p2-1
			if( present(oArray) ) then
				oArray(i) = iArray(i)*(-1.0_8)**(j-1)
			else
				iArray(i) = iArray(i)*(-1.0_8)**(j-1)
			end if
			
			i = i+1
		end do
	end subroutine FourierTransform_iphase_realArray
	
	!>
	!! @brief
	!! @deprecated Este metodo será eliminado si no se utiliza Mon Mar  9 16:43:34 CET 2015
	!! @todo Este metodo será eliminado si no se utiliza Mon Mar  9 16:43:34 CET 2015
	!!
	subroutine FourierTransform_iphase_complexArray( iArray, oArray )
		complex(8) :: iArray(:)
		complex(8), optional :: oArray(:)
		
		integer :: i, j, n, p2
		
		n = size( iArray )
		p2 = n-floor(n/2.0_8)
		
		i = 1
		do j=p2,n
			if( present(oArray) ) then
				oArray(i) = iArray(i)*(-1.0_8)**(j-1)
			else
				iArray(i) = iArray(i)*(-1.0_8)**(j-1)
			end if
			
			i = i+1
		end do
		
		do j=1,p2-1
			if( present(oArray) ) then
				oArray(i) = iArray(i)*(-1.0_8)**(j-1)
			else
				iArray(i) = iArray(i)*(-1.0_8)**(j-1)
			end if
			
			i = i+1
		end do
	end subroutine FourierTransform_iphase_complexArray
	
	!>
	!! @brief
	!!
	subroutine FourierTransform_phase_RNFunction( iFunc, oFunc )
		class(RNFunction) :: iFunc
		class(RNFunction), optional :: oFunc
		
		if( iFunc%isEquallyspaced() ) then
			call GOptions_warning( &
				"Grid should not be equally spaced", &
				"FourierTransform_shift_Grid()" &
			)
		end if
		
		if( present(oFunc) ) then
			oFunc = iFunc
			
			call FourierTransform_phase_realArray( oFunc%fArray )
		else
			call FourierTransform_phase_realArray( iFunc%fArray )
		end if
	end subroutine FourierTransform_phase_RNFunction
	
	!>
	!! @brief
	!!
	subroutine FourierTransform_phase_CNFunction( iFunc, oFunc )
		class(CNFunction) :: iFunc
		class(CNFunction), optional :: oFunc
		
		if( iFunc%isEquallyspaced() ) then
			call GOptions_warning( &
				"Grid should not be equally spaced", &
				"FourierTransform_shift_Grid()" &
			)
		end if
		
		if( present(oFunc) ) then
			oFunc = iFunc
			
			call FourierTransform_phase_complexArray( oFunc%fArray )
		else
			call FourierTransform_phase_complexArray( iFunc%fArray )
		end if		
	end subroutine FourierTransform_phase_CNFunction
	
	!>
	!! @brief
	!!
	subroutine FourierTransform_iphase_CNFunction( iFunc, oFunc )
		class(CNFunction) :: iFunc
		class(CNFunction), optional :: oFunc
		
		if( .not. iFunc%isEquallyspaced() ) then
			call GOptions_warning( &
				"Grid should be equally spaced", &
				"FourierTransform_ishift_Grid()" &
			)
		end if
		
		if( present(oFunc) ) then
			oFunc = iFunc
			
			call FourierTransform_iphase_complexArray( oFunc%fArray )
		else
			call FourierTransform_iphase_complexArray( iFunc%fArray )
		end if
	end subroutine FourierTransform_iphase_CNFunction

	!>
	!! @brief Shift the zero-frequency component to the center of the spectrum
	!! @see http://docs%scipy%org/doc/numpy/reference/generated/numpy%fft%fftshift%html#numpy%fft%fftshift
	!! 
	!! This function swaps half-spaces for all axes listed (defaults to all). Note that y[0] is the Nyquist component only if len(x) is even.
	!! 
	!! iArray : array_like. Input array.
	!! oArray : array_like. Output array (optional). The shifted array.
	!!
	subroutine FourierTransform_shift_realArray( iArray, oArray )
		real(8) :: iArray(:)
		real(8), optional :: oArray(:)
		
		real(8), allocatable :: tmpArray(:)
		integer :: i, j, n, p0
		integer, allocatable :: pos(:)
		
		n = size( iArray )
		p0 = floor( n/2.0_8 )+2
		
		allocate( pos(n) )
		
		i = 1
		do j=p0,n
			pos(i) = j
			i = i+1
		end do
		
		do j=1,p0-1
			pos(i) = j
			i = i+1
		end do
		
		if( present(oArray) ) then
			oArray = iArray
			
			do i=1,n
				oArray( i ) = iArray( pos(i) )
			end do
		else
			allocate( tmpArray(n) )
			tmpArray = iArray
			
			do i=1,n
				iArray( i ) = tmpArray( pos(i) )
			end do
			
			deallocate( tmpArray )
		end if
		
		deallocate( pos )
	end subroutine FourierTransform_shift_realArray
	
	!>
	!! @brief Exactly the same that FourierTransform_shift_realArray, but for complex numbers
	!!
	subroutine FourierTransform_shift_complexArray( iArray, oArray )
		complex(8) :: iArray(:)
		complex(8), optional :: oArray(:)
		
		complex(8), allocatable :: tmpArray(:)
		integer :: i, j, n, p0
		integer, allocatable :: pos(:)
		
		n = size( iArray )
		p0 = floor( n/2.0_8 )+2
		
		allocate( pos(n) )
		
		i = 1
		do j=p0,n
			pos(i) = j
			i = i+1
		end do
		
		do j=1,p0-1
			pos(i) = j
			i = i+1
		end do
		
		if( present(oArray) ) then
			oArray = iArray
			
			do i=1,n
				oArray( i ) = iArray( pos(i) )
			end do
		else
			allocate( tmpArray(n) )
			tmpArray = iArray
			
			do i=1,n
				iArray( i ) = tmpArray( pos(i) )
			end do
			
			deallocate( tmpArray )
		end if
		
		deallocate( pos )
	end subroutine FourierTransform_shift_complexArray
	
	!>
	!! @brief The inverse of FourierTransform_shift. Although identical for even-length x, the functions differ by one sample for odd-length x.
	!! 
	!! @parameter iArray : array_like. Input array.
	!! @parameter oArray : array_like. Output array (optional). The shifted array.
	!!
	subroutine FourierTransform_ishift_realArray( iArray, oArray )
		real(8) :: iArray(:)
		real(8), optional :: oArray(:)
		
		real(8), allocatable :: tmpArray(:)
		integer :: i, j, n, p2
		integer, allocatable :: pos(:)
		
		n = size( iArray )
		p2 = n-floor(n/2.0_8)
		
		allocate( pos(n) )
		
		i = 1
		do j=p2,n
			pos(i) = j
			i = i+1
		end do
		
		do j=1,p2-1
			pos(i) = j
			i = i+1
		end do
		
		if( present(oArray) ) then
			oArray = iArray
			
			do i=1,n
				oArray( i ) = iArray( pos(i) )
			end do
		else
			allocate( tmpArray(n) )
			tmpArray = iArray
			
			do i=1,n
				iArray( i ) = tmpArray( pos(i) )
			end do
			
			deallocate( tmpArray )
		end if
		
		deallocate( pos )
	end subroutine FourierTransform_ishift_realArray
	
	!>
	!! @brief The inverse of FourierTransform_shift. Although identical for even-length x, the functions differ by one sample for odd-length x.
	!! 
	!! @parameter iArray : array_like. Input array.
	!! @parameter oArray : array_like. Output array (optional). The shifted array.
	!!
	subroutine FourierTransform_ishift_complexArray( iArray, oArray )
		complex(8) :: iArray(:)
		complex(8), optional :: oArray(:)
		
		complex(8), allocatable :: tmpArray(:)
		integer :: i, j, n, p2
		integer, allocatable :: pos(:)
		
		n = size( iArray )
		p2 = n-floor(n/2.0_8)
		
		allocate( pos(n) )
		
		i = 1
		do j=p2,n
			pos(i) = j
			i = i+1
		end do
		
		do j=1,p2-1
			pos(i) = j
			i = i+1
		end do
		
		if( present(oArray) ) then
			oArray = iArray
			
			do i=1,n
				oArray( i ) = iArray( pos(i) )
			end do
		else
			allocate( tmpArray(n) )
			tmpArray = iArray
			
			do i=1,n
				iArray( i ) = tmpArray( pos(i) )
			end do
			
			deallocate( tmpArray )
		end if
		
		deallocate( pos )
	end subroutine FourierTransform_ishift_complexArray
	
	!>
	!! @brief
	!!
	subroutine FourierTransform_shift_Grid( iGrid, oGrid )
		class(Grid) :: iGrid
		class(Grid), optional :: oGrid
		
		if( iGrid%isEquallyspaced ) then
			call GOptions_warning( &
				"Grid should not be equally spaced", &
				"FourierTransform_shift_Grid()" &
			)
		end if
		
		if( present(oGrid) ) then
			oGrid = iGrid
			
			call FourierTransform_shift_realArray( oGrid%data )
			call oGrid%checkEquallyspaced()
		else
			call FourierTransform_shift_realArray( iGrid%data )
			call iGrid%checkEquallyspaced()
		end if
		
	end subroutine FourierTransform_shift_Grid
	
	!>
	!! @brief
	!!
	subroutine FourierTransform_ishift_Grid( iGrid, oGrid )
		class(Grid) :: iGrid
		class(Grid), optional :: oGrid
		
		if( .not. iGrid%isEquallyspaced ) then
			call GOptions_warning( &
				"Grid should be equally spaced", &
				"FourierTransform_ishift_Grid()" &
			)
		end if
		
		if( present(oGrid) ) then
			oGrid = iGrid
			
			call FourierTransform_ishift_realArray( oGrid%data )
			call oGrid%checkEquallyspaced()
		else
			call FourierTransform_ishift_realArray( iGrid%data )
			call iGrid%checkEquallyspaced()
		end if
		
	end subroutine FourierTransform_ishift_Grid
	
	!>
	!! @brief
	!!
	subroutine FourierTransform_shift_RNFunction( iFunc, oFunc )
		class(RNFunction) :: iFunc
		class(RNFunction), optional :: oFunc
		
		if( iFunc%isEquallyspaced() ) then
			call GOptions_warning( &
				"Grid should not be equally spaced", &
				"FourierTransform_shift_Grid()" &
			)
		end if
		
		if( present(oFunc) ) then
			oFunc = iFunc
			
			call FourierTransform_shift_realArray( oFunc%fArray )
			call FourierTransform_shift_realArray( oFunc%xGrid%data )
			call oFunc%xGrid%checkEquallyspaced()
		else
			call FourierTransform_shift_realArray( iFunc%fArray )
			call FourierTransform_shift_realArray( iFunc%xGrid%data )
			call iFunc%xGrid%checkEquallyspaced()
		end if
		
	end subroutine FourierTransform_shift_RNFunction
	
	!>
	!! @brief
	!!
	subroutine FourierTransform_shift_CNFunction( iFunc, oFunc )
		class(CNFunction) :: iFunc
		class(CNFunction), optional :: oFunc
		
		if( iFunc%isEquallyspaced() ) then
			call GOptions_warning( &
				"Grid should not be equally spaced", &
				"FourierTransform_shift_Grid()" &
			)
		end if
		
		if( present(oFunc) ) then
			oFunc = iFunc
			
			call FourierTransform_shift_complexArray( oFunc%fArray )
			call FourierTransform_shift_realArray( oFunc%xGrid%data )
			call oFunc%xGrid%checkEquallyspaced()
		else
			call FourierTransform_shift_complexArray( iFunc%fArray )
			call FourierTransform_shift_realArray( iFunc%xGrid%data )
			call iFunc%xGrid%checkEquallyspaced()
		end if
		
	end subroutine FourierTransform_shift_CNFunction
	
	!>
	!! @brief
	!!
	subroutine FourierTransform_ishift_CNFunction( iFunc, oFunc )
		class(CNFunction) :: iFunc
		class(CNFunction), optional :: oFunc
		
		if( .not. iFunc%isEquallyspaced() ) then
			call GOptions_warning( &
				"Grid should be equally spaced", &
				"FourierTransform_ishift_Grid()" &
			)
		end if
		
		if( present(oFunc) ) then
			oFunc = iFunc
			
			call FourierTransform_ishift_complexArray( oFunc%fArray )
			call FourierTransform_ishift_realArray( oFunc%xGrid%data )
			call oFunc%xGrid%checkEquallyspaced()
		else
			call FourierTransform_ishift_complexArray( iFunc%fArray )
			call FourierTransform_ishift_realArray( iFunc%xGrid%data )
			call iFunc%xGrid%checkEquallyspaced()
		end if
	end subroutine FourierTransform_ishift_CNFunction
	
	!>
	!! @brief
	!!
	function FourierTransform_plan_Array( iArray, sgn, oArray ) result( plan )
		complex(8) :: iArray(:)
		integer, intent(in) :: sgn
		complex(8), optional :: oArray(:)
#ifdef __GFORTRAN__
		type(C_Ptr) :: plan
#else
		integer(8) :: plan
#endif
		
		if( present(oArray) ) then
			call dfftw_plan_dft_1d( plan, size(iArray), iArray, oArray, sgn, FFTW_ESTIMATE )
		else
			call dfftw_plan_dft_1d( plan, size(iArray), iArray, iArray, sgn, FFTW_ESTIMATE )
		end if
	end function FourierTransform_plan_Array
	
	!>
	!! @brief
	!!
	function FourierTransform_plan_CNFunction( iFunc, sgn, oFunc ) result( plan )
		class(CNFunction) :: iFunc
		integer, intent(in) :: sgn
		class(CNFunction), optional :: oFunc
#ifdef __GFORTRAN__
		type(C_Ptr) :: plan
#else
		integer(8) :: plan
#endif
		
		if( present(oFunc) ) then
			call dfftw_plan_dft_1d( plan, iFunc%nPoints(), iFunc%fArray, oFunc%fArray, sgn, FFTW_ESTIMATE )
		else
			call dfftw_plan_dft_1d( plan, iFunc%nPoints(), iFunc%fArray, iFunc%fArray, sgn, FFTW_ESTIMATE )
		end if
	end function FourierTransform_plan_CNFunction
	
	!>
	!! 
	!!
	subroutine FourierTransform_execute( plan )
#ifdef __GFORTRAN__
		type(C_Ptr), intent(in) :: plan
#else
		integer(8), intent(in) :: plan
#endif
		
		call dfftw_execute( plan )
	end subroutine FourierTransform_execute
	
	!>
	!! 
	!!
	subroutine FourierTransform_destroyPlan( plan )
#ifdef __GFORTRAN__
		type(C_Ptr), intent(in) :: plan
#else
		integer(8), intent(in) :: plan
#endif
		
		call dfftw_destroy_plan( plan )
	end subroutine FourierTransform_destroyPlan
	
	!>
	!! 
	!!
	subroutine FourierTransform_dft_CArray( iArray, oArray, sgn )
		complex(8) :: iArray(:)
		complex(8), optional :: oArray(:)
		integer, optional :: sgn
		
		integer :: effSgn
		
#ifdef __GFORTRAN__
		type(C_Ptr) :: plan
#else
		integer(8) :: plan
#endif
		
		effSgn = FourierTransform_FORWARD
		if( present(sgn) ) effSgn = sgn
		
		if( present(oArray) ) then
			if( size(iArray) /= size(oArray) ) &
				call GOptions_error( "iArray and oArray have not the same size", "FourierTransform_dft_CArray" )
			
			call dfftw_plan_dft_1d( plan, size(iArray), iArray, oArray, effSgn, FFTW_ESTIMATE )
			call dfftw_execute( plan )
		else
			call dfftw_plan_dft_1d( plan, size(iArray), iArray, iArray, effSgn, FFTW_ESTIMATE )
			call dfftw_execute( plan )
		end if
		
		call dfftw_destroy_plan( plan )
	end subroutine FourierTransform_dft_CArray
	
	!>
	!! 
	!!
	subroutine FourierTransform_idft_Array( iArray, oArray, sgn )
		complex(8) :: iArray(:)
		complex(8), optional :: oArray(:)
		integer, optional :: sgn
		
		integer :: effSgn
		
#ifdef __GFORTRAN__
		type(C_Ptr) :: plan
#else
		integer(8) :: plan
#endif
		
		effSgn = FourierTransform_BACKWARD
		if( present(sgn) ) effSgn = sgn
		
		if( present(oArray) ) then
			if( size(iArray) /= size(oArray) ) &
				call GOptions_error( "iArray and oArray have not the same size", "FourierTransform_idft_Array" )
			
			call dfftw_plan_dft_1d( plan, size(iArray), iArray, oArray, effSgn, FFTW_ESTIMATE )
			call dfftw_execute( plan )
			
			oArray = oArray/real( size(iArray), 8 )
		else
			call dfftw_plan_dft_1d( plan, size(iArray), iArray, iArray, effSgn, FFTW_ESTIMATE )
			call dfftw_execute( plan )
			
			iArray = iArray/real( size(iArray), 8 )
		end if
		
		call dfftw_destroy_plan( plan )
	end subroutine FourierTransform_idft_Array
	
	!>
	!! 
	!!
	function FourierTransform_fft_CNFunction( iFunc, sgn ) result( oFunc )
		class(CNFunction), intent(in) :: iFunc
		integer, optional :: sgn
		type(CNFunction) :: oFunc
		
		integer :: n
		real(8) :: dx, dp
		type(Grid) :: xGrid
		
		! @todo Check for checkEquallyspaced
		n = iFunc%nPoints()
		dx = iFunc%xGrid%stepSize
		dp = 2.0_8*Math_PI/dx/real(n,8)
		
		oFunc = iFunc
		call FourierTransform_dft( iFunc%fArray, oFunc%fArray, sgn=sgn )
		oFunc%xGrid = FourierTransform_omegaGrid( n, dx, order=FourierTransform_SORDER )
		
		call FourierTransform_phase( oFunc )
		call FourierTransform_shift( oFunc )
		
		oFunc = oFunc*dx/sqrt(2.0_8*Math_PI)
	end function FourierTransform_fft_CNFunction
	
	!>
	!! 
	!!
	function FourierTransform_ifft_CNFunction( iFunc, sgn ) result( oFunc )
		class(CNFunction), intent(in) :: iFunc
		integer, optional :: sgn
		type(CNFunction) :: oFunc
		
		integer :: n
		real(8) :: dx, dp
		type(Grid) :: xGrid
		
		integer :: i
		
		! @todo Check for checkEquallyspaced
		n = iFunc%nPoints()
		dp = iFunc%xGrid%stepSize
		dx = 2.0_8*Math_PI/dp/real(n,8)
		
		oFunc = iFunc
		
		call FourierTransform_ishift( oFunc )
		call FourierTransform_phase( oFunc )
		
		call FourierTransform_dft( oFunc%fArray, sgn=FourierTransform_BACKWARD )
		oFunc%xGrid = FourierTransform_xGrid( n, dx, order=FourierTransform_NORDER )
		
		oFunc = oFunc*dp/sqrt(2.0_8*Math_PI)
	end function FourierTransform_ifft_CNFunction
	
	!>
	!! @todo Esta funcion debe ser modificada, para utilizar la FourierTransform para arrays de tipo real(8)
	!!
	function FourierTransform_spectrumR( iFunc, sgn, type, method, xrange, ixrange, window ) result( oFunc )
		class(RNFunction), intent(in) :: iFunc
		integer, optional, intent(in) :: sgn
		integer, optional, intent(in) :: type
		integer, optional, intent(in) :: method
		real(8), optional, intent(in) :: xrange(2)
		integer, optional, intent(in) :: ixrange(2)
		type(FourierTransform_Window), optional, intent(in) :: window
		type(RNFunction) :: oFunc
		
		integer :: effMethod
		integer :: effIXRange(2)
		integer :: effNPoints
		
		type(CNFunction) :: tmpFunc
		complex(8), allocatable :: array(:)
		
		effMethod = FourierTransform_FFT_METHOD
		if( present(method) ) effMethod = method
		
		effIXRange = [1,iFunc%nPoints()]
		if( present(xrange) ) then
			effIXRange = [ &
				floor( 1.0000001*(xrange(1)-iFunc%xGrid%min)/iFunc%xGrid%stepSize+1.0 ), &
				floor( 1.0000001*(xrange(2)-iFunc%xGrid%min)/iFunc%xGrid%stepSize+1.0 ) ]
		else if( present(ixrange) ) then
			effIXRange = ixrange
		end if
		
		effNPoints = abs( effIXRange(2)-effIXRange(1)+1 )
		
		allocate( array(effNPoints) )
		array = iFunc%fArray( effIXRange(1):effIXRange(2) )
		tmpFunc = CNFunction( iFunc%xGrid%data( effIXRange(1):effIXRange(2) ), array )
		deallocate( array )
		
		if( present(window) ) then
			tmpFunc = FourierTransform_applyWindow( tmpFunc, window )
		end if
		
		select case( effMethod )
			case( FourierTransform_FFT_METHOD )
				tmpFunc = FourierTransform_fft( tmpFunc, sgn=sgn )
			case( FourierTransform_NUMERICAL_METHOD )
				tmpFunc = FourierTransform_nft( tmpFunc, sgn=sgn )
			case default
				call GOptions_error( "The method = "//FString_fromInteger(effMethod)//" is not available", "FourierTransform_spectrumR()" )
		end select
		
		select case( type )
			case( FourierTransform_NORM_SPECTRUM )
				oFunc = RNFunction( tmpFunc%xGrid, abs(tmpFunc%fArray) )
				
			case( FourierTransform_REALPART_SPECTRUM )
				oFunc = RNFunction( tmpFunc%xGrid, real(tmpFunc%fArray) )
				
			case( FourierTransform_IMAGPART_SPECTRUM )
				oFunc = RNFunction( tmpFunc%xGrid, aimag(tmpFunc%fArray) )
				
			case( FourierTransform_PHASE_SPECTRUM )
				oFunc = RNFunction( tmpFunc%xGrid, atan2( aimag(tmpFunc%fArray), real(tmpFunc%fArray) ) )
				
			case( FourierTransform_POWER_SPECTRUM )
				oFunc = RNFunction( tmpFunc%xGrid, real(tmpFunc%fArray)**2+aimag(tmpFunc%fArray)**2 )
				
			case default
				oFunc = RNFunction( tmpFunc%xGrid, abs(tmpFunc%fArray) )
		end select
	end function FourierTransform_spectrumR
	
	!>
	!! @brief
	!!
	function FourierTransform_spectrumC( iFunc, sgn, type, method, xrange, ixrange, window ) result( oFunc )
		class(CNFunction), intent(in) :: iFunc
		integer, optional, intent(in) :: sgn
		integer, optional, intent(in) :: type
		integer, optional, intent(in) :: method
		real(8), optional, intent(in) :: xrange(2)
		integer, optional, intent(in) :: ixrange(2)
		type(FourierTransform_Window), optional, intent(in) :: window
		type(RNFunction) :: oFunc
		
		integer :: effMethod
		integer :: effIXRange(2)
		integer :: effNPoints
		
		type(CNFunction) :: tmpFunc
		complex(8), allocatable :: array(:)
		
		effMethod = FourierTransform_FFT_METHOD
		if( present(method) ) effMethod = method
		
		effIXRange = [1,iFunc%nPoints()]
		if( present(xrange) ) then
			effIXRange = [ &
				floor( 1.0000001*(xrange(1)-iFunc%xGrid%min)/iFunc%xGrid%stepSize+1.0 ), &
				floor( 1.0000001*(xrange(2)-iFunc%xGrid%min)/iFunc%xGrid%stepSize+1.0 ) ]
		else if( present(ixrange) ) then
			effIXRange = ixrange
		end if
		
		effNPoints = abs( effIXRange(2)-effIXRange(1)+1 )
		
		allocate( array(effNPoints) )
		array = iFunc%fArray( effIXRange(1):effIXRange(2) )
		tmpFunc = CNFunction( iFunc%xGrid%data( effIXRange(1):effIXRange(2) ), array )
		deallocate( array )
		
		if( present(window) ) then
			tmpFunc = FourierTransform_applyWindow( tmpFunc, window )
		end if
		
		if( present(window) ) then
			! @warning Se podría hacer un if que diferenciara los casos en que se diera o no
			!          un xrange o un ixrange y así permitiera mayor velocidad y no desperdicio
			!          de memoria al llamar a array como un paso intermedio. Sin embargo he
			!          optado por dejarlo lo más similar al caso Real para dejar claras
			!          las futuras modificaciones. Esto de debería cambiar, solo si hay promenas
			!          de velocidad o memoria en el futuro.
! 			tmpFunc = FourierTransform_applyWindow( iFunc, window )
			
			select case( effMethod )
				case( FourierTransform_FFT_METHOD )
					tmpFunc = FourierTransform_fft( tmpFunc, sgn=sgn )
				case( FourierTransform_NUMERICAL_METHOD )
					tmpFunc = FourierTransform_nft( tmpFunc, sgn=sgn )
				case default
					call GOptions_error( "The method = "//FString_fromInteger(effMethod)//" is not available", "FourierTransform_spectrumC()" )
			end select
		else
			select case( effMethod )
				case( FourierTransform_FFT_METHOD )
					tmpFunc = FourierTransform_fft( tmpFunc, sgn=sgn )
! 					tmpFunc = FourierTransform_fft( iFunc, sgn=sgn )
				case( FourierTransform_NUMERICAL_METHOD )
					tmpFunc = FourierTransform_nft( tmpFunc, sgn=sgn )
! 					tmpFunc = FourierTransform_nft( iFunc, sgn=sgn )
				case default
					call GOptions_error( "The method = "//FString_fromInteger(effMethod)//" is not available", "FourierTransform_spectrumC()" )
			end select
		end if
		
		select case( type )
			case( FourierTransform_NORM_SPECTRUM )
				oFunc = RNFunction( tmpFunc%xGrid, abs(tmpFunc%fArray) )
				
			case( FourierTransform_REALPART_SPECTRUM )
				oFunc = RNFunction( tmpFunc%xGrid, real(tmpFunc%fArray) )
				
			case( FourierTransform_IMAGPART_SPECTRUM )
				oFunc = RNFunction( tmpFunc%xGrid, aimag(tmpFunc%fArray) )
				
			case( FourierTransform_PHASE_SPECTRUM )
				oFunc = RNFunction( tmpFunc%xGrid, atan2( aimag(tmpFunc%fArray), real(tmpFunc%fArray) ) )
				
			case( FourierTransform_POWER_SPECTRUM )
				oFunc = RNFunction( tmpFunc%xGrid, real(tmpFunc%fArray)**2+aimag(tmpFunc%fArray)**2 )
				
			case default
				oFunc = RNFunction( tmpFunc%xGrid, abs(tmpFunc%fArray) )
		end select
	end function FourierTransform_spectrumC
	
	!>
	!! @brief
	!!
	function FourierTransform_realPartSpectrumR( iFunc, sgn, method, xrange, ixrange, window ) result( oFunc )
		class(RNFunction), intent(in) :: iFunc
		integer, optional, intent(in) :: sgn
		integer, optional, intent(in) :: method
		real(8), optional, intent(in) :: xrange(2)
		integer, optional, intent(in) :: ixrange(2)
		type(FourierTransform_Window), optional, intent(in) :: window
		type(RNFunction) :: oFunc
		
		oFunc = FourierTransform_spectrumR( iFunc, sgn, FourierTransform_REALPART_SPECTRUM, method, xrange, ixrange, window )
	end function FourierTransform_realPartSpectrumR
	
	!>
	!! @brief
	!!
	function FourierTransform_realPartSpectrumC( iFunc, sgn, method, xrange, ixrange, window ) result( oFunc )
		class(CNFunction), intent(in) :: iFunc
		integer, optional :: sgn
		integer, optional :: method
		real(8), optional, intent(in) :: xrange(2)
		integer, optional, intent(in) :: ixrange(2)
		type(FourierTransform_Window), optional, intent(in) :: window
		type(RNFunction) :: oFunc
		
		oFunc = FourierTransform_spectrumC( iFunc, sgn, FourierTransform_REALPART_SPECTRUM, method, xrange, ixrange, window )
	end function FourierTransform_realPartSpectrumC
	
	!>
	!! @brief
	!!
	function FourierTransform_imagPartSpectrumR( iFunc, sgn, method, xrange, ixrange, window ) result( oFunc )
		class(RNFunction), intent(in) :: iFunc
		integer, optional :: sgn
		integer, optional :: method
		real(8), optional, intent(in) :: xrange(2)
		integer, optional, intent(in) :: ixrange(2)
		type(FourierTransform_Window), optional, intent(in) :: window
		type(RNFunction) :: oFunc
		
		oFunc = FourierTransform_spectrumR( iFunc, sgn, FourierTransform_IMAGPART_SPECTRUM, method, xrange, ixrange, window )
	end function FourierTransform_imagPartSpectrumR
	
	!>
	!! @brief
	!!
	function FourierTransform_imagPartSpectrumC( iFunc, sgn, method, xrange, ixrange, window ) result( oFunc )
		class(CNFunction), intent(in) :: iFunc
		integer, optional, intent(in) :: sgn
		integer, optional, intent(in) :: method
		real(8), optional, intent(in) :: xrange(2)
		integer, optional, intent(in) :: ixrange(2)
		type(FourierTransform_Window), optional, intent(in) :: window
		type(RNFunction) :: oFunc
		
		oFunc = FourierTransform_spectrumC( iFunc, sgn, FourierTransform_IMAGPART_SPECTRUM, method, xrange, ixrange, window )
	end function FourierTransform_imagPartSpectrumC
	
	!>
	!! @brief
	!!
	function FourierTransform_phaseSpectrumR( iFunc, sgn, method, xrange, ixrange, window ) result( oFunc )
		class(RNFunction), intent(in) :: iFunc
		integer, optional, intent(in) :: sgn
		integer, optional, intent(in) :: method
		real(8), optional, intent(in) :: xrange(2)
		integer, optional, intent(in) :: ixrange(2)
		type(FourierTransform_Window), optional, intent(in) :: window
		type(RNFunction) :: oFunc
		
		oFunc = FourierTransform_spectrumR( iFunc, sgn, FourierTransform_PHASE_SPECTRUM, method, xrange, ixrange, window )
	end function FourierTransform_phaseSpectrumR
	
	!>
	!! @brief
	!!
	function FourierTransform_phaseSpectrumC( iFunc, sgn, method, xrange, ixrange, window ) result( oFunc )
		class(CNFunction), intent(in) :: iFunc
		integer, optional, intent(in) :: sgn
		integer, optional, intent(in) :: method
		real(8), optional, intent(in) :: xrange(2)
		integer, optional, intent(in) :: ixrange(2)
		type(FourierTransform_Window), optional, intent(in) :: window
		type(RNFunction) :: oFunc
		
		oFunc = FourierTransform_spectrumC( iFunc, sgn, FourierTransform_PHASE_SPECTRUM, method, xrange, ixrange, window )
	end function FourierTransform_phaseSpectrumC
	
	!>
	!! @brief
	!!
	function FourierTransform_powerSpectrumR( iFunc, sgn, method, xrange, ixrange, window ) result( oFunc )
		class(RNFunction), intent(in) :: iFunc
		integer, optional, intent(in) :: sgn
		integer, optional, intent(in) :: method
		real(8), optional, intent(in) :: xrange(2)
		integer, optional, intent(in) :: ixrange(2)
		type(FourierTransform_Window), optional, intent(in) :: window
		type(RNFunction) :: oFunc
		
		oFunc = FourierTransform_spectrumR( iFunc, sgn, FourierTransform_POWER_SPECTRUM, method, xrange, ixrange, window )
	end function FourierTransform_powerSpectrumR
	
	!>
	!! @brief
	!!
	function FourierTransform_powerSpectrumC( iFunc, sgn, method, xrange, ixrange, window ) result( oFunc )
		class(CNFunction), intent(in) :: iFunc
		integer, optional, intent(in) :: sgn
		integer, optional, intent(in) :: method
		real(8), optional, intent(in) :: xrange(2)
		integer, optional, intent(in) :: ixrange(2)
		type(FourierTransform_Window), optional, intent(in) :: window
		type(RNFunction) :: oFunc
		
		oFunc = FourierTransform_spectrumC( iFunc, sgn, FourierTransform_POWER_SPECTRUM, method, xrange, ixrange, window )
	end function FourierTransform_powerSpectrumC
	
	!>
	!! Windows - Minimize Leakage
	!! In order to better satisfy the periodicity requirement of the FFT
	!! process, time weighting functions, called windows, are used.
	!! Essentially, these weighting functions attempt to heavily weight the
	!! beginning and end of the sample record to zero - the middle of the
	!! sample is heavily weighted towards unity
	!!
	!! Rectangular - Unity gain applied to entire sample interval; this
	!!    window can have up to 36% amplitude error if the signal is not
	!!    periodic in the sample interval; good for signals that inherently
	!!    satisfy the periodicity requirement of the FFT process
	!! Hanning - Cosine bell shaped weighting which heavily weights the
	!!    beginning and end of the sample interval to zero; this window can
	!!    have up to 16% amplitude error; the main frequency will show
	!!    some adjacent side band frequencies but then quickly attenuates;
	!!    good for general purpose signal applications
	!! Flat Top - Multi-sine weighting function; this window has excellent
	!!    amplitude characteristics (0.1% error) but very poor frequency
	!!    resolution; very good for calibration purposes with discrete sine
	!!
	!! http://en%wikipedia%org/wiki/Window_function
	!!
	function FourierTransform_applyWindow( iFunc, window ) result( oFunc )
		class(CNFunction), intent(in) :: iFunc
		type(FourierTransform_Window), optional, intent(in) :: window
		type(CNFunction) :: oFunc
		
		integer :: nPoints
		logical :: effCentered
		integer :: effType
		real(8) :: effParam
		logical :: useOFile
		
		integer :: i
		real(8) :: t0, w, dw, range, windowValue
		
		oFunc = iFunc
		nPoints = iFunc%nPoints()
		
		if( .not. present(window) ) return
		
		effType = window%type
		
		if( effType == FourierTransform_WINDOW_NONE ) return
		
		select case( effType )
			case( FourierTransform_WINDOW_COS )
				effParam = 2.0_8
			case( FourierTransform_WINDOW_GAUSS )
				effParam = 4.0_8
			case( FourierTransform_WINDOW_ERF_TOPHAT )
				effParam = 0.2_8
			case( FourierTransform_WINDOW_FLATTOP )
				effParam = 0.0_8
		end select
		
		if( window%param > 0.0 ) effParam = window%param
		
		effCentered = window%centered
		
		if( .not. FString_isNull(window%oFile) ) useOFile = .true.
		
		if( useOFile ) open( unit=32, file=trim(window%oFile), status="unknown" )
		
		if( effCentered ) then
			range = abs( iFunc%xGrid%at(nPoints)-iFunc%xGrid%at(1) )/2.0_8
			t0 = iFunc%xGrid%at(1) + range
		else
			range = abs( iFunc%xGrid%at(nPoints)-iFunc%xGrid%at(1) )
			t0 = iFunc%xGrid%at(1)
		end if
		
		select case( effType )
			case( FourierTransform_WINDOW_COS )
				
				do i=1,nPoints
					windowValue = cos( Math_PI*( iFunc%xGrid%at(i) - t0 )/2.0_8/range )**effParam
					call oFunc%set( i, oFunc%at(i)*windowValue )
					
					if( useOFile ) write(32,*) oFunc%xGrid%at(i), windowValue
				end do
				
			case( FourierTransform_WINDOW_GAUSS )
				
				do i=1,nPoints
! 					windowValue = effParam*exp(-0.5_8*effParam**2*(iFunc%xGrid%at(i)-t0)**2/range**2)/range/sqrt(2.0*Math_PI)
					windowValue = exp(-0.5_8*effParam**2*(iFunc%xGrid%at(i)-t0)**2/range**2)
					call oFunc%set( i, oFunc%at(i)*windowValue )
					
					if( useOFile ) write(32,*) iFunc%xGrid%at(i), windowValue
				end do
				
			case( FourierTransform_WINDOW_ERF_TOPHAT )
				
				if( effCentered ) then
					w = 2.0_8*(1.0_8-2.0_8*effParam)*range
				else
					w = 2.0_8*(1.0_8-effParam)*range
				end if

				dw = effParam*range
				
				do i=1,nPoints
					windowValue = Math_erfTophat( iFunc%xGrid%at(i), t0, w, dw )
					call oFunc%set( i, oFunc%at(i)*windowValue )
					
					if( useOFile ) write(32,*) iFunc%xGrid%at(i), windowValue
				end do
				
			case( FourierTransform_WINDOW_FLATTOP )
				
				if( effCentered ) then
					w = 2.0_8*range
				else
					w = range
				end if
				
				do i=1,nPoints
					windowValue = Math_flatTopWindow( iFunc%xGrid%at(i), t0, w )
					call oFunc%set( i, oFunc%at(i)*windowValue )
					
					if( useOFile ) write(32,*) iFunc%xGrid%at(i), windowValue
				end do
				
		end select
		
		if( useOFile ) close(32)
		
	end function FourierTransform_applyWindow
	
	!>
	!! @brief
	!!
	function FourierTransform_nft_CNFunction( iFunc, sgn ) result( oFunc )
		class(CNFunction), intent(in) :: iFunc
		integer, optional, intent(in) :: sgn
		type(CNFunction) :: oFunc
		
		integer :: effSgn
		
		integer :: i, j, n
		real(8) :: dx, dp
		type(Grid) :: xGrid
		
		effSgn = FourierTransform_FORWARD
		if( present(sgn) ) effSgn = sgn
		
		! @todo Check for checkEquallyspaced
		n = iFunc%nPoints()
		dx = iFunc%xGrid%stepSize
		dp = 2.0_8*Math_PI/dx/real(n,8)
		
		oFunc = iFunc
		oFunc%xGrid = FourierTransform_omegaGrid( n, dx, order=FourierTransform_NORDER )
		
		do i=1,n
			oFunc%fArray(i) = 0.0_8
			do j=1,n
				oFunc%fArray(i) = oFunc%fArray(i) + iFunc%fArray(j)*exp( real(effSgn,8)*Math_I*iFunc%xGrid%at(j)*oFunc%xGrid%at(i) )
			end do
		end do
		
		oFunc = oFunc*dx/sqrt(2.0_8*Math_PI)
	end function FourierTransform_nft_CNFunction

	!>
	!! @brief
	!!
	function FourierTransform_inft_CNFunction( iFunc, sgn ) result( oFunc )
		class(CNFunction), intent(in) :: iFunc
		integer, optional, intent(in) :: sgn
		type(CNFunction) :: oFunc
		
		integer :: effSgn
		
		integer :: i, j, n
		real(8) :: dx, dp
		type(Grid) :: xGrid
		
		effSgn = FourierTransform_BACKWARD
		if( present(sgn) ) effSgn = sgn
		
		! @todo Check for checkEquallyspaced
		n = iFunc%nPoints()
		dp = iFunc%xGrid%stepSize
		dx = 2.0_8*Math_PI/dp/real(n,8)
		
		oFunc = iFunc
		oFunc%xGrid = FourierTransform_xGrid( n, dx, order=FourierTransform_NORDER )
		
		do i=1,n
			oFunc%fArray(i) = 0.0_8
			do j=1,n
				oFunc%fArray(i) = oFunc%fArray(i) + iFunc%fArray(j)*exp( real(effSgn,8)*Math_I*iFunc%xGrid%at(j)*oFunc%xGrid%at(i) )
			end do
		end do
		
		oFunc = oFunc*dp/sqrt(2.0_8*Math_PI)
	end function FourierTransform_inft_CNFunction
	
	!>
	!! @brief
	!!
	function FourierTransform_filterByAmplitude( this, cutoff ) result( oFunc )
		class(FourierTransform), intent(in) :: this
		real(8), intent(in), optional :: cutoff
		type(CNFunction) :: oFunc
		
		call GOptions_error( "This function is not implamented yet", "FourierTransform_filterByAmplitude()" )
	end function FourierTransform_filterByAmplitude
	
	!>
	!! @brief
	!!
	function FourierTransform_filterByFrequency( this, cutoff ) result( oFunc )
		class(FourierTransform), intent(in) :: this
		real(8), intent(in), optional :: cutoff
		type(CNFunction) :: oFunc
		
		call GOptions_error( "This function is not implamented yet", "FourierTransform_filterByFrequency()" )
	end function FourierTransform_filterByFrequency
	
	!>
	!! 
	!!
	subroutine FourierTransform_derivate_CNFunction( iFunc, order, oFunc )
		class(CNFunction) :: iFunc
		integer, intent(in) :: order
		class(CNFunction), optional :: oFunc
		
#ifdef __GFORTRAN__
		type(C_Ptr) :: planF, planB
#else
		integer(8) :: planF, planB
#endif
		type(Grid) :: omega
		
		if( present(oFunc) ) then
			oFunc = iFunc
			
			call dfftw_plan_dft_1d( planF, iFunc%nPoints(), iFunc%fArray, oFunc%fArray, FFTW_FORWARD, FFTW_ESTIMATE )
			call dfftw_plan_dft_1d( planB, iFunc%nPoints(), iFunc%fArray, oFunc%fArray, FFTW_BACKWARD, FFTW_ESTIMATE )
			
			omega = FourierTransform_omegaGrid( iFunc%xGrid )
			
			call dfftw_execute( planF )
			
			oFunc%fArray = ( Math_I*omega%data )**order*oFunc%fArray
			
			call dfftw_execute( planB )
				
			oFunc = oFunc/real(iFunc%nPoints(),8)
		else
			call dfftw_plan_dft_1d( planF, iFunc%nPoints(), iFunc%fArray, iFunc%fArray, FFTW_FORWARD, FFTW_ESTIMATE )
			call dfftw_plan_dft_1d( planB, iFunc%nPoints(), iFunc%fArray, iFunc%fArray, FFTW_BACKWARD, FFTW_ESTIMATE )
			
			omega = FourierTransform_omegaGrid( iFunc%xGrid )
			
			call dfftw_execute( planF )
			
			iFunc%fArray = ( Math_I*omega%data )**order*iFunc%fArray
			
			call dfftw_execute( planB )
				
			iFunc = iFunc/real(iFunc%nPoints(),8)
		end if
			
		call dfftw_destroy_plan( planF )
		call dfftw_destroy_plan( planB )
	end subroutine FourierTransform_derivate_CNFunction
	
end module FourierTransform_

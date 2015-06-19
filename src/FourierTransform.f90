!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!  This file is part of scift (Scientific Fortran Tools).
!!  Copyright (C) by authors (2013-2015)
!!  
!!  Authors (alphabetic order):
!!    * Aguirre N.F. (nfaguirrec@gmail.com)  (2013-2015)
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

!!
!! Taken from: http://www.cs.otago.ac.nz/cosc453/student_tutorials/fourier_analysis.pdf
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
		FourierTransform_derivate, &
		FourierTransform_test
		
	type, public :: FourierTransform
		class(CNFunction), pointer, private :: iFunc
		type(C_Ptr), private :: planF, planB
		
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
		if( .not. associated( this.iFunc, target=iFunc ) ) then
			if( associated(this.iFunc) ) then
				call dfftw_destroy_plan( this.planF )
				call dfftw_destroy_plan( this.planB )
				
				nullify(this.iFunc)
			end if
			
			this.iFunc => iFunc
			this.nPoints = iFunc.nPoints() ! << Hay que cambiar para hacer la transformada de algunos puntos
			
			call dfftw_plan_dft_1d( this.planF, this.nPoints, this.iFunc.fArray, this.iFunc.fArray, FFTW_FORWARD, FFTW_ESTIMATE )
			call dfftw_plan_dft_1d( this.planB, this.nPoints, this.iFunc.fArray, this.iFunc.fArray, FFTW_BACKWARD, FFTW_ESTIMATE )
			
			if( effDomain == FourierTransform_SPATIAL_DOMAIN ) then
				
				this.x = this.iFunc.xGrid ! << Hay que cambiar para hacer la transformada de algunos puntos
				this.omega = FourierTransform_omegaGrid( this.x )
				
			else if( effDomain == FourierTransform_FREQUENCY_DOMAIN ) then
				
				call GOptions_warning( "FourierTransform_FREQUENCY_DOMAIN have not been tested yet", "FourierTransform.init()" )
				this.omega = this.iFunc.xGrid ! << Hay que cambiar para hacer la transformada de algunos puntos
				this.x = FourierTransform_xGrid( this.omega )
			
			end if
		end if
		
	end subroutine init
	
	!>
	!! @brief Destructor
	!!
	subroutine destroy( this )
		type(FourierTransform) :: this
		
		if( associated(this.iFunc) ) then
			call dfftw_destroy_plan( this.planF )
			call dfftw_destroy_plan( this.planB )
			
			nullify(this.iFunc)
		end if
		
		this.nPoints = -1
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
		class(FourierTransform) :: this
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
		
			call dfftw_execute( this.planF )
			
			if( effSync ) then
				this.iFunc.xGrid = this.omega
				
				if( effShift ) then
					call FourierTransform_phase( this.iFunc )
					call FourierTransform_shift( this.iFunc )
				end if
			end if
			
		else if ( sgn == FourierTransform_BACKWARD ) then
		
			if( effSync ) then
				if( effShift ) then
					call FourierTransform_ishift( this.iFunc )
					call FourierTransform_phase( this.iFunc )
				end if
			end if
			
			call dfftw_execute( this.planB )
			
			if( effSync ) then
				this.iFunc.xGrid = this.x
			end if
			
			this.iFunc = this.iFunc/real(this.nPoints,8)
			
		else
			call GOptions_error( "Bad value for sgn", "FourierTransform.execute()" )
		end if
	end subroutine execute
	
	!>
	!! @brief Return the Discrete Fourier Transform sample frequencies. ( see: numpy.fft.fftfreq )
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
		call output.init( array )
		
		deallocate( array )
	end function FourierTransform_omegaGridFromData
	
	!>
	!! @brief
	!!
	function FourierTransform_omegaGridFromXGrid( xGrid, order ) result( output )
		type(Grid), intent(in) :: xGrid
		integer, optional, intent(in) :: order
		type(Grid) :: output
		
		output = FourierTransform_omegaGridFromData( xGrid.nPoints, xGrid.stepSize, order=order )
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
		call output.init( array )
		
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
		dx = 2.0_8*MATH_PI/real(omegaGrid.nPoints,8)/omegaGrid.stepSize
		
		output = FourierTransform_xGridFromData( omegaGrid.nPoints, dx, order=order )
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
		
		if( iFunc.isEquallyspaced() ) then
			call GOptions_warning( &
				"Grid should not be equally spaced", &
				"FourierTransform_shift_Grid()" &
			)
		end if
		
		if( present(oFunc) ) then
			oFunc = iFunc
			
			call FourierTransform_phase_realArray( oFunc.fArray )
		else
			call FourierTransform_phase_realArray( iFunc.fArray )
		end if
	end subroutine FourierTransform_phase_RNFunction
	
	!>
	!! @brief
	!!
	subroutine FourierTransform_phase_CNFunction( iFunc, oFunc )
		class(CNFunction) :: iFunc
		class(CNFunction), optional :: oFunc
		
		if( iFunc.isEquallyspaced() ) then
			call GOptions_warning( &
				"Grid should not be equally spaced", &
				"FourierTransform_shift_Grid()" &
			)
		end if
		
		if( present(oFunc) ) then
			oFunc = iFunc
			
			call FourierTransform_phase_complexArray( oFunc.fArray )
		else
			call FourierTransform_phase_complexArray( iFunc.fArray )
		end if		
	end subroutine FourierTransform_phase_CNFunction
	
	!>
	!! @brief
	!!
	subroutine FourierTransform_iphase_CNFunction( iFunc, oFunc )
		class(CNFunction) :: iFunc
		class(CNFunction), optional :: oFunc
		
		if( .not. iFunc.isEquallyspaced() ) then
			call GOptions_warning( &
				"Grid should be equally spaced", &
				"FourierTransform_ishift_Grid()" &
			)
		end if
		
		if( present(oFunc) ) then
			oFunc = iFunc
			
			call FourierTransform_iphase_complexArray( oFunc.fArray )
		else
			call FourierTransform_iphase_complexArray( iFunc.fArray )
		end if
	end subroutine FourierTransform_iphase_CNFunction

	!>
	!! @brief Shift the zero-frequency component to the center of the spectrum
	!! @see http://docs.scipy.org/doc/numpy/reference/generated/numpy.fft.fftshift.html#numpy.fft.fftshift
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
		
		if( iGrid.isEquallyspaced ) then
			call GOptions_warning( &
				"Grid should not be equally spaced", &
				"FourierTransform_shift_Grid()" &
			)
		end if
		
		if( present(oGrid) ) then
			oGrid = iGrid
			
			call FourierTransform_shift_realArray( oGrid.data )
			call oGrid.checkEquallyspaced()
		else
			call FourierTransform_shift_realArray( iGrid.data )
			call iGrid.checkEquallyspaced()
		end if
		
	end subroutine FourierTransform_shift_Grid
	
	!>
	!! @brief
	!!
	subroutine FourierTransform_ishift_Grid( iGrid, oGrid )
		class(Grid) :: iGrid
		class(Grid), optional :: oGrid
		
		if( .not. iGrid.isEquallyspaced ) then
			call GOptions_warning( &
				"Grid should be equally spaced", &
				"FourierTransform_ishift_Grid()" &
			)
		end if
		
		if( present(oGrid) ) then
			oGrid = iGrid
			
			call FourierTransform_ishift_realArray( oGrid.data )
			call oGrid.checkEquallyspaced()
		else
			call FourierTransform_ishift_realArray( iGrid.data )
			call iGrid.checkEquallyspaced()
		end if
		
	end subroutine FourierTransform_ishift_Grid
	
	!>
	!! @brief
	!!
	subroutine FourierTransform_shift_RNFunction( iFunc, oFunc )
		class(RNFunction) :: iFunc
		class(RNFunction), optional :: oFunc
		
		if( iFunc.isEquallyspaced() ) then
			call GOptions_warning( &
				"Grid should not be equally spaced", &
				"FourierTransform_shift_Grid()" &
			)
		end if
		
		if( present(oFunc) ) then
			oFunc = iFunc
			
			call FourierTransform_shift_realArray( oFunc.fArray )
			call FourierTransform_shift_realArray( oFunc.xGrid.data )
			call oFunc.xGrid.checkEquallyspaced()
		else
			call FourierTransform_shift_realArray( iFunc.fArray )
			call FourierTransform_shift_realArray( iFunc.xGrid.data )
			call iFunc.xGrid.checkEquallyspaced()
		end if
		
	end subroutine FourierTransform_shift_RNFunction
	
	!>
	!! @brief
	!!
	subroutine FourierTransform_shift_CNFunction( iFunc, oFunc )
		class(CNFunction) :: iFunc
		class(CNFunction), optional :: oFunc
		
		if( iFunc.isEquallyspaced() ) then
			call GOptions_warning( &
				"Grid should not be equally spaced", &
				"FourierTransform_shift_Grid()" &
			)
		end if
		
		if( present(oFunc) ) then
			oFunc = iFunc
			
			call FourierTransform_shift_complexArray( oFunc.fArray )
			call FourierTransform_shift_realArray( oFunc.xGrid.data )
			call oFunc.xGrid.checkEquallyspaced()
		else
			call FourierTransform_shift_complexArray( iFunc.fArray )
			call FourierTransform_shift_realArray( iFunc.xGrid.data )
			call iFunc.xGrid.checkEquallyspaced()
		end if
		
	end subroutine FourierTransform_shift_CNFunction
	
	!>
	!! @brief
	!!
	subroutine FourierTransform_ishift_CNFunction( iFunc, oFunc )
		class(CNFunction) :: iFunc
		class(CNFunction), optional :: oFunc
		
		if( .not. iFunc.isEquallyspaced() ) then
			call GOptions_warning( &
				"Grid should be equally spaced", &
				"FourierTransform_ishift_Grid()" &
			)
		end if
		
		if( present(oFunc) ) then
			oFunc = iFunc
			
			call FourierTransform_ishift_complexArray( oFunc.fArray )
			call FourierTransform_ishift_realArray( oFunc.xGrid.data )
			call oFunc.xGrid.checkEquallyspaced()
		else
			call FourierTransform_ishift_complexArray( iFunc.fArray )
			call FourierTransform_ishift_realArray( iFunc.xGrid.data )
			call iFunc.xGrid.checkEquallyspaced()
		end if
	end subroutine FourierTransform_ishift_CNFunction
	
	!>
	!! @brief
	!!
	function FourierTransform_plan_Array( iArray, sgn, oArray ) result( plan )
		complex(8) :: iArray(:)
		integer, intent(in) :: sgn
		complex(8), optional :: oArray(:)
		integer(8) :: plan
		
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
		integer(8) :: plan
		
		if( present(oFunc) ) then
			call dfftw_plan_dft_1d( plan, iFunc.nPoints(), iFunc.fArray, oFunc.fArray, sgn, FFTW_ESTIMATE )
		else
			call dfftw_plan_dft_1d( plan, iFunc.nPoints(), iFunc.fArray, iFunc.fArray, sgn, FFTW_ESTIMATE )
		end if
	end function FourierTransform_plan_CNFunction
	
	!>
	!! 
	!!
	subroutine FourierTransform_execute( plan )
		integer(8), intent(in) :: plan
		
		call dfftw_execute( plan )
	end subroutine FourierTransform_execute
	
	!>
	!! 
	!!
	subroutine FourierTransform_destroyPlan( plan )
		integer(8), intent(in) :: plan
		
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
		
		integer(8) :: plan
		
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
		
		integer(8) :: plan
		
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
		n = iFunc.nPoints()
		dx = iFunc.xGrid.stepSize
		dp = 2.0_8*Math_PI/dx/real(n,8)
		
		oFunc = iFunc
		call FourierTransform_dft( iFunc.fArray, oFunc.fArray, sgn=sgn )
		oFunc.xGrid = FourierTransform_omegaGrid( n, dx, order=FourierTransform_SORDER )
		
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
		n = iFunc.nPoints()
		dp = iFunc.xGrid.stepSize
		dx = 2.0_8*Math_PI/dp/real(n,8)
		
		oFunc = iFunc
		
		call FourierTransform_ishift( oFunc )
		call FourierTransform_phase( oFunc )
		
		call FourierTransform_dft( oFunc.fArray, sgn=FourierTransform_BACKWARD )
		oFunc.xGrid = FourierTransform_xGrid( n, dx, order=FourierTransform_NORDER )
		
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
		
		effIXRange = [1,iFunc.nPoints()]
		if( present(xrange) ) then
			effIXRange = [ &
				floor( 1.0000001*(xrange(1)-iFunc.xGrid.min)/iFunc.xGrid.stepSize+1.0 ), &
				floor( 1.0000001*(xrange(2)-iFunc.xGrid.min)/iFunc.xGrid.stepSize+1.0 ) ]
		else if( present(ixrange) ) then
			effIXRange = ixrange
		end if
		
		effNPoints = abs( effIXRange(2)-effIXRange(1)+1 )
		
		allocate( array(effNPoints) )
		array = iFunc.fArray( effIXRange(1):effIXRange(2) )
		call tmpFunc.init( iFunc.xGrid.data( effIXRange(1):effIXRange(2) ), array )
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
				call oFunc.init( tmpFunc.xGrid, abs(tmpFunc.fArray) )
				
			case( FourierTransform_REALPART_SPECTRUM )
				call oFunc.init( tmpFunc.xGrid, real(tmpFunc.fArray) )
				
			case( FourierTransform_IMAGPART_SPECTRUM )
				call oFunc.init( tmpFunc.xGrid, aimag(tmpFunc.fArray) )
				
			case( FourierTransform_PHASE_SPECTRUM )
				call oFunc.init( tmpFunc.xGrid, atan2( aimag(tmpFunc.fArray), real(tmpFunc.fArray) ) )
				
			case( FourierTransform_POWER_SPECTRUM )
				call oFunc.init( tmpFunc.xGrid, real(tmpFunc.fArray)**2+aimag(tmpFunc.fArray)**2 )
				
			case default
				call oFunc.init( tmpFunc.xGrid, abs(tmpFunc.fArray) )
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
		
		effIXRange = [1,iFunc.nPoints()]
		if( present(xrange) ) then
			effIXRange = [ &
				floor( 1.0000001*(xrange(1)-iFunc.xGrid.min)/iFunc.xGrid.stepSize+1.0 ), &
				floor( 1.0000001*(xrange(2)-iFunc.xGrid.min)/iFunc.xGrid.stepSize+1.0 ) ]
		else if( present(ixrange) ) then
			effIXRange = ixrange
		end if
		
		effNPoints = abs( effIXRange(2)-effIXRange(1)+1 )
		
		allocate( array(effNPoints) )
		array = iFunc.fArray( effIXRange(1):effIXRange(2) )
		call tmpFunc.init( iFunc.xGrid.data( effIXRange(1):effIXRange(2) ), array )
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
				call oFunc.init( tmpFunc.xGrid, abs(tmpFunc.fArray) )
				
			case( FourierTransform_REALPART_SPECTRUM )
				call oFunc.init( tmpFunc.xGrid, real(tmpFunc.fArray) )
				
			case( FourierTransform_IMAGPART_SPECTRUM )
				call oFunc.init( tmpFunc.xGrid, aimag(tmpFunc.fArray) )
				
			case( FourierTransform_PHASE_SPECTRUM )
				call oFunc.init( tmpFunc.xGrid, atan2( aimag(tmpFunc.fArray), real(tmpFunc.fArray) ) )
				
			case( FourierTransform_POWER_SPECTRUM )
				call oFunc.init( tmpFunc.xGrid, real(tmpFunc.fArray)**2+aimag(tmpFunc.fArray)**2 )
				
			case default
				call oFunc.init( tmpFunc.xGrid, abs(tmpFunc.fArray) )
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
	!! http://en.wikipedia.org/wiki/Window_function
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
		nPoints = iFunc.nPoints()
		
		if( .not. present(window) ) return
		
		effType = window.type
		
		if( effType == FourierTransform_WINDOW_NONE ) return
		
		select case( effType )
			case( FourierTransform_WINDOW_COS )
				effParam = 2.0_8
			case( FourierTransform_WINDOW_GAUSS )
				effParam = 4.0_8
			case( FourierTransform_WINDOW_ERF_TOPHAT )
				effParam = 0.2_8
		end select
		
		if( window.param > 0.0 ) effParam = window.param
		
		effCentered = window.centered
		
		if( .not. FString_isNull(window.oFile) ) useOFile = .true.
		
		if( useOFile ) open( unit=32, file=trim(window.oFile), status="unknown" )
		
		if( effCentered ) then
			range = abs( iFunc.xGrid.at(nPoints)-iFunc.xGrid.at(1) )/2.0_8
			t0 = iFunc.xGrid.at(1) + range
		else
			range = abs( iFunc.xGrid.at(nPoints)-iFunc.xGrid.at(1) )
			t0 = iFunc.xGrid.at(1)
		end if
		
		select case( effType )
			case( FourierTransform_WINDOW_COS )
				
				do i=1,nPoints
					windowValue = cos( Math_PI*( iFunc.xGrid.at(i) - t0 )/2.0_8/range )**effParam
					call oFunc.set( i, oFunc.at(i)*windowValue )
					
					if( useOFile ) write(32,*) oFunc.xGrid.at(i), windowValue
				end do
				
			case( FourierTransform_WINDOW_GAUSS )
				
				do i=1,nPoints
! 					windowValue = effParam*exp(-0.5_8*effParam**2*(iFunc.xGrid.at(i)-t0)**2/range**2)/range/sqrt(2.0*Math_PI)
					windowValue = exp(-0.5_8*effParam**2*(iFunc.xGrid.at(i)-t0)**2/range**2)
					call oFunc.set( i, oFunc.at(i)*windowValue )
					
					if( useOFile ) write(32,*) iFunc.xGrid.at(i), windowValue
				end do
				
			case( FourierTransform_WINDOW_ERF_TOPHAT )
				
				if( effCentered ) then
					w = 2.0_8*(1.0_8-2.0_8*effParam)*range
				else
					w = 2.0_8*(1.0_8-effParam)*range
				end if

				dw = effParam*range
				
				do i=1,nPoints
					windowValue = Math_erfTophat( iFunc.xGrid.at(i), t0, w, dw )
					call oFunc.set( i, oFunc.at(i)*windowValue )
					
					if( useOFile ) write(32,*) iFunc.xGrid.at(i), windowValue
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
		n = iFunc.nPoints()
		dx = iFunc.xGrid.stepSize
		dp = 2.0_8*Math_PI/dx/real(n,8)
		
		oFunc = iFunc
		oFunc.xGrid = FourierTransform_omegaGrid( n, dx, order=FourierTransform_NORDER )
		
		do i=1,n
			oFunc.fArray(i) = 0.0_8
			do j=1,n
				oFunc.fArray(i) = oFunc.fArray(i) + iFunc.fArray(j)*exp( real(effSgn,8)*Math_I*iFunc.xGrid.at(j)*oFunc.xGrid.at(i) )
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
		n = iFunc.nPoints()
		dp = iFunc.xGrid.stepSize
		dx = 2.0_8*Math_PI/dp/real(n,8)
		
		oFunc = iFunc
		oFunc.xGrid = FourierTransform_xGrid( n, dx, order=FourierTransform_NORDER )
		
		do i=1,n
			oFunc.fArray(i) = 0.0_8
			do j=1,n
				oFunc.fArray(i) = oFunc.fArray(i) + iFunc.fArray(j)*exp( real(effSgn,8)*Math_I*iFunc.xGrid.at(j)*oFunc.xGrid.at(i) )
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
		
		integer(8) :: planF, planB
		type(Grid) :: omega
		
		if( present(oFunc) ) then
			oFunc = iFunc
			
			call dfftw_plan_dft_1d( planF, iFunc.nPoints(), iFunc.fArray, oFunc.fArray, FFTW_FORWARD, FFTW_ESTIMATE )
			call dfftw_plan_dft_1d( planB, iFunc.nPoints(), iFunc.fArray, oFunc.fArray, FFTW_BACKWARD, FFTW_ESTIMATE )
			
			omega = FourierTransform_omegaGrid( iFunc.xGrid )
			
			call dfftw_execute( planF )
			
			oFunc.fArray = ( Math_I*omega.data )**order*oFunc.fArray
			
			call dfftw_execute( planB )
				
			oFunc = oFunc/real(iFunc.nPoints(),8)
		else
			call dfftw_plan_dft_1d( planF, iFunc.nPoints(), iFunc.fArray, iFunc.fArray, FFTW_FORWARD, FFTW_ESTIMATE )
			call dfftw_plan_dft_1d( planB, iFunc.nPoints(), iFunc.fArray, iFunc.fArray, FFTW_BACKWARD, FFTW_ESTIMATE )
			
			omega = FourierTransform_omegaGrid( iFunc.xGrid )
			
			call dfftw_execute( planF )
			
			iFunc.fArray = ( Math_I*omega.data )**order*iFunc.fArray
			
			call dfftw_execute( planB )
				
			iFunc = iFunc/real(iFunc.nPoints(),8)
		end if
			
		call dfftw_destroy_plan( planF )
		call dfftw_destroy_plan( planB )
	end subroutine FourierTransform_derivate_CNFunction
	
	!>
	!! This is neccesary only for FourierTransform_test()
	!! Maxima:
	!!   f(x) := exp(-0.1*x**2)*(   0.5*cos(9.0*x) -  0.5*Math_I*sin(5.0*x) +  2.0*sin(2.5*x) );
	!!
	!!   fortran( diff( f(x), x, 1 ) );
	!!   fortran( diff( f(x), x, 2 ) );
	!!
	function funcTest( x ) result( output )
		real(8), intent(in) :: x
		complex(8) :: output
		
		output = exp(-0.1_8*x**2)*( 0.5_8*cos(9.0_8*x) - 0.5_8*Math_I*sin(5.0_8*x) + 2.0_8*sin(2.5_8*x) )

! 		http://www.ee.nmt.edu/~wedeward/EE341/FA97/example9.html
! 		output = exp(-2.0_8*x)*Math_ustep(x)
	end function funcTest
	
	!>
	!! This is neccesary only for FourierTransform_test()
	!! Maxima:
	!!   f(x) := exp(-0.1*x**2)*(   0.5*cos(9.0*x) -  0.5*Math_I*sin(5.0*x) +  2.0*sin(2.5*x) );
	!!
	!!   fortran( diff( f(x), x, 1 ) );
	!!   fortran( diff( f(x), x, 2 ) );
	!!
	function funcTestWithNoise( x ) result( output )
		real(8), intent(in) :: x
		complex(8) :: output
		
		output = exp(-0.1_8*x**2)*( 0.5_8*cos(9.0_8*x) - 0.5_8*Math_I*sin(5.0_8*x) + 2.0_8*sin(2.5_8*x) ) &
					+ 0.3_8*sin(RandomUtils_uniform([7.0_8,10.0_8])*x)  !  uniform noise
	end function funcTestWithNoise
	
	!>
	!! This is neccesary only for FourierTransform_test()
	!! Maxima:
	!!   f(x) := exp(-0.1*x**2)*(   0.5*cos(9.0*x) -  0.5*Math_I*sin(5.0*x) +  2.0*sin(2.5*x) );
	!!
	!!   fortran( diff( f(x), x, 1 ) );
	!!   fortran( diff( f(x), x, 2 ) );
	!!
	function dfuncTest( x ) result( output )
		real(8), intent(in) :: x
		complex(8) :: output
		
		output = exp(-1.0E-1*x**2)*(-4.5E+0*sin(9.0E+0*x)-2.5E+0*Math_I*cos(5.0E+0*x) &
					+5.0E+0*cos(2.5E+0*x))-2.0E-1*x*exp(-1.0E-1*x**2)*(5.0E-1*cos(9.0E+0*x) &
						-5.0E-1*Math_I*sin(5.0E+0*x)+2.0E+0*sin(2.5E+0*x))
	end function dfuncTest
	
	!>
	!! This is neccesary only for FourierTransform_test()
	!! Maxima:
	!!   f(x) := exp(-0.1*x**2)*(   0.5*cos(9.0*x) -  0.5*Math_I*sin(5.0*x) +  2.0*sin(2.5*x) );
	!!
	!!   fortran( diff( f(x), x, 1 ) );
	!!   fortran( diff( f(x), x, 2 ) );
	!!
	function d2funcTest( x ) result( output )
		real(8), intent(in) :: x
		complex(8) :: output
		
		output = -4.0E-1*x*exp(-1.0E-1*x**2)*(-4.5E+0*sin(9.0E+0*x)-2.5E+0*Math_I*cos(5.0E+0*x) &
					+5.0E+0*cos(2.5E+0*x))+4.000000000000001E-2*x**2*exp(-1.0E-1*x**2)*(5.0E-1*cos(9.0E+0*x) &
						-5.0E-1*Math_I*sin(5.0E+0*x)+2.0E+0*sin(2.5E+0*x))-2.0E-1*exp(-1.0E-1*x**2)*(5.0E-1*cos(9.0E+0*x) &
							-5.0E-1*Math_I*sin(5.0E+0*x)+2.0E+0*sin(2.5E+0*x))+exp(-1.0E-1*x**2)*(-4.05E+1*cos(9.0E+0*x) &
								+1.25E+1*Math_I*sin(5.0E+0*x)-1.25E+1*sin(2.5E+0*x))
	end function d2funcTest
	
	!>
	!! This is neccesary only for FourierTransform_test()
	!! Maxima:
	!!   f(x) := exp(-0.5*x**2);
	!!
	function funcGaussian( x ) result( output )
		real(8), intent(in) :: x
		complex(8) :: output
		
		real(8) :: alpha
		
		alpha = 0.5_8
		output = exp(-alpha*x**2)
	end function funcGaussian
	
	!>
	!! This is neccesary only for FourierTransform_test()
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
	!! This is neccesary only for FourierTransform_test()
	!!
	function funcRectangular( x ) result( output )
		real(8), intent(in) :: x
		complex(8) :: output
		
		real(8) :: a
		
		a = 0.5_8
		
		output = Math_ubox( a*x )
	end function funcRectangular
	
	!>
	!! This is neccesary only for FourierTransform_test()
	!!
	function FfuncRectangular( omega ) result( output )
		real(8), intent(in) :: omega
		complex(8) :: output
		
		real(8) :: a
		
		a = 0.5_8
		
		output = Math_nsinc( omega/(2.0_8*Math_PI*a) )/sqrt(2.0_8*Math_PI)/a
	end function FfuncRectangular
	
	!>
	!! @bief test
	!!
	subroutine FourierTransform_test()
		type(Grid) :: xGrid, omegaGrid
		type(Grid) :: xGrid2, omegaGrid2
		type(CNFunction) :: funcA, dFuncA, fftFuncA, fftDFuncA
		type(CNFunction) :: funcB, dFuncB, fftFuncB, fftDFuncB
		type(CNFunction) :: funcAB, dFuncAB, fftFuncAB, fftDFuncAB
		type(RNFunction) :: aSpectrum, pSpectrum
		type(FourierTransform) :: fft
		real(8) :: exactValue
		real(8) :: value
		integer :: i, j
		real(8), allocatable :: array(:)
		complex(8), allocatable :: cArray(:)
		integer(8) :: planF, planB
		character :: cBuffer
		
! 		write(*,*) "------------------------------------"
! 		write(*,*) " Verifing Array constructors (even) "
! 		write(*,*) "------------------------------------"
! 		allocate( array(10) )
! 		
! 		call FourierTransform_xArray( array, size(array), 0.1_8 )
! 		write(*,"(A30,<size(array)>F10.5)") "        x array = ", array
! 		call FourierTransform_shift( array )
! 		write(*,"(A30,<size(array)>F10.5)") "  shift x array = ", array
! 		call FourierTransform_ishift( array )
! 		write(*,"(A30,<size(array)>F10.5)") " ishift x array = ", array
! 		call FourierTransform_omegaArray( array, size(array), 0.1_8 )
! 		write(*,"(A30,<size(array)>F10.5)") "        p array = ", array
! 		call FourierTransform_shift( array )
! 		write(*,"(A30,<size(array)>F10.5)") "  shift p array = ", array
! 		call FourierTransform_ishift( array )
! 		write(*,"(A30,<size(array)>F10.5)") " ishift p array = ", array
! 		
! 		deallocate( array )
! 		
! 		write(*,*) "-----------------------------------"
! 		write(*,*) " Verifing Array constructors (odd) "
! 		write(*,*) "-----------------------------------"
! 		allocate( array(11) )
! 		
! 		call FourierTransform_xArray( array, size(array), 0.1_8 )
! 		write(*,"(A30,<size(array)>F10.5)") "        x array = ", array
! 		call FourierTransform_shift( array )
! 		write(*,"(A30,<size(array)>F10.5)") "  shift x array = ", array
! 		call FourierTransform_ishift( array )
! 		write(*,"(A30,<size(array)>F10.5)") " ishift x array = ", array
! 		call FourierTransform_omegaArray( array, size(array), 0.1_8 )
! 		write(*,"(A30,<size(array)>F10.5)") "        p array = ", array
! 		call FourierTransform_shift( array )
! 		write(*,"(A30,<size(array)>F10.5)") "  shift p array = ", array
! 		call FourierTransform_ishift( array )
! 		write(*,"(A30,<size(array)>F10.5)") " ishift p array = ", array
! 		
! 		deallocate( array )
! 
! 		write(*,*) "---------------------------"
! 		write(*,*) " Verifing shift Grid (odd)"
! 		write(*,*) "---------------------------"
! 		allocate( array(11) )
! 		
! 		call xGrid.init( array )
! 		write(*,"(A30,<xGrid.nPoints>F10.5)") "freq array = ", xGrid.data
! 		call FourierTransform_shift( xGrid )
! 		write(*,"(A30,<xGrid.nPoints>F10.5)") "shifted freq array = ", xGrid.data
! 		call FourierTransform_ishift( xGrid )
! 		write(*,"(A30,<xGrid.nPoints>F10.5)") "inverse shifted freq array = ", xGrid.data
! 		
! 		deallocate( array )
! ! 		call GOptions_doYouWantToContinue()
! 		
! 		write(*,*) "---------------------------------------"
! 		write(*,*) " Verifing DFT Array @internal use only"
! 		write(*,*) "---------------------------------------"
! 		
! 		allocate( cArray(9) )
! 		
! 		do i=-size(cArray)/2,size(cArray)/2
! 			cArray(i+size(cArray)/2+1) = exp(-0.1_8*real(i,8)**2)
! 		end do
! 		
! 		write(*,*) "REAL PART"
! 		write(*,"(A10,<size(cArray)>F10.5)") " input = ", real(cArray)
! 		call FourierTransform_dft( cArray )
! 		write(*,"(A10,<size(cArray)>F10.5)") "FourierTransform = ", real(cArray)
! 		call FourierTransform_idft( cArray )
! 		write(*,"(A10,<size(cArray)>F10.5)") "IFourierTransform = ", real(cArray)
! 		
! 		write(*,*) ""
! 		write(*,*) "IMAGINARY PART"
! 		write(*,"(A10,<2*size(cArray)>F10.5)") " input = ", aimag(cArray)
! 		call FourierTransform_dft( cArray )
! 		write(*,"(A10,<2*size(cArray)>F10.5)") "FourierTransform = ", aimag(cArray)
! 		call FourierTransform_idft( cArray )
! 		write(*,"(A10,<2*size(cArray)>F10.5)") "IFourierTransform = ", aimag(cArray)
! 		
! 		deallocate( cArray )
! 		call GOptions_doYouWantToContinue()
! 		
! 		write(*,*) "----------------------------"
! 		write(*,*) " FourierTransform of a gaussian function"
! 		write(*,*) "----------------------------"
! 		
! ! 		call xGrid.init( -10.0_8, 10.0_8, 100 )
! ! 		call funcA.init( xGrid, funcGaussian )
! 		call xGrid.init( -10.0_8, 10.0_8, 200 )
! 		call funcA.init( xGrid, funcRectangular )
! ! 		call xGrid.init( -3.0_8*Math_PI, 3.0_8*Math_PI, 1001 )
! ! 		call funcA.init( xGrid, funcTestWithNoise )
! 		write(*,"(A)") "input (.func) = "
! 		call funcA.show()
! 		call funcA.save(".func.dat")
! 		
! 		write(*,"(A)") "FourierTransform (.Ffunc) = "
! 		funcB = FourierTransform_fft( funcA )
! 		call funcB.show()
! 		call funcB.save(".Ffunc.dat")
! 		
! 		funcB = FourierTransform_nft( funcA )
! 		write(*,"(A)") "exact (.Fexact) = "
! 		call funcB.show()
! 		call funcB.save(".Fexact.dat")
! 		
! 		call system( "echo plot \"//achar(34)//".func.dat\"//achar(34)//" u 1:2 w l lw 2, \"//achar(34)//"\"//achar(34)//" u 1:3 w l lw 1.5 | gnuplot -p" )
! 		call system( "echo plot \"//achar(34)//".Ffunc.dat\"//achar(34)//" u 1:2 w l lw 2, \"//achar(34)//"\"//achar(34)//" u 1:3 w l lw 1.5 | gnuplot -p" )
! 		call system( "echo plot \"//achar(34)//".Fexact.dat\"//achar(34)//" u 1:2 w l lw 2, \"//achar(34)//"\"//achar(34)//" u 1:3 w l lw 1.5 | gnuplot -p" )
! 		call GOptions_doYouWantToContinue()
! 		call system( "rm .func.dat .Ffunc.dat .Fexact.dat" )
! 		
! 		write(*,*) "-----------------------------"
! 		write(*,*) " iFourierTransform of a gaussian function"
! 		write(*,*) "-----------------------------"
! 		
! ! 		call omegaGrid.init( -15.239866_8, 15.239866_8, 100 )
! ! 		call funcA.init( omegaGrid, funcFGaussian )
! 		funcA = funcB
! 		write(*,"(A)") "input (.Ffunc) = "
! 		call funcA.show()
! 		call funcA.save(".Ffunc.dat")
! 		
! 		write(*,"(A)") "iFourierTransform (.func) = "
! 		funcB = funcA
! 		funcB = FourierTransform_ifft( funcB )
! 		
! 		call funcB.show()
! 		call funcB.save(".func.dat")
! 		
! 		funcB = FourierTransform_inft( funcA )
! 		write(*,"(A)") "exact (.exact) = "
! 		call funcB.show()
! 		call funcB.save(".exact.dat")
! 		
! 		call system( "echo plot \"//achar(34)//".Ffunc.dat\"//achar(34)//" u 1:2 w l lw 2, \"//achar(34)//"\"//achar(34)//" u 1:3 w l lw 1.5 | gnuplot -p" )
! 		call system( "echo plot [] [-3:3] \"//achar(34)//".func.dat\"//achar(34)//" u 1:2 w l lw 2, \"//achar(34)//"\"//achar(34)//" u 1:3 w l lw 1.5 | gnuplot -p" )
! 		call system( "echo plot [] [-3:3] \"//achar(34)//".exact.dat\"//achar(34)//" u 1:2 w l lw 2, \"//achar(34)//"\"//achar(34)//" u 1:3 w l lw 1.5 | gnuplot -p" )
! 		call GOptions_doYouWantToContinue()
! 		call system( "rm .Ffunc.dat .func.dat .exact.dat" )
! 		
! 		write(*,*) "-------------------------------"
! 		write(*,*) " iFourierTransform( FourierTransform(func) ). CNFunction "
! 		write(*,*) "-------------------------------"
! 		
! 		call xGrid.init( -3.0_8*Math_PI, 3.0_8*Math_PI, 1001 )
! 		call funcA.init( xGrid, funcTestWithNoise )
! 
! 		write(*,"(A)") "input (.func) = "
! 		call funcA.show()
! 		call funcA.save(".func.dat")
! 		
! 		write(*,"(A)") "FourierTransform (.Ffunc) = "
! 		funcB = FourierTransform_fft( funcA )
! 		call funcB.show()
! 		call funcB.save(".Ffunc.dat")
! 		
! 		write(*,"(A)") "iFourierTransform (.iFFfunc) = "
! 		funcA = FourierTransform_ifft( funcB )
! 		call funcA.show()
! 		call funcA.save(".iFFfunc.dat")
! 		
! 		call system( "echo plot \"//achar(34)//".func.dat\"//achar(34)//" u 1:2 w l lw 1.5, \"//achar(34)//"\"//achar(34)//" u 1:3 w l lw 1.5 | gnuplot -p" )
! 		call system( "echo plot \"//achar(34)//".Ffunc.dat\"//achar(34)//" u 1:2 w l lw 1.5, \"//achar(34)//"\"//achar(34)//" u 1:3 w l lw 1.5 | gnuplot -p" )
! 		call system( "echo plot \"//achar(34)//".iFFfunc.dat\"//achar(34)//" u 1:2 w l lw 1.5, \"//achar(34)//"\"//achar(34)//" u 1:3 w l lw 1.5 | gnuplot -p" )
! 		call GOptions_doYouWantToContinue()
! 		call system( "rm .func.dat .Ffunc.dat .iFFfunc.dat" )
! 		
! 		write(*,*) "--------------------------------"
! 		write(*,*) " FourierTransform( iFourierTransform(Ffunc) ). CNFunction "
! 		write(*,*) "--------------------------------"
! 		
! 		call xGrid.init( -3.0_8*Math_PI, 3.0_8*Math_PI, 1001 )
! 		call funcA.init( xGrid, funcTestWithNoise )
! 		funcA = FourierTransform_fft( funcA )
! 
! 		write(*,"(A)") "input (.Ffunc) = "
! 		call funcA.show()
! 		call funcA.save(".Ffunc.dat")
! 		
! 		write(*,"(A)") "iFourierTransform (.iFfunc) = "
! 		funcA = FourierTransform_ifft( funcA )
! 		call funcA.show()
! 		call funcA.save(".iFfunc.dat")
! 		
! 		write(*,"(A)") "FourierTransform (.FiFfunc) = "
! 		funcA = FourierTransform_fft( funcA )
! 		call funcA.show()
! 		call funcA.save(".FiFfunc.dat")
! 		
! 		call system( "echo plot \"//achar(34)//".Ffunc.dat\"//achar(34)//" u 1:2 w l lw 1.5, \"//achar(34)//"\"//achar(34)//" u 1:3 w l lw 1.5 | gnuplot -p" )
! 		call system( "echo plot \"//achar(34)//".iFfunc.dat\"//achar(34)//" u 1:2 w l lw 1.5, \"//achar(34)//"\"//achar(34)//" u 1:3 w l lw 1.5 | gnuplot -p" )
! 		call system( "echo plot \"//achar(34)//".FiFfunc.dat\"//achar(34)//" u 1:2 w l lw 1.5, \"//achar(34)//"\"//achar(34)//" u 1:3 w l lw 1.5 | gnuplot -p" )
! 		call GOptions_doYouWantToContinue()
! 		call system( "rm .Ffunc.dat .iFfunc.dat .FiFfunc.dat" )
! 		
! 		write(*,*) "------------------------------------"
! 		write(*,*) " Verifing FourierTransform CNFunction with plans"
! 		write(*,*) "------------------------------------"
! 		
! ! 		call xGrid.init( -3.0_8*Math_PI, 3.0_8*Math_PI, 1001 )
! ! 		call funcA.init( xGrid, funcTestWithNoise )
! 		call xGrid.init( -2.5_8, 2.5_8, 90 )
! 		call funcA.init( xGrid, funcRectangular )
! 		write(*,"(A)") "input (.func) = "
! 		call funcA.show()
! 		call funcA.save(".func.dat")
! 		
! 		planF = FourierTransform_plan( funcA, FourierTransform_FORWARD )
! 		planB = FourierTransform_plan( funcA, FourierTransform_BACKWARD )
! 		
! 		write(*,"(A)") "FourierTransform (.Ffunc) = "
! 		call FourierTransform_execute( planF )
! 		funcA.xGrid = FourierTransform_omegaGrid( funcA.xGrid, order=FourierTransform_SORDER )
! 		call FourierTransform_phase( funcA )
! 		call FourierTransform_shift( funcA )
! 		
! 		funcA = funcA*xGrid.stepSize/sqrt(2.0_8*Math_PI)
! 
! 		call funcA.show()
! 		call funcA.save(".Ffunc.dat")
! 		
! 		write(*,"(A)") "iFourierTransform (.iFFfunc) = "
! 		call FourierTransform_ishift( funcA )
! 		call FourierTransform_phase( funcA )
! 		call FourierTransform_execute( planB )
! 		funcA.xGrid = FourierTransform_xGrid( funcA.xGrid, order=FourierTransform_NORDER )
! 		funcA = funcA*xGrid.stepSize/sqrt(2.0_8*Math_PI)
! ! 		funcA = funcA/real( funcA.nPoints(), 8 )
! 		call funcA.show()
! 		call funcA.save(".iFFfunc.dat")
! 		
! 		call system( "echo plot \"//achar(34)//".func.dat\"//achar(34)//" u 1:2 w l lw 1.5, \"//achar(34)//"\"//achar(34)//" u 1:3 w l lw 1.5 | gnuplot -p" )
! 		call system( "echo plot \"//achar(34)//".Ffunc.dat\"//achar(34)//" u 1:2 w l lw 1.5, \"//achar(34)//"\"//achar(34)//" u 1:3 w l lw 1.5 | gnuplot -p" )
! 		call system( "echo plot \"//achar(34)//".iFFfunc.dat\"//achar(34)//" u 1:2 w l lw 1.5, \"//achar(34)//"\"//achar(34)//" u 1:3 w l lw 1.5 | gnuplot -p" )
! 		call GOptions_doYouWantToContinue()
! 		call system( "rm .func.dat .Ffunc.dat .iFFfunc.dat" )
! 
! 		call FourierTransform_destroyPlan( planF )
! 		call FourierTransform_destroyPlan( planB )
! 		
! 		write(*,*) "------------------------------------------"
! 		write(*,*) " iFourierTransform( FourierTransform(func) ). CNFunction with plans"
! 		write(*,*) "------------------------------------------"
! 		
! 		call xGrid.init( -3.0_8*Math_PI, 3.0_8*Math_PI, 1001 )
! 		call funcA.init( xGrid, funcTestWithNoise )
! 		write(*,"(A)") "input (.func) = "
! 		call funcA.show()
! 		call funcA.save(".func.dat")
! 		
! 		planF = FourierTransform_plan( funcA, FourierTransform_FORWARD )
! 		planB = FourierTransform_plan( funcA, FourierTransform_BACKWARD )
! 		
! 		call FourierTransform_execute( planF )
! 		call FourierTransform_execute( planB )
! 		funcA = funcA/real( funcA.nPoints(), 8 )
! 		
! 		call funcA.show()
! 		call funcA.save(".iFFfunc.dat")
! 		
! 		call system( "echo plot \"//achar(34)//".func.dat\"//achar(34)//" u 1:2 w l lw 1.5, \"//achar(34)//"\"//achar(34)//" u 1:3 w l lw 1.5 | gnuplot -p" )
! 		call system( "echo plot \"//achar(34)//".iFFfunc.dat\"//achar(34)//" u 1:2 w l lw 1.5, \"//achar(34)//"\"//achar(34)//" u 1:3 w l lw 1.5 | gnuplot -p" )
! 		call GOptions_doYouWantToContinue()
! 		call system( "rm .func.dat .iFFfunc.dat" )
! 
! 		call FourierTransform_destroyPlan( planF )
! 		call FourierTransform_destroyPlan( planB )
! 		
! 		write(*,*) "-----------------------------------------"
! 		write(*,*) " Verifing FourierTransform CNFunction oriented object"
! 		write(*,*) "-----------------------------------------"
! 		
! 		call xGrid.init( -3.0_8*Math_PI, 3.0_8*Math_PI, 1001 )
! 		
! 		call funcA.init( xGrid, funcTestWithNoise )
! 		write(*,"(A)") "input (.func) = "
! 		call funcA.show()
! 		call funcA.save(".func.dat")
! 		
! 		call fft.init( funcA, FourierTransform_SPATIAL_DOMAIN )
! 		
! 		call fft.execute( FourierTransform_FORWARD, sync=.true., shift=.true. )
! 		write(*,"(A)") "FourierTransform (.Ffunc) = "
! 		call funcA.show()
! 		call funcA.save(".Ffunc.dat")
! 		
! 		call fft.execute( FourierTransform_BACKWARD, sync=.true., shift=.true. )
! 		write(*,"(A)") "iFourierTransform (.iFFfunc) = "
! 		call funcA.show()
! 		call funcA.save(".iFFfunc.dat")
! 		
! 		call system( "echo plot \"//achar(34)//".func.dat\"//achar(34)//" u 1:2 w l lw 1.5, \"//achar(34)//"\"//achar(34)//" u 1:3 w l lw 1.5 | gnuplot -p" )
! 		call system( "echo plot \"//achar(34)//".Ffunc.dat\"//achar(34)//" u 1:2 w l lw 1.5, \"//achar(34)//"\"//achar(34)//" u 1:3 w l lw 1.5 | gnuplot -p" )
! 		call system( "echo plot \"//achar(34)//".iFFfunc.dat\"//achar(34)//" u 1:2 w l lw 1.5, \"//achar(34)//"\"//achar(34)//" u 1:3 w l lw 1.5 | gnuplot -p" )
! 		call GOptions_doYouWantToContinue()
! 		call system( "rm .func.dat .Ffunc.dat .iFFfunc.dat" )
		
! 		write(*,*) "--------------------------------------"
! 		write(*,*) " Second derivative via FourierTransform with plans"
! 		write(*,*) "--------------------------------------"
! 		
! 		call xGrid.init( -3.0_8*Math_PI, 3.0_8*Math_PI, 1001 )
! 		omegaGrid = FourierTransform_omegaGrid( xGrid )
! 		call funcA.init( xGrid, funcTest )
! 		write(*,"(A)") "input (.func) = "
! 		call funcA.show()
! 		call funcA.save(".func.dat")
! 		
! 		planF = FourierTransform_plan( funcA, FourierTransform_FORWARD )
! 		planB = FourierTransform_plan( funcA, FourierTransform_BACKWARD )
! 		
! 		call FourierTransform_execute( planF )
! 		
! 		funcA.fArray = ( Math_I*fft.omega.data )**2*funcA.fArray
! 		
! 		write(*,"(A)") "iFourierTransform (.dfunc) = "
! 		call FourierTransform_execute( planB )
! 		funcA = funcA/real( funcA.nPoints(), 8 )
! 		call funcA.show()
! 		call funcA.save(".dfunc.dat")
! 		
! 		call funcB.init( xGrid, d2funcTest )
! 		write(*,"(A)") "exact (.exact) = "
! 		call funcB.show()
! 		call funcB.save(".exact.dat")
! 		
! 		call system( "echo plot \"//achar(34)//".func.dat\"//achar(34)//" u 1:2 w l lw 1.5, \"//achar(34)//"\"//achar(34)//" u 1:3 w l lw 1.5 | gnuplot -p" )
! 		call system( "echo plot \"//achar(34)//".dfunc.dat\"//achar(34)//" u 1:2 w l lw 1.5, \"//achar(34)//"\"//achar(34)//" u 1:3 w l lw 1.5 | gnuplot -p" )
! 		call system( "echo plot \"//achar(34)//".exact.dat\"//achar(34)//" u 1:2 w l lw 1.5, \"//achar(34)//"\"//achar(34)//" u 1:3 w l lw 1.5 | gnuplot -p" )
! 		call GOptions_doYouWantToContinue()
! 		call system( "rm .func.dat .dfunc.dat .exact.dat" )
! 
! 		call FourierTransform_destroyPlan( planF )
! 		call FourierTransform_destroyPlan( planB )
! 
! 		write(*,*) "------------------------------------------------------"
! 		write(*,*) " Second derivative via FourierTransform CNFunction oriented object"
! 		write(*,*) "------------------------------------------------------"
! 		
! 		call xGrid.init( -3.0_8*Math_PI, 3.0_8*Math_PI, 1001 )
! 		call funcA.init( xGrid, funcTest )
! 		write(*,"(A)") "input (.func) = "
! 		call funcA.show()
! 		call funcA.save(".func.dat")
! 		
! 		call FourierTransform_derivate( funcA, 2 )
! ! 		call fft.init( funcA, FourierTransform_SPATIAL_DOMAIN )
! ! 		
! ! 		call fft.execute( FourierTransform_FORWARD )
! ! 		
! ! 		funcA.fArray = ( Math_I*fft.omega.data )**2*funcA.fArray
! ! 		
! ! 		call fft.execute( FourierTransform_BACKWARD )
! 		
! 		write(*,"(A)") "iFourierTransform (.dfunc) = "
! 		call funcA.show()
! 		call funcA.save(".dfunc.dat")
! 		
! 		call funcB.init( xGrid, d2funcTest )
! 		write(*,"(A)") "exact (.exact) = "
! 		call funcB.show()
! 		call funcB.save(".exact.dat")
! 		
! 		call system( "echo plot \"//achar(34)//".func.dat\"//achar(34)//" u 1:2 w l lw 1.5, \"//achar(34)//"\"//achar(34)//" u 1:3 w l lw 1.5 | gnuplot -p" )
! 		call system( "echo plot \"//achar(34)//".dfunc.dat\"//achar(34)//" u 1:2 w l lw 1.5, \"//achar(34)//"\"//achar(34)//" u 1:3 w l lw 1.5 | gnuplot -p" )
! 		call system( "echo plot \"//achar(34)//".exact.dat\"//achar(34)//" u 1:2 w l lw 1.5, \"//achar(34)//"\"//achar(34)//" u 1:3 w l lw 1.5 | gnuplot -p" )
! 		call GOptions_doYouWantToContinue()
! 		call system( "rm .func.dat .dfunc.dat .exact.dat" )
! 		
! 		write(*,*) "----------"
! 		write(*,*) " Spectrum "
! 		write(*,*) "----------"
! 		
! ! 		call xGrid.init( -3.0_8*Math_PI, 3.0_8*Math_PI, 1001 )
! 		call xGrid.init( -15.0_8, 15.0_8, 1001 )
! 		call funcA.init( xGrid, funcTest )
! 		write(*,"(A)") "input (.func) = "
! 		call funcA.show()
! 		call funcA.save(".func.dat")
! 		
! 		aSpectrum = FourierTransform_powerSpectrum( funcA )
! 		
! 		write(*,"(A)") "aspec (.asfunc) = "
! 		call aSpectrum.show()
! 		call aSpectrum.save(".asfunc.dat")
! 		
! 		pSpectrum = FourierTransform_phaseSpectrum( funcA )
! 		
! 		write(*,"(A)") "pspec (.psfunc) = "
! 		call pSpectrum.show()
! 		call pSpectrum.save(".psfunc.dat")
! 		
! 		call system( "echo plot \"//achar(34)//".func.dat\"//achar(34)//" u 1:2 w l lw 1.5, \"//achar(34)//"\"//achar(34)//" u 1:3 w l lw 1.5 | gnuplot -p" )
! 		call system( "echo plot \"//achar(34)//".asfunc.dat\"//achar(34)//" u 1:2 w l lw 1.5 | gnuplot -p" )
! 		call system( "echo plot \"//achar(34)//".psfunc.dat\"//achar(34)//" u 1:2 w l lw 1.5 | gnuplot -p" )
! 		call GOptions_doYouWantToContinue()
! 		call system( "rm .func.dat .asfunc.dat .psfunc.dat" )
! 		
! 		write(*,*) "--------------------------------------------------------"
! 		write(*,*) " Filtering by amplitude. FourierTransform CNFunction oriented object"
! 		write(*,*) "--------------------------------------------------------"
! 		
! 		call xGrid.init( -3.0_8*Math_PI, 3.0_8*Math_PI, 1001 )
! 		call funcA.init( xGrid, funcTestWithNoise )
! 		write(*,"(A)") "input (.func) = "
! 		call funcA.show()
! 		call funcA.save(".func.dat")
! 		
! 		call fft.init( funcA, FourierTransform_SPATIAL_DOMAIN )
! 		
! 		call fft.execute( FourierTransform_FORWARD )
! 		
! 		where( abs( funcA.fArray ) <= 20.0_8 ) funcA.fArray = 0.0_8
! 		
! 		call fft.execute( FourierTransform_BACKWARD )
! 		
! 		write(*,"(A)") "iFourierTransform (.ffunc) = "
! 		call funcA.show()
! 		call funcA.save(".ffunc.dat")
! 		
! 		call system( "echo plot \"//achar(34)//".func.dat\"//achar(34)//" u 1:2 w l lw 1.5, \"//achar(34)//"\"//achar(34)//" u 1:3 w l lw 1.5 | gnuplot -p" )
! 		call system( "echo plot \"//achar(34)//".ffunc.dat\"//achar(34)//" u 1:2 w l lw 1.5, \"//achar(34)//"\"//achar(34)//" u 1:3 w l lw 1.5 | gnuplot -p" )
! 		call GOptions_doYouWantToContinue()
! 		call system( "rm .func.dat .ffunc.dat" )
! 		
! 		write(*,*) "--------------------------------------------------------"
! 		write(*,*) " Filtering by frequency. FourierTransform CNFunction oriented object"
! 		write(*,*) "--------------------------------------------------------"
! 		
! 		call xGrid.init( -3.0_8*Math_PI, 3.0_8*Math_PI, 1001 )
! 		call funcA.init( xGrid, funcTestWithNoise )
! 		write(*,"(A)") "input (.func) = "
! 		call funcA.show()
! 		call funcA.save(".func.dat")
! 		
! 		call fft.init( funcA, FourierTransform_SPATIAL_DOMAIN )
! 		
! 		call fft.execute( FourierTransform_FORWARD )
! 		
! 		do i=1,funcA.nPoints()
! 			if( abs( fft.omega.at(i) ) > 10.0_8 ) then
! 				funcA.fArray(i) = 0.0_8
! 			end if
! 		end do
! 		
! 		call fft.execute( FourierTransform_BACKWARD )
! 		
! 		write(*,"(A)") "iFourierTransform (.ffunc) = "
! 		call funcA.show()
! 		call funcA.save(".ffunc.dat")
! 		
! 		call system( "echo plot \"//achar(34)//".func.dat\"//achar(34)//" u 1:2 w l lw 1.5, \"//achar(34)//"\"//achar(34)//" u 1:3 w l lw 1.5 | gnuplot -p" )
! 		call system( "echo plot \"//achar(34)//".ffunc.dat\"//achar(34)//" u 1:2 w l lw 1.5, \"//achar(34)//"\"//achar(34)//" u 1:3 w l lw 1.5 | gnuplot -p" )
! 		call GOptions_doYouWantToContinue()
! 		call system( "rm .func.dat .ffunc.dat" )


! 		write(*,*) "-----------------------------------------"
! 		write(*,*) " Verifing Memory Manage"
! 		write(*,*) "-----------------------------------------"
! 		
! 		call xGrid.init( -3.0_8*Math_PI, 3.0_8*Math_PI, 1001 )
! 		
! 		do i=1,10000000
! 			if( mod(i,2) == 0 ) then
! 				call funcA.init( xGrid, funcTestWithNoise )
! 				call fft.init( funcA, FourierTransform_SPATIAL_DOMAIN )
! 			else
! 				call funcB.init( xGrid, funcTestWithNoise )
! 				call fft.init( funcB, FourierTransform_SPATIAL_DOMAIN )
! 			end if
! 			
! 			call fft.execute( FourierTransform_FORWARD, sync=.true., shift=.true. )
! 			call fft.execute( FourierTransform_BACKWARD, sync=.true., shift=.true. )
! 		end do
		
		write(*,*) "-----------------------------------------"
		write(*,*) " Verifing resize grids"
		write(*,*) "-----------------------------------------"
		
		call xGrid.init( -10.0_8, 10.0_8, 10 )
		omegaGrid = FourierTransform_omegaGrid( xGrid )
		
		xGrid2 = xGrid
		call xGrid2.resize( 5, 0 )
		omegaGrid2 = FourierTransform_omegaGrid( xGrid2 )
		
		call xGrid.show()
		call xGrid2.show()
		call omegaGrid.show()
		call omegaGrid2.show()
		
! 		write(*,*) ""
! 		write(*,*) " Testing resize grid 5, dir = 0"
! 		write(*,*) "--------------------------------"
! 		funcB = funcA
! 		call funcA.resize( 5, 0 )
! 		call funcA.show()
! 		do i=1,funcA.nPoints()
! 			write(*,"(I5,F10.5,5X,2F10.5)") i, funcA.x(i), funcA.at(i)
! 		end do
		
		
	end subroutine FourierTransform_test
	
end module FourierTransform_

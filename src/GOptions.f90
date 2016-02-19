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

!>
!! @brief
!!
module GOptions_
	use IOStream_
	use Timer_
	
	implicit none
	public
	
	real(8) :: GOptions_zero = 1e-12
	
	! 1)
	! 2)
	! 3)
	! 4)
	logical :: GOptions_printLevel = 1
	
	! 1) NOTHING
	! 2) INFO
	! 3) INFO + WARNING
	logical :: GOptions_debugLevel = 1
	
	integer :: GOptions_indentLength = 5
	
	type(Timer) :: GOptions_timer
	
	!---------------------------------------------------------------------------
	! FFT and FFT3D constants
	!---------------------------------------------------------------------------
	enum, BIND(c)
		enumerator :: FourierTransform_FORWARD   = -1
		enumerator :: FourierTransform_BACKWARD  = +1
	end enum
	
	enum, BIND(c)
		enumerator :: FourierTransform_NORDER
		enumerator :: FourierTransform_SORDER
	end enum
	
	enum, BIND(c)
		enumerator :: FourierTransform_SPATIAL_DOMAIN
		enumerator :: FourierTransform_FREQUENCY_DOMAIN
	end enum
	
	enum, BIND(c)
		enumerator :: FourierTransform_FILTER_NONE = 0
		enumerator :: FourierTransform_FILTER_AMPLITUDE
		enumerator :: FourierTransform_FILTER_AMPLITUDE_FRACTION
		enumerator :: FourierTransform_FILTER_FREQUENCY
	end enum
	
	enum, BIND(c)
		enumerator :: FourierTransform_NORM_SPECTRUM = 0
		enumerator :: FourierTransform_REALPART_SPECTRUM
		enumerator :: FourierTransform_IMAGPART_SPECTRUM
		enumerator :: FourierTransform_PHASE_SPECTRUM
		enumerator :: FourierTransform_POWER_SPECTRUM
	end enum
	
	enum, BIND(c)
		enumerator :: FourierTransform_FFT_METHOD = 0
		enumerator :: FourierTransform_NUMERICAL_METHOD
	end enum
	
	enum, BIND(c)
		enumerator :: FourierTransform_WINDOW_NONE = 0
		enumerator :: FourierTransform_WINDOW_COS
		enumerator :: FourierTransform_WINDOW_GAUSS
		enumerator :: FourierTransform_WINDOW_ERF_TOPHAT
	end enum
	
	!---------------------------------------------------------------------------
	! NFunctionND available formats
	!---------------------------------------------------------------------------
	enum, BIND(c)
		enumerator :: AUTO_FORMAT = 0
		enumerator :: N3DF_FORMAT
		enumerator :: N2DF_FORMAT
		enumerator :: N1DF_FORMAT
		enumerator :: CUBE_FORMAT
		enumerator :: RCUBE_FORMAT
		enumerator :: ICUBE_FORMAT
		enumerator :: BLKS_FORMAT
	end enum
	
	interface GOptions_valueReport
		module procedure GOptions_lValueReport
		module procedure GOptions_iValueReport
		module procedure GOptions_rValueReport
		module procedure GOptions_sValueReport
		module procedure GOptions_rArrValueReport
		module procedure GOptions_iArrValueReport
	end interface GOptions_valueReport
	
	!---------------------------------------------------------------------------
	! NIntegrator constants
	!---------------------------------------------------------------------------
	enum, BIND(c)
		enumerator :: NIntegrator_SIMPSON = 0
		enumerator :: NIntegrator_EXTSIMPSON
		enumerator :: NIntegrator_SIMPSON38
		enumerator :: NIntegrator_TRAPEZOIDAL
		enumerator :: NIntegrator_FIXED_QUADRATURE
		enumerator :: NIntegrator_QUADRATURE
		enumerator :: NIntegrator_ADAPTIVE_QUADRATURE
		enumerator :: NIntegrator_BOOLE
	end enum
	
	!---------------------------------------------------------------------------
	! NPeakFinder constants
	!---------------------------------------------------------------------------
	enum, BIND(c)
		enumerator :: NPeakFinder_MAX_DIST   = 0
		enumerator :: NPeakFinder_MAX_AVER_NEIG
		enumerator :: NPeakFinder_MAX_AVER_NEIG_AVER
		enumerator :: NPeakFinder_ENTROPY
	end enum
	
	contains
	
	!>
	!! @brief
	!!
	subroutine GOptions_section( message, indent )
		character(*), intent(in) :: message
		integer, intent(in), optional :: indent
		
		integer :: effIndent
		
		effIndent = 0
		if( present(indent) ) effIndent = indent
		
		write(STDOUT,"(A)") ""
		write(STDOUT,"(<GOptions_indentLength*effIndent>X,A)") "+"//repeat("-",len_trim(message)+2)//"+"
		write(STDOUT,"(<GOptions_indentLength*effIndent>X,A)") "| "//trim(message)//" |"
		write(STDOUT,"(<GOptions_indentLength*effIndent>X,A)") "+"//repeat("-",len_trim(message)+2)//"+"
		write(STDOUT,"(A)") ""
	end subroutine GOptions_section
	
	!>
	!! @brief
	!!
	subroutine GOptions_subsection( message, indent )
		character(*), intent(in) :: message
		integer, intent(in), optional :: indent
		
		integer :: effIndent
		
		effIndent = 0
		if( present(indent) ) effIndent = indent
		
		write(STDOUT,"(A)") ""
		write(STDOUT,"(<GOptions_indentLength*effIndent>X,A)") repeat("-",len_trim(message)+2)
		write(STDOUT,"(<GOptions_indentLength*effIndent>X,A)") " "//trim(message)//" "
		write(STDOUT,"(<GOptions_indentLength*effIndent>X,A)") repeat("-",len_trim(message)+2)
		write(STDOUT,"(A)") ""
	end subroutine GOptions_subsection

	!>
	!! @brief
	!!
	subroutine GOptions_paragraph( message, indent )
		character(*), intent(in) :: message
		integer, intent(in), optional :: indent
		
		integer :: effIndent
		
		effIndent = 0
		if( present(indent) ) effIndent = indent
		
		write(STDOUT,"(A)") ""
		write(STDOUT,"(<GOptions_indentLength*effIndent>X,A)") " "//trim(message)//" "
		write(STDOUT,"(<GOptions_indentLength*effIndent>X,A)") repeat("-",len_trim(message)+2)
		write(STDOUT,"(A)") ""
	end subroutine GOptions_paragraph
	
	!>
	!! @brief
	!!
	subroutine GOptions_error( message, where, addinfo )
		character(*), intent(in) :: message
		character(*), intent(in), optional :: where
		character(*), intent(in), optional :: addinfo
		
		character(1000) :: effWhere
		character(1000) :: effAddinfo
		
		effWhere = ""
		if( present(where) ) effWhere = where

		effAddinfo = ""
		if( present(addinfo) ) effAddinfo = addinfo
		
		write(STDOUT,"(A)") ""
		write(STDERR,"(A)") "### ERROR ### "//trim(effWhere)//": "//trim(message)
		write(STDERR,"(A)") "              "//trim(effAddinfo)
		
		stop
	end subroutine GOptions_error
	
	!>
	!! @brief
	!!
	subroutine GOptions_warning( message, where, addinfo )
		character(*), intent(in) :: message
		character(*), intent(in), optional :: where
		character(*), intent(in), optional :: addinfo
		
		character(1000) :: effWhere
		character(1000) :: effAddinfo
		
		effWhere = ""
		if( present(where) ) effWhere = where

		effAddinfo = ""
		if( present(addinfo) ) effAddinfo = addinfo
		
! 		if( GOptions_debugLevel >= 1 ) then
			write(STDOUT,"(A)") ""
			write(STDERR,"(A)") "!!! WARNING ¡¡¡ "//trim(effWhere)//": "//trim(message)
			write(STDERR,"(A)") "                "//trim(effAddinfo)
! 		end if
	end subroutine GOptions_warning
	
	!>
	!! @brief
	!!
	subroutine GOptions_info( message, where, addinfo )
		character(*), intent(in) :: message
		character(*), intent(in), optional :: where
		character(*), intent(in), optional :: addinfo
		
		character(1000) :: effWhere
		character(1000) :: effAddinfo
		
		effWhere = ""
		if( present(where) ) effWhere = where
		
		effAddinfo = ""
		if( present(addinfo) ) effAddinfo = addinfo
		
		if( GOptions_debugLevel >= 2 ) then
			write(STDOUT,"(A)") ""
			write(STDOUT,"(A)") "%%% INFO %%% "//trim(effWhere)//": "//trim(message)
			write(STDOUT,"(A)") "             "//trim(effAddinfo)
		end if
	end subroutine GOptions_info
		
	!>
	!! @brief
	!!
	subroutine GOptions_doYouWantToContinue()
		character :: cBuffer
		
		write(*,*) ""
		write(*,"(A)", advance="no") "Do you want to continue [y/n] ? "
		cBuffer = ""
		do while ( cBuffer /= "y" .and. cBuffer /= "n" )
			read(*,*) cBuffer
		end do
		if( cBuffer == 'n' ) stop
		write(*,*) ""
	end subroutine GOptions_doYouWantToContinue
	
	!>
	!! @brief
	!!
	subroutine GOptions_rValueReport( varName, value, message, units, indent )
		character(*), intent(in) :: varName
		real(8), intent(in) :: value
		character(*), intent(in), optional :: message
		character(*), intent(in), optional :: units
		integer, intent(in), optional :: indent
		
		character(1000) :: effMessage
		character(100) :: effUnits
		integer :: effIndent
		
		effMessage = ""
		if( present(message) ) effMessage = message
		
		effUnits = ""
		if( present(units) ) effUnits = units
		
		effIndent = 0
		if( present(indent) ) effIndent = indent
		
		write(STDOUT,"(<GOptions_indentLength*effIndent>X,A10,F20.5,A8,5X,A)") trim(varName), value, trim(effUnits), trim(effMessage)
	end subroutine GOptions_rValueReport
	
	!>
	!! @brief
	!!
	subroutine GOptions_lValueReport( varName, value, message, units, indent )
		character(*), intent(in) :: varName
		logical, intent(in) :: value
		character(*), intent(in), optional :: message
		character(*), intent(in), optional :: units
		integer, intent(in), optional :: indent
		
		character(1000) :: effMessage
		character(100) :: effUnits
		integer :: effIndent
		
		effMessage = ""
		if( present(message) ) effMessage = message
		
		effUnits = ""
		if( present(units) ) effUnits = units
		
		effIndent = 0
		if( present(indent) ) effIndent = indent
		
		write(STDOUT,"(<GOptions_indentLength*effIndent>X,A10,L14,6X,A8,5X,A)") trim(varName), value, trim(effUnits), trim(effMessage)
	end subroutine GOptions_lValueReport
	
	!>
	!! @brief
	!!
	subroutine GOptions_iValueReport( varName, value, message, units, indent )
		character(*), intent(in) :: varName
		integer, intent(in) :: value
		character(*), intent(in), optional :: message
		character(*), intent(in), optional :: units
		integer, intent(in), optional :: indent
		
		character(1000) :: effMessage
		character(100) :: effUnits
		integer :: effIndent
		
		effMessage = ""
		if( present(message) ) effMessage = message
		
		effUnits = ""
		if( present(units) ) effUnits = units
		
		effIndent = 0
		if( present(indent) ) effIndent = indent
		
		write(STDOUT,"(<GOptions_indentLength*effIndent>X,A10,I14,6X,A8,5X,A)") trim(varName), value, trim(effUnits), trim(effMessage)
	end subroutine GOptions_iValueReport

	!>
	!! @brief
	!!
	subroutine GOptions_sValueReport( varName, value, message, indent )
		character(*), intent(in) :: varName
		character(*), intent(in) :: value
		character(*), intent(in), optional :: message
		integer, intent(in), optional :: indent
		
		character(1000) :: effMessage
		integer :: effIndent
		
		effMessage = ""
		if( present(message) ) effMessage = message
		
		effIndent = 0
		if( present(indent) ) effIndent = indent
		
		write(STDOUT,"(<GOptions_indentLength*effIndent>X,A10,A20,A8,5X,A)") trim(varName), trim(value), trim(effMessage)
	end subroutine GOptions_sValueReport
	
	!>
	!! @brief
	!!
	subroutine GOptions_rArrValueReport( varName, values, message, units, indent )
		character(*), intent(in) :: varName
		real(8), intent(in) :: values(:)
		character(*), intent(in), optional :: message
		character(*), intent(in), optional :: units
		integer, intent(in), optional :: indent
		
		character(1000) :: effMessage
		character(100) :: effUnits
		integer :: effIndent
		
		effMessage = ""
		if( present(message) ) effMessage = message
		
		effUnits = ""
		if( present(units) ) effUnits = units
		
		effIndent = 0
		if( present(indent) ) effIndent = indent
		
		write(STDOUT,"(<GOptions_indentLength*effIndent>X,A10,<size(values)>F15.5,A8,5X,A)") trim(varName), values, trim(effUnits), trim(effMessage)
	end subroutine GOptions_rArrValueReport
	
	!>
	!! @brief
	!!
	subroutine GOptions_iArrValueReport( varName, values, message, units, indent )
		character(*), intent(in) :: varName
		integer, intent(in) :: values(:)
		character(*), intent(in), optional :: message
		character(*), intent(in), optional :: units
		integer, intent(in), optional :: indent
		
		character(1000) :: effMessage
		character(100) :: effUnits
		integer :: effIndent
		
		effMessage = ""
		if( present(message) ) effMessage = message
		
		effUnits = ""
		if( present(units) ) effUnits = units
		
		effIndent = 0
		if( present(indent) ) effIndent = indent
		
		write(STDOUT,"(A10,<size(values)>I15,A8,5X,A)") trim(varName), values, trim(effUnits), trim(effMessage)
	end subroutine GOptions_iArrValueReport
	
end module GOptions_

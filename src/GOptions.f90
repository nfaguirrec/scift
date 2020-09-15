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

!>
!! @brief
!!
module GOptions_
	use IOStream_
	use Timer_
        use String_
	
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
	integer :: GOptions_debugLevel = 1
	
	integer :: GOptions_indentLength = 5
	
	type(Timer) :: GOptions_timer
	
	!---------------------------------------------------------------------------
	! AtomicElementsDB constants
	!---------------------------------------------------------------------------
	enum, BIND(c)
		enumerator :: AtomicElementsDB_COVALENT_RADIUS = 0
		enumerator :: AtomicElementsDB_VANDERWAALS_RADIUS
	end enum
	
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
		enumerator :: FourierTransform_WINDOW_FLATTOP
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
	
	!---------------------------------------------------------------------------
	! Histogram constants
	!---------------------------------------------------------------------------
	enum, BIND(c)
		enumerator :: Histogram_SQUAREROOT = 0
		enumerator :: Histogram_STURGES
		enumerator :: Histogram_RICE
		enumerator :: Histogram_DOANE
		enumerator :: Histogram_SCOTT
		enumerator :: Histogram_FREEDMAN_DIACONIS
		enumerator :: Histogram_GAUSSIAN_DRESSING
		enumerator :: Histogram_LORENTZIAN_DRESSING
	end enum
	
	enum, BIND(c)
		enumerator :: Histogram_STORING = 0
		enumerator :: Histogram_RUNNING
	end enum
	
	!---------------------------------------------------------------------------
	! Molecule constants
	!---------------------------------------------------------------------------
	enum, BIND(c)
		enumerator :: Molecule_FIXED_DISTORSION = 0
		enumerator :: Molecule_RANDOM_DISTORSION
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
		
		write(IO_STDOUT,"(A)") ""
		write(IO_STDOUT,"("//FString_fromInteger(GOptions_indentLength*effIndent)//"X,A)") "+"//repeat("-",len_trim(message)+2)//"+"
		write(IO_STDOUT,"("//FString_fromInteger(GOptions_indentLength*effIndent)//"X,A)") "| "//trim(message)//" |"
		write(IO_STDOUT,"("//FString_fromInteger(GOptions_indentLength*effIndent)//"X,A)") "+"//repeat("-",len_trim(message)+2)//"+"
		write(IO_STDOUT,"(A)") ""
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
		
		write(IO_STDOUT,"(A)") ""
		write(IO_STDOUT,"("//FString_fromInteger(GOptions_indentLength*effIndent)//"X,A)") repeat("-",len_trim(message)+2)
		write(IO_STDOUT,"("//FString_fromInteger(GOptions_indentLength*effIndent)//"X,A)") " "//trim(message)//" "
		write(IO_STDOUT,"("//FString_fromInteger(GOptions_indentLength*effIndent)//"X,A)") repeat("-",len_trim(message)+2)
		write(IO_STDOUT,"(A)") ""
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
		
		write(IO_STDOUT,"(A)") ""
		write(IO_STDOUT,"("//FString_fromInteger(GOptions_indentLength*effIndent)//"X,A)") " "//trim(message)//" "
		write(IO_STDOUT,"("//FString_fromInteger(GOptions_indentLength*effIndent)//"X,A)") repeat("-",len_trim(message)+2)
		write(IO_STDOUT,"(A)") ""
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
		
		write(IO_STDOUT,"(A)") ""
		write(IO_STDERR,"(A)") "### ERROR ### "//trim(effWhere)//": "//trim(message)
		write(IO_STDERR,"(A)") "              "//trim(effAddinfo)
		
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
			write(IO_STDOUT,"(A)") ""
			write(IO_STDERR,"(A)") "!!! WARNING ¡¡¡ "//trim(effWhere)//": "//trim(message)
			write(IO_STDERR,"(A)") "                "//trim(effAddinfo)
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
			write(IO_STDOUT,"(A)") ""
			write(IO_STDOUT,"(A)") "%%% INFO %%% "//trim(effWhere)//": "//trim(message)
			write(IO_STDOUT,"(A)") "             "//trim(effAddinfo)
		end if
	end subroutine GOptions_info
		
	!>
	!! @brief
	!!
	subroutine GOptions_doYouWantToContinue( assumeYes, assumeNo )
		logical, intent(in), optional :: assumeYes
		logical, intent(in), optional :: assumeNo
		
		logical :: assumeYesEff
		logical :: assumeNoEff
		
		character :: cBuffer
		
		assumeYesEff = .false.
		if( present(assumeYes) ) assumeYesEff = assumeYes
		
		assumeNoEff = .false.
		if( present(assumeNo) ) assumeNoEff = assumeNo
		
		write(*,*) ""
		write(*,"(A)", advance="no") "Do you want to continue [y/n] ? "
		
		if( assumeYesEff ) then
			write(*,*) "y"
			return
		else if( assumeNoEff ) then
			write(*,*) "n"
			stop
		end if
		
		cBuffer = ""
		do while ( cBuffer /= "y" .and. cBuffer /= "n" )
			read(*,*) cBuffer
		end do
		
		write(*,*) trim(cBuffer)
		if( cBuffer == 'n' ) stop
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
		
		write(IO_STDOUT, &
                        "("//FString_fromInteger(GOptions_indentLength*effIndent)//"X,A10,F20.5,A8,5X,A)") &
                        trim(varName), value, trim(effUnits), trim(effMessage)
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
		
		write(IO_STDOUT, &
                        "("//FString_fromInteger(GOptions_indentLength*effIndent)//"X,A10,L14,6X,A8,5X,A)") &
                        trim(varName), value, trim(effUnits), trim(effMessage)
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
		
		write(IO_STDOUT, &
                        "("//FString_fromInteger(GOptions_indentLength*effIndent)//"X,A10,I14,6X,A8,5X,A)") &
                        trim(varName), value, trim(effUnits), trim(effMessage)
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
		
		write(IO_STDOUT, &
                        "("//FString_fromInteger(GOptions_indentLength*effIndent)//"X,A10,A20,A8,5X,A)") &
                        trim(varName), trim(value), trim(effMessage)
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
		
		write(IO_STDOUT, &
                        "("//FString_fromInteger(GOptions_indentLength*effIndent)//"X,A10,<size(values)>F15.5,A8,5X,A)") &
                        trim(varName), values, trim(effUnits), trim(effMessage)
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
		
		write(IO_STDOUT, &
                        "(A10,"//FString_fromInteger(size(values))//"I15,A8,5X,A)") &
                        trim(varName), values, trim(effUnits), trim(effMessage)
	end subroutine GOptions_iArrValueReport
	
end module GOptions_

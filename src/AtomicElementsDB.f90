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
!! @brief
!!
module AtomicElementsDB_
	use GOptions_
	use UnitsConverter_
	use String_
	use SpecialAtomsPair_
	implicit none
	private
	
	public :: &
		AtomicElementsDB_test
		
	integer, public, parameter :: AtomicElementsDB_nElems = 103
	
	character(2), parameter :: UPPERCASE_SYMBOLS(AtomicElementsDB_nElems) = [ &
			' H',                                                                                'HE', &
			'LI','BE',                                                  ' B',' C',' N',' O',' F','NE', &
			'NA','MG',                                                  'AL','SI',' P',' S','CL','AR', &
			' K','CA','SC','TI',' V','CR','MN','FE','CO','NI','CU','ZN','GA','GE','AS','SE','BR','KR', &
			'RB','SR',' Y','ZR','NB','MO','TC','RU','RH','PD','AG','CD','IN','SN','SB','TE',' I','XE', &
			'CS','BA','LA', &
			          'CE','PR','ND','PM','SM','EU','GD','TB','DY','HO','ER','TM','YB','LU', &
			               'HF','TA',' W','RE','OS','IR','PT','AU','HG','TL','PB','BI','PO','AT','RN', &
			'FR','RA','AC', &
			          'TH','PA',' U','NP','PU','AM','CM','BK','CF','ES','FM','MD','NO','LR' &
		]
		
	character(2), parameter :: LOWERCASE_SYMBOLS(AtomicElementsDB_nElems) = [ &
			' H',                                                                                'He', &
			'Li','Be',                                                  ' B',' C',' N',' O',' F','Ne', &
			'Na','Mg',                                                  'Al','Si',' P',' S','Cl','Ar', &
			' K','Ca','Sc','Ti',' V','Cr','Mn','Fe','Co','Ni','Cu','Zn','Ga','Ge','As','Se','Br','Kr', &
			'Rb','Sr',' Y','Zr','Nb','Mo','Tc','Ru','Rh','Pd','Ag','Cd','In','Sn','Sb','Te',' I','Xe', &
			'Cs','Ba','La', &
			          'Ce','Pr','Nd','Pm','Sm','Eu','Gd','Tb','Dy','Ho','Er','Tm','Yb','Lu', &
			               'Hf','Ta',' W','Re','Os','Ir','Pt','Au','Hg','Tl','Pb','Bi','Po','At','Rn', &
			'Fr','Ra','Ac', &
			          'Th','Pa',' U','Np','Pu','Am','Cm','Bk','Cf','Es','Fm','Md','No','Lr' &
		]
		
	!-----------------------------------------------------------------------------------------------
	! Taken from: http://www.nist.gov/pml/data/comp.cfm
	! Hay que copiar este formato: http://www.lfd.uci.edu/~gohlke/code/elements.py.html
	!-----------------------------------------------------------------------------------------------
		
	real(8), parameter :: ATOMIC_MASS(AtomicElementsDB_nElems) = [ &
! Esta versión es tomada del NIST descrito arriba. Es la masa del isótopo más abundante o del más estable si no hay información de abundancia
		       1.0078,                                                                                                                                                  4.0026, &
		       7.0160,  9.0122,                                                                                           11.0093, 12.0000, 14.0031, 15.9949, 18.9984, 19.9924, &
		      22.9898, 23.9850,                                                                                           26.9815, 27.9769, 30.9738, 31.9721, 34.9689, 39.9624, &
		      38.9637, 39.9626, 44.9559, 47.9479, 50.9440, 51.9405, 54.9380, 55.9349, 58.9332, 57.9353, 62.9296, 63.9291, 68.9256, 73.9212, 74.9216, 79.9165, 78.9183, 83.9115, &
		      84.9118, 87.9056, 88.9058, 89.9047, 92.9064, 97.9054, 96.9064,101.9043,102.9055,105.9035,106.9051,113.9034,114.9039,119.9022,120.9038,129.9062,126.9045,131.9042, &
		     132.9055,137.9052,138.9064, &
				       139.9054,140.9077,141.9077,144.9128,151.9197,152.9212,157.9241,158.9254,163.9292,164.9303,165.9303,168.9342,173.9389,174.9408, &
					        179.9466,180.9480,183.9509,186.9558,191.9615,192.9629,194.9648,196.9666,201.9706,204.9744,207.9767,208.9804,232.0381,210.9875,222.0176, &
		     223.0197,226.0254,227.0278, &
			               232.0381,231.0359,238.0508,237.0482,242.0587,241.0568,245.0655,247.0703,251.0796,252.0830,257.0951,258.0984,259.1010,262.1096 &
! 			 1.0080,                                                                                                                                 4.0026, &
! 			 6.9400, 9.0122,                                                                                10.8100,12.0110,14.0070,15.9990,18.9988,20.1797, &
! 			22.9900,24.3050,                                                                                26.9820,28.0850,30.9738,32.0650,35.4530,39.9480, &
! 			 0.0000, 0.0000, 0.0000, 47.867, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, &
! 			 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, &
! 			 0.0000, 0.0000, 0.0000, &
! 			                 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, &
! 			                         0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 196.97, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, &
! 			 0.0000, 0.0000, 0.0000, &
! 			                 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000 &
		]
		
! 	! Self-Consistent, Year-2009 Covalent Radii
! 	! single-bond covalent radii
! 	! [1] P. Pyykkö, M. Atsumi, Chem. Eur. J. 15 (2009) 186.
! 	! [2] P. Pyykkö, M. Atsumi, J. Chem. Eur. J. 15 (2009) 12770.
! 	! [3] P. Pyykkö, S. Riedel, M. Patzschke, Chem. Eur. J. 11 (2005) 3511.
! 	real(8), parameter :: COVALENT_RADIUS(AtomicElementsDB_nElems) = [ &
! 			 0.32,                                                                                                 0.46, &
! 			 1.33, 1.02,                                                             0.85, 0.75, 0.71, 0.63, 0.64, 0.67, &
! 			 1.55, 1.39,                                                             1.26, 1.16, 1.11, 1.03, 0.99, 0.96, &
! 			 1.96, 1.71, 1.48, 1.36, 1.34, 1.22, 1.19, 1.16, 1.11, 1.10, 1.12, 1.18, 1.24, 1.21, 1.21, 1.16, 1.14, 1.17, &
! 			 2.10, 1.85, 1.63, 1.54, 1.47, 1.38, 1.28, 1.25, 1.25, 1.20, 1.28, 1.36, 1.42, 1.40, 1.40, 1.36, 1.33, 1.31, &
! 			 2.32, 1.96, 1.80, &
! 			             1.63, 1.76, 1.74, 1.73, 1.72, 1.68, 1.69, 1.68, 1.67, 1.66, 1.65, 1.64, 1.70, 1.62, &
! 			                   1.52, 1.46, 1.37, 1.31, 1.29, 1.22, 1.23, 1.24, 1.33, 1.44, 1.44, 1.51, 1.45, 1.47, 1.42, &
! 			 2.23, 2.01, 1.86, &
! 			             1.75, 1.69, 1.70, 1.71, 1.72, 1.66, 1.66, 1.68, 1.68, 1.65, 1.67, 1.73, 1.76, 1.61 &
! ! Rf .......
! ! 1.57, 1.49, 1.43, 1.41, 1.34, 1.29, 1.28, 1.21, 1.22, 1.36, 1.43, 1.62, 1.75, 1.65, 1.57 &
! 		]

	! De piamod, pero hay que revisar el molden.f
	! @todo Revisar. Los valores de los actinidos los he tomado de la tabla anterior que esta comentada
	real(8), parameter :: COVALENT_RADIUS(AtomicElementsDB_nElems) = [ &
			 0.37,                                                                                                 0.70, &
			 1.23, 0.89,                                                             0.90, 0.85, 0.74, 0.74, 0.72, 0.70, &
			 1.00, 1.36,                                                             1.25, 1.17, 1.10, 1.10, 0.99, 0.70, &
			 0.00, 0.00, 0.00, 1.36, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, &  ! < Ti from Pyykkö
			 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, &
			 0.00, 0.00, 0.00, &
			             0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, &
			                   0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, &
			 0.00, 0.00, 0.00, &
			             1.75, 1.69, 1.70, 1.71, 1.72, 1.66, 1.66, 1.68, 1.68, 1.65, 1.67, 1.73, 1.76, 1.61 &
		]
		
	! Mainly taken from:
	!    S. S. Batsanov. Van der Waals Radii of Elements. Inorganic Materials 37 (2001) 871
	!    http://download.springer.com/static/pdf/639/art%253A10.1023%252FA%253A1011625728803.pdf?originUrl=http%3A%2F%2Flink.springer.com%2Farticle%2F10.1023%2FA%3A1011625728803&token2=exp=1458657684~acl=%2Fstatic%2Fpdf%2F639%2Fart%25253A10.1023%25252FA%25253A1011625728803.pdf%3ForiginUrl%3Dhttp%253A%252F%252Flink.springer.com%252Farticle%252F10.1023%252FA%253A1011625728803*~hmac=6b9748bab749f1bbd3299905115c9d1804349b2665a6a454182c877b6b911379
	!    Table 9. Equilibrium (lower figures) van der Waals radii of elements.
	! Rest of elements were taken from:
	!     http://www.webelements.com/periodicity/van_der_waals_radius/
	!     A. Bondi, J. Phys. Chem., 1964, 68, 441.
	real(8), parameter :: VANDERWAALS_RADIUS(AtomicElementsDB_nElems) = [ &
			 1.56,                                                                                                 1.40, &
			 2.63, 2.23,                                                             0.00, 0.00, 0.00, 0.00, 0.00, 1.54, &
			 2.77, 2.42,                                                             0.00, 0.00, 0.00, 0.00, 0.00, 1.88, &
			 3.02, 2.78, 2.62, 2.44, 2.27, 2.23, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 2.02, &
			 3.15, 2.94, 2.71, 2.57, 2.46, 2.39, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 2.16, &
			 3.30, 3.05, 2.81, &
			             0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, &
			                   2.52, 2.42, 2.36, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 1.96, 2.02, 0.00, 0.00, 0.00, 0.00, &
			 0.00, 0.00, 0.00, &
			             0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00 &
! Values from second reference
! 			 1.20,                                                                                                 1.40, &
! 			 1.82, 0.00,                                                             0.00, 1.70, 1.55, 1.52, 1.47, 1.54, &
! 			 2.27, 1.73,                                                             0.00, 2.10, 1.80, 1.80, 1.75, 1.88, &
! 			 2.75, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 1.63, 1.40, 1.39, 1.87, 0.00, 1.85, 1.90, 1.85, 2.02, &
! 			 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 1.63, 1.72, 1.58, 1.93, 2.17, 0.00, 2.06, 1.98, 2.16, &
! 			 0.00, 0.00, 0.00, &
! 			             0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, &
! 			                   0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 1.75, 1.66, 1.55, 1.96, 2.02, 0.00, 0.00, 0.00, 0.00, &
! 			 0.00, 0.00, 0.00, &
! 			             0.00, 1.86, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00 &
		]
		
	! Taken from JMol: http://www.biorom.uma.es/contenido/biomodel/Jmol/colors/jmol_colors.en.htm
	character(6), parameter :: ATOMIC_COLOR(AtomicElementsDB_nElems) = [ &
			 "FFFFFF",                                                                                                                                                                 "D9FFFF", &
			 "CC80FF", "C2FF00",                                                                                                     "FFB5B5", "909090", "3050F8", "FF0D0D", "90E050", "B3E3F5", &
			 "AB5CF2", "8AFF00",                                                                                                     "BFA6A6", "F0C8A0", "FF8000", "FFFF30", "1FF01F", "80D1E3", &
			 "8F40D4", "3DFF00", "E6E6E6", "BFC2C7", "A6A6AB", "8A99C7", "9C7AC7", "E06633", "F090A0", "50D050", "C88033", "7D80B0", "C28F8F", "668F8F", "BD80E3", "FFA100", "A62929", "5CB8D1", &
			 "702EB0", "00FF00", "94FFFF", "94E0E0", "73C2C9", "54B5B5", "3B9E9E", "248F8F", "0A7D8C", "006985", "C0C0C0", "FFD98F", "A67573", "668080", "9E63B5", "D47A00", "940094", "429EB0", &
			 "57178F", "00C900", "70D4FF", &
			                     "FFFFC7", "D9FFC7", "C7FFC7", "A3FFC7", "8FFFC7", "61FFC7", "45FFC7", "30FFC7", "1FFFC7", "00FF9C", "00E675", "00D452", "00BF38", "00AB24", &
						       "4DC2FF", "4DA6FF", "2194D6", "267DAB", "266696", "175487", "D0D0E0", "FFD123", "B8B8D0", "A6544D", "575961", "9E4FB5", "AB5C00", "754F45", "428296", &
			 "420066", "007D00", "70ABFA", &
				   "00BAFF", "00A1FF", "008FFF", "0080FF", "006BFF", "545CF2", "785CE3", "8A4FE3", "A136D4", "B31FD4", "B31FBA", "B30DA6", "BD0D87", "C70066" &
		]
		
	type, public :: AtomicElementsDB
	
		type(SpecialAtomsPair), allocatable :: specialPairs(:)
		
		contains
			final :: destroyAtomicElementsDB
			procedure :: nElements
			procedure :: atomicNumber
			procedure :: atomicMass
			procedure :: atomicMassNumber
			procedure :: radius
			procedure, private :: covalentRadius
			procedure, private :: VanDerWaalsRadius
			procedure :: color
			procedure :: symbol
			procedure :: setSpecialAtomPairs
	end type AtomicElementsDB
	
	type(AtomicElementsDB), public :: AtomicElementsDB_instance
	
	contains
	
	!>
	!! @brief
	!!
	pure subroutine upper( str, ucStr )
		character(*), intent(in) :: str
		character(*), intent(out) :: ucStr
		
		character(*), parameter :: lc = 'abcdefghijklmnopqrstuvwxyz'
		character(*), parameter :: uc = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
		
		integer :: j,k
		
		ucStr = str
		do j=1,len(str)
			k = index( lc, str(j:j) )
			if (k > 0) ucStr(j:j) = uc(k:k)
		end do
	end subroutine upper
	
	!>
	!! @brief Destructor
	!!
	subroutine destroyAtomicElementsDB( this )
		type(AtomicElementsDB) :: this
		
		if( allocated( this%specialPairs ) ) deallocate( this%specialPairs )
	end subroutine destroyAtomicElementsDB

	!>
	!! @brief
	!!
	function nElements( this ) result( output )
		class(AtomicElementsDB), intent(in) :: this
		integer :: output
		
		output = AtomicElementsDB_nElems
	end function nElements
	
	!>
	!! @brief
	!!
	pure function atomicNumber( this, symbol ) result( Z )
		class(AtomicElementsDB), intent(in) :: this
		character(*), intent(in) :: symbol
		integer :: Z
		
		integer :: i
		character(255) :: upperSymb
		
		call upper( symbol, upperSymb )
		
		Z = -1
		do i=1,AtomicElementsDB_nElems
			if( trim(adjustl(upperSymb)) == trim(adjustl(UPPERCASE_SYMBOLS(i))) ) then
				Z = i
				exit
			end if
		end do
	end function atomicNumber
	
	!>
	!! @brief Returns the atomic mass of the element with symbol "symbol" in atomic units
	!!
	pure function atomicMass( this, symbol ) result( M )
		class(AtomicElementsDB), intent(in) :: this
		character(*), intent(in) :: symbol
		real(8) :: M
		
		integer :: i
		character(255) :: upperSymb
		
		call upper( symbol, upperSymb )
		
		M = -1.0_8
		do i=1,AtomicElementsDB_nElems
			if( trim(adjustl(upperSymb)) == trim(adjustl(UPPERCASE_SYMBOLS(i))) ) then
				M = ATOMIC_MASS(i)*amu
				exit
			end if
		end do
	end function atomicMass
	
	!>
	!! @brief
	!!
	pure function atomicMassNumber( this, symbol ) result( M )
		class(AtomicElementsDB), intent(in) :: this
		character(*), intent(in) :: symbol
		integer :: M
		
		integer :: i
		character(255) :: upperSymb
		
		call upper( symbol, upperSymb )
		
		M = -1
		do i=1,AtomicElementsDB_nElems
			if( trim(adjustl(upperSymb)) == trim(adjustl(UPPERCASE_SYMBOLS(i))) ) then
! 				@todo Hay que implementar el uso de isotopos, por ejemplo con keys como 3^He o He_3
				M = ATOMIC_MASS(i)!+number of neutrons
				exit
			end if
		end do
	end function atomicMassNumber
	
	!>
	!! @brief Returns the covalent radius of the element with symbol "symbol" in atomic units
	!!
	pure function radius( this, symbol, type ) result( R )
		class(AtomicElementsDB), intent(in) :: this
		character(*), intent(in) :: symbol
		integer, optional, intent(in) :: type
		real(8) :: R
		
		integer :: effType
		
		effType = AtomicElementsDB_COVALENT_RADIUS
		if( present(type) ) effType = type
		
		select case( effType )
			case( AtomicElementsDB_COVALENT_RADIUS )
				R = this%covalentRadius( symbol )
			case( AtomicElementsDB_VANDERWAALS_RADIUS )
				R = this%VanDerWaalsRadius( symbol )
		end select
	end function radius
	
	!>
	!! @brief Returns the covalent radius of the element with symbol "symbol" in atomic units
	!!
	pure function covalentRadius( this, symbol ) result( R )
		class(AtomicElementsDB), intent(in) :: this
		character(*), intent(in) :: symbol
		real(8) :: R
		
		integer :: i
		character(255) :: upperSymb
		
		call upper( symbol, upperSymb )
		
		R = -1.0_8
		do i=1,AtomicElementsDB_nElems
			if( trim(adjustl(upperSymb)) == trim(adjustl(UPPERCASE_SYMBOLS(i))) ) then
				R = COVALENT_RADIUS(i)*angs
				exit
			end if
		end do
	end function covalentRadius
	
	!>
	!! @brief Returns the Van der Waals radius of the element with symbol "symbol" in atomic units
	!!
	pure function VanDerWaalsRadius( this, symbol ) result( R )
		class(AtomicElementsDB), intent(in) :: this
		character(*), intent(in) :: symbol
		real(8) :: R
		
		integer :: i
		character(255) :: upperSymb
		
		call upper( symbol, upperSymb )
		
		R = -1.0_8
		do i=1,AtomicElementsDB_nElems
			if( trim(adjustl(upperSymb)) == trim(adjustl(UPPERCASE_SYMBOLS(i))) ) then
				R = VANDERWAALS_RADIUS(i)*angs
				exit
			end if
		end do
	end function VanDerWaalsRadius
	
	!>
	!! @brief Returns the Van der Waals radius of the element with symbol "symbol" in atomic units
	!!
	pure function color( this, symbol ) result( output )
		class(AtomicElementsDB), intent(in) :: this
		character(*), intent(in) :: symbol
		character(6) :: output
		
		integer :: i
		character(255) :: upperSymb
		
		call upper( symbol, upperSymb )
		
		output = "FFFFFF"
		do i=1,AtomicElementsDB_nElems
			if( trim(adjustl(upperSymb)) == trim(adjustl(UPPERCASE_SYMBOLS(i))) ) then
				output = ATOMIC_COLOR(i)
				exit
			end if
		end do
	end function color
	
	!>
	!! @brief
	!!
	pure function symbol( this, atomicNumber, upperCase ) result( S )
		class(AtomicElementsDB), intent(in) :: this
		integer, intent(in) :: atomicNumber
		logical, optional, intent(in) :: upperCase
		character(2) :: S
		
		logical :: upperCaseEff
		if( present(upperCase) ) then
			upperCaseEff = upperCase
		else
			upperCaseEff = .false.
		end if
		
		if( upperCaseEff ) then
			S = UPPERCASE_SYMBOLS(atomicNumber)
		else
			S = LOWERCASE_SYMBOLS(atomicNumber)
		end if
	end function symbol
	
	!>
	!! @brief
	!!
	subroutine setSpecialAtomPairs( this, specialPairs )
		class(AtomicElementsDB) :: this
		type(SpecialAtomsPair), allocatable, intent(in) :: specialPairs(:)
		
		integer :: i
		
		if( allocated( this%specialPairs ) ) deallocate( this%specialPairs )
		allocate( this%specialPairs( size(specialPairs) ) )
		this%specialPairs = specialPairs
		
		write(6,"(A)") ""
		write(6,"(A)") "---------------------"
		write(6,"(A)") " Special Atom Pairs  "
		write(6,"(A)") "---------------------"
		write(6,"(A)") ""
		write(6,"(A10,A10,2A15)") "symbol1", "symbol2", "bondCutoff(A)", "dbondCutoff(A)"
		write(6,"(A10,A10,2A15)") "-------", "-------", "-------------", "--------------"
		do i=1,size(specialPairs)
			write(6,"(A10,A10,2F15.5)") trim(specialPairs(i)%symbol1), trim(specialPairs(i)%symbol2), &
				specialPairs(i)%bondCutoff/angs, specialPairs(i).doubleBondCutoff/angs
		end do
		write(6,"(A)") ""
		
	end subroutine setSpecialAtomPairs

	
	!>
	!! @test Testing the AtomicElementsDB class
	!! @brief Testing the AtomicElementsDB class
	!!
	subroutine AtomicElementsDB_test()
		type(AtomicElementsDB) :: atomicDB
		character(20) :: upperSymb
		type(String) :: tmpStr
		integer, allocatable :: hashTableList(:)
		integer :: i, nBits
		
		write(*,*) ""
		write(*,*) "Symbol to atomic number"
		write(*,*) "-----------------------"
		call upper( "   Ti ", upperSymb )
		write(*,*) upperSymb, atomicDB%atomicNumber( upperSymb )
		
		write(*,*) " Ti ", atomicDB%atomicNumber( " Ti " )
		write(*,*) "TI ", atomicDB%atomicNumber( "TI" )
		
		write(*,*) ""
		write(*,*) "Symbol to atomic mass"
		write(*,*) "---------------------"
		call upper( "   Ti ", upperSymb )
		write(*,*) upperSymb, atomicDB%atomicMass( upperSymb )/amu, " amu"
		
		write(*,*) " He ", atomicDB%atomicMass( " He " )/amu, " amu"
		write(*,*) " Ti ", atomicDB%atomicMass( " Ti " )/amu, " amu"
		write(*,*) "AU ", atomicDB%atomicMass( "AU" )/amu, " amu"
		write(*,*) "AR ", atomicDB%atomicMass( "AR" )/amu, " amu"
		
		write(*,*) ""
		write(*,*) "Atomic number to symbol"
		write(*,*) "-----------------------"
		
		write(*,*) "22 ", atomicDB%symbol( 22 )
		write(*,*) "22 ", atomicDB%symbol( 22, .true. )
		write(*,*) "12 ", atomicDB%symbol( 12 )
		write(*,*) "79 ", atomicDB%symbol( 79 )
		
		write(*,*) ""
		write(*,*) "Atomic properties"
		write(*,*) "-----------------------"
		
! 		write(*,*) "22 ", atomicDB%mass( 22 )
! 		write(*,*) "79 ", atomicDB.covalentRadius( 79 )
		write(*,*) "mass    Ti = ", atomicDB%atomicMass( " Ti " )/amu
		write(*,*) "cradius Ti = ", atomicDB.covalentRadius( " Ti " )/angs
		write(*,*) "cradius  O = ", atomicDB.covalentRadius( " O " )/angs
		write(*,*) "cradius Re = ", atomicDB.covalentRadius( " Re" )/angs
		write(*,*) "cradius Au = ", atomicDB.covalentRadius( "Au" )/angs
		
		write(*,*) "Using the singleton instance"
		write(*,*) "----------------------------"
		write(*,*) AtomicElementsDB_instance%symbol(22)
		
! 		write(*,*)
! 		write(*,*) "Testing hash key for atomic labels"
! 		write(*,*) "----------------------------------"
! 		nBits = 32
! 		allocate( hashTableList(AtomicElementsDB_instance.nElements()) )
! 		hashTableList = -1
! 		do i=1,AtomicElementsDB_instance.nElements()
! 			tmpStr = AtomicElementsDB_instance%symbol( i )
! 			
! 			if( any( hashTableList-tmpStr%hashKey(nBits) == 0 ) ) then
! 				write(*,*) "Hashkey replicated"
! 				stop
! 			end if
! 				
! 			hashTableList(i) = tmpStr%hashKey(nBits)
! 			write(*,*) i, tmpStr%fstr, " ==> ", tmpStr%hashKey(nBits)
! 		end do
! 		write(*,*) "key range = ", minval(hashTableList), maxval(hashTableList)
	end subroutine AtomicElementsDB_test
	
end module AtomicElementsDB_

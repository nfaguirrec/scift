!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!  This file is part of scift (Scientific Fortran Tools).
!!  Copyright (C) by authors (2011-2013)
!!  
!!  Authors (alphabetic order):
!!    * Aguirre N.F. (nfaguirrec@gmail.com)  (2011-2013)
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
module AtomicElementsDB_
	use UnitsConverter_
	use String_
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
			 1.0080,                                                                                                                                 4.0026, &
			 6.9400, 9.0122,                                                                                10.8100,12.0110,14.0070,15.9990,18.9988,20.1797, &
			22.9900,24.3050,                                                                                26.9820,28.0850,30.9738,32.0650,35.4530,39.9480, &
			 0.0000, 0.0000, 0.0000, 47.867, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, &
			 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, &
			 0.0000, 0.0000, 0.0000, &
			                 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, &
			                         0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 196.97, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, &
			 0.0000, 0.0000, 0.0000, &
			                 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000 &
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
	real(8), parameter :: COVALENT_RADIUS(AtomicElementsDB_nElems) = [ &
			 0.37,                                                                                                 0.70, &
			 1.23, 0.89,                                                             0.90, 0.85, 0.74, 0.74, 0.72, 0.70, &
			 1.00, 1.36,                                                             1.25, 1.17, 1.10, 1.10, 0.99, 0.70, &
			 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, &
			 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, &
			 0.00, 0.00, 0.00, &
			             0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, &
			                   0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, &
			 0.00, 0.00, 0.00, &
			             0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00 &
		]
		
	type, public :: AtomicElementsDB
		
		contains
			procedure :: nElements
			procedure :: atomicNumber
			procedure :: atomicMass
			procedure :: atomicMassNumber
			procedure :: covalentRadius
			procedure :: symbol
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
	!! @brief Returns the atomic mass of the element with symbol "symbol" in atomic units
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
		write(*,*) upperSymb, atomicDB.atomicNumber( upperSymb )
		
		write(*,*) " Ti ", atomicDB.atomicNumber( " Ti " )
		write(*,*) "TI ", atomicDB.atomicNumber( "TI" )
		
		write(*,*) ""
		write(*,*) "Symbol to atomic mass"
		write(*,*) "---------------------"
		call upper( "   Ti ", upperSymb )
		write(*,*) upperSymb, atomicDB.atomicMass( upperSymb )/amu, " amu"
		
		write(*,*) " He ", atomicDB.atomicMass( " He " )/amu, " amu"
		write(*,*) " Ti ", atomicDB.atomicMass( " Ti " )/amu, " amu"
		write(*,*) "AU ", atomicDB.atomicMass( "AU" )/amu, " amu"
		write(*,*) "AR ", atomicDB.atomicMass( "AR" )/amu, " amu"
		
		write(*,*) ""
		write(*,*) "Atomic number to symbol"
		write(*,*) "-----------------------"
		
		write(*,*) "22 ", atomicDB.symbol( 22 )
		write(*,*) "22 ", atomicDB.symbol( 22, .true. )
		write(*,*) "12 ", atomicDB.symbol( 12 )
		write(*,*) "79 ", atomicDB.symbol( 79 )
		
		write(*,*) ""
		write(*,*) "Atomic properties"
		write(*,*) "-----------------------"
		
! 		write(*,*) "22 ", atomicDB.mass( 22 )
! 		write(*,*) "79 ", atomicDB.covalentRadius( 79 )
		write(*,*) "mass    Ti = ", atomicDB.atomicMass( " Ti " )/angs
		write(*,*) "cradius Re = ", atomicDB.covalentRadius( " Re" )/angs
		write(*,*) "cradius Au = ", atomicDB.covalentRadius( "Au" )/angs
		
		write(*,*) "Using the singleton instance"
		write(*,*) "----------------------------"
		write(*,*) AtomicElementsDB_instance.symbol(22)
		
! 		write(*,*)
! 		write(*,*) "Testing hash key for atomic labels"
! 		write(*,*) "----------------------------------"
! 		nBits = 32
! 		allocate( hashTableList(AtomicElementsDB_instance.nElements()) )
! 		hashTableList = -1
! 		do i=1,AtomicElementsDB_instance.nElements()
! 			tmpStr = AtomicElementsDB_instance.symbol( i )
! 			
! 			if( any( hashTableList-tmpStr.hashKey(nBits) == 0 ) ) then
! 				write(*,*) "Hashkey replicated"
! 				stop
! 			end if
! 				
! 			hashTableList(i) = tmpStr.hashKey(nBits)
! 			write(*,*) i, tmpStr.fstr, " ==> ", tmpStr.hashKey(nBits)
! 		end do
! 		write(*,*) "key range = ", minval(hashTableList), maxval(hashTableList)
	end subroutine AtomicElementsDB_test
	
end module AtomicElementsDB_

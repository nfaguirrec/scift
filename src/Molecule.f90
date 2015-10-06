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
module Molecule_
	use GOptions_
	use UnitsConverter_
	use String_
	use StringList_
	use IOStream_
	use Math_
	use Matrix_
	use SpecialMatrix_
	use RandomSampler_
	use Atom_
	use AtomicElementsDB_
	implicit none
	private
	
	!>
	!! @brief Public parameters
	!!
	integer, public, parameter :: XYZ  = 1
	integer, public, parameter :: MOLDEN  = 2
	
	public :: &
		Molecule_test
	
	type, public :: Molecule
		character(:), allocatable :: name
		type(Atom), allocatable :: atoms(:)
		type(Matrix) :: diagInertiaTensor  !<- Diagonal inertia tensor respect to its center of mass. It is calculated only one time
		logical, private :: axesChosen = .false.
		integer :: composition( AtomicElementsDB_nElems ) !<- [ n1, n2, ..., nN ] n=numberOfAtomsWithZ, pos = atomicNumber
		
		!---------------------------------------------------------------
		! This parameters will be calculated only if necessary,
		! which is checked through the testXXXX corresponding parameter
		real(8), private :: radius_ = 0.0_8
		logical, private :: testRadius_ = .false.
		
		real(8), private :: geomCenter_(3) = 0.0_8
		logical, private :: testGeomCenter_ = .false.
		
		real(8), private :: centerOfMass_(3) = 0.0_8
		logical, private :: testMassCenter_ = .false.
		
		real(8), private :: inertiaAxes_(3,3) = 0.0_8    !<- Centered in its center of mass. inertiaAxes_(:,1), inertiaAxes_(:,2), inertiaAxes_(:,3)
		
		real(8), private :: mass_ = 0.0_8
		logical, private :: testMass_ = .false.
		
		character(100), private :: chemicalFormula_
		logical, private :: testChemicalFormula_ = .false.
		
		integer, private :: massNumber_ = 0
		logical, private :: testMassNumber_ = .false.
		
		integer :: isLineal_ = -1  ! 0 == false, 1 == true
		integer, private :: fv_ = -1  ! Vibrational number of degrees of freedom
		integer, private :: fr_ = -1  ! Rotational number of degrees of freedom
		!---------------------------------------------------------------
		
		contains
			generic :: init => initBase, fromFile
			generic :: assignment(=) => copyMolecule
			procedure :: initBase
			procedure :: fromFile
			procedure :: copyMolecule
			final :: destroyMolecule
			procedure :: str
			procedure :: show
			procedure :: showCompositionVector
			procedure :: load => fromFile
			procedure :: loadXYZ
			procedure :: loadGeomMOLDEN
			procedure :: save
			procedure, private :: saveXYZ
			
			procedure :: randomGeometry
			
			procedure :: set
			procedure :: radius
			procedure :: nAtoms
			procedure :: fv
			procedure :: fr
			procedure :: isLineal
			procedure :: compareFormula
			procedure :: compareConnectivity
			procedure :: compareGeometry
			
			procedure, private :: updateMassNumber
			procedure :: massNumber
			procedure, private :: updateMass
			procedure :: mass
			procedure, private :: updateChemicalFormula
			procedure :: chemicalFormula
			procedure, private :: updateGeomCenter
			procedure :: center
			procedure :: setCenter
			procedure, private :: updateCenterOfMass
			procedure :: centerOfMass
			procedure, private :: buildInertiaTensor
			procedure :: inertiaTensor
			procedure :: inertiaAxis
			procedure :: inertiaAxes
			procedure :: connectivity
			procedure :: minSpinMultiplicity
			
			procedure :: orient
			procedure :: rotate
			
	end type Molecule
	
	contains
	
	!>
	!! @brief Constructor
	!!
	subroutine initBase( this, nAtoms, name )
		class(Molecule) :: this 
		integer, intent(in) :: nAtoms
		character(*), optional, intent(in) :: name
		
		character(:), allocatable :: effName
		integer :: i
		
		if( allocated(this.name) ) deallocate(this.name)
		
		if( present(name) ) then
			this.name = name
		else
			this.name = "unknown"
		end if
		
		if( allocated(this.atoms) ) deallocate(this.atoms)
		allocate( this.atoms(nAtoms) )
		
		do i=1,size(this.atoms)
			call this.atoms(i).init()
		end do
		
		this.composition = -1
	end subroutine initBase
	
	!>
	!! @brief Constructor
	!!
	subroutine fromFile( this, fileName, format, loadName )
		class(Molecule) :: this 
		character(*), intent(in) :: fileName
		integer, optional, intent(in) :: format
		logical, optional, intent(in) :: loadName
		
		type(IFStream) :: ifile
		logical :: loadNameEff

		integer :: i
		integer :: effFormat
		
		character(100) :: sBuffer
		character(:), allocatable :: extension 
		
		effFormat = AUTO_FORMAT
		if( present(format) ) effFormat = format
		
		loadNameEff = .true.
		if( present(loadName) ) loadNameEff = loadName
		
		call ifile.init( trim(fileName) )
		
		if( GOptions_printLevel > 1 ) then
			write(*,*) "Reading molecule from file "//trim(fileName)
		end if
		
		select case ( effFormat )
			case( AUTO_FORMAT )
				sBuffer = FString_removeFileExtension( fileName, extension=extension )
				
				if( trim(extension) == ".xyz" ) then
					call this.loadXYZ( ifile, loadName=loadName )
				else if( trim(extension) == ".rxyz" ) then
					call this.loadXYZ( ifile, loadName=loadName )
				else if( trim(extension) == ".molden" ) then
					call this.loadGeomMOLDEN( ifile, loadName=loadName )
				else
					call GOptions_error( &
						"Unknown format file (AUTO_FORMAT). fileName = "//trim(fileName), &
						"Molecule.fromFile(extension="//trim(extension)//")" &
					)
				end if
				
			case( XYZ )
				call this.loadXYZ( ifile, loadName=loadName )
			case( MOLDEN )
				call this.loadGeomMOLDEN( ifile, loadName=loadName )
			case default
				write(*,*) "This format file is not implemented"
				stop
		end select
		
		call ifile.close()
		
		deallocate( extension )
		
	end subroutine fromFile
	
	!>
	!! @brief Copy constructor
	!!
	subroutine copyMolecule( this, other )
		class(Molecule), intent(out) :: this
		class(Molecule), intent(in) :: other
		
		integer :: i
		
		if( allocated(this.name) ) deallocate(this.name)
		if( allocated(this.atoms) ) deallocate(this.atoms)
		
		this.name = other.name
		
		allocate( this.atoms( size(other.atoms) ) )
		do i=1,size(other.atoms)
			this.atoms(i) = other.atoms(i)
		end do
		
		this.radius_ = other.radius_
		this.testRadius_ = other.testRadius_
		
		this.geomCenter_ = other.geomCenter_
		this.testGeomCenter_ = other.testGeomCenter_
		
		this.centerOfMass_ = other.centerOfMass_
		this.testMassCenter_ = other.testMassCenter_
		
		this.diagInertiaTensor = other.diagInertiaTensor
		this.inertiaAxes_ = other.inertiaAxes_
		
		this.mass_ = other.mass_
		this.testMass_ = other.testMass_
		
		this.composition = other.composition
		this.chemicalFormula_ = other.chemicalFormula_
		this.testChemicalFormula_ = other.testChemicalFormula_
		
		this.massNumber_ = other.massNumber_
		this.testMassNumber_ = other.testMassNumber_
		
		this.isLineal_ = other.isLineal_
		this.fv_ = other.fv_
		this.fr_ = other.fr_
	end subroutine copyMolecule
	
	!>
	!! @brief Destructor
	!!
	subroutine destroyMolecule( this )
		type(Molecule) :: this
		
		if( allocated(this.atoms) ) deallocate( this.atoms )
	end subroutine destroyMolecule
	
	!>
	!! @brief Convert to string
	!!
	function str( this, formatted, prefix ) result( output )
		class(Molecule) :: this 
		character(:), allocatable :: output
		logical, optional :: formatted
		character(*), optional :: prefix
		
		logical :: effFormatted
		character(:), allocatable :: effPrefix
		
		integer :: fmt
		character(200) :: fstr
		integer :: i
		
		effFormatted = .false.
		if( present(formatted) ) effFormatted = formatted
		
		effPrefix = ""
		if( present(prefix) ) effPrefix = prefix
		
		output = ""
		
		if( .not. effFormatted ) then
			output = trim(output)//"<Molecule:"
		
#define RFMT(v) int(log10(max(abs(v),1.0)))+merge(1,2,v>=0)
#define ITEMS(l,v) output = trim(output)//effPrefix//trim(l)//trim(adjustl(v))
#define ITEMI(l,v) output = trim(output)//l; fmt = RFMT(v); write(fstr, "(i<fmt>)") v; output = trim(output)//trim(fstr)
#define ITEMR(l,v) output = trim(output)//l; fmt = RFMT(v); write(fstr, "(f<fmt+7>.6)") v; output = trim(output)//trim(fstr)
			
			do i=1,size(this.atoms)
				if( i == 0 ) then
					ITEMS( "", this.atoms(i).str() )
				else
					ITEMS( ",", this.atoms(i).str() )
				end if
			end do
			
			output = trim(output)//",gCenter="; write(fstr, "(3f10.6)") this.center()
			output = trim(output)//trim(fstr)
			output = trim(output)//",mCenter="; write(fstr, "(3f10.6)") this.centerOfMass()
			output = trim(output)//trim(fstr)
			output = trim(output)//",testCenter="; write(fstr, "(L1)") this.testGeomCenter_
			output = trim(output)//trim(fstr)
#undef RFMT
#undef ITEMS
#undef ITEMI
#undef ITEMR
		
			output = trim(output)//">"
		else
		
#define LINE(l) output = trim(output)//effPrefix//l//new_line('')
#define ITEMS(l,v) output = trim(output)//effPrefix//l; write(fstr, "(x,a)") trim(v); output = trim(output)//trim(fstr)//new_line('')
#define ITEMI(l,v) output = trim(output)//effPrefix//l; write(fstr, "(i10)") v; output = trim(output)//trim(fstr)//new_line('')
#define ITEMR(l,v) output = trim(output)//effPrefix//l; write(fstr, "(f10.5)") v; output = trim(output)//trim(fstr)//new_line('')

			ITEMS("name =", this.name)
			LINE("")
			
			do i=1,size(this.atoms)
				write(fstr, "(A10,3f10.5)") this.atoms(i).symbol, this.atoms(i).r/angs
				if( i /= size(this.atoms) ) then
					output = trim(output)//trim(fstr)//new_line('')
				else
					output = trim(output)//trim(fstr)
				end if
			end do
#undef LINE
#undef ITEMS
#undef ITEMI
#undef ITEMR
		end if
		
	end function str
	
	!>
	!! @brief Show 
	!!
	subroutine show( this, unit, formatted )
		class(Molecule) :: this
		integer, optional, intent(in) :: unit
		logical, optional :: formatted
		
		integer :: effunit
		logical :: effFormatted
		
		effFormatted = .false.
		if( present(formatted) ) effFormatted = formatted
		
		effunit = 6
		if( present(unit) ) effunit = unit
		
		write(effunit,"(a)") trim(str(this,effFormatted))
	end subroutine show
	
	!>
	!! @brief Show 
	!!
	subroutine showCompositionVector( this, unit )
		class(Molecule) :: this
		integer, optional, intent(in) :: unit
		
		integer :: effunit
		
		integer :: i, j
		
		effunit = 6
		if( present(unit) ) effunit = unit

		j=1
		do i=1,AtomicElementsDB_nElems
			write(effunit,"(A6)",advance="no") AtomicElementsDB_instance.symbol(i)//":"//trim(FString_fromInteger(this.composition(i)))//"  "
			
			if( j == 10 ) then
				write(effunit,*) ""
				j = 1
			end if
			
			j = j + 2
		end do
		write(effunit,*) ""
	end subroutine showCompositionVector
	
	!>
	!! @brief Load the molecule from XYZ file in angstrom
	!!
	subroutine loadXYZ( this, ifile, loadName )
		class(Molecule) :: this
		type(IFStream), intent(inout) :: ifile
		logical, optional, intent(in) :: loadName
		
		logical :: loadNameEff
		
		type(String) :: buffer
		character(1000), allocatable :: tokens(:)
		integer :: i, nAtoms
		type(Atom) :: currentAtom
		
		loadNameEff = .true.
		if( present(loadName) ) loadNameEff = loadName
		
		if( allocated(this.atoms) ) deallocate(this.atoms)
		
		if( ifile.numberOfLines < 3 ) then
			write(*,*) "### ERROR ### bad format in XYZ file"
			stop
		end if
		
		buffer = ifile.readLine()
		
		call buffer.split( tokens, " " )
		nAtoms = FString_toInteger(tokens(1))
		
		allocate( this.atoms( nAtoms ) )
		
		buffer = ifile.readLine()
		
		if( loadNameEff ) then
			if( allocated(this.name) ) deallocate(this.name)
			
			if( len_trim(buffer.fstr) /= 0 ) then
				this.name = trim(buffer.fstr)
			else
				this.name = trim(ifile.name)
			end if
		end if
			
		do i=1,nAtoms
			if( ifile.eof() ) then
				write(*,*) "### ERROR ### Inconsistent number of atoms in XYZ file, line "//FString_fromInteger(ifile.currentLine)
				stop
			end if
			
			buffer = ifile.readLine()
			
			call buffer.split( tokens, " " )
			
			if( buffer.length() == 0 .or. size(tokens)<3 ) then
				write(*,*) "### ERROR ### Inconsistent XYZ file, line "//FString_fromInteger(ifile.currentLine)
				stop
			end if
			
			call this.atoms(i).init( &
				trim(tokens(1)), &
				FString_toReal(tokens(2))*angs, &
				FString_toReal(tokens(3))*angs, &
				FString_toReal(tokens(4))*angs &
				)
		end do
		
		if( .not. ifile.eof() ) then
			buffer = ifile.readLine()
			
			if( buffer.length() /= 0 ) then
				write(*,*) "### ERROR ### Inconsistent XYZ file, line "//FString_fromInteger(ifile.currentLine)
				stop
			end if
		end if
		
		deallocate( tokens )
		
		this.axesChosen = .false.
		
		this.testRadius_ = .false.
		this.testGeomCenter_ = .false.
		this.testMassCenter_ = .false.
		this.testMass_ = .false.
		this.testChemicalFormula_ = .false.
		this.testMassNumber_ = .false.
		
		! Actualiza el vector de composición y la formula química
		call this.updateChemicalFormula()

	end subroutine loadXYZ
	
	!>
	!! @brief Load the molecule geometry from MOLDEN file
	!!
	subroutine loadGeomMOLDEN( this, ifile, loadName )
		class(Molecule) :: this
		type(IFStream), intent(inout) :: ifile
		logical, optional, intent(in) :: loadName
		
		logical :: loadNameEff
		
		type(String) :: buffer
		character(1000), allocatable :: tokens(:)
		integer :: iBuffer, i
		logical :: advance
		real(8) :: geometryUnits
		
		type(StringList) :: geometryBlock
		type(StringListIterator), pointer :: iter
		character(3) :: symbol
		real(8) :: x, y, z
		
		loadNameEff = .true.
		if( present(loadName) ) loadNameEff = loadName
		
		if( allocated(this.atoms) ) deallocate(this.atoms)
		
		buffer = ifile.readLine()
		
		if( buffer /= "[Molden Format]" ) then
			write(*,*) "### ERROR ### bad format in MOLDEN file"
			stop
		end if
		
		if( loadNameEff ) then
			this.name = ifile.name
		end if
		
		call geometryBlock.init()
		
		advance = .true.
		do while( .not. ifile.eof() )
			if( advance ) then
				buffer = ifile.readLine()
				call buffer.split( tokens, " " )
				advance = .true.
			end if
			
			!-----------------------------------
			! search for Atoms data
			!-----------------------------------
			if( tokens(1) == "[Atoms]" ) then
				
				geometryUnits = angs
				if( tokens(2) == "Angs" ) then
					geometryUnits = angs
				else if( tokens(2) == "AU" ) then
					geometryUnits = bohr
				end if
				
				do while( .not. ifile.eof() )
					buffer = ifile.readLine()
					call buffer.split( tokens, " " )
					
					if( index( tokens(1), "[" ) == 1 ) then
						advance = .false.
						exit
					end if
					
					call geometryBlock.append( buffer )
				end do
			end if
			
			if( geometryBlock.size() > 0 ) exit
				
			if( .not. advance ) then
				buffer = ifile.readLine()
				call buffer.split( tokens, " " )
			end if
		end do
		
		if( allocated(tokens) ) deallocate( tokens )
		
		allocate( this.atoms( geometryBlock.size() ) )
		
		i=1
		iter => geometryBlock.begin
		do while( associated(iter) )
			read( iter.data.fstr, * ) symbol, iBuffer, iBuffer, x, y, z
			
			call this.atoms(i).init( &
				trim(symbol), &
				x*geometryUnits, &
				y*geometryUnits, &
				z*geometryUnits &
				)
			
			iter => iter.next
			i = i + 1
		end do
		
		this.axesChosen = .false.
		
		this.testRadius_ = .false.
		this.testGeomCenter_ = .false.
		this.testMassCenter_ = .false.
		this.testMass_ = .false.
		this.testChemicalFormula_ = .false.
		this.testMassNumber_ = .false.
		
		! Actualiza el vector de composición y la formula química
		call this.updateChemicalFormula()

	end subroutine loadGeomMOLDEN
		
	!>
	!! @brief Save the molecule to file
	!!
	subroutine save( this, fileName, format, units, append )
		class(Molecule) :: this
		character(*), optional, intent(in) :: fileName
		integer, optional, intent(in) :: format
		real(8), optional, intent(in) :: units
		logical, optional, intent(in) :: append
		
		integer :: effFormat
		
		effFormat = XYZ
		if( present(format) ) effFormat = format
		
		select case ( effFormat )
			case( XYZ )
				call this.saveXYZ( fileName, units, append )
		end select
	end subroutine save
	
	!>
	!! @brief Save the molecule to file
	!!
	subroutine saveXYZ( this, fileName, units, append )
		class(Molecule) :: this
		character(*), optional, intent(in) :: fileName
		real(8), optional, intent(in) :: units
		logical, optional, intent(in) :: append
		
		real(8) :: effUnits
		logical :: effAppend
		
		integer :: fileUnit
		type(OFStream) :: ofile
		integer :: i
		
		effUnits = angs
		if( present(units) ) effUnits = units
		
		effAppend = .false.
		if( present(append) ) effAppend = append
		
		if( present(fileName) ) then
			if( effAppend ) then
				call ofile.init( fileName, append=effAppend )
			else
				call ofile.init( fileName )
			end if
			
			fileUnit = ofile.unit
		else
			fileUnit = 6
		end if
		
		write(fileUnit,*) this.nAtoms()
		write(fileUnit,*) trim(this.name)
		
		do i=1,this.nAtoms()
			write(fileUnit,"(A5,3F20.8)") this.atoms(i).symbol, this.atoms(i).x/effUnits, this.atoms(i).y/effUnits, this.atoms(i).z/effUnits
		end do
		
		if( present(fileName) ) then
			call ofile.close()
		end if
	end subroutine saveXYZ
	
	!>
	!! @brief
	!!
	subroutine randomGeometry( this, radius, maxIter, overlappingRadius, gamma )
		class(Molecule) :: this 
		real(8), optional, intent(in) :: radius
		integer, optional, intent(in) :: maxIter
		real(8), optional, intent(in) :: overlappingRadius
		real(8), optional, intent(in) :: gamma
		
		integer :: effMaxIter
		real(8) :: effRadius
		real(8) :: effOverlappingRadius
		real(8) :: effGamma
		
		integer :: nTrials_
		type(RandomSampler) :: rs
		real(8), allocatable :: sample(:,:)
		real(8) :: rVec1(3), rVec2(3), cm(3)
		integer :: i, j, n
		logical :: overlap
		
		effMaxIter = 100000
		if( present(maxIter) ) effMaxIter = maxIter
		
		effGamma = 1.0
		if( present(gamma) ) effGamma = gamma
		
		if( present(radius) ) then
			effRadius = radius
		else
			effRadius = 0.0_8
			do i=1,this.nAtoms()
				effRadius = effRadius + this.atoms(i).covalentRadius()
			end do
			effRadius = effGamma*effRadius
		end if
		
		effOverlappingRadius = 0.0
		if( present(overlappingRadius) ) effOverlappingRadius = overlappingRadius
		
! Testing overlap
#define OVERLAPPING(i,j) this.atoms(i).covalentRadius()+this.atoms(j).covalentRadius()-effOverlappingRadius > norm2( rVec2-rVec1 )

! ! Sampling on "x" axis, which is the axis with inertia moment equal to cero
! #define RVEC_X(i) [ sample(1,i), 0.0_8, 0.0_8 ]
! ! Sampling on "x-y" axes in polar coordinates
! #define RVEC_XY(i) [ sample(1,i)*cos(sample(2,i)), sample(1,i)*sin(sample(2,i)), 0.0_8 ]
! ! Sampling on "x-y-z" axes in spherical coordinates
! #define RVEC_XYZ(i) [ sample(1,i)*sin(sample(2,i))*cos(sample(3,i)), sample(1,i)*sin(sample(2,i))*sin(sample(3,i)), sample(1,i)*cos(sample(2,i)) ]
! 		
! 		allocate( sample(3,this.nAtoms()) )
! 		
! 		call rs.init( nDim=3 )
! 		call rs.setRange( 1, [0.0_8,effRadius] )         ! r in (0,Rsys)
! 		call rs.setRange( 2, [0.0_8,MATH_PI] )        ! theta in (0,pi)
! 		call rs.setRange( 3, [0.0_8,2.0_8*MATH_PI] )  ! phi in (0,2pi)

! Sampling on "x" axis, which is the axis with inertia moment equal to cero
#define RVEC_X(i) [ sample(1,i), 0.0_8, 0.0_8 ]
! Sampling on "x-y" axes in polar coordinates
#define RVEC_XY(i) [ sample(1,i), sample(2,i), 0.0_8 ]
! Sampling on "x-y-z" axes in spherical coordinates
#define RVEC_XYZ(i) [ sample(1,i), sample(2,i), sample(3,i) ]
		
		allocate( sample(3,this.nAtoms()) )
		
		call rs.init( nDim=3 )
		call rs.setRange( 1, [-effRadius,effRadius] )
		call rs.setRange( 2, [-effRadius,effRadius] )
		call rs.setRange( 3, [-effRadius,effRadius] )
		
		overlap = .false.
		nTrials_ = 0
		do n=1,effMaxIter
			call rs.uniform( sample )
			
			overlap = .false.
			
			!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
			! Se verifica que a los centros no sobrelapen ni que
			! se salgan del radio del sistema
			do i=1,this.nAtoms()-1
				rVec1 = RVEC_XYZ(i)
				
				overlap = norm2(rVec1) > effRadius
				if( overlap ) exit
				
				do j=i+1,this.nAtoms()
					
					rVec2 = RVEC_XYZ(j)
					
					overlap = OVERLAPPING(i,j) .or. norm2(rVec2) > effRadius
					
					if( overlap ) exit
				end do
				
				if( overlap ) exit
			end do
			
			!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
			! Se verifica que a ajustar el centro de masas el sistema
			! no se salga del volumen de simulación
			cm = 0.0_8
			do i=1,this.nAtoms()
				cm = cm + this.atoms(i).mass()*RVEC_XYZ(i)
			end do
			cm = cm/this.mass()
			
			do i=1,this.nAtoms()
				if( norm2(RVEC_XYZ(i)-cm) > effRadius ) then
					overlap = .true.
				end if
			end do
			
			nTrials_ = nTrials_ + 1
			if( .not. overlap ) exit
		end do
		
		do i=1,this.nAtoms()
			this.atoms(i).r = RVEC_XYZ(i)
		end do
#undef OVERLAPPING
#undef RVEC_X
#undef RVEC_XY
#undef RVEC_XYZ
			
		deallocate( sample )
			
		if( overlap ) then
			call GOptions_error( &
				"Maximum number of iterations reached"//" (n = "//trim(FString_fromInteger(n))//")", &
				"Molecule.randomCenters()", &
				"Consider to increase systemRadius("//trim(FString_fromReal(effRadius/angs))//" angs)" &
				)
		end if
		
		call this.orient()
	end subroutine randomGeometry
	
	!>
	!! @brief
	!!
	subroutine set( this, pos, atm )
		class(Molecule) :: this
		integer :: pos
		type(Atom), intent(in) :: atm
		
		this.atoms(pos) = atm
		
		this.testRadius_ = .false.
		this.testGeomCenter_ = .false.
		this.testMassCenter_ = .false.
		this.testMass_ = .false.
		this.testChemicalFormula_ = .false.
		this.testMassNumber_ = .false.
	end subroutine set
	
	!>
	!! @brief Return the radius of the system in atomic units
	!!        defined as half the largest distance between
	!!        two molecule atoms
	!!
	real(8) function radius( this ) result ( output )
		class(Molecule) :: this
		
		real(8) :: rij
		real(8) :: cRadius1, cRadius2
		integer :: i, j
		integer :: si, sj
		
		if( .not. this.testRadius_ ) then
			this.radius_ = 0.0_8
			
			if( this.nAtoms() > 1 ) then
				
				si = -1
				sj = -1
				do i=1,size(this.atoms)-1
					do j=i+1,size(this.atoms)
						rij = norm2( this.atoms(j).r-this.atoms(i).r )
						
						if( rij > this.radius_ ) then
							this.radius_ = rij
							si = i
							sj = j
						end if
					end do
				end do
				
				cRadius1 = AtomicElementsDB_instance.covalentRadius( this.atoms(si).symbol )
				cRadius2 = AtomicElementsDB_instance.covalentRadius( this.atoms(sj).symbol )
				
				this.radius_ = this.radius_/2.0_8 + max( cRadius1, cRadius2 )
			else
				this.radius_ = AtomicElementsDB_instance.covalentRadius( this.atoms(1).symbol )
! 				this.radius_ = 0.76720_8*angs ! <--- radio covalente del átomo de carbono
! 				this.radius_ = 3.40_8*angs/2.0_8    ! <--- mitad del radio cero del diátomo de argon
! 				this.radius_ = 0.97_8*angs    ! <--- radio covalente del argon
			end if
			
			this.testRadius_ = .true.
		end if
		
		output = this.radius_
	end function radius
	
	!>
	!! @brief
	!!
	pure function nAtoms( this ) result( output )
		class(Molecule), intent(in) :: this
		integer :: output
		
		output = size(this.atoms)
	end function nAtoms
	
	!>
	!! @brief
	!!
	function isLineal( this ) result( output )
		class(Molecule), intent(in) :: this
		logical :: output
		
		if( this.isLineal_ /= -1 ) then
			output = ( this.isLineal_==1 )
		else
			write(6,*) "### ERROR ### Cluster.isLineal(). it is first necessary to orient the molecule ( see orient() method )"
			stop
		end if
	end function isLineal
	
	!>
	!! @brief
	!!
	function compareFormula( this, other ) result( output )
		class(Molecule), intent(in) :: this
		class(Molecule), intent(in) :: other
		logical :: output
		
		output = .false.
		if( trim(this.chemicalFormula()) == trim(other.chemicalFormula()) ) then
			output = .true.
		end if
	end function compareFormula
	
	!>
	!! @brief
	!!
	function compareConnectivity( this, other, alpha ) result( output )
		class(Molecule), intent(in) :: this
		class(Molecule), intent(in) :: other
		real(8), optional :: alpha
		logical :: output
		
		integer, allocatable :: conn1(:,:), conn2(:,:)
		
		call this.connectivity( conn1, alpha )
		call other.connectivity( conn2, alpha )
		
! 		write(*,*) "mol1"
! 		write(*,*) conn1(:,1)
! 		write(*,*) conn1(:,2)
! 		write(*,*) "mol2"
! 		write(*,*) conn2(:,1)
! 		write(*,*) conn2(:,2)
		
		output = .false.
		
		if( size(conn1) /= 0 .and. size(conn2) /= 0 ) then
			if( all( conn1 == conn2 ) ) then
				output = .true.
			end if
		end if
		
		if( allocated(conn1) ) deallocate( conn1 )
		if( allocated(conn2) ) deallocate( conn2 )
	end function compareConnectivity
	
	!>
	!! @brief
	!!
	function compareGeometry( this ) result( output )
		class(Molecule), intent(in) :: this
		logical :: output
		
		write(6,*) "### ERROR ### Cluster.compareGeometry(). This method is not implemented yet"
		stop
	end function compareGeometry
	
	!>
	!! @brief
	!!
	function fv( this ) result( output )
		class(Molecule), intent(in) :: this
		integer :: output
		
		if( this.fv_ /= -1 ) then
			output = this.fv_
		else
			write(6,*) "### ERROR ### Cluster.fv(). Vibrational number of degrees of freedom have not selected"
			stop
		end if
	end function fv
	
	!>
	!! @brief
	!!
	function fr( this ) result( output )
		class(Molecule), intent(in) :: this
		integer :: output
		
		if( this.fr_ /= -1 ) then
			output = this.fr_
		else
			write(6,*) "### ERROR ### Cluster.fr(). Rotational number of degrees of freedom have not selected"
			stop
		end if
	end function fr
	
	!>
	!! @brief
	!!
	subroutine updateMassNumber( this )
		class(Molecule) :: this
		
		integer :: i
		
		this.massNumber_ = 0
		do i=1,size(this.atoms)
			this.massNumber_ = this.massNumber_ + AtomicElementsDB_instance.atomicMassNumber( this.atoms(i).symbol )
		end do
	end subroutine updateMassNumber
	
	!>
	!! @brief
	!!
	integer function massNumber( this )
		class(Molecule) :: this 
		
		if( .not. this.testMassNumber_ ) then
			call this.updateMassNumber()
			this.testMassNumber_ = .true.
		end if
		
		massNumber = this.massNumber_
	end function massNumber
	
	!>
	!! @brief
	!!
	subroutine updateMass( this )
		class(Molecule) :: this
		
		integer :: i
		
		this.mass_ = 0.0_8
		do i=1,size(this.atoms)
			this.mass_ = this.mass_ + AtomicElementsDB_instance.atomicMass( this.atoms(i).symbol )
		end do
	end subroutine updateMass
	
	!>
	!! @brief Return the mass of the system in atomic units
	!!
	real(8) function mass( this )
		class(Molecule) :: this 
		
		if( .not. this.testMass_ ) then
			call this.updateMass()
			this.testMass_ = .true.
		end if
		
		mass = this.mass_
	end function mass
	
	!>
	!! @brief
	!!
	subroutine updateChemicalFormula( this )
		class(Molecule) :: this
		
		integer :: key, i
		integer, allocatable :: keys(:)
		character(10), allocatable :: symb(:)
		integer, allocatable :: zVec(:)
		character(200) :: buffer
		integer, allocatable :: counts(:)
		integer :: nDiffAtoms ! numero de átomos de diferente tipo
		
		allocate( keys(size(this.atoms)) ) ! En el peor de los casos cada atomo es diferente
		
		keys = 0
		do i=1,size(this.atoms)
			keys(i) = FString_hashKey( this.atoms(i).symbol )
		end do
		
		allocate( counts(minval( keys ):maxval( keys )) )
		allocate(   symb(minval( keys ):maxval( keys )) )
		allocate(   zVec(minval( keys ):maxval( keys )) )
		
		counts = 0
		do i=1,size(this.atoms)
			key = FString_hashKey( this.atoms(i).symbol )
			counts( key ) = counts( key ) + 1
			
			symb( key ) = this.atoms(i).symbol
			zVec( key ) = AtomicElementsDB_instance.atomicNumber( symb(key) )
		end do
		
		nDiffAtoms = 0
		do i=maxval(keys),minval(keys),-1
			if( counts(i) /= 0 ) then
				nDiffAtoms = nDiffAtoms + 1
			end if
		end do
		
		this.composition = 0
		
		this.chemicalFormula_ = ""
		do i=maxval(keys),minval(keys),-1
			if( counts(i) /= 0 ) then
				this.chemicalFormula_ = trim(adjustl(this.chemicalFormula_))//trim(adjustl(symb( i )))//"_"//trim(FString_fromInteger( counts(i) ))
				this.composition( zVec(i) ) = counts(i)
			end if
		end do
		
		deallocate( keys )
		deallocate( zVec )
		deallocate( counts )
		deallocate( symb )
		
		this.testChemicalFormula_ = .true.
	end subroutine updateChemicalFormula
	
	!>
	!! @brief
	!!
	function chemicalFormula( this ) result ( output )
		class(Molecule) :: this
		character(100) :: output
		
		if( .not. this.testChemicalFormula_ ) then
			call this.updateChemicalFormula()
		end if
		
		output = trim(adjustl(this.chemicalFormula_))
	end function chemicalFormula
	
	!>
	!! @brief
	!!
	subroutine updateGeomCenter( this )
		class(Molecule) :: this
		
		integer :: i
		
		this.geomCenter_ = 0.0_8
		
		do i=1,size(this.atoms)
			this.geomCenter_ = this.geomCenter_ + this.atoms(i).r
		end do
		
		this.geomCenter_ = this.geomCenter_/real( size(this.atoms), 8 )
	end subroutine updateGeomCenter
	
	!>
	!! @brief
	!!
	function center( this ) result( output )
		class(Molecule) :: this
		real(8) :: output(3)
		
		if( .not. this.testGeomCenter_ ) then
			call this.updateGeomCenter()
			this.testGeomCenter_ = .true.
		end if

		output = this.geomCenter_
	end function center
	
	!>
	!! @brief
	!!
	subroutine setCenter( this, center )
		class(Molecule) :: this
		real(8), intent(in) :: center(3)
		
		real(8) :: dr(3)
		integer :: i
		
		dr = center-this.center()
		
		do i=1,size(this.atoms)
			this.atoms(i).r = this.atoms(i).r + dr
		end do
		
		this.geomCenter_ = center
		this.testGeomCenter_ = .true.
		this.testMassCenter_ = .false.
	end subroutine setCenter
	
	!>
	!! @brief
	!!
	subroutine updateCenterOfMass( this )
		class(Molecule) :: this
		
		integer :: i
		
		this.centerOfMass_ = 0.0_8
		
		do i=1,size(this.atoms)
			this.centerOfMass_ = this.centerOfMass_ + &
				AtomicElementsDB_instance.atomicMass( this.atoms(i).symbol )*this.atoms(i).r
		end do
		
		this.centerOfMass_ = this.centerOfMass_/this.mass()
	end subroutine updateCenterOfMass
	
	!>
	!! @brief
	!!
	function centerOfMass( this ) result( output )
		class(Molecule) :: this
		real(8) :: output(3)
		
		if( .not. this.testMassCenter_ ) then
			call this.updateCenterOfMass()
			this.testMassCenter_ = .true.
		end if

		output = this.centerOfMass_
	end function centerOfMass
	
	!>
	!! @brief Returns the inertia tensor onto the axes chosen centered at "center" in atomic units
	!! @todo Hay que unificar los cuatro posibles casos en uno solo para no repetir tanto código
	!!
	type(Matrix) function inertiaTensor( this, center, axes, debug, angles )
		class(Molecule) :: this
		real(8), optional, intent(in) :: center(3)
		type(Matrix), optional, intent(in) :: axes
		logical, optional, intent(in) :: debug
		real(8), optional, intent(out) :: angles(3)
		
		logical :: effDebug
		
		type(Matrix) :: r, c, d, Rot, Im, axisProj
		real(8) :: rThetaPhi(3)
		
		effDebug = .false.
		if( present(debug) ) effDebug = debug
		
		if( .not. this.axesChosen ) then
			call this.orient( moveCM=.false., debug=debug )
		end if
		
		if( ( .not. present(center) ) .and. ( .not. present(axes) ) ) then
			inertiaTensor = this.diagInertiaTensor
			return
		end if
		
		if( present(center) .and. ( .not. present(axes) ) ) then
			rThetaPhi = Math_cart2Spher( this.inertiaAxes_(:,3) )
			Rot = SpecialMatrix_rotation( rThetaPhi(3), rThetaPhi(2), 0.0_8 )
			
			call r.columnVector( 3, values=this.inertiaAxes_(:,1) )
			r = Rot*r
			rThetaPhi = Math_cart2Spher( r.data(:,1) )
			Rot = SpecialMatrix_zRotation( rThetaPhi(3) )*Rot
			
			if( present(angles) ) angles = rThetaPhi
			
			! It rotates the inertia tensor
			Im = Rot.transpose()*this.diagInertiaTensor*Rot
			
			call r.columnVector( 3, values=this.centerOfMass() )
			call c.columnVector( 3, values=center )
			d = r-c
			
			inertiaTensor = Im + ( SpecialMatrix_identity(3,3)*d.norm2()**2 - d*d.transpose() )*this.mass()
			
			if( effDebug ) then
				write(*,*) ""
				write(*,*) ">>>>>>>>>>>>> BEGIN DEBUG: Molecule.inertiaTensor.center"
				write(*,*) ""
				call this.show( formatted=.true. )
				write(*,*) ""
				write(*,*) "Inertia tensor by rotations around arbitrary center = "
				call inertiaTensor.show( formatted=.true. )
				
				write(*,*) ""
				write(*,*) "Exact inertia tensor around arbitrary center = "
				call this.buildInertiaTensor( Im, center=center )
				call Im.show( formatted=.true. )
				write(*,*) ""
				write(*,*) ">>>>>>>>>>>>> END DEBUG: Molecule.inertiaTensor.center"
			end if

			return
		end if
		
		if( ( .not. present(center) ) .and. present(axes) ) then
			call axisProj.columnVector( 3, values=this.inertiaAxes_(:,3) )
			axisProj = axisProj.projectionOntoNewAxes( axes )
			
			rThetaPhi = Math_cart2Spher( axisProj.data(:,1) )
			Rot = SpecialMatrix_rotation( rThetaPhi(3), rThetaPhi(2), 0.0_8 )
			
			if( present(angles) ) then
				angles(1) = rThetaPhi(3) ! alpha
				angles(2) = rThetaPhi(2) ! beta
			end if
			
			call axisProj.columnVector( 3, values=this.inertiaAxes_(:,1) )
			axisProj = axisProj.projectionOntoNewAxes( axes )
			
			call r.columnVector( 3, values=axisProj.data(:,1) )
			r = Rot*r
			rThetaPhi = Math_cart2Spher( r.data(:,1) )
			Rot = SpecialMatrix_zRotation( rThetaPhi(3) )*Rot
			
			if( present(angles) ) then
				angles(3) = rThetaPhi(3) ! gamma
			end if
			
			! It rotates the inertia tensor around its center of mass
			inertiaTensor = Rot.transpose()*this.diagInertiaTensor*Rot
			
			if( effDebug ) then
				write(*,*) ""
				write(*,*) ">>>>>>>>>>>>> BEGIN DEBUG: Molecule.inertiaTensor.axes"
				write(*,*) ""
				call this.show( formatted=.true. )
				write(*,*) ""
				write(*,*) "Inertia tensor by rotating around CM and projection onto chosen axes = "
				call inertiaTensor.show( formatted=.true. )
				
				write(*,*) ""
				write(*,*) "Exact inertia tensor around CM and projection onto chosen axes = "
				call this.buildInertiaTensor( Im, axes=axes )
				call Im.show( formatted=.true. )
				write(*,*) ""
				write(*,*) ">>>>>>>>>>>>> END DEBUG: Molecule.inertiaTensor.axes"
			end if

			return
		end if
		
		if( present(center) .and. present(axes) ) then
			call axisProj.columnVector( 3, values=this.inertiaAxes_(:,3) )
			axisProj = axisProj.projectionOntoNewAxes( axes )
			
			rThetaPhi = Math_cart2Spher( axisProj.data(:,1) )
			Rot = SpecialMatrix_rotation( rThetaPhi(3), rThetaPhi(2), 0.0_8 )
			
			call axisProj.columnVector( 3, values=this.inertiaAxes_(:,1) )
			axisProj = axisProj.projectionOntoNewAxes( axes )
			
			call r.columnVector( 3, values=axisProj.data(:,1) )
			r = Rot*r
			rThetaPhi = Math_cart2Spher( r.data(:,1) )
			Rot = SpecialMatrix_zRotation( rThetaPhi(3) )*Rot
			
			if( present(angles) ) angles = rThetaPhi
			
			! It rotates the inertia tensor around its center of mass
			Im = Rot.transpose()*this.diagInertiaTensor*Rot
			
			call r.columnVector( 3, values=this.centerOfMass() )
			call c.columnVector( 3, values=center )
			d = r-c
			
			inertiaTensor = Im + ( SpecialMatrix_identity(3,3)*d.norm2()**2 - d*d.transpose() )*this.mass()
			
			if( effDebug ) then
				write(*,*) ""
				write(*,*) ">>>>>>>>>>>>> BEGIN DEBUG: Molecule.inertiaTensor.(center,axes)"
				write(*,*) ""
				call this.show( formatted=.true. )
				write(*,*) ""
				write(*,*) "Inertia tensor by rotating around center and projection onto chosen axes = "
				call inertiaTensor.show( formatted=.true. )
				
				write(*,*) ""
				write(*,*) "Exact inertia tensor around center and projection onto chosen axes = "
				call this.buildInertiaTensor( Im, center=center, axes=axes )
				call Im.show( formatted=.true. )
				write(*,*) ""
				write(*,*) ">>>>>>>>>>>>> END DEBUG: Molecule.inertiaTensor.(center,axes)"
			end if
			
			return
		end if
	end function inertiaTensor
	
	!>
	!! @brief Returns the i-th principal axis of inertia
	!!
	function inertiaAxis( this, i ) result( output )
		class(Molecule) :: this
		integer, intent(in) :: i
		real(8) :: output(3)
		
		if( .not. this.axesChosen ) then
			call this.orient( moveCM=.false. )
		end if
		
		output(:) = this.inertiaAxes_(:,i)
	end function inertiaAxis
	
	!>
	!! @brief Returns the principal axes of inertia
	!!
	function inertiaAxes( this ) result( output )
		class(Molecule), intent(in) :: this
		type(Matrix) :: output
		
		if( .not. this.axesChosen ) then
			call this.orient( moveCM=.false. )
		end if
		
		call output.init( this.inertiaAxes_ )
	end function inertiaAxes
	
	!>
	!! @brief
	!!
	subroutine connectivity( this, output, alpha )
		class(Molecule), intent(in) :: this
		integer, allocatable :: output(:,:)
		real(8), optional :: alpha
		
		integer, allocatable :: patter(:,:)
		integer :: i, j, k
		
		allocate( patter(this.nAtoms()*this.nAtoms(),2) )
		
		k=1
		do i=1,this.nAtoms()-1
			do j=i+1,this.nAtoms()
				if( this.atoms(i).isConnectedWith( this.atoms(j), alpha ) ) then
					patter( k, : ) = [ i, j ]
					k = k+1
				end if
			end do
		end do
		k = k-1
		
		if( allocated(output) ) deallocate( output )
		allocate( output(k,2) )
		do i=1,k
			output(i,:) = patter(i,:)
		end do
		
		deallocate( patter )
	end subroutine connectivity
	
	!>
	!! @brief Returns the minimum spin multiplicity (i.e. singlet(1) or triplet(3) )
	!!
	function minSpinMultiplicity( this, charge ) result( output )
		class(Molecule), intent(in) :: this
		integer, optional, intent(in) :: charge
		integer :: output
		
		integer :: i, ssum
		
		ssum = 0
		do i=1,this.nAtoms()
			ssum = ssum + this.atoms(i).atomicNumber()
		end do
		
		if( present(charge) ) then
			ssum = ssum - charge
		end if
		
		if( Math_isEven( ssum ) ) then
			output = 1
		else
			output = 3
		end if
	end function minSpinMultiplicity
	
	!>
	!! @brief builds the inertia moment with current geometry. If CM=.true.
	!!        the moment of inertia tensor will be calculated around of the
	!!        center of mass
	!!
	subroutine buildInertiaTensor( this, Im, CM, center, axes )
		class(Molecule), intent(in) :: this
		type(Matrix), intent(out) :: Im
		logical, optional, intent(in) :: CM
		real(8), optional, intent(in) :: center(3)
		type(Matrix), optional, intent(in) :: axes
		
		integer :: i
		real(8) :: centerOfMass(3)
		real(8), allocatable :: X(:), Y(:), Z(:), m(:)
		type(Matrix) :: r, u
		
		logical :: effCM
		real(8) :: effCenter(3)
		
		effCM = .true.
		if( present(CM) ) effCM = CM
		
		effCenter = 0.0_8
		if( present(center) ) then
			effCM = .false.
			effCenter = center
		end if
		
		allocate( X(this.nAtoms()) )
		allocate( Y(this.nAtoms()) )
		allocate( Z(this.nAtoms()) )
		allocate( m(this.nAtoms()) )
		
		if( present(axes) ) then
			do i=1,this.nAtoms()
				call r.columnVector( 3, values=this.atoms(i).r )
				r = r.projectionOntoNewAxes( axes )
				
				X(i) = r.get(1,1)
				Y(i) = r.get(2,1)
				Z(i) = r.get(3,1)
				m(i) = AtomicElementsDB_instance.atomicMass( this.atoms(i).symbol )
			end do
		else
			do i=1,this.nAtoms()
				X(i) = this.atoms(i).x
				Y(i) = this.atoms(i).y
				Z(i) = this.atoms(i).z
				m(i) = AtomicElementsDB_instance.atomicMass( this.atoms(i).symbol )
			end do
		end if
		
		if( effCM ) then
			centerOfMass = this.centerOfMass()
			X = X - centerOfMass(1)
			Y = Y - centerOfMass(2)
			Z = Z - centerOfMass(3)
		else if( present(center) ) then
			X = X - effCenter(1)
			Y = Y - effCenter(2)
			Z = Z - effCenter(3)
		end if
		
		call Im.init(3,3)
		
		call Im.set( 1, 1,  sum( m*(Y**2+Z**2) ) )
		call Im.set( 1, 2, -sum( m*X*Y ) )
		call Im.set( 1, 3, -sum( m*X*Z ) )
		call Im.set( 2, 1, -sum( m*Y*X ) )
		call Im.set( 2, 2,  sum( m*(X**2+Z**2) ) )
		call Im.set( 2, 3, -sum( m*Y*Z ) )
		call Im.set( 3, 1, -sum( m*Z*X ) )
		call Im.set( 3, 2, -sum( m*Z*Y ) )
		call Im.set( 3, 3,  sum( m*(X**2+Y**2) ) )
		
		deallocate( X )
		deallocate( Y )
		deallocate( Z )
		deallocate( m )
	end subroutine buildInertiaTensor

	!>
	!! @brief Orient molecule
	!!
	!! Orient molecule such that origin is centre of mass, and axes are eigenvectors of inertia tensor
	!! primary axis   Z
	!! primary plane YZ
	!! 
	!!
	subroutine orient( this, moveCM, debug )
		class(Molecule) :: this
		logical, optional :: moveCM
		logical, optional :: debug
		
		logical :: effMoveCM
		logical :: effDebug
		
		real(8) :: centerOfMass(3)
		type(Matrix) :: Im, Vm, Rot, r
		real(8), allocatable :: Imd(:)
		real(8) :: rThetaPhi(3)
		integer :: i
		
		effMoveCM = .true.
		if( present(moveCM) ) effMoveCM = moveCM
		
		effDebug = .false.
		if( present(debug) ) effDebug = debug
		
		if( effDebug ) then
			write(*,*) ""
			write(*,*) ">>>>>>>>>>>>> BEGIN DEBUG: Molecule.orient"
			write(*,*) ""
			write(*,*) "Initial geometry = "
			call this.show( formatted=.true. )
			write(*,*) ""
		end if
		
		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		! 1) It choose the origin as the center of mass
		centerOfMass = this.centerOfMass()
		do i=1,this.nAtoms()
			this.atoms(i).r = this.atoms(i).r - centerOfMass
		end do
		
		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		! 2) It builds the inertia tensor which is diagonalizes
		!    in order to get the principal axes
		call this.buildInertiaTensor( Im, CM=.false. )
		call Im.eigen( eVecs=Vm, eVals=this.diagInertiaTensor )
! 		call Vm.show( formatted=.true. )
		
		rThetaPhi = Math_cart2Spher( Vm.data(:,3) )
		Rot = SpecialMatrix_rotation( rThetaPhi(3), rThetaPhi(2), 0.0_8 )
		
		call r.columnVector( 3, values=Vm.data(:,1) )
		r = Rot*r
		rThetaPhi = Math_cart2Spher( r.data(:,1) )
		Rot = SpecialMatrix_zRotation( rThetaPhi(3) )*Rot
		
		do i=1,this.nAtoms()
			call r.columnVector( 3, values=this.atoms(i).r )
			r = Rot*r
			this.atoms(i).r = r.data(:,1)
		end do
		
		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		! 3) It restore the center of mass if moveCM=false
		if( .not. effMoveCM ) then
			do i=1,this.nAtoms()
				this.atoms(i).r = this.atoms(i).r + centerOfMass
			end do
			
			this.centerOfMass_ = centerOfMass
			this.testMassCenter_ = .true.
			this.testGeomCenter_ = .true.
		else
			this.centerOfMass_ = [ 0.0_8, 0.0_8, 0.0_8 ]
			this.testMassCenter_ = .true.
			this.testGeomCenter_ = .false.
		end if
		
		this.inertiaAxes_(:,1) = [ 1.0_8, 0.0_8, 0.0_8 ]
		this.inertiaAxes_(:,2) = [ 0.0_8, 1.0_8, 0.0_8 ]
		this.inertiaAxes_(:,3) = [ 0.0_8, 0.0_8, 1.0_8 ]
		
		this.axesChosen = .true.
		
		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		! 4) By using the diagonal representation of the inertia
		!    tensor, we can get the number of degrees of freedom
		!    for the rotational and the vibrational motions
		this.isLineal_ = 0
		if( this.nAtoms() == 1 ) then
			this.fv_ = 0
			this.fr_ = 0
! 		else if( abs( this.diagInertiaTensor.get(1,1) ) < 1e-3 ) then ! Este valor detecta a partir 179.988 deg.
		else if( abs( this.diagInertiaTensor.get(1,1) ) < 1e-4 ) then ! Este valor detecta a partir 179.991 deg.
			this.fv_ = 3*this.nAtoms()-5
			this.fr_ = 2
			
			this.isLineal_ = 1
		else
			this.fv_ = 3*this.nAtoms()-6
			this.fr_ = 3
		end if
		
		if( effDebug ) then
			write(*,*) "Final geometry = "
			call this.show( formatted=.true. )
			write(*,*) ""
			write(*,*) ">>>>>>>>>>>>> END DEBUG: Molecule.orient"
			write(*,*) ""
		end if
	end subroutine orient
	
	!>
	!! @brief It rotates the molecule around its center
	!!        of mass. The inertia axes are updated according to.
	!!        Remind that the inertia tensor always is diagonal onto these axes
	!!
	subroutine rotate( this, alpha, beta, gamma, random, debug, force1D, force2D )
		class(Molecule) :: this
		real(8), optional :: alpha, beta, gamma
		logical, optional, intent(in) :: random
		logical, optional, intent(in) :: debug
		logical, optional, intent(in) :: force1D
		logical, optional, intent(in) :: force2D
		
		real(8) :: effAlpha, effBeta, effGamma
		logical :: effRandom
		logical :: effDebug
		logical :: effForce1D
		logical :: effForce2D
		
		real(8) :: centerOfMass(3)
		type(Matrix) :: r, Rot
		integer :: i
		
		type(Matrix) :: Im, Vm, A !<- for debug purposes
		
		effAlpha = 0.0_8
		effBeta = 0.0_8
		effGamma = 0.0_8
		effRandom = .false.
		effDebug = .false.
		effForce1D = .false.
		effForce2D = .false.
		if( present(alpha) )     effAlpha = alpha
		if( present(beta) )       effBeta = beta
		if( present(gamma) )     effGamma = gamma
		if( present(random) )   effRandom = random
		if( present(debug) )     effDebug = debug
		if( present(force1D) ) effForce1D = force1D
		if( present(force2D) ) effForce2D = force2D
		
		if( this.fr_ == 0 ) then
			return
		end if
		
		if( effDebug ) then
			write(*,*) ""
			write(*,*) ">>>>>>>>>>>>> BEGIN DEBUG: Molecule.rotate"
			write(*,*) ""
			write(*,*) "Initial geometry = "
			call this.show( formatted=.true. )
			write(*,*) ""
		end if
		
		if( .not. this.axesChosen ) then
			call this.orient( moveCM=.false., debug=debug )
		end if
		
		centerOfMass = this.centerOfMass()		
		do i=1,this.nAtoms()
			this.atoms(i).r = this.atoms(i).r - centerOfMass
		end do
		
		if( effRandom ) then
			if( effForce1D ) then
				Rot = SpecialMatrix_randomRotation( alpha=effAlpha, beta=effBeta, gamma=effGamma, fr=1 )
			else if( effForce2D ) then
				Rot = SpecialMatrix_randomRotation( alpha=effAlpha, beta=effBeta, gamma=effGamma, fr=2 )
			else
				Rot = SpecialMatrix_randomRotation( alpha=effAlpha, beta=effBeta, gamma=effGamma, fr=this.fr_ )
			end if
		else
			Rot = SpecialMatrix_rotation( effAlpha, effBeta, effGamma )
		end if
		
		! It rotates the atoms one by one around of its center of mass
		do i=1,this.nAtoms()
			call r.columnVector( 3, values=this.atoms(i).r )
			r = Rot*r
			this.atoms(i).r = r.data(:,1)
		end do
		
		! It restores its center
		do i=1,this.nAtoms()
			this.atoms(i).r = this.atoms(i).r + centerOfMass
		end do
		
		! It rotates the inertia axes
		do i=1,3
			call r.columnVector( 3, values=this.inertiaAxes_(:,i) )
			r = Rot*r
			this.inertiaAxes_(:,i) = r.data(:,1)
		end do
		
		if( effRandom ) then
			if( present(alpha) ) alpha = effAlpha
			if( present(beta) )   beta = effBeta
			if( present(gamma) ) gamma = effGamma
		end if
		
		if( effDebug ) then
			write(*,*) "Final geometry = "
			call this.show( formatted=.true. )
			write(*,*) ""
			write(*,*) "Inertia axes by rotation = "
			call A.init( this.inertiaAxes_ )
			call A.show( formatted=.true. )
			
			write(*,*) ""
			write(*,*) "Exact inertia axes by diagonalization of inertia tensor = "
			call this.buildInertiaTensor( Im, CM=.true. )
			call Im.eigen( eVecs=Vm )
			call Vm.show( formatted=.true. )
			
			write(*,*) ""
			write(*,*) ">>>>>>>>>>>>> END DEBUG: Molecule.rotate"
			write(*,*) ""
		end if
	end subroutine rotate
	
	!>
	!! @brief Test method
	!!
	subroutine Molecule_test()
		type(Molecule) :: mol1, mol2
		type(Atom) :: atom, atom1, atom2
		real(8) :: rBufferArr3(3)
		real(8) :: arr(3,3)
		type(Matrix) :: Im, Vm, refAxes
		integer :: i, j
	
! Composition vector operators	
! 	integer :: compA(6) = [ 22, 2, 8, 16, 1, 20 ]
! 	integer :: compB(6) = [ 22, 1, 8, 10, 1, 20 ]
! 	integer :: compC(6) = [ 22, 3, 8, 10, 1, 20 ]
! 	
! 	write(*,*) "compA = [ 22, 2, 8, 16, 1, 20 ]    Ti_2 O_16 H_20"
! 	write(*,*) "compB = [ 22, 1, 8, 10, 1, 20 ]    Ti_1 O_10 H_20"
! 	write(*,*) "compC = [ 22, 3, 8, 10, 1, 20 ]    Ti_3 O_10 H_20"
! 	
! 	write(*,*) "( compA == compA ) ==>", all( compA == compA )
! 	write(*,*) "( compA == compB ) ==>", all( compA == compB )
! 	write(*,*) "( compA <= compB ) ==>", all( compA <= compB )
! 	write(*,*) "( compA >= compB ) ==>", all( compA >= compB )
! 	write(*,*) "( compA <= compC ) ==>", all( compA <= compC )
! 	write(*,*) "( compA >= compC ) ==>", all( compA >= compC )
		
		call mol1.init( 2 )
		call atom.init( " H", 0.0_8, 0.0_8, 0.3561_8 )
		mol1.atoms(1) = atom
		call atom.init( " H", 0.0_8, 0.0_8,-0.3561_8 )
		mol1.atoms(2) = atom
		call mol1.show( formatted=.true. )
		
		call mol1.setCenter( [1.0_8, 1.0_8, 1.0_8] )
		call mol1.show( formatted=.true. )
		
		call mol1.init( 5 )
		
		call atom1.init( " He", 0.15_8, 2.15_8, 1.15_8 )
		call atom1.init( " He", 0.0_8, 0.0_8, 0.0_8 )
		call atom1.show()
		mol1.atoms(1) = atom
		call atom2.init( " He", 1.64_8, 2.35_8, 1.28_8 )
		call atom2.show()
		mol1.atoms(2) = atom
		call atom.init( "  He", 1.75_8, 3.15_8, 0.85_8 )
		mol1.atoms(3) = atom
		call atom.init( "He  ", 1.62_8, 3.45_8, 0.70_8 )
		mol1.atoms(4) = atom
		call atom.init( "  He", 3.42_8, 2.04_8, 0.98_8 )
		mol1.atoms(5) = atom
		
		call mol1.show( formatted=.true. )
		write(*,*) "molecule radius = ", mol1.radius()
! 		call mol1.save( "salida.xyz", format=XYZ )
		
		write(*,*) ""
		write(*,*) "Testing load procedures"
		write(*,*) "======================="
		call mol1.load( "data/formats/RXYZ", format=XYZ )
		call mol1.show( formatted=.true. )
		call mol1.load( "data/formats/MOLDEN", format=MOLDEN )
		call mol1.show( formatted=.true. )
		stop
		
		write(*,*) ""
		write(*,*) "Testing copy constructor"
		write(*,*) "========================"
		mol2 = mol1
		call mol1.show()
		call mol2.show()
		
		write(*,*) ""
		write(*,*) "Testing properties"
		write(*,*) "=================="
		write(*,*) "mol2.massNumber() = ", mol2.massNumber()
		write(*,*) "mol2.chemicalFormula() = ", mol2.chemicalFormula()
		write(*,*) ""
		
		write(*,*) "Testing center of molecule"
		write(*,*) "=========================="
		call mol2.show(formatted=.true.)
		write(*,"(A,3F10.5)") "mol2.center = ", mol2.center()
		call mol2.setCenter( [-2.0_8, 1.0_8, 2.0_8] )
		write(*,"(A,3F10.5)") "mol2.geomCenter = ", mol2.center()
! 		call mol2.setCenter( [ 2.0_8,-1.0_8,-2.0_8] )
		call mol2.setCenter( atom.r )
		write(*,"(A,3F10.5)") "mol2.geomCenter = ", mol2.center()
! 		call mol2.show(formatted=.true.)
		
		call mol1.init( 2 )
		mol1.atoms(1) = atom1
		mol1.atoms(2) = atom2
! 		call mol1.fromFile( "prueba.xyz" )
		call mol1.show( formatted=.true. )
		write(*,"(A,3F10.5)") "mol2.geomCenterA = ", mol1.center()
		call mol1.show( formatted=.true. )
		call mol1.setCenter( [-2.0_8, 1.0_8, 2.0_8] )
		call mol1.show( formatted=.true. )
		write(*,"(A,3F10.5)") "mol2.geomCenter = ", mol1.center()
		
		write(*,*) ""
		write(*,*) "Testing rotation of molecule"
		write(*,*) "============================"
! 		call mol1.init( "data/formats/XYZ" )
! 		call mol1.init( "prueba.xyz" )
! 		call mol1.init( "C9T-cyclic.xyz" )
		call mol1.init( "C2.xyz" )
		call mol1.rotate( alpha=45.0_8*deg, beta=45.0_8*deg, gamma=0.0_8*deg, debug=.true. )
		write(*,*) ""
		write(*,*) "Inertia tensor around its center of mass"
		write(*,*) "----------------------------------------"
		Im = mol1.inertiaTensor( debug=.true. )
		write(*,*) ""
		call Im.show( formatted=.true. )
		write(*,*) ""
		write(*,*) "Inertia tensor around arbitrary center"
		write(*,*) "--------------------------------------"
		write(*,*) "center = [-2.0, 1.0, 2.0]"
		Im = mol1.inertiaTensor( center=[-2.0_8, 1.0_8, 2.0_8], debug=.true. )
		write(*,*) ""
		call Im.show( formatted=.true. )
		write(*,*) ""
		write(*,*) "Inertia tensor around its center of mass and arbitrary axes"
		write(*,*) "-----------------------------------------------------------"
		
		! These axis are equivalent to a rotation with alpha=45º and beta=45º
		call refAxes.init(3,3)
		refAxes.data(1,:) = [  0.5000, 0.5000, -0.7071 ]
		refAxes.data(2,:) = [ -0.7071, 0.7071,  0.0000 ]
		refAxes.data(3,:) = [  0.5000, 0.5000,  0.7071 ]

		write(*,*) ""
		write(*,*) "axes = "
		call refAxes.show( formatted=.true. )

		Im = mol1.inertiaTensor( axes=refAxes, debug=.true. )
		write(*,*) ""
		call Im.show( formatted=.true. )
		
		write(*,*) ""
		write(*,*) "Inertia tensor around arbitrary center and axes"
		write(*,*) "------------------------------------------------"
		write(*,*) "center = [-2.0, 1.0, 2.0]"
		write(*,*) ""
		write(*,*) "axes = "
		call refAxes.show( formatted=.true. )

		Im = mol1.inertiaTensor( center=[-2.0_8, 1.0_8, 2.0_8], axes=refAxes, debug=.true. )
		write(*,*) ""
		call Im.show( formatted=.true. )
		
		write(*,*) "Composition vector"
		write(*,*) "-------------------"
		call mol1.init( "data/formats/XYZ" )
		call mol1.showCompositionVector()
		
		write(*,*) ""
		write(*,*) "Inertia tensor around arbitrary center and axes"
		write(*,*) "--------------------------------------"
		write(*,*) "center = [-2.0, 1.0, 2.0]"
		write(*,*) ""
		write(*,*) "axes   = [-1.0 | 1.0 | 0.0]"
		write(*,*) "         [ 1.0 | 1.0 | 0.0]"
		write(*,*) "         [ 0.0 | 0.0 | 1.0]"
		write(*,*) ""
		call refAxes.init(3,3)
		refAxes.data(:,1) = [-1.0, 1.0, 0.0]
		refAxes.data(:,2) = [ 1.0, 1.0, 0.0]
		refAxes.data(:,3) = [ 0.0, 0.0, 1.0]
		
		Im = mol1.inertiaTensor( center=[-2.0_8, 1.0_8, 2.0_8], axes=refAxes, debug=.true. )
		write(*,*) ""
		call Im.show( formatted=.true. )
		
		write(*,*) "Testing rotation of molecule"
		write(*,*) "============================"
! 		call mol1.init( "data/formats/XYZ" )
! 		call mol1.init( "prueba.xyz" )
! 		call mol1.init( "C9T-cyclic.xyz" )
! 		call mol1.init( "C2.xyz" )
		call mol1.init( "C1.xyz" )
		
! 		call mol1.show( formatted=.true. )
		write(*,*) "Initial inertia tensor = "
		call mol1.buildInertiaTensor( Im, CM=.false. )
		call Im.show( formatted=.true. )
		write(*,*) ""
		write(*,"(A,3F7.2)") "  initial center of mass = ", mol1.centerOfMass()
		write(*,"(A,3F7.2)") "initial geometric center = ", mol1.center()
		write(*,*) ""
		write(*,*) "initial inertia axes = "
		do i=1,3
			write(*,"(5X,A,3F7.2)") "V_"//FString_fromInteger(i)//" = ", mol1.inertiaAxis(i)
		end do
		write(*,*) ""
		write(*,*) ">>>>>>>>>  Orienting molecule ... OK"
		call mol1.orient()
		
		call mol1.buildInertiaTensor( Im, CM=.true. )
		write(*,*) ""
		write(*,*) "new inertia tensor = "
		call Im.show( formatted=.true. )
		write(*,*) ""
		write(*,"(A,3F7.2)") "  new center of mass = ", mol1.centerOfMass()
		write(*,"(A,3F7.2)") "new geometric center = ", mol1.center()
		
		write(*,*) ""
		write(*,*) "new inertia axes = "
		do i=1,3
			write(*,"(5X,A,3F7.2)") "V_"//FString_fromInteger(i)//" = ", mol1.inertiaAxis(i)
		end do
		
		write(*,*) ""
		write(*,*) ">>>>>>>>>  Randomly oriented molecule ... OK"
		call mol1.rotate( random=.true. )
		
		call mol1.buildInertiaTensor( Im, CM=.true. )
		write(*,*) ""
		write(*,*) "new inertia tensor = "
		call Im.show( formatted=.true. )
		write(*,*) ""
		write(*,"(A,3F7.2)") "  new center of mass = ", mol1.centerOfMass()
		write(*,"(A,3F7.2)") "new geometric center = ", mol1.center()
		
		write(*,*) ""
		write(*,*) "new inertia axes = "
		do i=1,3
			write(*,"(5X,A,3F7.2)") "V_"//FString_fromInteger(i)//" = ", mol1.inertiaAxis(i)
		end do
		
		call mol1.save("salida.xyz")

	end subroutine Molecule_test
	
end module Molecule_

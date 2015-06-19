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
module MoldenParser_
	use UnitsConverter_
	use String_
	use IOStream_
	implicit none
	private
	
	public :: &
		MoldenParser_test
	
	type, public :: MoldenParser
		real(8) :: val
		
		contains
			generic :: init => initDefault
			generic :: assignment(=) => copyMoldenParser
			
			procedure :: initDefault
			procedure :: copyMoldenParser
			final :: destroyMoldenParser
			
			procedure :: load
	end type MoldenParser
	
	contains
	
	!>
	!! @brief Constructor
	!!
	subroutine initDefault( this )
		class(MoldenParser) :: this 
		
	end subroutine initDefault
	
	!>
	!! @brief Copy constructor
	!!
	subroutine copyMoldenParser( this, other )
		class(MoldenParser), intent(out) :: this
		class(MoldenParser), intent(in) :: other

		this.val = other.val
	end subroutine copyMoldenParser
	
	!>
	!! @brief Destructor
	!!
	subroutine destroyMoldenParser( this )
		type(MoldenParser) :: this
		
	end subroutine destroyMoldenParser
	
	!>
	!! @brief
	!!
	subroutine load( this, iFileName )
		class(MoldenParser) :: this
		character(*), intent(in) :: iFileName
		
		type(IFStream) :: ifile
		
		logical :: loadNameEff
		
		type(String) :: buffer
		character(1000), allocatable :: tokens(:)
		integer :: i, nAtoms
! 		type(Atom) :: currentAtom
		logical :: advance
		real(8) :: geometryUnits
		
! 		type(StringList) :: geometryBlock
		
		buffer = ifile.readLine()
		
		if( buffer /= "[Molden Format]" ) then
			write(*,*) "### ERROR ### bad format in MOLDEN file"
			stop
		end if
		
		advance = .true.
		do while( .not. ifile.eof() )
			if( advance ) then
				buffer = ifile.readLine()
				call buffer.split( tokens, " " )
				advance = .true.
			end if
			
			!-----------------------------------
			! search for title line
			!-----------------------------------
! 			if( tokens(1) == "[Title]" ) then
! 				write(*,*) "NNN = ", trim(tokens(1))
				
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
					
					write(*,*) "MMM = ", trim(tokens(1))
				end do
			
			!-----------------------------------
			! search for Atoms data
			!-----------------------------------
			else if( tokens(1) == "[FREQ]" ) then
				do while( .not. ifile.eof() )
					buffer = ifile.readLine()
					call buffer.split( tokens, " " )
					
					if( index( tokens(1), "[" ) == 1 ) then
						advance = .false.
						exit
					end if
					
					write(*,*) "KKK = ", trim(tokens(1))
				end do
			end if
			
			if( .not. advance ) then
				buffer = ifile.readLine()
				call buffer.split( tokens, " " )
			end if
		end do

	end subroutine load
	
	!*
	! @brief Test method
	!*
	subroutine MoldenParser_test()
		type(MoldenParser) :: parser
		
! 		call parser.load( "data/formats/MOLDEN" )
! 		call parser.getBlock( "[FREQ]" )
	end subroutine MoldenParser_test
	
end module MoldenParser_

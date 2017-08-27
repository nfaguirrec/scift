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

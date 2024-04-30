!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                                                                                   !!
!!  This file is part of SciFT project                                               !!
!!  Copyright (c) 2012-2013 Nestor F. Aguirre (nfaguirrec@gmail.com)                 !!
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
module BlocksIFileParser_
	use String_
	implicit none
	private
	
	public :: &
		BlocksIFileParser_test
	
	type, public :: BlocksIFileParser
		character(:), private, allocatable :: iFileName
		logical, private :: notifyGetMethods
		
		integer, private :: numberOfLines
		character(1000), allocatable, private :: lines(:)
		
		integer :: numberOfVariables
		type(String), allocatable :: varName(:)
		type(String), allocatable :: varValue(:)
		type(String), allocatable :: varUnit(:)
		
		!------------------------------------------------------------
		! @todo
		! date: 20141007
		! La idea es que desde el constructor, inmediatamente
		! se debe cargar el bloque de GLOBAL_VARIABLES, así que
		! varName, varValue, varUnit y numberOfVariables serán
		! variables privadas. Se creará un método llamado
		! getRealMathEx, que leerá del fichero input
		! el valor de la variable como un string y se lo pasará
		! al MathParser, evaluandolo con las variables deefinidas
		! en el bloque GLOBAL_VARIABLES. Esto ya está implementado
		! en el proyecto WPP, pero hay que migrarglo a scift.
		! También habrá que implementar getRealArrayMathEx
		!------------------------------------------------------------
		
		contains
			generic :: init => initDefault
			generic :: assignment(=) => copy
			
			procedure :: initDefault
			procedure :: copy
			final :: destroy
			procedure :: str
			procedure :: show
			procedure :: showContent
			procedure, private :: load
			procedure :: isEmpty
			procedure :: inputFileName
			procedure :: isThereBlock
			procedure :: get => getStringBase
			procedure, private :: getStringBase
			procedure :: getString
			procedure :: getLogical
			procedure :: getInteger
			procedure :: getReal
			procedure :: getRealArray
			procedure :: getBlock
			procedure :: loadVariablesBlock
	end type BlocksIFileParser
	
	contains
	
	!>
	!! @brief Constructor
	!!
	subroutine initDefault( this, iFileName, cComments, notifyGetMethods )
		class(BlocksIFileParser) :: this 
		character(*), intent(in) :: iFileName
		character(*), optional, intent(in) :: cComments
		logical, optional, intent(in) :: notifyGetMethods
		
		character(10) :: effcComments
		logical :: effNotifyGetMethods
		
		effcComments = "#"
		if( present(cComments) ) effcComments = cComments
		
		effNotifyGetMethods = .false.
		if( present(notifyGetMethods) ) effNotifyGetMethods = notifyGetMethods
		
		this.iFileName = iFileName
		this.notifyGetMethods = effNotifyGetMethods
		
		this.numberOfLines = 0
		if( allocated(this.lines) ) deallocate(this.lines)
		call this.load( effcComments )
		
		this.numberOfVariables = 0
		if( allocated(this.varName) ) deallocate(this.varName)
		if( allocated(this.varValue) ) deallocate(this.varValue)
		if( allocated(this.varUnit) ) deallocate(this.varUnit)
	end subroutine initDefault
	
	!>
	!! @brief Copy constructor
	!!
	subroutine copy( this, other )
		class(BlocksIFileParser), intent(inout) :: this
		type(BlocksIFileParser), intent(in) :: other
		
		write(*,*) "### Error ### BlocksIFileParser.copy is not implemented"
		stop
	end subroutine copy
	
	!>
	!! @brief Destructor
	!!
	subroutine destroy( this )
		type(BlocksIFileParser) :: this
		
		if( allocated(this.lines) ) deallocate(this.lines)
		if( allocated(this.varName) ) deallocate(this.varName)
		if( allocated(this.varValue) ) deallocate(this.varValue)
		if( allocated(this.varUnit) ) deallocate(this.varUnit)
	end subroutine destroy
	
	!>
	!! @brief Convert to string
	!!
	function str( this ) result( output )
		class(BlocksIFileParser) :: this 
		character(len=200) :: output
		
		integer :: fmt
		character(len=200) :: strBuffer
		
		output = ""
		
		output = trim(output)//"<BlocksIFileParser:"
		
!! 		output = trim(output)//"min="
!! 		fmt = int(log10(this.min+1.0))+1
!! 		write(strBuffer, "(f<fmt+7>.6)") this.min
!! 		output = trim(output)//trim(strBuffer)
!! 		
!! 		output = trim(output)//",size="
!! 		fmt = int(log10(float(this.size+1)))+1
!! 		write(strBuffer, "(i<fmt>)") this.size
!! 		output = trim(output)//trim(strBuffer)
		
		output = trim(output)//">"
	end function str
	
	!>
	!! @brief Show 
	!!
	subroutine show( this, unit )
		class(BlocksIFileParser) :: this
		integer, optional, intent(in) :: unit
		
		integer :: effunit
		
		effunit = 6
		if( present(unit) ) effunit = unit
		
		write(effunit,"(a)") trim(str(this))
	end subroutine show
	
	!>
	!! @brief Load the input file
	!!
	subroutine showContent( this, unit )
		class(BlocksIFileParser) :: this
		integer, optional, intent(in) :: unit
		
		integer :: effunit
		
		integer :: i
		
		effunit = 6
		if( present(unit) ) effunit = unit
		
		write(effunit,"(A)") "Blocks input file"
		write(effunit,"(A)") "-----------------"
		
		do i=1,this.numberOfLines
			write(effunit,"(A)") "> "//trim(this.lines(i))
		end do
		
		write(effunit,*) ""
	end subroutine showContent
	
	!>
	!! @brief Load the input file
	!!
	subroutine load( this, cComments )
		class(BlocksIFileParser) :: this
		character(*), optional, intent(in) :: cComments
		
		integer :: iostat
		integer :: posComment
		integer :: i
		character(1000) :: buffer
		character(:), allocatable :: line
		character(1000), allocatable :: fileContent(:)
		
		open( 10, file=this.iFileName, status="old", iostat=iostat )
		
		! This block get the number of lines, including comments
		if( iostat /= 0 ) then
			write(*, *) "### Error ###: The file ( ", this.iFileName, " ) cannot be open"
			stop
		else
			this.numberOfLines = 1
			iostat = 1
			do while( .true. )
				read(10,'(A)', iostat=iostat) buffer
				
				if( iostat == -1 ) exit
				this.numberOfLines = this.numberOfLines + 1
			end do
		end if
		this.numberOfLines = this.numberOfLines - 1
		
		close(10)
		
		allocate( fileContent(this.numberOfLines) )
		
		! This block get the effective number of lines (without comments), and loads the information
		open( 10, file=this.iFileName, status="old", iostat=iostat )
		
		if( iostat /= 0 ) then
			write(*, *) "### Error ###: The file ( ", this.iFileName, " ) cannot be open"
			stop
		else
			this.numberOfLines = 1
			iostat = 1
			do while( .true. )
				read(10,'(A)', iostat=iostat) buffer
				
				if( iostat == -1 ) exit
				
				buffer = FString_removeTabs( buffer )
				
				if( present(cComments) ) then
					posComment = index( buffer, trim(cComments) )
					
					if( posComment /= 0 ) then
						line = trim((buffer(1:posComment-1)))
					else
						line = trim(buffer)
					end if
				else
					line = trim(buffer)
				end if
				
				if( len(adjustl(trim(line))) > 0 ) then
					if( adjustl(trim(line)) == "STOP" ) exit
					
					fileContent( this.numberOfLines ) = FString_removeTabs( line )
! 					fileContent( this.numberOfLines ) = line
					this.numberOfLines = this.numberOfLines + 1
				end if
				
			end do
		end if
		this.numberOfLines = this.numberOfLines - 1
		
		close( 10 )
		
		allocate( this.lines( this.numberOfLines ) )
		
		do i=1,this.numberOfLines
			this.lines(i) = trim( fileContent(i) )
		end do
		
		deallocate( fileContent )
	end subroutine load
	
	!>
	!! @brief
	!!
	function isEmpty( this ) result( output )
		class(BlocksIFileParser), intent(in) :: this
		logical :: output
		
		output = .false.
		if( this.numberOfLines == 0 ) then
			output = .true.
		end if
	end function isEmpty
	
	!>
	!! @brief
	!!
	function inputFileName( this ) result( output )
		class(BlocksIFileParser), intent(in) :: this
		type(String) :: output
		
		output = this.iFileName
	end function inputFileName
	
	!>
	!! @brief
	!!
	function getStringBase( this, item, def ) result( output )
		class(BlocksIFileParser), intent(in) :: this 
		character(*), intent(in) :: item
		character(*), optional, intent(in) :: def
		type(String) :: output
		
		character(len=100) :: effdef
		
		integer :: i
		real(8) :: rbuffer
		integer :: ssize
		integer :: iostat
		logical :: foundBegin
		
		character(1000), allocatable :: targetItem(:)
		character(1000), allocatable :: fstrBufferList(:)
		
		effdef = "$$EMPTY**$$..@"
		if( present(def) ) effdef = def
		
		call FString_split( item, targetItem, ":" )
		
		if( .not. this.isThereBlock( targetItem(1) ) .and. .not. present(def) ) then
			write(*,*) "### ERROR ### BlocksIFileParser: Block "//trim(targetItem(1))//" is required"
			stop
		end if
		
		foundBegin = .false.
		i=1
		do while( i <= this.numberOfLines )
			call FString_split( this.lines(i), fstrBufferList, " " )
			
			if( trim(adjustl(fstrBufferList(1))) == "BEGIN" .and. &
				trim(adjustl(fstrBufferList(2))) == trim(adjustl(targetItem(1))) ) then
				
				i=i+1
				
				foundBegin = .true.
				
				! @todo Hay que convertirlo al estilo del BEGIN, utilizando fstrBufferList
				do while( index( this.lines(i), "END" ) == 0 )
					
					call FString_split( this.lines(i), fstrBufferList, "=" )
					
					if( trim(adjustl(fstrBufferList(1))) == trim(adjustl(targetItem(2))) ) then
						output = trim(adjustl(fstrBufferList(2)))
						
						deallocate( targetItem )
						deallocate( fstrBufferList )
						
						return
					end if
					
					i=i+1
				end do
				
				if( effdef /= "$$EMPTY**$$..@" ) then
					output = trim(adjustl(effdef))
				else
					write(*,*) "### ERROR ### BlocksIFileParser: Parameter "//trim(item)//" is required"
					stop
				end if
				
				return
			else
				i = i+1
			end if
		end do
		
		if( .not. foundBegin .and. effdef /= "$$EMPTY**$$..@" ) then
			output = trim(adjustl(effdef))
		else
			write(*,*) "### ERROR ### BlocksIFileParser: Parameter "//trim(item)//" is required"
			stop
		end if

! 		if( .not. foundBegin ) then
! 			write(*,*) "### ERROR ### BlocksIFileParser: Block "//trim(targetItem(1))//" not found"
! 			stop
! 		end if
		
		if( allocated(targetItem) ) deallocate( targetItem )
		if( allocated(fstrBufferList) ) deallocate( fstrBufferList )
	end function getStringBase
	
	!>
	!! @brief
	!! @todo Si por casualidad el END de final de bloque no existe, el programa se puede caer. Hay que arreglarlo
	!!
	function isThereBlock( this, item ) result( output )
		class(BlocksIFileParser), intent(in) :: this 
		character(*), intent(in) :: item
		logical :: output
		
		character(1000), allocatable :: fstrBufferList(:)
		integer :: i, j
		
		i=1 ! Contador sobre las lineas del fichero
		j=1 ! Contador sobre las lineas del bloque
		do while( i <= this.numberOfLines )
		
			call FString_split( this.lines(i), fstrBufferList, " " )
			
			if( trim(adjustl(fstrBufferList(1))) == "BEGIN" .and. &
				trim(adjustl(fstrBufferList(2))) == trim(adjustl(item)) ) then

				i=i+1
				
				do while( index( this.lines(i), "END" ) == 0 )
					
					j=j+1
					i=i+1
					
					if( i > this.numberOfLines ) then
						write(*,*) "### ERROR ### BlocksIFileParser.isThereABlock: Unbalanced BEGIN-END statement for block "//trim(item)
						stop
					end if
				end do
				
				output = .true.
				return
			else
				i = i+1
			end if
		end do
		
		output = .false.
		
		if( allocated(fstrBufferList) ) deallocate( fstrBufferList )
	end function isThereBlock
	
	!>
	!! @brief
	!!
	subroutine getBlock( this, item, output, ignoreVars )
		class(BlocksIFileParser), intent(in) :: this 
		character(*), intent(in) :: item
		type(String), allocatable :: output(:)
		logical, optional :: ignoreVars
		
		logical :: effIgnoreVars
		
		integer :: i, j
		type(String), allocatable :: strBufferList(:)
		character(1000), allocatable :: fstrBufferList(:)
		
		effIgnoreVars = .true.
		if( present(ignoreVars) ) effIgnoreVars = ignoreVars
		
		if( allocated(output) ) then
			deallocate(output)
		end if
		
		allocate( strBufferList(this.numberOfLines) ) ! En el peor de los casos todas las lineas son el bloque
		
		i=1 ! Contador sobre las lineas del fichero
		j=1 ! Contador sobre las lineas del bloque
		do while( i <= this.numberOfLines )
		
			call FString_split( this.lines(i), fstrBufferList, " " )
			
			if( trim(adjustl(fstrBufferList(1))) == "BEGIN" .and. &
				trim(adjustl(fstrBufferList(2))) == trim(adjustl(item)) ) then
				
				i=i+1
				
				do while( index( this.lines(i), "END" ) == 0 )
					if( index( this.lines(i), " = " ) == 0 .or. .not. effIgnoreVars ) then
						strBufferList(j) = adjustl(trim(this.lines(i)))
						j=j+1
					end if
					
					i=i+1
					
					if( i > this.numberOfLines ) then
						write(*,*) "### ERROR ### BlocksIFileParser.isThereABlock: Unbalanced BEGIN-END statement for block "//trim(item)
						stop
					end if
				end do
				
				allocate( output(j-1) )
				output(:) = strBufferList(1:j-1)
				
! 				write(*,*) "### ERROR ### BlocksIFileParser: Block "//item//" is required"
! 				stop
				
				return
			else
				i = i+1
			end if
		end do
		
		if( allocated(fstrBufferList) ) deallocate( fstrBufferList )
		if( allocated(strBufferList) ) deallocate( strBufferList )
	end subroutine getBlock
	
	!>
	!! @brief
	!!
	function getString( this, item, def ) result( output )
		class(BlocksIFileParser), intent(in) :: this 
		character(*), intent(in) :: item
		character(*), optional, intent(in) :: def
		type(String) :: output
		
		type(String) :: tmp
		
		if( present(def) ) then
			output = this.getStringBase( item, trim(def) )
			
			if( this.notifyGetMethods ) then
				write(*,"(A40,A20,A,A20,A)") item//" = ", trim(output.fstr), "        ( ", def, " )"
			end if
		else
			output = this.getStringBase( item )
			
			if( this.notifyGetMethods ) then
				write(*,"(A40,A20)") item//" = ", trim(output.fstr)
			end if
		end if
	end function getString
	
	!>
	!! @brief
	!!
	function getLogical( this, item, def ) result( output )
		class(BlocksIFileParser), intent(in) :: this
		character(*), intent(in) :: item
		logical, optional, intent(in) :: def
		logical :: output
		
		type(String) :: tmp
		character(100) :: buffer
		
		if( present(def) ) then
			write( buffer, * ) def
			output = String_toLogical( this.getStringBase( item, trim(buffer) ) )
			
			if( this.notifyGetMethods ) then
				write(*,"(A40,L20,A,L20,A)") item//" = ", output, "        ( ", def, " )"
			end if
		else
			output = String_toLogical( this.getStringBase( item ) )
			
			if( this.notifyGetMethods ) then
				write(*,"(A40,L20)") item//" = ", output
			end if
		end if
		
	end function getLogical
	
	!>
	!! @brief
	!!
	function getInteger( this, item, def ) result( output )
		class(BlocksIFileParser), intent(in) :: this
		character(*), intent(in) :: item
		integer, optional, intent(in) :: def
		integer :: output
		
		type(String) :: tmp
		character(100) :: buffer
		
		if( present(def) ) then
			write( buffer, * ) def
			output = String_toInteger( this.getStringBase( item, trim(buffer) ) )
			
			if( this.notifyGetMethods ) then
				write(*,"(A40,I20,A,I20,A)") item//" = ", output, "        ( ", def, " )"
			end if
		else
			output = String_toInteger( this.getStringBase( item ) )
			
			if( this.notifyGetMethods ) then
				write(*,"(A40,I20)") item//" = ", output
			end if
		end if
		
	end function getInteger
	
	!>
	!! @brief
	!!
	function getReal( this, item, def ) result( output )
		class(BlocksIFileParser), intent(in) :: this 
		character(*), intent(in) :: item
		real(8), optional, intent(in) :: def
		real(8) :: output
		
		character(100) :: buffer
		
		if( present(def) ) then
			write( buffer, * ) def
			output = String_toReal( this.getStringBase( item, trim(buffer) ) )
			
			if( this.notifyGetMethods ) then
				write(*,"(A40,F20.8,A,F20.8,A)") item//" = ", output, "        ( ", def, " )"
			end if
		else
			output = String_toReal( this.getStringBase( item ) )
			
			if( this.notifyGetMethods ) then
				write(*,"(A40,F20.8,A,F20.8,A)") item//" = ", output
			end if
		end if
		
	end function getReal
	
	!>
	!! @brief
	!!
	function getRealArray( this, item, def ) result( output )
		class(BlocksIFileParser), intent(in) :: this 
		character(*), intent(in) :: item
		real(8), optional, intent(in) :: def(:)
		real(8), allocatable :: output(:)
		
		character(100) :: buffer
		
		allocate( output(size(def)) )
		output = 0.0_8
		
		write(*,*) "### ERROR ### BlocksIFileParser.getRealArray() is not implemented yet"
		stop
! 		if( present(def) ) then
! 			write( buffer, * ) def
! 			output = String_toRealArray( this.getStringBase( item, "("//trim(buffer) ) )
! 		else
! 			output = String_toRealArray( this.getStringBase( item ) )
! 		end if
		
! 		write(*,*) "  ", item, " = ", output
	end function getRealArray
	
	!>
	!! @brief Constructor
	!!
	subroutine loadVariablesBlock( this, item )
		class(BlocksIFileParser) :: this 
		character(*), intent(in) :: item
		
		type(String), allocatable :: tableBuffer(:)
		type(String) :: sBuffer
		character(1000), allocatable :: tokens(:)
		integer :: i
		
		call this.getBlock( item, tableBuffer, ignoreVars=.false. )
		
		if( allocated(this.varName) ) deallocate(this.varName)
		allocate( this.varName( size(tableBuffer) ) )
		
		if( allocated(this.varValue) ) deallocate(this.varValue)
		allocate( this.varValue( size(tableBuffer) ) )
		
		if( allocated(this.varUnit) ) deallocate(this.varUnit)
		allocate( this.varUnit( size(tableBuffer) ) )

		do i=1,size(tableBuffer)
			call tableBuffer(i).split( tokens, "=" )
			
			this.varName(i) = trim(tokens(1))
			
			sBuffer = tokens(2)
			call sBuffer.split( tokens, " " )
			
! 			this.varValue(i) = FString_toReal( tokens(1) )
			this.varValue(i) = trim(tokens(1))
			
			if( size(tokens) > 1 ) then
				this.varUnit(i) = trim(tokens(2))
			else
				this.varUnit(i) = ""
			end if
		end do
		
		if( allocated(tableBuffer) ) deallocate(tableBuffer)
		
		this.numberOfVariables = size(tableBuffer)
	end subroutine loadVariablesBlock
	
	!>
	!! @test Testing the BlocksIFileParser class
	!! @brief Testing the BlocksIFileParser class
	!!
	subroutine BlocksIFileParser_test()
		type(BlocksIFileParser) :: parser
		type(String) :: buffer
		type(String), allocatable :: block(:)
		integer :: i
		real(8) :: rarray(3)
		real(8) :: iarray(4)
		
		call parser.init( "data/formats/BLOCKIFILE", notifyGetMethods=.true. )
		call parser.showContent()
		
		buffer = parser.get( "ICONS:betin", def="0.0" )
		write(*,*) "betin = ", buffer.toReal()
		write(*,*) "betin = ", buffer.toInteger()
		
! 		help = "This is a test to"//ENDL// &
! 		       "include in the feature "//ENDL
! 		buffer = parser.getString( "ICONS:hola", def="kk", help=help.fstr )
		
		write(*,*) "rsys = ", String_toReal( parser.get( "ICONS:rsys", def="0.0" ) )
		
! 		rarray = parser.getRealArray( "ICONS:rarray", def=[1.0_8,1.0_8,1.0_8] )
! 		write(*,*) "rarray = ", rarray
		
		write(*,*) "rsys = ", String_toReal( parser.get( "STEP2ROUTINES:rsys", def="0.0" ) )
		buffer = parser.get( "STEP2ROUTINES:file", def="xxx" )
		write(*,*) "file = ", buffer.fstr
		
		
! 		buffer = parser.get( "STEP2ROUTINES:rsys" )
! 		write(*,*) "rsys = ", buffer.fstr
		
		call parser.getBlock( "TABLE", block )
		write(*,*) "Block TABLE"
		do i=1,size(block)
			write(*,*) i, trim(block(i).fstr)
		end do
		deallocate( block )
	end subroutine BlocksIFileParser_test
	
end module BlocksIFileParser_

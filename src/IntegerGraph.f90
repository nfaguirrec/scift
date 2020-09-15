!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                                                                                   !!
!!  This file is part of SciFT project                                               !!
!!  Copyright (c) 2016-2016 Nestor F. Aguirre (nfaguirrec@gmail.com)                 !!
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

module IntegerGraph_
	use Math_
	use String_
	use IOStream_
	use RealVector_
	use IntegerVector_
	use IntegerHyperVector_
	use Node_
	use Edge_
	use NodeVector_
	use EdgeVector_
	use Matrix_
	use StringIntegerMap_
	
	use StringIntegerPair_
	
	implicit none
	private
	
        !>
        !! @brief Public parameters
        !!
        integer, public, parameter :: GML  = 1
        integer, public, parameter :: DOT  = 2
        integer, public, parameter :: DAT  = 3
	
	public :: &
		IntegerGraph_test
		
	type, public :: IntegerGraph
		character(:), allocatable :: name
		
		type(IntegerHyperVector), private :: node2Neighbors
		type(IntegerHyperVector), private :: node2InEdges
		type(IntegerHyperVector), private :: node2OutEdges
		
		type(NodeVector), private :: nodeProperties
		type(EdgeVector), private :: edgeProperties
		
		logical, private :: directed
		
		! These variables are related with the Dijkstra algorithm
		integer, private :: sNode = -1
		real(8), private, allocatable :: minDistance(:)
		type(IntegerVector), private :: previous
		
		contains
		
		generic :: init => initIntegerGraph
		generic :: assignment(=) => copyIntegerGraph
		generic :: operator(==) => equalIntegerGraph
		
		procedure :: initIntegerGraph
		procedure :: initFromDATLine
		procedure :: copyIntegerGraph
		procedure :: equalIntegerGraph
		final :: destroyIntegerGraph
		procedure :: clear
		
		procedure :: show
		procedure :: str
		procedure :: toDATString
		procedure :: nNodes
		procedure :: nEdges
		procedure :: isDirected
		procedure :: isConnected
		procedure :: nComponents
		procedure :: newNode
		procedure :: newNodes
		procedure :: deleteNode
		procedure :: deleteNodes
		procedure, private :: newEdgeBase
		procedure :: newEdge
		procedure :: newEdges
		procedure :: deleteEdge
		procedure :: deleteEdges
		procedure :: getEdgeId
		procedure :: setNodeProperties
		procedure :: getNodeProperties
		procedure :: setEdgeProperties
		procedure :: getEdgeProperties
		procedure :: save
		procedure, private :: saveGML
		procedure, private :: saveDOT
		procedure, private :: saveDAT
		procedure :: computeDijkstraPaths
		procedure :: distance
		procedure :: shortestPath
		procedure :: adjacencyMatrix
		procedure :: distanceMatrix
		procedure :: laplacianMatrix
		procedure :: resistanceDistanceMatrix
		procedure :: diameter
		procedure :: randicIndex
		procedure :: wienerIndex
		procedure :: inverseWienerIndex
		procedure :: balabanIndex
		procedure :: molecularTopologicalIndex
		procedure :: kirchhoffIndex
		procedure :: kirchhoffSumIndex
		procedure :: wienerSumIndex
		procedure :: JOmegaIndex
		procedure :: indicesBasedComparison
		procedure :: inducedSubgraph
		procedure :: isExclusive
		procedure :: neighbors
		procedure, private :: makeComplete
		
	end type IntegerGraph
	
	contains
	
	!>
	!! @brief Constructor
	!!
	subroutine initIntegerGraph( this, directed, complete, name )
		class(IntegerGraph) :: this
		logical, optional :: directed
		integer, optional :: complete
		character(*), optional, intent(in) :: name
		
		if( allocated(this%name) ) deallocate(this%name)
		this%name = "unknown"
                if( present(name) ) this%name = name
		
		this%directed = .false.
		if( present(directed) ) this%directed = directed
		
		call this%clear()
		
		call this%node2Neighbors%init()
		call this%node2InEdges%init()
		call this%node2OutEdges%init()
		
		call this%nodeProperties%init()
		call this%edgeProperties%init()
		
		if( present(complete) ) call this%makeComplete( n=complete )
	end subroutine initIntegerGraph
	
	!>
	!! @brief Constructor
	!!
	subroutine initFromDATLine( this, DATLine, nodePropertiesUnits, edgePropertiesUnits )
		class(IntegerGraph) :: this
		character(*), intent(in) :: DATLine
		real(8), optional, intent(in) :: nodePropertiesUnits
		real(8), optional, intent(in) :: edgePropertiesUnits
		
		real(8) :: effNodePropertiesUnits
		real(8) :: effEdgePropertiesUnits
		
		integer :: i, j
		character(1000), allocatable :: tokens(:)
		character(1000), allocatable :: tokens2(:)
		character(1000), allocatable :: tokens3(:)
		character(1000), allocatable :: tokens4(:)
		character(1000) :: label
		real(8) :: weight
		integer :: s, t
		
		effNodePropertiesUnits = 1.0_8
		if( present(nodePropertiesUnits) ) effNodePropertiesUnits = nodePropertiesUnits
		
		effEdgePropertiesUnits = 1.0_8
		if( present(edgePropertiesUnits) ) effEdgePropertiesUnits = edgePropertiesUnits
		
		call this%clear()
		
		call FString_split( DATLine, tokens, ";" )
		
		do i=1,size(tokens)
			call FString_split( trim(tokens(i)), tokens2, "[]" )
			
			if( FString_count( trim(tokens2(1)), "--", wholeWords=.true. ) == 0 ) then
! 				write(*,*) "NODE-->", trim(tokens(i))
				
				call FString_split( trim(tokens2(2)), tokens3, "," )
				
				do j=1,size(tokens3)
					call FString_split( trim(tokens3(j)), tokens4, "=" )
					
					select case( trim(tokens4(1)) )
						case( "label" )
! 							write(*,*) "   label-->", trim(tokens4(2))
							label = trim(tokens4(2))
						case( "weight" )
! 							write(*,*) "   weight-->", trim(tokens4(2))
							weight = FString_toReal(trim(tokens4(2)))
					end select
				end do
				
				call this%newNode( label, weight*effNodePropertiesUnits )
			else
! 				write(*,*) "EDGE-->", trim(tokens(i))
				
				call FString_split( trim(tokens2(1)), tokens3, "--" )
! 				write(*,*) "   source-->", trim(tokens3(1))
! 				write(*,*) "   target-->", trim(tokens3(2))
				
				s = FString_toInteger( trim(tokens3(1)) )
				t = FString_toInteger( trim(tokens3(2)) )
				
				call FString_split( trim(tokens2(2)), tokens3, "," )
				
				do j=1,size(tokens3)
					call FString_split( trim(tokens3(j)), tokens4, "=" )
					
					select case( trim(tokens4(1)) )
						case( "label" )
! 							write(*,*) "   label-->", trim(tokens4(2))
							label = trim(tokens4(2))
						case( "weight" )
! 							write(*,*) "   weight-->", trim(tokens4(2))
							weight = FString_toReal(trim(tokens4(2)))
					end select
				end do
				
				call this%newEdge( s, t, label, weight*effEdgePropertiesUnits )
			end if
		end do
		
		if( allocated(tokens) ) deallocate(tokens)
		if( allocated(tokens2) ) deallocate(tokens2)
		if( allocated(tokens3) ) deallocate(tokens3)
		if( allocated(tokens4) ) deallocate(tokens4)
	end subroutine initFromDATLine
	
	!>
	!! @brief Copy constructor
	!!
	subroutine copyIntegerGraph( this, other )
		class(IntegerGraph), intent(inout) :: this
		class(IntegerGraph), intent(in) :: other
		
		if( allocated(this%name) ) deallocate(this%name)
		this%name = other%name
		
		this%node2Neighbors = other%node2Neighbors
		this%node2InEdges = other%node2InEdges
		this%node2OutEdges = other%node2OutEdges
		
		this%nodeProperties = other%nodeProperties
		this%edgeProperties = other%edgeProperties
		
		this%directed = other%directed
		
		this%sNode = other%sNode
		
		if( allocated(this%minDistance) ) deallocate( this%minDistance )
		allocate( this%minDistance( size(other%minDistance) ) )
		this%minDistance = other%minDistance
		
		this%previous = other%previous
	end subroutine copyIntegerGraph
	
	!>
	!! @brief 
	!!
	function equalIntegerGraph( this, other ) result( output )
		class(IntegerGraph), intent(in) :: this
		class(IntegerGraph), intent(in) :: other
		logical :: output
		
		write(*,*) "### ERROR ### IntegerGraph.equalIntegerGraph() is not implemented"
		stop
		
		output = .false.
	end function equalIntegerGraph
	
	!>
	!! @brief Destructor
	!!
	subroutine destroyIntegerGraph( this )
		type(IntegerGraph), intent(inout) :: this
		
		call this%clear()
	end subroutine destroyIntegerGraph
	
	!>
	!! @brief
	!!
	subroutine clear( this )
		class(IntegerGraph), intent(inout) :: this
		
		call this%node2Neighbors%clear()
		call this%node2InEdges%clear()
		call this%node2OutEdges%clear()
		
		call this%nodeProperties%clear()
		call this%edgeProperties%clear()
		
		this%sNode = -1
		if( allocated(this%minDistance) ) deallocate(this%minDistance)
		call this%previous%clear()
	end subroutine clear
	
	!>
	!! @brief Show 
	!!
	subroutine show( this, unit, formatted, nodesLabels, nodePropertiesUnits, edgePropertiesUnits )
		class(IntegerGraph) :: this
		integer, optional, intent(in) :: unit
		logical, optional :: formatted
		character(*), optional, intent(in) :: nodesLabels(:) 
		real(8), optional, intent(in) :: nodePropertiesUnits
		real(8), optional, intent(in) :: edgePropertiesUnits
		
		integer :: effunit
		
		effunit = 6
		if( present(unit) ) effunit = unit
		
		write(effunit,"(a)") trim(this%str( formatted=formatted, nodesLabels=nodesLabels, nodePropertiesUnits=nodePropertiesUnits, edgePropertiesUnits=edgePropertiesUnits ))
	end subroutine show
	
	!>
	!! @brief Convert to string
	!!
	function str( this, formatted, nodesLabels, nodePropertiesUnits, edgePropertiesUnits ) result( output )
		class(IntegerGraph), target :: this 
		character(:), allocatable :: output
		logical, optional :: formatted
		character(*), optional, intent(in) :: nodesLabels(:)
		real(8), optional, intent(in) :: nodePropertiesUnits
		real(8), optional, intent(in) :: edgePropertiesUnits
		
		logical :: effFormatted
		real(8) :: effNodePropertiesUnits
		real(8) :: effEdgePropertiesUnits
		
		integer :: i, j, loc
		type(IntegerVector), pointer :: ivec
		type(Node), pointer :: node
		type(Edge), pointer :: edge
		character(100) :: edgeLabel
		
		effFormatted = .false.
		if( present(formatted) ) effFormatted = formatted
		
		effNodePropertiesUnits = 1.0_8
		if( present(nodePropertiesUnits) ) effNodePropertiesUnits = nodePropertiesUnits
		
		effEdgePropertiesUnits = 1.0_8
		if( present(edgePropertiesUnits) ) effEdgePropertiesUnits = edgePropertiesUnits
		
		output = ""
		
		if( .not. effFormatted ) then
			output = trim(output)//"<IntegerGraph:"
! 			output = trim(output)//"nodes="//trim(FString_fromInteger(this%nNodes()))//","
			
			output = trim(output)//"nodes={"
			do i=1,this%nNodes()
				node => this%nodeProperties%data(i)
				
				if( i /= this%nNodes() ) then
					output = trim(output)//trim(node%label)//","
				else
					output = trim(output)//trim(node%label)
				end if
			end do
			output = trim(output)//"},"
			
			output = trim(output)//"edges={"
			do i=1,this%nEdges()
				
				loc = 0
				if( .not. this%isDirected() .and. i > 1 ) then
					do j=i-1,1,-1
						if( this%edgeProperties%data(i)%sNode == this%edgeProperties%data(j)%tNode .and. &
							this%edgeProperties%data(i)%tNode == this%edgeProperties%data(j)%sNode ) then
							loc = 1
							exit
						end if
					end do
				end if
				
				if( loc == 1 ) cycle
				
				if( i /= 1 ) output = trim(output)//", "
			
				edge => this%edgeProperties%data(i)
				
				edgeLabel = edge%label
				if( present(nodesLabels) ) then
					if( FString_hashKey(trim( nodesLabels(edge%sNode)) ) > FString_hashKey(trim( nodesLabels(edge%tNode)) ) ) then
						edgeLabel = trim(nodesLabels(edge%sNode))//"--"//trim(nodesLabels(edge%tNode))
					else
						edgeLabel = trim(nodesLabels(edge%tNode))//"--"//trim(nodesLabels(edge%sNode))
					end if
				end if
				
				output = trim(output)//trim(edgeLabel)
			end do
			output = trim(output)//"}>"
		else
			output = trim(output)//new_line('')
			output = trim(output)//"---------------"//new_line('')
			output = trim(output)//" IntegerGraph  "//new_line('')
			output = trim(output)//"---------------"//new_line('')
			output = trim(output)//new_line('')
			output = trim(output)//"Number of nodes = "//trim(FString_fromInteger(this%nNodes()))//new_line('')
			output = trim(output)//"Number of edges = "//trim(FString_fromInteger(this%nNodes()))//new_line('')
			output = trim(output)//"      Directed? = "//trim(FString_fromLogical(this%isDirected()))//new_line('')
			output = trim(output)//new_line('')
			
			output = trim(output)//"node2Neighbors"//new_line('')
			do i=1,this%nNodes()
				ivec => this%node2Neighbors%data(i)
				
				if( this%isDirected() ) then
					output = trim(output)//"   "//trim(FString_fromInteger(i))//" --> "
				else
					output = trim(output)//"   "//trim(FString_fromInteger(i))//" -- "
				end if
				
				do j=1,ivec%size()
					if( j /= ivec%size() ) then
						output = trim(output)//" "//trim(FString_fromInteger(ivec%at(j)))//","
					else
						output = trim(output)//" "//trim(FString_fromInteger(ivec%at(j)))
					end if
				end do
				
				output = trim(output)//new_line('')
			end do
			
			output = trim(output)//new_line('')
			output = trim(output)//"Node properties"//new_line('')
			do i=1,this%nNodes()
				node => this%nodeProperties%data(i)
				
				output = trim(output)//"   "//trim(FString_fromInteger(i))//" --> "
				
				if( present(nodesLabels) ) then
					output = trim(output)//" [ "//trim(nodesLabels(i))
				else
					output = trim(output)//" [ "//trim(node%label)
				end if
				
				output = trim(output)//", "//trim(adjustl(FString_fromReal(node%weight/effNodePropertiesUnits,format="(F10.3)")))
				output = trim(output)//", "//trim(FString_fromInteger(node%id))
				output = trim(output)//" ]"//new_line('')
			end do
			
			if( this%isDirected() ) then
				write(5,*) "@@@ WARNING @@@ nodesLabels is not implemented for directed graphs"
				
				output = trim(output)//new_line('')
				output = trim(output)//"node2InEdges"//new_line('')
				do i=1,this%nNodes()
					ivec => this%node2InEdges%data(i)
					output = trim(output)//"   "//trim(FString_fromInteger(i))//" --> "
					
					do j=1,ivec%size()
! 						if( this%isDirected() .or. ( .not. this%isDirected() .and. mod(ivec%at(j),2) /= 0 ) ) then
							if( j /= ivec%size() ) then
								output = trim(output)//"   "//trim(FString_fromInteger(ivec%at(j)))//","
							else
								output = trim(output)//"   "//trim(FString_fromInteger(ivec%at(j)))
							end if
! 						end if
					end do
					
					output = trim(output)//new_line('')
				end do
				
				output = trim(output)//new_line('')
				output = trim(output)//"node2OutEdges"//new_line('')
				do i=1,this%nNodes()
					ivec => this%node2OutEdges%data(i)
					output = trim(output)//"   "//trim(FString_fromInteger(i))//" --> "
					
					do j=1,ivec%size()
! 						if( this%isDirected() .or. ( .not. this%isDirected() .and. mod(ivec%at(j),2) /= 0 ) ) then
							if( j /= ivec%size() ) then
								output = trim(output)//"   "//trim(FString_fromInteger(ivec%at(j)))//","
							else
								output = trim(output)//"   "//trim(FString_fromInteger(ivec%at(j)))
							end if
! 						end if
					end do
					
					output = trim(output)//new_line('')
				end do
			end if
			
			output = trim(output)//new_line('')
			output = trim(output)//"Edge properties"//new_line('')
			do i=1,this%nEdges()
			
				loc = 0
				if( .not. this%isDirected() .and. i > 1 ) then
					do j=i-1,1,-1
						if( this%edgeProperties%data(i)%sNode == this%edgeProperties%data(j)%tNode .and. &
							this%edgeProperties%data(i)%tNode == this%edgeProperties%data(j)%sNode ) then
							loc = 1
							exit
						end if
					end do
				end if
				
				if( loc == 1 ) cycle
			
				edge => this%edgeProperties%data(i)
				
				if( this%isDirected() ) then
					output = trim(output)//"  "//trim(FString_fromInteger(i))//" --> "
				else
					output = trim(output)//"  "//trim(FString_fromInteger((i+1)/2))//" --> "
				end if
				
				edgeLabel = edge%label
				if( present(nodesLabels) ) then
					if( FString_hashKey(trim( nodesLabels(edge%sNode)) ) > FString_hashKey(trim( nodesLabels(edge%tNode)) ) ) then
						edgeLabel = trim(nodesLabels(edge%sNode))//"--"//trim(nodesLabels(edge%tNode))
					else
						edgeLabel = trim(nodesLabels(edge%tNode))//"--"//trim(nodesLabels(edge%sNode))
					end if
				end if
				
				output = trim(output)//" [ "
				
				if( this%isDirected() ) then
					output = trim(output)//trim(FString_fromInteger(edge%sNode))
					output = trim(output)//", "//trim(FString_fromInteger(edge%tNode))
				else
					if( present(nodesLabels) ) then
						if( FString_hashKey(trim( nodesLabels(edge%sNode)) ) > FString_hashKey(trim( nodesLabels(edge%tNode)) ) ) then
							output = trim(output)//trim(FString_fromInteger(edge%sNode))
							output = trim(output)//", "//trim(FString_fromInteger(edge%tNode))
						else
							output = trim(output)//trim(FString_fromInteger(edge%tNode))
							output = trim(output)//", "//trim(FString_fromInteger(edge%sNode))
						end if
					else
						if( trim(this%nodeProperties%data(edge%sNode)%label) == trim(FString_fromInteger(this%nodeProperties%data(edge%sNode)%id)) \
							.and. trim(this%nodeProperties%data(edge%tNode)%label) == trim(FString_fromInteger(this%nodeProperties%data(edge%tNode)%id)) ) then
							
							if( this%nodeProperties%data(edge%sNode)%id > this%nodeProperties%data(edge%tNode)%id ) then
								output = trim(output)//trim(FString_fromInteger(edge%sNode))
								output = trim(output)//", "//trim(FString_fromInteger(edge%tNode))
							else
								output = trim(output)//trim(FString_fromInteger(edge%tNode))
								output = trim(output)//", "//trim(FString_fromInteger(edge%sNode))
							end if
						else
							if( FString_hashKey( trim(this%nodeProperties%data(edge%sNode)%label) ) > FString_hashKey( trim(this%nodeProperties%data(edge%tNode)%label) ) ) then
								output = trim(output)//trim(FString_fromInteger(edge%sNode))
								output = trim(output)//", "//trim(FString_fromInteger(edge%tNode))
							else
								output = trim(output)//trim(FString_fromInteger(edge%tNode))
								output = trim(output)//", "//trim(FString_fromInteger(edge%sNode))
							end if
						end if
					end if
				end if
				
				
				output = trim(output)//", "//trim(edgeLabel)
				output = trim(output)//", "//trim(adjustl(FString_fromReal(edge%weight/effEdgePropertiesUnits,format="(F10.3)")))
				
				if( this%isDirected() ) then
					output = trim(output)//", "//trim(FString_fromInteger(edge%id))
				else
					output = trim(output)//", "//trim(FString_fromInteger((edge%id+1)/2))
				end if
				
				output = trim(output)//" ]"//new_line('')
			end do
		end if
		
		ivec => null()
	end function str
	
	!>
	!! @brief
	!!
	function toDATString( this, name, nodesLabels, nodePropertiesUnits, edgePropertiesUnits, nodesColors ) result( output )
		class(IntegerGraph) :: this
		character(*), optional, intent(in) :: name
		character(*), optional, intent(in) :: nodesLabels(:)
		real(8), optional, intent(in) :: nodePropertiesUnits
		real(8), optional, intent(in) :: edgePropertiesUnits
		character(*), optional, intent(in) :: nodesColors(:)
		character(:), allocatable :: output
		
		character(100) :: effName
		real(8) :: effNodePropertiesUnits
		real(8) :: effEdgePropertiesUnits
		
		integer :: i, j, loc, idNode
		integer :: effUnit
		character(100) :: colorOptions
		
		output = ""
		
		effName = "mygraph"
		if( present(name) ) effName = name
		
		effNodePropertiesUnits = 1.0_8
		if( present(nodePropertiesUnits) ) effNodePropertiesUnits = nodePropertiesUnits
		
		effEdgePropertiesUnits = 1.0_8
		if( present(edgePropertiesUnits) ) effEdgePropertiesUnits = edgePropertiesUnits
		
! 		if( this%isDirected() ) then
! 			write(effUnit,"(A)") "digraph "//trim(effName)//" {"
! 		else
! 			write(effUnit,"(A)") "graph "//trim(effName)//" {"
! 		end if
		
		do i=1,this%nNodes()
! 			write(effUnit,"(A)") "   "//trim(FString_fromInteger(i))//" [shape=box]"
			output = output//trim(FString_fromInteger(i))

! 			colorOptions = ""
! 			if( present(nodesColors) ) then
! 				colorOptions = " style = filled, fillcolor = "//char(34)//"#"//trim(nodesColors(i))//char(34)
! 			end if
			
			if( present(nodesLabels) ) then
! 				write(effUnit,"(A)",advance="no") "[label="//char(34)//trim(nodesLabels(i))//"("//trim(FString_fromReal(this%nodeProperties%data(i)%weight))//")"//char(34)//trim(colorOptions)//"]"

				output = output//"[label="//trim(nodesLabels(i))//"("//trim(this%nodeProperties%data(i)%label)//")"//trim(colorOptions)//"]"
! 				write(effUnit,"(A)",advance="no") "[label="//trim(nodesLabels(i))//"("//trim(FString_fromReal(this%nodeProperties%data(i)%weight))//")"//trim(colorOptions)//"]"
			else
! 				write(effUnit,"(A)") " [ label = "//char(34)//trim(this%nodeProperties%data(i)%label)//"("//trim(adjustl(FString_fromReal(this%nodeProperties%data(i)%weight,"(F10.1)")))//")"//char(34)//trim(colorOptions)//" ]"
! 				write(effUnit,"(A)",advance="no") "[label="//char(34)//trim(this%nodeProperties%data(i)%label)//char(34)//trim(colorOptions)//"],"
				output = output//"[label="//trim(this%nodeProperties%data(i)%label)//",weight="//trim(adjustl(FString_fromReal(this%nodeProperties%data(i)%weight,format="(F10.5)")))//"];"
			end if
		end do
		
		do i=1,this%nEdges()
			
			loc = 0
			if( .not. this%isDirected() .and. i > 1 ) then
				do j=i-1,1,-1
					if( this%edgeProperties%data(i)%sNode == this%edgeProperties%data(j)%tNode .and. &
						this%edgeProperties%data(i)%tNode == this%edgeProperties%data(j)%sNode ) then
						loc = 1
						exit
					end if
				end do
			end if
			
			if( loc == 1 ) cycle
			
			idNode = this%edgeProperties%data(i)%sNode
			output = output//trim(FString_fromInteger(idNode))
			
			if( this%isDirected() ) then
				output = output//"->"
			else
				output = output//"--"
			end if
			
			idNode = this%edgeProperties%data(i)%tNode
			output = output//trim(FString_fromInteger(idNode))
			! @todo Hay que poner un parametro que hage swap entre weight y label, para que quede bien el dibujo en dot
! 			write(effUnit,"(A)",advance="no") " [ label = "//char(34)//trim(this%edgeProperties%data(i)%label)//char(34)
! 			write(effUnit,"(A)",advance="no") " [ label = "//char(34)//trim(adjustl(FString_fromReal(this%edgeProperties%data(i)%weight/effEdgePropertiesUnits,format="(F5.2)")))//char(34)

! 			write(effUnit,"(A)",advance="no") "[label="//char(34)//trim(adjustl(FString_fromReal(this%edgeProperties%data(i)%weight/effEdgePropertiesUnits,format="(F2.0)")))//char(34)//","
! 			write(effUnit,"(A)",advance="no") "weight="//trim(adjustl(FString_fromReal(this%edgeProperties%data(i)%weight/effEdgePropertiesUnits,format="(F5.1)")))//"]"

			output = output//"[label="//trim(adjustl(this%edgeProperties%data(i)%label))//","
			output = output//"weight="//trim(adjustl(FString_fromReal(this%edgeProperties%data(i)%weight/effEdgePropertiesUnits,format="(F10.5)")))//"]"//";"
		end do
		
	end function toDATString
	
	!>
	!! @brief
	!!
	pure function nNodes( this ) result( output )
		class(IntegerGraph), intent(in) :: this
		integer :: output
		
		output = this%nodeProperties%size()
	end function nNodes
	
	!>
	!! @brief
	!!
	pure function nEdges( this ) result( output )
		class(IntegerGraph), intent(in) :: this
		integer :: output
		
! 		if( this%isDirected() ) then
			output = this%edgeProperties%size()
! 		else
! 			output = ( this%edgeProperties%size() + 1 )/2
! 		end if
	end function nEdges
	
	!>
	!! @brief
	!!
	pure function isDirected( this ) result( output )
		class(IntegerGraph), intent(in) :: this
		logical :: output
		
		output = this%directed
	end function isDirected
	
	!>
	!! @brief
	!!
	function isConnected( this, nComponents, laplacianMatrix ) result( output )
		class(IntegerGraph), intent(in) :: this
		integer, optional, intent(out) :: nComponents
		type(Matrix), optional, intent(in) :: laplacianMatrix
		logical :: output
		
		integer :: i, nComp
		type(Matrix) :: L
		real(8), allocatable :: diagL(:)
		
		allocate( diagL(this%nNodes()) )
		
		if( present(laplacianMatrix) ) then
			L = laplacianMatrix
		else
			L = this%laplacianMatrix()
		end if
		
		call L.eigen( eValues=diagL )
		
		nComp = 0
		do i=1,this%nNodes()
			if( abs(diagL(i)) < 1e-5 ) then
				nComp = nComp + 1
			end if
		end do
		
		output = .false.
		if( nComp == 1 ) output = .true.
		
		if( present(nComponents) ) nComponents = nComp
		
		deallocate( diagL )
		
	end function isConnected
	
	!>
	!! @brief
	!!
	function nComponents( this, laplacianMatrix ) result( output )
		class(IntegerGraph), intent(in) :: this
		type(Matrix), optional, intent(in) :: laplacianMatrix
		integer :: output
		
		type(Matrix) :: L
		logical :: isConnected
		
		if( present(laplacianMatrix) ) then
			L = laplacianMatrix
		else
			L = this%laplacianMatrix()
		end if
		
		isConnected = this%isConnected( output, laplacianMatrix )
	end function nComponents
	
	!>
	!! @brief
	!!
	subroutine newNode( this, label, weight, id )
		class(IntegerGraph) :: this
		character(*), optional, intent(in) :: label
		real(8), optional, intent(in) :: weight
		integer, optional, intent(out) :: id
		
		integer :: effId
		type(Node) :: node
		type(IntegerVector) :: ivec
		
		call ivec%init()
		call this%node2Neighbors%append( ivec )
		call this%node2InEdges%append( ivec )
		call this%node2OutEdges%append( ivec )
		
		effId = this%nodeProperties%size()+1
		call node%init( id=effId, label=label, weight=weight )
		call this%nodeProperties%append( node )
		
		if( present(id) ) id = effId
	end subroutine newNode
	
	!>
	!! @brief
	!!
	subroutine newNodes( this, n, labels, weights )
		class(IntegerGraph) :: this
		integer, intent(in) :: n
		character(*), optional, intent(in) :: labels(:)
		real(8), optional, intent(in) :: weights(:)
		
		integer :: i
		
		if( present(labels) .and. present(weights) ) then
			do i=1,n
				call this%newNode( label=labels(i), weight=weights(i) )
			end do
		else if( present(labels) ) then
			do i=1,n
				call this%newNode( label=labels(i) )
			end do
		else if( present(weights) ) then
			do i=1,n
				call this%newNode( weight=weights(i) )
			end do
		else
			do i=1,n
				call this%newNode()
			end do
		end if
	end subroutine newNodes
	
	!>
	!! @brief
	!!
	subroutine deleteNode( this, nodeId )
		class(IntegerGraph), target :: this
		integer, intent(in) :: nodeId
		
		integer :: i, j, k, idEdge
		type(IntegerVector), pointer :: ivec
		type(IntegerVector) :: edgesToRemove
		
! 		!        (1)          node2Neighbors         node2InEdges           node2OutEdges
! 		!         |1,2        1 -->   2              1 -->   2              1 -->   1
! 		!        (2)          2 -->   1,   3,   4    2 -->   1,   4,   6    2 -->   2,   3,   5
! 		!    3,4/   \5,6      3 -->   2,   5         3 -->   3,   8         3 -->   4,   7
! 		!     (3)   (4)       4 -->   2,   5         4 -->   5,   10        4 -->   6,   9
! 		!    7,8\   /9,10     5 -->   3,   4         5 -->   7,   9         5 -->   8,   10
! 		!        (5)
! 		!
!		! erase(1)
!		! --------
!		!
! 		!        (2)          node2Neighbors         node2InEdges           node2OutEdges
! 		!    3,4/   \5,6      2 -->   3,   4         2 -->   4,   6         2 -->   3,   5
! 		!     (3)   (4)       3 -->   2,   5         3 -->   3,   8         3 -->   4,   7
! 		!    7,8\   /9,10     4 -->   2,   5         4 -->   5,   10        4 -->   6,   9
! 		!        (5)          5 -->   3,   4         5 -->   7,   9         5 -->   8,   10
!		!                                           
!		! after updating numbers
!		!
! 		!        (1)          node2Neighbors         node2InEdges           node2OutEdges
! 		!    1,2/   \3,4      1 -->   2,   3         1 -->   2,   4         1 -->   1,   3
! 		!     (2)   (3)       2 -->   1,   4         2 -->   1,   6         2 -->   2,   5
! 		!    5,6\   /7,8      3 -->   1,   4         3 -->   3,   8         3 -->   4,   7
! 		!        (4)          4 -->   2,   3         4 -->   5,   7         4 -->   6,   8
! 		!
!		! erase(3)
!		! --------
!		!
! 		!        (1)          node2Neighbors         node2InEdges           node2OutEdges
! 		!    1,2/             1 -->   2              1 -->   2              1 -->   1
! 		!     (2)             2 -->   1,   4         2 -->   1,   6         2 -->   2,   5
! 		!    5,6\             4 -->   2              4 -->   5              4 -->   6
! 		!        (4)                                 
!		!
!		! after updating numbers
!		!
! 		!        (1)          node2Neighbors         node2InEdges           node2OutEdges
! 		!    1,2/             1 -->   2              1 -->   2              1 -->   1
! 		!     (2)             2 -->   1,   3         2 -->   1,   4         2 -->   2,   3
! 		!    3,4\             3 -->   2              3 -->   3              3 -->   4
! 		!        (3)                                 
!		!
		
		call this%node2Neighbors%erase( nodeId ) ! Removes the item in the position nodeId
		call this%node2InEdges%erase( nodeId )   ! Removes the item in the position nodeId
		call this%node2OutEdges%erase( nodeId )  ! Removes the item in the position nodeId
		call this%nodeProperties%erase( nodeId ) ! Removes the item in the position nodeId
		
		! Updates the ids of the nodes in node2Neighbors
		do i=1,this%node2Neighbors%size()
			ivec => this%node2Neighbors%data(i)
			call ivec%remove( nodeId ) ! Removes the item with the value nodeId
			
			do j=1,ivec%size()
				if( ivec%at(j) > nodeId ) then
					call ivec%set( j, ivec%at(j)-1 )
				end if
			end do
		end do
		
		do i=1,this%nodeProperties%size()
			if( this%nodeProperties%data(i)%id > nodeId ) then
				this%nodeProperties%data(i)%id = this%nodeProperties%data(i)%id -1
			end if
		end do
		
		do i=1,this%edgeProperties%size()
			if( this%edgeProperties%data(i)%sNode == nodeId .or. this%edgeProperties%data(i)%tNode == nodeId ) then
				call edgesToRemove%append( i )
			end if
		end do
		
		k=0
		do i=1,edgesToRemove%size()
			call this%edgeProperties%erase( edgesToRemove%at(i)-k )
			
			do j=1,this%node2InEdges%size()
				ivec => this%node2InEdges%data(j)
				
				call ivec%remove( edgesToRemove%at(i) )
			end do
			
			do j=1,this%node2OutEdges%size()
				ivec => this%node2OutEdges%data(j)
				
				call ivec%remove( edgesToRemove%at(i) )
			end do
			
			k = k+1
		end do
		
		do i=1,this%edgeProperties%size()
			if( this%edgeProperties%data(i)%sNode > nodeId ) then
				this%edgeProperties%data(i)%sNode = this%edgeProperties%data(i)%sNode - 1
			end if
			
			if( this%edgeProperties%data(i)%tNode > nodeId ) then
				this%edgeProperties%data(i)%tNode = this%edgeProperties%data(i)%tNode - 1
			end if
		end do
		
		! Updates the ids of the edges in node2InEdges and node2OutEdges vectors
		do i=1,edgesToRemove%size()
			do j=1,this%node2InEdges%size()
				ivec => this%node2InEdges%data(j)
				
				do k=1,ivec%size()
					if( ivec%at(k) >= edgesToRemove%at(i) ) then
						call ivec%set( k, ivec%at(k)-1 )
					end if
				end do
			end do
			
			do j=1,this%node2OutEdges%size()
				ivec => this%node2OutEdges%data(j)
				
				do k=1,ivec%size()
					if( ivec%at(k) >= edgesToRemove%at(i) ) then
						call ivec%set( k, ivec%at(k)-1 )
					end if
				end do
			end do
		end do
		
		do i=1,edgesToRemove%size()
			do j=1,this%edgeProperties%size()
				if( this%edgeProperties%data(j)%id >= edgesToRemove%at(i) ) then
					this%edgeProperties%data(j)%id = this%edgeProperties%data(j)%id - 1
				end if
			end do
		end do
		
	end subroutine deleteNode
	
	!>
	!! @brief
	!!
	subroutine deleteNodes( this, nodeIds )
		class(IntegerGraph) :: this
		integer, intent(in) :: nodeIds(:)
		
		integer :: i
		
		do i=1,size(nodeIds)
			call this%deleteNode( nodeIds(i) )
		end do
	end subroutine deleteNodes
	
	!>
	!! @brief
	!!
	subroutine newEdgeBase( this, sNode, tNode, label, weight, id )
		class(IntegerGraph), target :: this
		integer, intent(in) :: sNode, tNode
		character(*), optional, intent(in) :: label
		real(8), optional, intent(in) :: weight
		integer, optional, intent(out) :: id
		
		integer :: effId
		character(100) :: effLabel
		
		type(Edge) :: edge
		type(IntegerVector), pointer :: ivec1
		
		if( this%isDirected() ) then
			effLabel = trim(this%nodeProperties%data(sNode)%label)//"-->"//trim(this%nodeProperties%data(tNode)%label)
		else
			if( trim(this%nodeProperties%data(sNode)%label) == trim(FString_fromInteger(this%nodeProperties%data(sNode)%id)) \
				.and. trim(this%nodeProperties%data(tNode)%label) == trim(FString_fromInteger(this%nodeProperties%data(tNode)%id)) ) then
				
				if( this%nodeProperties%data(sNode)%id > this%nodeProperties%data(tNode)%id ) then
					effLabel = trim(this%nodeProperties%data(sNode)%label)//"--"//trim(this%nodeProperties%data(tNode)%label)
				else
					effLabel = trim(this%nodeProperties%data(tNode)%label)//"--"//trim(this%nodeProperties%data(sNode)%label)
				end if
			else
				if( FString_hashKey( trim(this%nodeProperties%data(sNode)%label) ) > FString_hashKey( trim(this%nodeProperties%data(tNode)%label) ) ) then
					effLabel = trim(this%nodeProperties%data(sNode)%label)//"--"//trim(this%nodeProperties%data(tNode)%label)
				else
					effLabel = trim(this%nodeProperties%data(tNode)%label)//"--"//trim(this%nodeProperties%data(sNode)%label)
				end if
			end if
		end if
		if( present(label) ) effLabel = label
		
		! Busco en las vecindades
		ivec1 => this%node2Neighbors%data(sNode)
		if( .not. ivec1.contains(tNode) ) then  ! Si no ha sido agregado ...
			call ivec1%append( tNode )      ! lo adiciono
			
			ivec1 => this%node2OutEdges%data(sNode) ! Busco en las aristas salientes de sNode ...
			effId = this%edgeProperties%size()+1    ! calculo el id que tendra la siguiente arista
			call ivec1%append( effId )   ! Agrego la nueva arista como una arista saliente de sNode
			
			ivec1 => this%node2InEdges%data(tNode) ! Busco en las aristas entrantes de tNode ...
			effId = this%edgeProperties%size()+1    ! calculo el id que tendra la siguiente arista
			call ivec1%append( effId )   ! Agrego la nueva arista como una arista entrantes de tNode
			
			! Creo una nueva arista en las propiedades de arista con el id que he calculado
			call edge%init( sNode, tNode, id=effId, label=effLabel, weight=weight )
			call this%edgeProperties%append( edge )
			
			if( present(id) ) id = effId
		end if
		
		ivec1 => null()
	end subroutine newEdgeBase
	
	!>
	!! @brief
	!!
	subroutine newEdge( this, sNode, tNode, label, weight, id )
		class(IntegerGraph), target :: this
		integer, intent(in) :: sNode, tNode
		character(*), optional, intent(in) :: label
		real(8), optional :: weight
		integer, optional, intent(out) :: id
		
		if( this%directed ) then
			call this%newEdgeBase( sNode, tNode, label, weight, id )
		else
			call this%newEdgeBase( sNode, tNode, label, weight, id )
			call this%newEdgeBase( tNode, sNode, label, weight )
		end if
	end subroutine newEdge
	
	!>
	!! @brief
	!!
	subroutine newEdges( this, sNode, tNodesVec, labels, weights )
		class(IntegerGraph) :: this
		integer, intent(in) :: sNode
		integer, intent(in) :: tNodesVec(:)
		character(*), optional, intent(in) :: labels(:)
		real(8), optional, intent(in) :: weights(:)
		
		integer :: i
		
		if( present(labels) .and. present(weights) ) then
			do i=1,size(tNodesVec)
				call this%newEdge( sNode, tNodesVec(i), label=labels(i), weight=weights(i) )
			end do
		else if( present(labels) ) then
			do i=1,size(tNodesVec)
				call this%newEdge( sNode, tNodesVec(i), label=labels(i) )
			end do
		else if( present(weights) ) then
			do i=1,size(tNodesVec)
				call this%newEdge( sNode, tNodesVec(i), weight=weights(i) )
			end do
		else
			do i=1,size(tNodesVec)
				call this%newEdge( sNode, tNodesVec(i) )
			end do
		end if
	end subroutine newEdges
	
	!>
	!! @brief
	!!
	subroutine deleteEdge( this, edgeId )
		class(IntegerGraph), target :: this
		integer, intent(in) :: edgeId
		
		integer :: i, j
		type(Edge), pointer :: edgei
		type(IntegerVector), pointer :: ivec
		
! 		!        (1)          node2Neighbors         node2InEdges           node2OutEdges         
! 		!         |1,2        1 -->   2              1 -->   2              1 -->   1
! 		!        (2)          2 -->   1,   3,   4    2 -->   1,   4,   6    2 -->   2,   3,   5
! 		!    3,4/   \5,6      3 -->   2,   5         3 -->   3,   8         3 -->   4,   7
! 		!     (3)   (4)       4 -->   2,   5         4 -->   5,   10        4 -->   6,   9
! 		!    7,8\   /9,10     5 -->   3,   4         5 -->   7,   9         5 -->   8,   10
! 		!        (5)
! 		!
!		! erase(6)
!		! --------
!		!
! 		!        (1)          node2Neighbors         node2InEdges           node2OutEdges
! 		!         |1,2        1 -->   2              1 -->   2              1 -->   1
! 		!        (2)          2 -->   1,   3,   4    2 -->   1,   4         2 -->   2,   3,   5
! 		!    3,4/   \5        3 -->   2,   5         3 -->   3,   8         3 -->   4,   7
! 		!     (3)   (4)       4 -->   2,   5         4 -->   5,   10        4 -->   9
! 		!    7,8\   /9,10     5 -->   3,   4         5 -->   7,   9         5 -->   8,   10
! 		!        (5)          
!		!                                           
!		! after updating numbers
! 		!        (1)          node2Neighbors         node2InEdges           node2OutEdges
! 		!         |1,2        1 -->   2              1 -->   2              1 -->   1
! 		!        (2)          2 -->   1,   3,   4    2 -->   1,   4         2 -->   2,   3,   5
! 		!    3,4/   \5        3 -->   2,   5         3 -->   3,   7         3 -->   4,   6
! 		!     (3)   (4)       4 -->   2,   5         4 -->   5,   9         4 -->   8
! 		!    6,7\   /8,9      5 -->   3,   4         5 -->   6,   8         5 -->   7,   9
! 		!        (5)          
!		!     
		
		call this%edgeProperties%erase( edgeId ) ! Removes the item in the position nodeId
		
		! Updates the ids of the edges in edgeProperties
		do i=1,this%edgeProperties%size()
			edgei => this%edgeProperties%data(i)
			
			if( edgei%id > edgeId ) then
				edgei%id = edgei%id - 1
			end if
		end do
		
		! Remove the edge from node2InEdges
		do i=1,this%node2InEdges%size()
			ivec => this%node2InEdges%data(i)
			
			call ivec%remove( edgeId )
		end do
		
		! Remove the edge from node2OutEdges
		do i=1,this%node2OutEdges%size()
			ivec => this%node2OutEdges%data(i)
			
			call ivec%remove( edgeId )
		end do
		
		! Updates the edge ids from node2InEdges
		do i=1,this%node2InEdges%size()
			ivec => this%node2InEdges%data(i)
			
			do j=1,ivec%size()
				if( ivec%at(j) >= edgeId ) then
					call ivec%set( j, ivec%at(j)-1 )
				end if
			end do
		end do
		
		! Updates the edge ids from node2OutEdges
		do i=1,this%node2OutEdges%size()
			ivec => this%node2OutEdges%data(i)
			
			do j=1,ivec%size()
				if( ivec%at(j) >= edgeId ) then
					call ivec%set( j, ivec%at(j)-1 )
				end if
			end do
		end do
	end subroutine deleteEdge
	
	!>
	!! @brief
	!!
	subroutine deleteEdges( this, edgesIds )
		class(IntegerGraph), target :: this
		integer, intent(in) :: edgesIds(:)
		
		integer :: i
		
		do i=1,size(edgesIds)
			call this%deleteEdge( edgesIds(i) )
		end do
	end subroutine deleteEdges
	
	!>
	!! @brief
	!!
	function getEdgeId( this, sNode, tNode ) result( output )
		class(IntegerGraph), target :: this
		integer, intent(in) :: sNode
		integer, intent(in) :: tNode
		integer :: output
		
		type(IntegerVector), pointer :: ivec
		integer :: i, j, idEdge
		
		! Busco en las aristas salientes de sNode
		ivec => this%node2OutEdges%data(sNode)
		
		output = -1
		do i=1,ivec%size()
			idEdge = ivec%at(i)
			
			if( this%edgeProperties%data(idEdge)%tNode == tNode ) then
				output = this%edgeProperties%data(idEdge)%id
			end if
		end do
		
		ivec => null()
	end function getEdgeId
	
	!>
	!! @brief
	!!
	subroutine setNodeProperties( this, nodeId, nodeProperties )
		class(IntegerGraph), target :: this
		integer, intent(in) :: nodeId
		type(Node), intent(in) :: nodeProperties
		
		this%nodeProperties%data(nodeId) = nodeProperties
	end subroutine setNodeProperties
	
	!>
	!! @brief
	!!
	function getNodeProperties( this, nodeId ) result( output )
		class(IntegerGraph), target :: this
		integer, intent(in) :: nodeId
		type(Node) :: output
		
		output = this%nodeProperties%data(nodeId)
	end function getNodeProperties
	
	!>
	!! @brief
	!!
	subroutine setEdgeProperties( this, edgeId, edgeProperties )
		class(IntegerGraph), target :: this
		integer, intent(in) :: edgeId
		type(Edge), intent(in) :: edgeProperties
		
		this%edgeProperties%data(edgeId) = edgeProperties
	end subroutine setEdgeProperties
	
	!>
	!! @brief
	!!
	function getEdgeProperties( this, edgeId ) result( output )
		class(IntegerGraph), target :: this
		integer, intent(in) :: edgeId
		type(Edge) :: output
		
		output = this%edgeProperties%data(edgeId)
	end function getEdgeProperties
	
	!>
	!! @brief
	!!
	subroutine save( this, oFileName, format, name, nodesLabels, nodePropertiesUnits, edgePropertiesUnits, nodesColors, append )
		class(IntegerGraph) :: this
		character(*), optional, intent(in) :: oFileName
		integer, optional, intent(in) :: format
		character(*), optional, intent(in) :: name
		character(*), optional, intent(in) :: nodesLabels(:)
		real(8), optional, intent(in) :: nodePropertiesUnits
		real(8), optional, intent(in) :: edgePropertiesUnits
		character(*), optional, intent(in) :: nodesColors(:)
		logical, optional, intent(in) :: append
		
		integer :: effFormat
		
		effFormat = GML
		if( present(format) ) effFormat = format
		
		select case ( effFormat )
			case( GML )
				call this%saveGML( oFileName, name, nodesLabels, nodePropertiesUnits, edgePropertiesUnits, nodesColors )
			case( DOT )
				call this%saveDOT( oFileName, name, nodesLabels, nodePropertiesUnits, edgePropertiesUnits, nodesColors )
			case( DAT )
				call this%saveDAT( oFileName, name, nodesLabels, nodePropertiesUnits, edgePropertiesUnits, nodesColors, append )
		end select
	end subroutine save
	
	!>
	!! @brief
	!!
	subroutine saveGML( this, oFileName, name, nodesLabels, nodePropertiesUnits, edgePropertiesUnits, nodesColors )
		class(IntegerGraph) :: this
		character(*), optional, intent(in) :: oFileName
		character(*), optional, intent(in) :: name
		character(*), optional, intent(in) :: nodesLabels(:)
		real(8), optional, intent(in) :: nodePropertiesUnits
		real(8), optional, intent(in) :: edgePropertiesUnits
		character(*), optional, intent(in) :: nodesColors(:)
		
		integer :: effUnit
		real(8) :: effNodePropertiesUnits
		real(8) :: effEdgePropertiesUnits
		
		integer :: i
		
		if( present(oFileName) ) then
			effUnit = 31
			open( unit=effUnit, file=trim(oFileName) )
		else
			effUnit = 6
		end if
		
		effNodePropertiesUnits = 1.0_8
		if( present(nodePropertiesUnits) ) effNodePropertiesUnits = nodePropertiesUnits
		
		effEdgePropertiesUnits = 1.0_8
		if( present(edgePropertiesUnits) ) effEdgePropertiesUnits = edgePropertiesUnits
		
		write(*,"(A)") "@@@ WARNING @@@ IntegerGraph%saveGML() is not well tested yet"
		
		write(effUnit,"(A)") "graph["
		write(effUnit,"(A)") "   comment "//achar(34)//"Created by scift library"//achar(34)
		
		if( this%isDirected() ) then
			write(effUnit,"(A)") "   directed 1"
		end if
		
		do i=1,this%nNodes()
			write(effUnit,"(A)") "   node[ id "//trim(FString_fromInteger(i)) \
				//" label "//achar(34)//trim(FString_fromInteger(i))//achar(34)//" ]"
		end do
		
		do i=1,this%nEdges()
			write(effUnit,"(A)") "   edge[ source "//trim(FString_fromInteger(this%edgeProperties%data(i)%sNode)) \
				//" target "//trim(FString_fromInteger(this%edgeProperties%data(i)%tNode)) \
				//" label "//achar(34)//trim(FString_fromInteger(i))//achar(34)//" ]"
		end do
		
		write(effUnit,"(A)") "]"
		
		close(effUnit)
	end subroutine saveGML
	
	!>
	!! @brief
	!!
	subroutine saveDOT( this, oFileName, name, nodesLabels, nodePropertiesUnits, edgePropertiesUnits, nodesColors )
		class(IntegerGraph) :: this
		character(*), optional, intent(in) :: oFileName
		character(*), optional, intent(in) :: name
		character(*), optional, intent(in) :: nodesLabels(:)
		real(8), optional, intent(in) :: nodePropertiesUnits
		real(8), optional, intent(in) :: edgePropertiesUnits
		character(*), optional, intent(in) :: nodesColors(:)
		
		character(100) :: effName
		real(8) :: effNodePropertiesUnits
		real(8) :: effEdgePropertiesUnits
		
		integer :: i, j, loc, idNode
		integer :: effUnit
		character(100) :: colorOptions
		
		if( present(oFileName) ) then
			effUnit = 31
			open( unit=effUnit, file=trim(oFileName) )
		else
			effUnit = 6
		end if
		
		effName = "mygraph"
		if( present(name) ) effName = name
		
		effNodePropertiesUnits = 1.0_8
		if( present(nodePropertiesUnits) ) effNodePropertiesUnits = nodePropertiesUnits
		
		effEdgePropertiesUnits = 1.0_8
		if( present(edgePropertiesUnits) ) effEdgePropertiesUnits = edgePropertiesUnits
		
		if( this%isDirected() ) then
			write(effUnit,"(A)") "digraph "//trim(effName)//" {"
		else
			write(effUnit,"(A)") "graph "//trim(effName)//" {"
		end if
		
		do i=1,this%nNodes()
! 			write(effUnit,"(A)") "   "//trim(FString_fromInteger(i))//" [shape=box]"
			write(effUnit,"(A)", advance="no") "   "//trim(FString_fromInteger(i))

			colorOptions = ""
			if( present(nodesColors) ) then
				colorOptions = " style = filled, fillcolor = "//char(34)//"#"//trim(nodesColors(i))//char(34)
			end if
			
			if( present(nodesLabels) ) then
				write(effUnit,"(A)") " [ label = "//char(34)//trim(nodesLabels(i))//"("//trim(FString_fromReal(this%nodeProperties%data(i)%weight))//")"//char(34)//trim(colorOptions)//" ]"
			else
! 				write(effUnit,"(A)") " [ label = "//char(34)//trim(this%nodeProperties%data(i)%label)//"("//trim(adjustl(FString_fromReal(this%nodeProperties%data(i)%weight,"(F10.1)")))//")"//char(34)//trim(colorOptions)//" ]"
				write(effUnit,"(A)") " [ label = "//char(34)//trim(this%nodeProperties%data(i)%label)//char(34)//trim(colorOptions)//" ]"
			end if
		end do
		
		do i=1,this%nEdges()
			
			loc = 0
			if( .not. this%isDirected() .and. i > 1 ) then
				do j=i-1,1,-1
					if( this%edgeProperties%data(i)%sNode == this%edgeProperties%data(j)%tNode .and. &
						this%edgeProperties%data(i)%tNode == this%edgeProperties%data(j)%sNode ) then
						loc = 1
						exit
					end if
				end do
			end if
			
			if( loc == 1 ) cycle
			
			idNode = this%edgeProperties%data(i)%sNode
			write(effUnit,"(A)",advance="no") "   "//trim(FString_fromInteger(idNode))
			
			if( this%isDirected() ) then
				write(effUnit,"(A)",advance="no") " -> "
			else
				write(effUnit,"(A)",advance="no") " -- "
			end if
			
			idNode = this%edgeProperties%data(i)%tNode
			write(effUnit,"(A)",advance="no") trim(FString_fromInteger(idNode))
			! @todo Hay que poner un parametro que hage swap entre weight y label, para que quede bien el dibujo en dot
! 			write(effUnit,"(A)",advance="no") " [ label = "//char(34)//trim(this%edgeProperties%data(i)%label)//char(34)
! 			write(effUnit,"(A)",advance="no") " [ label = "//char(34)//trim(adjustl(FString_fromReal(this%edgeProperties%data(i)%weight/effEdgePropertiesUnits,format="(F5.2)")))//char(34)
			write(effUnit,"(A)",advance="no") " [ label = "//char(34)//trim(adjustl(FString_fromReal(this%edgeProperties%data(i)%weight/effEdgePropertiesUnits,format="(F2.0)")))//char(34)
			write(effUnit,"(A)") " weight = "//trim(FString_fromReal(this%edgeProperties%data(i)%weight/effEdgePropertiesUnits,format="(F5.1)"))//" ]"
		end do
		
		write(effUnit,"(A)") "}"
		
		close(effUnit)
	end subroutine saveDOT
	
	!>
	!! @brief
	!!
	subroutine saveDAT( this, oFileName, name, nodesLabels, nodePropertiesUnits, edgePropertiesUnits, nodesColors, append )
		class(IntegerGraph) :: this
		character(*), optional, intent(in) :: oFileName
		character(*), optional, intent(in) :: name
		character(*), optional, intent(in) :: nodesLabels(:)
		real(8), optional, intent(in) :: nodePropertiesUnits
		real(8), optional, intent(in) :: edgePropertiesUnits
		character(*), optional, intent(in) :: nodesColors(:)
		logical, optional, intent(in) :: append
		
		character(100) :: effName
		real(8) :: effNodePropertiesUnits
		real(8) :: effEdgePropertiesUnits
		
		integer :: i, j, loc, idNode
		integer :: effUnit
		character(100) :: colorOptions
		
		if( present(oFileName) ) then
			effUnit = 31
			
			if( present(append) .and. append ) then
				open( unit=effUnit, file=trim(oFileName), status="old", access="append" )
			else
				open( unit=effUnit, file=trim(oFileName) )
			end if
		else
			effUnit = 6
		end if
		
		effName = "mygraph"
		if( present(name) ) effName = name
		
		effNodePropertiesUnits = 1.0_8
		if( present(nodePropertiesUnits) ) effNodePropertiesUnits = nodePropertiesUnits
		
		effEdgePropertiesUnits = 1.0_8
		if( present(edgePropertiesUnits) ) effEdgePropertiesUnits = edgePropertiesUnits
		
! 		if( this%isDirected() ) then
! 			write(effUnit,"(A)") "digraph "//trim(effName)//" {"
! 		else
! 			write(effUnit,"(A)") "graph "//trim(effName)//" {"
! 		end if
		
		do i=1,this%nNodes()
! 			write(effUnit,"(A)") "   "//trim(FString_fromInteger(i))//" [shape=box]"
			write(effUnit,"(A)",advance="no") trim(FString_fromInteger(i))

! 			colorOptions = ""
! 			if( present(nodesColors) ) then
! 				colorOptions = " style = filled, fillcolor = "//char(34)//"#"//trim(nodesColors(i))//char(34)
! 			end if
			
			if( present(nodesLabels) ) then
! 				write(effUnit,"(A)",advance="no") "[label="//char(34)//trim(nodesLabels(i))//"("//trim(FString_fromReal(this%nodeProperties%data(i)%weight))//")"//char(34)//trim(colorOptions)//"]"

				write(effUnit,"(A)",advance="no") "[label="//trim(nodesLabels(i))//"("//trim(this%nodeProperties%data(i)%label)//")"//trim(colorOptions)//"]"
! 				write(effUnit,"(A)",advance="no") "[label="//trim(nodesLabels(i))//"("//trim(FString_fromReal(this%nodeProperties%data(i)%weight))//")"//trim(colorOptions)//"]"
			else
! 				write(effUnit,"(A)") " [ label = "//char(34)//trim(this%nodeProperties%data(i)%label)//"("//trim(adjustl(FString_fromReal(this%nodeProperties%data(i)%weight,"(F10.1)")))//")"//char(34)//trim(colorOptions)//" ]"
! 				write(effUnit,"(A)",advance="no") "[label="//char(34)//trim(this%nodeProperties%data(i)%label)//char(34)//trim(colorOptions)//"],"
				write(effUnit,"(A)",advance="no") "[label="//trim(this%nodeProperties%data(i)%label)//",weight="//trim(adjustl(FString_fromReal(this%nodeProperties%data(i)%weight,format="(F10.5)")))//"],"
			end if
		end do
		
		do i=1,this%nEdges()
			
			loc = 0
			if( .not. this%isDirected() .and. i > 1 ) then
				do j=i-1,1,-1
					if( this%edgeProperties%data(i)%sNode == this%edgeProperties%data(j)%tNode .and. &
						this%edgeProperties%data(i)%tNode == this%edgeProperties%data(j)%sNode ) then
						loc = 1
						exit
					end if
				end do
			end if
			
			if( loc == 1 ) cycle
			
			idNode = this%edgeProperties%data(i)%sNode
			write(effUnit,"(A)",advance="no") trim(FString_fromInteger(idNode))
			
			if( this%isDirected() ) then
				write(effUnit,"(A)",advance="no") "->"
			else
				write(effUnit,"(A)",advance="no") "--"
			end if
			
			idNode = this%edgeProperties%data(i)%tNode
			write(effUnit,"(A)",advance="no") trim(FString_fromInteger(idNode))
			! @todo Hay que poner un parametro que hage swap entre weight y label, para que quede bien el dibujo en dot
! 			write(effUnit,"(A)",advance="no") " [ label = "//char(34)//trim(this%edgeProperties%data(i)%label)//char(34)
! 			write(effUnit,"(A)",advance="no") " [ label = "//char(34)//trim(adjustl(FString_fromReal(this%edgeProperties%data(i)%weight/effEdgePropertiesUnits,format="(F5.2)")))//char(34)

! 			write(effUnit,"(A)",advance="no") "[label="//char(34)//trim(adjustl(FString_fromReal(this%edgeProperties%data(i)%weight/effEdgePropertiesUnits,format="(F2.0)")))//char(34)//","
! 			write(effUnit,"(A)",advance="no") "weight="//trim(adjustl(FString_fromReal(this%edgeProperties%data(i)%weight/effEdgePropertiesUnits,format="(F5.1)")))//"]"

			write(effUnit,"(A)",advance="no") "[label="//trim(adjustl(this%edgeProperties%data(i)%label))//","
			write(effUnit,"(A)",advance="no") "weight="//trim(adjustl(FString_fromReal(this%edgeProperties%data(i)%weight/effEdgePropertiesUnits,format="(F10.5)")))//"]"//","
		end do
		
		close(effUnit)
	end subroutine saveDAT
	
	!>
	!! @brief Breadth First Search
	!!
	subroutine computeDijkstraPaths( this, sNode )
		class(IntegerGraph) :: this
		integer, intent(in) :: sNode
		
		integer :: i, n
		type(RealVector) :: nodeDist
		type(IntegerVector) :: nodeSource
		real(8) :: dist
		integer :: u, v
		real(8) :: distance_through_u
		type(IntegerVector) :: neighbors
		
		this%sNode = sNode
		if( allocated(this%minDistance) ) deallocate(this%minDistance)
		call this%previous%clear()
		
		n = this%node2Neighbors%size()
		allocate( this%minDistance(n) )
		
		this%minDistance = Math_INF
		this%minDistance(sNode) = 0
		call this%previous%init( n, value=-1 )
		
		call nodeDist%init()
		call nodeDist%append( this%minDistance(sNode) )
		call nodeSource%append( sNode )
		
		do while( .not. nodeSource%isEmpty() )
			dist = nodeDist%first()
			u = nodeSource%first()
			
			call nodeDist%erase( 1 )
			call nodeSource%erase( 1 )
			
			!! Visit each edge exiting u
			neighbors = this%node2Neighbors%at(u)
			
			do i=1,neighbors%size()
				v = neighbors%at(i)
				distance_through_u = dist + this%edgeProperties%data( this%getEdgeId(u,v) )%weight
				
				if( distance_through_u < this%minDistance(v) ) then
					call nodeDist%remove( this%minDistance(v) )
					call nodeSource%remove( v )
			
					this%minDistance(v) = distance_through_u
					call this%previous%set( v, u )
					
					call nodeDist%append( this%minDistance(v) );
					call nodeSource%append( v );
				end if
			end do
		end do
	end subroutine computeDijkstraPaths
	
	!>
	!! @brief Returns the distance from source node (@see computeDijkstraPaths) to node tNode.
	!!        If tNode was not reached, it returns -1
	!!
	function distance( this, tNode ) result( output )
		class(IntegerGraph), intent(in) :: this
		integer, intent(in) :: tNode
		real(8) :: output
		
		if( this%sNode == -1 ) then
			output = -1
			return
		end if
		
		output = this%minDistance( tNode )
		if( Math_isInf(output) ) output = -1
	end function distance
	
	!>
	!! @brief Returns the distance from source node (@see computeDijkstraPaths) to node tNode.
	!!        If tNode was not reached, it returns -1
	!!
	function shortestPath( this, tNode ) result( output )
		class(IntegerGraph), intent(in) :: this
		integer, intent(in) :: tNode
		type(IntegerVector) :: output
		
		integer :: node
		
		if( this%sNode == -1 ) return
		
		call output%clear()
		
		node = tNode
		do while( node /= -1 )
			call output.prepend( node )
			node = this%previous%at(node)
		end do
	end function shortestPath
	
	!>
	!! @brief 
	!!
	function adjacencyMatrix( this ) result( output )
		class(IntegerGraph), target :: this
		type(Matrix) :: output
		
		integer :: i, j
		type(IntegerVector), pointer :: iNeighbors
		
		call output%init( this%nNodes(), this%nNodes(), val=0.0_8 )
		
		! Hago todos los i y todos los j para no tener que asumir que es
		! simetrica en el caso no dirigido. Se obtebdra automaticamente
		do i=1,this%nNodes()
			call output%set( i, i, this%nodeProperties%data(i)%weight )
			
			iNeighbors => this%node2Neighbors%data(i)
			
			do j=1,this%nNodes()
				if( i/=j .and. iNeighbors.contains(j) ) then
					call output%set( i, j, this%edgeProperties%data( this%getEdgeId(i,j) )%weight )
				end if
			end do
		end do
		
		iNeighbors => null()
	end function adjacencyMatrix
	
	!>
	!! @brief Returns the distance from source node (@see computeDijkstraPaths) to node tNode.
	!!        If tNode was not reached, it returns -1
	!!
	function distanceMatrix( this ) result( output )
		class(IntegerGraph) :: this
		type(Matrix) :: output
		
		integer :: i, j
		
		call output%init( this%nNodes(), this%nNodes(), val=0.0_8 )
		
		! Hago todos los i y todos los j para no tener que asumir que es
		! simetrica en el caso no dirigido. Se obtebdra automaticamente
		do i=1,this%nNodes()
			call this%computeDijkstraPaths( i )
			
			do j=1,this%nNodes()
				call output%set( i, j, real(this%distance(j),8) )
			end do
		end do
	end function distanceMatrix
	
	!>
	!! @brief
	!!
	function laplacianMatrix( this ) result( output )
		class(IntegerGraph), target, intent(in) :: this
		type(Matrix) :: output
		
		integer :: i, j, edgeId
		type(IntegerVector), pointer :: iNeighbors
		real(8) :: wi
		
		call output%init( this%nNodes(), this%nNodes(), val=0.0_8 )
		
		! Hago todos los i y todos los j para no tener que asumir que es
		! simetrica en el caso no dirigido. Se obtebdra automaticamente
		do i=1,this%nNodes()
			iNeighbors => this%node2Neighbors%data(i)
			
			wi = 0.0_8
			do j=1,iNeighbors%size()
				edgeId = this%getEdgeId( i, iNeighbors%at(j) )
				wi = wi + this%edgeProperties%data( edgeId )%weight
			end do
			
			do j=1,this%nNodes()
				if( i==j ) then
					call output%set( i, i, wi )
				else if( i/=j .and. iNeighbors.contains(j) ) then
					edgeId = this%getEdgeId(i,j)
					call output%set( i, j, -this%edgeProperties%data( edgeId )%weight )
				end if
			end do
		end do
		
		iNeighbors => null()
	end function laplacianMatrix
	
	!>
	!! @brief
	!!
	function resistanceDistanceMatrix( this, laplacianMatrix ) result( output )
		class(IntegerGraph), target :: this
		type(Matrix), optional, intent(in) :: laplacianMatrix
		type(Matrix) :: output
		
		integer :: i, j
		type(Matrix) :: L, Phi, Gamma
		
		if( present(laplacianMatrix) ) then
			L = laplacianMatrix
		else
			L = this%laplacianMatrix()
		end if
		
		call Phi%init( this%nNodes(), this%nNodes(), val=1.0_8/real(this%nNodes(),8) )
		
		Gamma = L + Phi
		Gamma = Gamma.inverse()
		
		call output%init( this%nNodes(), this%nNodes() )
		
		do i=1,this%nNodes()
			do j=1,this%nNodes()
				call output%set( i, j, Gamma%get(i,i)+Gamma%get(j,j)-Gamma%get(i,j)-Gamma%get(j,i) )
			end do
		end do
	end function resistanceDistanceMatrix
	
	!>
	!! @brief 
	!!
	function diameter( this, distanceMatrix ) result( output )
		class(IntegerGraph) :: this
		type(Matrix), optional, intent(in) :: distanceMatrix
		real(8) :: output
		
		type(Matrix) :: dMatrix
		integer :: i, j
		
		if( present(distanceMatrix) ) then
			dMatrix = distanceMatrix
		else
			dMatrix = this%distanceMatrix()
		end if
		
		output = maxval(dMatrix%data)
	end function diameter
	
	!>
	!! @brief 
	!!
	function randicIndex( this ) result( output )
		class(IntegerGraph), target :: this
		real(8) :: output
		
		integer :: i
		real(8) :: vs, vt
		type(Edge), pointer :: edgei
		type(IntegerVector), pointer :: neighbors
		
		output = 0.0_8
		
		do i=1,this%nEdges()
			edgei => this%edgeProperties%data(i)
			
			neighbors => this%node2Neighbors%data(edgei%sNode)
			vs = neighbors%size()
			
			neighbors => this%node2Neighbors%data(edgei%tNode)
			vt = neighbors%size()
			
			output = output + 1.0_8/sqrt(vs*vt)
		end do
		
		if( .not. this%isDirected() ) output = output/2.0_8
		
		neighbors => null()
	end function randicIndex
	
	!>
	!! @brief
	!! https://arxiv.org/pdf/1502.01216.pdf
	!!   In 1997 Klavzar and Gutman [12] suggested a generalization of the Wiener index to vertex-weighted graphs.
	!!      [12] S. Klavˇzar, I. Gutman, Wiener number of vertex–weighted graphs and a chemical
	!!           application, Discr. Appl. Math. 80 (1997) 73–81.
	!! 
	!!
	function wienerIndex( this, distanceMatrix ) result( output )
		class(IntegerGraph) :: this
		type(Matrix), optional, intent(in) :: distanceMatrix
		real(8) :: output
		
		type(Matrix) :: dMatrix
		integer :: i, j
		
		if( present(distanceMatrix) ) then
			dMatrix = distanceMatrix
		else
			dMatrix = this%distanceMatrix()
		end if
		
		output = 0.0_8
		do i=1,this%nNodes()
			do j=1,this%nNodes()
				output = output + this%nodeProperties%data(i)%weight*this%nodeProperties%data(j)%weight*dMatrix%get(i,j)
			end do
		end do
		output = output/2.0_8
	end function wienerIndex
	
	!>
	!! @brief 
	!!
	function inverseWienerIndex( this, distanceMatrix ) result( output )
		class(IntegerGraph) :: this
		type(Matrix), optional, intent(in) :: distanceMatrix
		real(8) :: output
		
		type(Matrix) :: dMatrix
		integer :: i, j
		real(8) :: diam
		
		if( present(distanceMatrix) ) then
			dMatrix = distanceMatrix
		else
			dMatrix = this%distanceMatrix()
		end if
		
		diam = this%diameter( distanceMatrix=dMatrix )
		
		output = 0.0_8
		do i=1,this%nNodes()
			do j=1,this%nNodes()
				if( i/=j ) then
					output = output + ( diam - dMatrix%get(i,j) )
				end if
			end do
		end do
		output = output/2.0_8
	end function inverseWienerIndex
	
	!>
	!! @brief 
	!!
	function balabanIndex( this, distanceMatrix ) result( output )
		class(IntegerGraph), target :: this
		type(Matrix), optional, intent(in) :: distanceMatrix
		real(8) :: output
		
		type(Matrix) :: dMatrix
		integer :: i
		real(8) :: ds, dt
		type(Edge), pointer :: edgei
		
		if( present(distanceMatrix) ) then
			dMatrix = distanceMatrix
		else
			dMatrix = this%distanceMatrix()
		end if
		
		output = 0.0_8
		
		do i=1,this%nEdges()
			edgei => this%edgeProperties%data(i)
			
			ds = sum( dMatrix%data(edgei%sNode,:) )
			dt = sum( dMatrix%data(edgei%tNode,:) )
			
			output = output + 1.0_8/sqrt(ds*dt)
		end do
		
		output = this%nEdges()*output/real(this%nEdges()-this%nNodes()+2,8)
		
		if( .not. this%isDirected() ) output = output/2.0_8
	end function balabanIndex
	
	!>
	!! @brief
	!!         Molecular topological index: a relation with the Wiener index
	!!         Douglas J. Klein, Zlatko Mihalic, Dejan Plavsic, Nenad Trinajstic
	!!         J. Chem. Inf. Comput. Sci., 1992, 32 (4), pp 304–305
	!!
	function molecularTopologicalIndex( this, adjacencyMatrix, distanceMatrix ) result( output )
		class(IntegerGraph), target, intent(in) :: this
		type(Matrix), optional, intent(in) :: adjacencyMatrix
		type(Matrix), optional, intent(in) :: distanceMatrix
		real(8) :: output
		
		type(Matrix) :: A, D
		integer :: i, j
		integer :: vj
		
		if( present(adjacencyMatrix) ) then
			A = adjacencyMatrix
		else
			A = this%adjacencyMatrix()
		end if
		
		if( present(distanceMatrix) ) then
			D = distanceMatrix
		else
			D = this%distanceMatrix()
		end if
		
		! Hago todos los i y todos los j para no tener que asumir que es
		! simetrica en el caso no dirigido. Se obtebdra automaticamente
		output = 0.0_8
		do i=1,this%nNodes()
			do j=1,this%nNodes()
				vj = this%node2Neighbors%data(j)%size()
				output = output + real(vj,8)*( A%get(i,j) + D%get(i,j) )
			end do
		end do
	end function molecularTopologicalIndex
	
	!>
	!! @brief 
	!!
	function kirchhoffIndex( this, resistanceDistanceMatrix ) result( output )
		class(IntegerGraph), target :: this
		type(Matrix), optional, intent(in) :: resistanceDistanceMatrix
		real(8) :: output
		
		type(Matrix) :: Omega
		
		if( present(resistanceDistanceMatrix) ) then
			Omega = resistanceDistanceMatrix
		else
			Omega = this%resistanceDistanceMatrix()
		end if
		
		output = this%wienerIndex( distanceMatrix=Omega )
	end function kirchhoffIndex
	
	!>
	!! @brief 
	!!
	function kirchhoffSumIndex( this, distanceMatrix, resistanceDistanceMatrix ) result( output )
		class(IntegerGraph), target :: this
		type(Matrix), optional, intent(in) :: distanceMatrix
		type(Matrix), optional, intent(in) :: resistanceDistanceMatrix
		real(8) :: output
		
		integer :: i, j
		type(Matrix) :: D, Omega, OmegaD
		
		if( present(distanceMatrix) ) then
			D = distanceMatrix
		else
			D = this%distanceMatrix()
		end if
		
		if( present(resistanceDistanceMatrix) ) then
			Omega = resistanceDistanceMatrix
		else
			Omega = this%resistanceDistanceMatrix()
		end if
		
		OmegaD = Omega
		do i=1,this%nNodes()
			do j=1,this%nNodes()
				if( i/=j ) then
					call OmegaD%set( i, j, OmegaD%get(i,j)/D%get(i,j) )
				end if
			end do
		end do
		
		output = this%wienerIndex( distanceMatrix=OmegaD )
	end function kirchhoffSumIndex
	
	!>
	!! @brief 
	!!
	function wienerSumIndex( this, distanceMatrix, resistanceDistanceMatrix ) result( output )
		class(IntegerGraph), target :: this
		type(Matrix), optional, intent(in) :: distanceMatrix
		type(Matrix), optional, intent(in) :: resistanceDistanceMatrix
		real(8) :: output
		
		integer :: i, j
		type(Matrix) :: D, Omega, DOmega
		
		if( present(distanceMatrix) ) then
			D = distanceMatrix
		else
			D = this%distanceMatrix()
		end if
		
		if( present(resistanceDistanceMatrix) ) then
			Omega = resistanceDistanceMatrix
		else
			Omega = this%resistanceDistanceMatrix()
		end if
		
		DOmega = Omega
		do i=1,this%nNodes()
			do j=1,this%nNodes()
				if( i/=j ) then
					call DOmega%set( i, j, D%get(i,j)/DOmega%get(i,j) )
				end if
			end do
		end do
		
		output = this%wienerIndex( distanceMatrix=DOmega )
	end function wienerSumIndex
	
	!>
	!! @brief 
	!!
	function JOmegaIndex( this, distanceMatrix, resistanceDistanceMatrix ) result( output )
		class(IntegerGraph), target :: this
		type(Matrix), optional, intent(in) :: distanceMatrix
		type(Matrix), optional, intent(in) :: resistanceDistanceMatrix
		real(8) :: output
		
		integer :: i, j
		type(Matrix) :: D, Omega, OmegaD
		
		if( present(distanceMatrix) ) then
			D = distanceMatrix
		else
			D = this%distanceMatrix()
		end if
		
		if( present(resistanceDistanceMatrix) ) then
			Omega = resistanceDistanceMatrix
		else
			Omega = this%resistanceDistanceMatrix()
		end if
		
		OmegaD = Omega
		do i=1,this%nNodes()
			do j=1,this%nNodes()
				if( i/=j ) then
					call OmegaD%set( i, j, OmegaD%get(i,j)/D%get(i,j) )
				end if
			end do
		end do
		
		output = this%balabanIndex( distanceMatrix=OmegaD )
	end function JOmegaIndex
	
	!>
	!! @brief 
	!! @todo This method is almost the same used in the class Molecule. Thay have to be merged in the future
	!!
	function indicesBasedComparison( this, other, thr, checkLabels, similarity ) result( output )
		class(IntegerGraph), intent(in) :: this
		class(IntegerGraph), intent(in) :: other
		real(8), optional, intent(in) :: thr
		logical, optional, intent(in) :: checkLabels
		real(8), optional, intent(out) :: similarity
		logical :: output
		
		real(8):: effThr
		logical:: effCheckLabels
		
		real(8) :: this_descrip(9), other_descrip(9)
		real(8) :: effSimilarity
		type(Matrix) :: A, D, L, Omega
		Type(StringIntegerMap) :: labelsMapThis, labelsMapOther
		type(String) :: sBuffer
		type(String) :: labelA, labelB
		integer :: i
		real(8) :: w1, w2
		
		effThr = 0.92_8
		if( present(thr) ) effThr = thr
		
		effCheckLabels = .false.
		if( present(checkLabels) ) effCheckLabels = checkLabels
		
		if( this%nNodes() /= other%nNodes() .or. this%nEdges() /= other%nEdges() ) then
			output = .false.
			return
		end if
		
		if( effCheckLabels ) then
			call labelsMapThis%init()
			do i=1,this%nNodes()
				sBuffer = trim(this%nodeProperties%data(i)%label)
				call labelsMapThis%set( sBuffer, labelsMapThis%at( sBuffer, defaultValue=0 )+1 )
			end do
			
			call labelsMapOther%init()
			do i=1,other%nNodes()
				sBuffer = trim(other%nodeProperties%data(i)%label)
				call labelsMapOther%set( sBuffer, labelsMapOther%at( sBuffer, defaultValue=0 )+1 )
			end do
			
			if( labelsMapThis%size() /= labelsMapOther%size() ) then
				output = .false.
				return
			end if
			
			do i=1,labelsMapThis%size()
				labelA = labelsMapThis.keyFromPos(i)
				labelB = labelsMapOther.keyFromPos(i)
				
				if( trim(labelA%fstr) /= trim(labelB%fstr) .or. labelsMapThis%atFromPos(i) /= labelsMapOther%atFromPos(i) ) then
					output = .false.
					return
				end if
			end do
		end if
		
		if( this%nNodes() == 1 ) then
			this_descrip = 0.0_8
			other_descrip = 0.0_8
			
			this_descrip(1) = this%nodeProperties%data(1)%weight
			other_descrip(1) = other%nodeProperties%data(1)%weight
			
		! Be Careful. Remember that undirected graphs include at least two directed edges for each undirected edge.
! 		if( .not. this%isDirected() .and. this%nNodes() == 2 ) then
! 			w1 = this%edgeProperties%data(i)%weight
! 			w2 = other%edgeProperties%data(i)%weight
! 			
! 			effSimilarity = 1.0_8 - abs( w1-w2 )/max(w1,w2)
! ! 			effSimilarity = 1.0_8/( 1.0_8+abs(w1-w2)/2 )
! 			
! 			output = .false.
! 			if( effSimilarity > effThr ) output = .true.
! 			
! 			if( present(similarity) ) similarity = effSimilarity
! 			
! 			return
! 		end if
		else
			A = this%adjacencyMatrix()
			D = this%distanceMatrix()
			L = this%laplacianMatrix()
			
			this_descrip(1) = this%randicIndex()
			this_descrip(2) = this%wienerIndex( distanceMatrix=D )
			this_descrip(3) = this%inverseWienerIndex( distanceMatrix=D )
			this_descrip(4) = this%balabanIndex( distanceMatrix=D )
			this_descrip(5) = this%molecularTopologicalIndex( adjacencyMatrix=A, distanceMatrix=D )
			
			if( other%nComponents( laplacianMatrix=L ) > 1 ) then
				this_descrip(6:9) = 0.0_8
			else
				Omega = this%resistanceDistanceMatrix( laplacianMatrix=L )
				
				this_descrip(6) = this%kirchhoffIndex( resistanceDistanceMatrix=Omega )
				this_descrip(7) = this%kirchhoffSumIndex( distanceMatrix=D, resistanceDistanceMatrix=Omega )
				this_descrip(8) = this%wienerSumIndex( distanceMatrix=D, resistanceDistanceMatrix=Omega )
				this_descrip(9) = this%JOmegaIndex( distanceMatrix=D, resistanceDistanceMatrix=Omega )
			end if
			
			A = other%adjacencyMatrix()
			D = other%distanceMatrix()
			L = other%laplacianMatrix()
			
			other_descrip(1) = other%randicIndex()
			other_descrip(2) = other%wienerIndex( distanceMatrix=D )
			other_descrip(3) = other%inverseWienerIndex( distanceMatrix=D )
			other_descrip(4) = other%balabanIndex( distanceMatrix=D )
			other_descrip(5) = other%molecularTopologicalIndex( adjacencyMatrix=A, distanceMatrix=D )
			
			if( other%nComponents( laplacianMatrix=L ) > 1 ) then
				other_descrip(6:9) = 1000.0_8
			else
				Omega = other%resistanceDistanceMatrix( laplacianMatrix=L )
				
				other_descrip(6) = other%kirchhoffIndex( resistanceDistanceMatrix=Omega )
				other_descrip(7) = other%kirchhoffSumIndex( distanceMatrix=D, resistanceDistanceMatrix=Omega )
				other_descrip(8) = other%wienerSumIndex( distanceMatrix=D, resistanceDistanceMatrix=Omega )
				other_descrip(9) = other%JOmegaIndex( distanceMatrix=D, resistanceDistanceMatrix=Omega )
			end if
		end if
		
		! Jaccard similarity index
! 		this_descrip  =  this_descrip + abs(minval(this_descrip))
! 		other_descrip = other_descrip + abs(minval(other_descrip))
! 		effSimilarity = sum( min(this_descrip,other_descrip) )
! 		effSimilarity = effSimilarity/sum( max(this_descrip,other_descrip) )
		
		! Native Ballester similarity index
		effSimilarity = 1.0_8/( 1.0_8+sum(abs( this_descrip-other_descrip ))/real(size(this_descrip),8) )
		
		output = .false.
		if( effSimilarity > effThr ) output = .true.
		
		if( present(similarity) ) similarity = effSimilarity
		
! 		call this%show( formatted=.true. )
! 		call other%show( formatted=.true. )
! 		write(*,"(A,<size(this_descrip)>F8.3)") "   Descriptor1 = ", this_descrip
! 		write(*,"(A,<size(this_descrip)>F8.3)") "   Descriptor2 = ", other_descrip
! 		write(*,"(A,F8.3)") "   similarity = ", effSimilarity
		
	end function indicesBasedComparison
	
	!>
	!! @brief 
	!!
	function inducedSubgraph( this, nodeIds ) result( output )
		class(IntegerGraph), intent(in) :: this
		integer, intent(in) :: nodeIds(:)
		type(IntegerGraph) :: output
		
		integer :: i, j, id
		integer :: sPos, tPos
		integer, allocatable :: transferIds(:)
		
		call output%init( directed=this%isDirected() )
		
		if( any( nodeIds > this%nNodes() ) ) then
			return
		end if
		
		allocate( transferIds(size(nodeIds)) )
		
		do i=1,size(nodeIds)
			call output%newNode( label=this%nodeProperties%data( nodeIds(i) )%label, weight=this%nodeProperties%data( nodeIds(i) )%weight, id=id )
			transferIds( i ) = id
		end do
		
		do i=1,this%nEdges()
			sPos = 0
			do j=1,size(nodeIds)
				if( this%edgeProperties%data(i)%sNode == nodeIds(j) ) then
					sPos = j
					exit
				end if
			end do
			
			tPos = 0
			do j=1,size(nodeIds)
				if( this%edgeProperties%data(i)%tNode == nodeIds(j) ) then
					tPos = j
					exit
				end if
			end do
			
			if( sPos /= 0 .and. tPos /= 0 ) then
				call output%newEdge( transferIds(sPos), transferIds(tPos), weight=this%edgeProperties%data(i)%weight, id=id )
			end if
		end do
		
		if( allocated(transferIds) ) deallocate(transferIds)
	end function inducedSubgraph
	
	!>
	!! @brief Returns .true. if the nodeId is exclusive to the given subGraphNodes.
	!!        That is, is not already in the subgraph, and is not adjacent to any of
	!!        the nodes in the subgraph)
	!!
	function isExclusive( this, nodeId, subGraphNodes ) result( output )
		class(IntegerGraph), target, intent(in) :: this
		integer, intent(in) :: nodeId
		type(IntegerVector), optional, intent(in) :: subGraphNodes
		logical :: output
		
		integer :: i
		type(IntegerVector), pointer :: ivec
		
		output = .true.
		
		if( subGraphNodes.contains( nodeId ) ) then
			output = .false.
			return
		end if
		
		do i=1,subGraphNodes%size()
			ivec => this%node2Neighbors%data( subGraphNodes%at(i) )
			
			if( ivec.contains( nodeId ) ) then
				output = .false.
				return
			end if
			
			ivec => null()
		end do
	end function isExclusive
	
	!>
	!! @brief Returns a type(IntegerVector) object containing the neighbors for the node nodeId
	!!
	function neighbors( this, nodeId, exclusive, minId ) result( output )
		class(IntegerGraph), target, intent(in) :: this
		integer, intent(in) :: nodeId
		type(IntegerVector), optional, intent(in) :: exclusive
		integer, optional :: minId
		type(IntegerVector) :: output
		
		type(IntegerVector), pointer :: ivec
		integer :: i
		logical :: isExclusive
		
		ivec => this%node2Neighbors%data( nodeId )
		
		call output%init()
		do i=1,ivec%size()
			if( present(exclusive) .and. present(minId) ) then
				if( this%isExclusive( ivec%at(i), exclusive ) .and. ivec%at(i) > minId ) then
					call output%append( ivec%at(i) )
				end if
			else if( present(exclusive) ) then
				if( this%isExclusive( ivec%at(i), exclusive ) ) then
					call output%append( ivec%at(i) )
				end if
			else if( present(minId) ) then
				if( ivec%at(i) > minId ) then
					call output%append( ivec%at(i) )
				end if
			else
				call output%append( ivec%at(i) )
			end if
		end do
		
		ivec => null()
	end function neighbors
	
	!>
	!! @brief Makes a complete graph with n nodes
	!!
	subroutine makeComplete( this, n )
		class(IntegerGraph), intent(inout) :: this
		integer, intent(in) :: n
		
		integer :: i, j
		
		call this%newNodes( n )
		
		do i=1,this%nNodes()
			do j=1,this%nNodes()
				if( i/=j ) then
					call this%newEdge( i, j )
				end if
			end do
		end do
	end subroutine makeComplete
	
	!>
	!! @brief Test method
	!!
	subroutine IntegerGraph_test()
		type(IntegerGraph) :: mygraph, mysubgraph
		type(IntegerGraph), allocatable :: mymotifs(:)
		integer, allocatable :: mymotifsFreq(:)
! 		class(IntegerHyperVectorIterator), pointer :: iter

		type(IntegerVector) :: ivec
		integer :: id
		type(IntegerVector) :: path
		
		type(Matrix) :: AMatrix, dMatrix, LMatrix, OmegaMatrix
		
		integer :: i
		type(Node) :: nodeProp
		
		call mygraph%init( directed=.false. )
		
		!------------------------------------------
		! Ejemplo 0
		!
		!        (1)          
		!         |           1 -->   2
		!        (2)          2 -->   1,   3,   4
		!       /   \         3 -->   2,   5
		!     (3)   (4)       4 -->   2,   5
		!       \   /         5 -->   3,   4
		!        (5)
		
		call mygraph%newNode()
		call mygraph%newNode()
		call mygraph%newNode()
		call mygraph%newNode()
		call mygraph%newNode()
		
		call mygraph%newEdges( 1, [2] )
		call mygraph%newEdges( 2, [1,3,4] )
		call mygraph%newEdges( 3, [2,5] )
		call mygraph%newEdges( 4, [2,5] )
		call mygraph%newEdges( 5, [3,4] )
		
		call mygraph%show( formatted=.true. )
! 		
! 		write(*,*) ">> Deleting node 1"
! 		call mygraph.deleteNode( 1 )
! 		call mygraph%show( formatted=.true. )
! 
! 		write(*,*) ">> Deleting node 3"
! 		call mygraph.deleteNode( 3 )
! 		call mygraph%show( formatted=.true. )

		write(*,*) ">> Deleting edge 6"
		call mygraph.deleteEdge( 6 )
		call mygraph%show( formatted=.true. )
! 		
! 		call mygraph%save( "salida.gml", format=GML )
! 		call mygraph%save( "salida.dot", format=DOT )
		
! 		stop
		
		!------------------------------------------
		! Ejemplo 1
! 		call mygraph%newNode()
! 		call mygraph%newNode()
! 		call mygraph%newNode()
! 		call mygraph%newNode()
! 		call mygraph%newNode()
! 		call mygraph%newNode()
! 		
! 		call mygraph%newEdges( 1, [2,3,6] )
! 		call mygraph%newEdges( 2, [1,3,4] )
! 		call mygraph%newEdges( 3, [1,2,4,6] )
! 		call mygraph%newEdges( 4, [2,3,5] )
! 		call mygraph%newEdges( 5, [4,6] )
! 		call mygraph%newEdges( 6, [1,3,5] )

! 		!------------------------------------------
! 		! Ejemplo 3
! 		call mygraph%newNode()
! 		call mygraph%newNode()
! 		call mygraph%newNode()
! 		call mygraph%newNode()
! 		call mygraph%newNode()
! 		call mygraph%newNode()
! 		call mygraph%newNode()
! 		call mygraph%newNode()
! 		
! 		call mygraph%newEdges( 1, [2,3,4] )
! 		call mygraph%newEdges( 2, [5,1] )
! 		call mygraph%newEdges( 3, [8] )
! 		call mygraph%newEdges( 4, [1] )
! 		call mygraph%newEdges( 5, [2,6,7] )
! 		call mygraph%newEdges( 6, [5] )
! 		call mygraph%newEdges( 7, [5] )
! 		call mygraph%newEdges( 8, [3] )
! 
! 		!------------------------------------------
! 		! Ejemplo 2
! ! 		call mygraph%newNode()
! ! 		call mygraph%newNode()
! ! 		call mygraph%newNode()
! ! 		call mygraph%newNode()
! ! 		call mygraph%newNode()
! ! 		call mygraph%newNode()
! ! 		
! ! 		call mygraph%newEdges( 1, [2] )
! ! 		call mygraph%newEdges( 2, [1,3,4] )
! ! 		call mygraph%newEdges( 3, [2,4] )
! ! 		call mygraph%newEdges( 4, [2,3,5] )
! ! 		call mygraph%newEdges( 5, [4,6] )
! ! 		call mygraph%newEdges( 6, [5] )
! 
! 		call mygraph%show()
! 		
! 		call mygraph.computeDijkstraPaths( 8 )
! 		write(*,*) "distance from 1 to 6 = ", mygraph.distance(7)
! 		path = mygraph.shortestPath(7)
! 		call path%show()

		!------------------------------------------
		! Ejemplo Diego
! 		call mygraph%newNode()
! 		call mygraph%newNode()
! 		call mygraph%newNode()
! 		call mygraph%newNode()
! 		
! 		call mygraph%newEdges( 1, [2,3,4], [1.01_8,1.43_8,1.01_8] )
! 		call mygraph%newEdges( 2, [1,4,3], [1.01_8,1.43_8,1.01_8] )
! 		call mygraph%newEdges( 3, [2,1,4], [1.01_8,1.43_8,1.01_8] )
! 		call mygraph%newEdges( 4, [1,2,3], [1.01_8,1.43_8,1.01_8] )
		
		call mygraph%show( formatted=.true. )
		
		call mygraph.computeDijkstraPaths( 1 )
		write(*,*) "Distance from 1 to 3 = ", mygraph.distance(3)
		path = mygraph.shortestPath(3)
		write(*,*) "Path(1,3) = "
		call path%show()
		
		write(*,*) "Adjacency matrix"
		AMatrix = mygraph%adjacencyMatrix()
		call AMatrix%show( formatted=.true. )
		write(*,*) ""
		write(*,*) "Distance matrix"
		dMatrix = mygraph%distanceMatrix()
		call dMatrix%show( formatted=.true. )
		write(*,*) ""
		write(*,*) "Laplacian matrix"
		LMatrix = mygraph%laplacianMatrix()
		call LMatrix%show( formatted=.true. )
		write(*,*) ""
		write(*,*) "Resistance-Distance matrix"
		OmegaMatrix = mygraph%resistanceDistanceMatrix( laplacianMatrix=LMatrix )
		call OmegaMatrix%show( formatted=.true. )
		write(*,*) ""
		write(*,*) "Indices"
		write(*,*) "-------"
		write(*,*) "Randic               = ", mygraph%randicIndex()
		write(*,*) "Wiener               = ", mygraph%wienerIndex( distanceMatrix=dMatrix )
		write(*,*) "Wiener               = ", mygraph%inverseWienerIndex( distanceMatrix=dMatrix )
		write(*,*) "Balaban              = ", mygraph%balabanIndex( distanceMatrix=dMatrix )
		write(*,*) "MolecularTopological = ", mygraph%molecularTopologicalIndex( adjacencyMatrix=AMatrix, distanceMatrix=dMatrix )
		write(*,*) "Kirchhoff            = ", mygraph%kirchhoffIndex( resistanceDistanceMatrix=OmegaMatrix )
		write(*,*) "KirchhoffSum         = ", mygraph%kirchhoffSumIndex( distanceMatrix=dMatrix, resistanceDistanceMatrix=OmegaMatrix )
		write(*,*) "wienerSum            = ", mygraph%wienerSumIndex( distanceMatrix=dMatrix, resistanceDistanceMatrix=OmegaMatrix )
		write(*,*) "JOmega               = ", mygraph%JOmegaIndex( distanceMatrix=dMatrix, resistanceDistanceMatrix=OmegaMatrix )
		
! 		write(*,*) "idEdge(3,4) = ", mygraph%getEdgeId( 3, 4 )
! 		write(*,*) "idEdge(4,3) = ", mygraph%getEdgeId( 4, 3 )
! 		write(*,*) "idEdge(2,4) = ", mygraph%getEdgeId( 2, 4 )
		
		write(*,*) ""
		write(*,*) "Apollonian Network 2"
		write(*,*) "--------------------"
		call mygraph%init()
		
		call mygraph%newNodes( 7 )
		call mygraph%newEdges( 1, [2,5,4,6,3] )
		call mygraph%newEdges( 2, [1,5,4,7,3] )
		call mygraph%newEdges( 3, [1,6,4,7,2] )
		call mygraph%newEdges( 4, [1,5,2,7,3,6] )
		call mygraph%newEdges( 5, [1,2,4] )
		call mygraph%newEdges( 6, [1,4,3] )
		call mygraph%newEdges( 7, [3,4,2] )
		
		write(*,*) "Index                             expected                obtained"
		write(*,*) "Wiener                 ", 27.0_8, mygraph%wienerIndex()
		write(*,*) "MolecularTopological   ", 360.0_8, mygraph%molecularTopologicalIndex()
		write(*,*) "Kirchhoff              ", 834.0_8/85.0_8, mygraph%kirchhoffIndex()
		write(*,*) "KirchhoffSum           ", 672.0_8/85.0_8, mygraph%kirchhoffSumIndex()
		
		write(*,*) ""
		write(*,*) "============================"
		write(*,*) " Testing inducedSubgraph"
		write(*,*) "============================"
		call mygraph%init()
		
		call mygraph%newNodes( 5, labels=["1.a","2.a","3.a","4.a","5.a"], weights=[1.0_8,2.0_8,3.0_8,4.0_8,5.0_8] )
		call mygraph%newEdges( 1, [2,3], weights=[1.2_8,1.3_8] )
		call mygraph%newEdges( 2, [1,3], weights=[2.1_8,2.3_8] )
		call mygraph%newEdges( 3, [1,2,4,5], weights=[3.1_8,3.2_8,3.4_8,3.5_8] )
		call mygraph%newEdges( 4, [3], weights=[4.3_8] )
		call mygraph%newEdges( 5, [3], weights=[5.3_8] )
		
		call mygraph%show( formatted=.true. )
		
		write(*,*) ">>>>> inducedSubgraph [1,2,3]"
		mysubgraph = mygraph.inducedSubgraph( [1,2,3] )
		call mysubgraph%show( formatted=.true. )
		write(*,*) ""
		write(*,*) ">>>>> inducedSubgraph [1,3,4]"
		mysubgraph = mygraph.inducedSubgraph( [1,3,4] )
		call mysubgraph%show( formatted=.true. )
		
! 		call mysubgraph%save( "salida.gml", format=GML )
! 		call mysubgraph%save( "salida.dot", format=DOT )
! 		call mysubgraph%save( "salida.dat", format=DAT )
! 		call mysubgraph%save( "salida.dat", format=DAT, append=.true. )
		
		call mysubgraph%initFromDATLine( "1[label=1.a,weight=1.00000];2[label=3.a,weight=3.00000];3[label=4.a,weight=4.00000];1--2[label=3.a--1.a,weight=1.30000];2--3[label=4.a--3.a,weight=3.40000];" )
		call mysubgraph%show( formatted=.true. )
		write(*,"(A)") mysubgraph.toDATString()
		
	end subroutine IntegerGraph_test

end module IntegerGraph_

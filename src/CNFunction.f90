!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                                                                                   !!
!!  This file is part of SciFT project                                               !!
!!  Copyright (c) 2010-2014 Nestor F. Aguirre (nfaguirrec@gmail.com)                 !!
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

module CNFunction_
	use GOptions_
	use Grid_
	use String_
	use IOStream_
	implicit none
	private
	
	public :: &
		CNFunction_checkTypeN1DF, &
		CNFunction_getFormatIdFromFileExtension, &
		CNFunction_test
		
!>
!! This class use the List template declared into List.h90 file,
!! please take a look to this file for details
!!
#define NFunction_checkTypeN1DF CNFunction_checkTypeN1DF
#define NFunction_getFormatIdFromFileExtension CNFunction_getFormatIdFromFileExtension
#define NFunction CNFunction
#define __TYPE_VALUE__ complex(8)
#define __ADD_METHODS__
#define __ID_TYPE__ 1
#include "NFunction.h90"
#undef NFunction
#undef __TYPE_VALUE__
#undef __ADD_METHODS__
#undef __ID_TYPE__

	!>
	!! @brief Constructor
	!!
	function fromFStream( stream, columns, cComments, units ) result( this )
		type(CNFunction) :: this
		type(IFStream), intent(in) :: stream
		integer, optional, intent(in) :: columns(:)
		character(*), optional, intent(in) :: cComments
		real(8), optional, intent(in) :: units(2)
		
		integer :: nData, nPoints
		integer, allocatable :: columnsEff(:)
		character(:), allocatable :: cCommentsEff
		character(:), allocatable :: line
		type(String) :: buffer
		character(20), allocatable :: tokens(:)
		real(8), allocatable :: x(:)
		complex(8), allocatable :: y(:)
		real(8) :: effUnits(2)
		real(8) :: rBuffer
		
		if( present(cComments) ) then
			cCommentsEff = cComments
		else
			cCommentsEff = "#"
		end if
		
		if( present(columns) ) then
			allocate( columnsEff(size(columns)) )
			columnsEff = columns
		else
			allocate( columnsEff(3) )
			columnsEff = [1,2,3]
		end if
		
		if( present(units) ) then
			effUnits = units
		else
			effUnits = [1.0_8, 1.0_8]
		end if
		
		! El peor de los casos es que todas las lineas sean datos
		allocate( x(stream.numberOfLines) )
		allocate( y(stream.numberOfLines) )
		
		nData = 1
		do while( .not. stream.eof() )
			line = stream.readLine( cCommentsEff )
			
			if( len(line) /= 0 ) then
				call buffer.fromFString( line )
				call buffer.split( tokens, " " )
				
				if( nData == 1 .and. .not. ( size(tokens) >= size(columnsEff) ) ) then
					write(*,*) "### ERROR ### NFunction.fromFStream(): Number of columns in file ("&
									//trim(FString_fromInteger(size(tokens)))&
									//") are not in agree with the parameter columns ("&
									//trim(FString_fromInteger(size(columnsEff)))//"). nFile >= nCols"
					stop
				end if
				
				if( size(columnsEff) == 3 ) then
					if( columnsEff(1) <= size(tokens) .and. columnsEff(2) <= size(tokens) .and. columnsEff(3) <= size(tokens) ) then
						if( len_trim(tokens(columnsEff(1))) /= 0 .and. len_trim(tokens(columnsEff(2))) /= 0 .and. len_trim(tokens(columnsEff(3))) /= 0 ) then
							read( tokens(columnsEff(1)),* ) x(nData)
							
							buffer = "("//trim(tokens(columnsEff(2)))//", "//trim(tokens(columnsEff(3)))//")"
							read( buffer.fstr,* ) y(nData)
							nData = nData + 1
						end if
					end if
				else if( size(columnsEff) == 2 ) then
					if( columnsEff(1) <= size(tokens) .and. columnsEff(2) <= size(tokens) ) then
						if( len_trim(tokens(columnsEff(1))) /= 0 .and. len_trim(tokens(columnsEff(2))) /= 0 ) then
							read( tokens(columnsEff(1)),* ) x(nData)
							read( tokens(columnsEff(2)),* ) rBuffer
							
							y(nData) = cmplx( rBuffer, 0.0_8 )
							
							nData = nData + 1
						end if
					end if
				else
					write(*,*) "### ERROR ### NFunction.fromFStream(). Bad number of tokens while reading file"
					stop
				end if
			end if
		end do
		
		nPoints = nData-1
		call this.xGrid.fromArray( x(1:nPoints) )
		if( allocated(this.fArray) ) deallocate(this.fArray)
		allocate( this.fArray(nPoints) )
		this.fArray = y(1:nPoints)
		call this.setUnits( effUnits )
		
		call this.checkEquallyspaced()
		
		deallocate(x)
		deallocate(y)
		deallocate(columnsEff)
	end function fromFStream
	
	!>
	!! @brief String representation of the object
	!!
	function str( this ) result( output )
		class(CNFunction) :: this 
		character(len=200) :: output
		
		integer :: fmt
		character(len=200) :: strBuffer
		
		output = ""
		
		output = trim(output)//"<CNFunction:"
		
		output = trim(output)//this.xGrid.str()
		
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
	
! 	!>
! 	!! Save the data in two column format in a
! 	!! selected unit
! 	!!
! 	subroutine toFStream( this, ofile, units, resolution, xrange, ixrange, beforeLine )
! 		class(CNFunction) :: this
! 		type(OFStream), optional, intent(in) :: ofile
! 		real(8), optional, intent(in) :: units(2)
! 		real(8), optional, intent(in) :: resolution
! 		real(8), optional, intent(in) :: xrange(2)
! 		integer, optional, intent(in) :: ixrange(2)
! 		character(*), optional, intent(in) :: beforeLine
! 		
! 		integer :: effUnit
! 		real(8) :: effUnits(2)
! 		integer :: effIXRange(2)
! 		character(100) :: effBeforeLine
! 		integer :: invResolution
! 		
! 		integer :: i
! 		character(255) :: date
! 		
! 		effUnit = IO_STDOUT
! 		if( present(ofile) ) effUnit = ofile.unit
! 		
! 		effUnits = [1.0_8, 1.0_8]
! 		if( present(units) ) effUnits = units
! 		
! 		effIXRange = [1,this.nPoints()]
! 		if( present(xrange) ) then
! 			effIXRange = [ &
! 				floor( 1.0000001*(xrange(1)-this.xGrid.min)/this.xGrid.stepSize+1.0 ), &
! 				floor( 1.0000001*(xrange(2)-this.xGrid.min)/this.xGrid.stepSize+1.0 ) ]
! 		else if( present(ixrange) ) then
! 			effIXRange = ixrange
! 		end if
! 		
! 		effBeforeLine = ""
! 		if( present(beforeLine) ) effBeforeLine = beforeLine
! 		
! 		invResolution = 1
! 		if( present(resolution) ) invResolution = ceiling( 1.0_8/resolution )
! 		
! 		call fdate(date)
! 		
! 		write(effUnit,"(A)") "# Complex Numerical Function"
! 		write(effUnit,"(A)") "# "//trim(date)
! 		
! 		do i=effIXRange(1),effIXRange(2),invResolution
! 			if( abs(this.fArray( i )) > 1d-98 ) then
! 				write(effUnit,"(A,E15.7,2E15.7)") trim(effBeforeLine), this.xGrid.data(i)/effUnits(1), &
! 					real(this.fArray( i ))/effUnits(2), &
! 					aimag(this.fArray( i ))/effUnits(2)
! 			else
! 				write(effUnit,"(A,E15.7,2E15.7)") trim(effBeforeLine), this.xGrid.data(i)/effUnits(1), 0.0_8, 0.0_8
! 			end if
! 		end do
! 		
! 		write(effUnit,"(a)") ""
! 		write(effUnit,"(a)") ""
! 	end subroutine toFStream
	
	!>
	!! This is neccesary only for CNFunction_test()
	!!       f = exp(-0.44*x)*sin(x)**2
	!!   df/dx = exp(-0.44*x)*(2.0*sin(x)*cos(x)-0.44*sin(x)**2)
	!! d2f/dx2 = exp(-0.44*x)*(2.0*cos(x)**2 - 1.76*cos(x)*sin(x) - 2.0*sin(x)**2 + 0.1936*sin(x)**2)
	!!
	function funcTest( x ) result( output )
		real(8), intent(in) :: x
		complex(8) :: output
		
		output = exp(-0.44*x)*sin(x)**2.0_8 + cmplx(0.0_8,1.0_8)*cos(x)
	end function funcTest
	
	!>
	!! This is neccesary only for CNFunction_test()
	!!       f = exp(-0.44*x)*sin(x)**2
	!!   df/dx = exp(-0.44*x)*(2.0*sin(x)*cos(x)-0.44*sin(x)**2)
	!! d2f/dx2 = exp(-0.44*x)*(2.0*cos(x)**2 - 1.76*cos(x)*sin(x) - 2.0*sin(x)**2 + 0.1936*sin(x)**2)
	!!
	function dfuncTest( x ) result( output )
		real(8), intent(in) :: x
		complex(8) :: output
		
		output = exp(-0.44*x)*(2.0*sin(x)*cos(x)-0.44*sin(x)**2) - cmplx(0.0_8,1.0_8)*sin(x)
	end function dfuncTest
	
	!>
	!! This is neccesary only for CNFunction_test()
	!!       f = exp(-0.44*x)*sin(x)**2
	!!   df/dx = exp(-0.44*x)*(2.0*sin(x)*cos(x)-0.44*sin(x)**2)
	!! d2f/dx2 = exp(-0.44*x)*(2.0*cos(x)**2 - 1.76*cos(x)*sin(x) - 2.0*sin(x)**2 + 0.1936*sin(x)**2)
	!!
	function d2funcTest( x ) result( output )
		real(8), intent(in) :: x
		complex(8) :: output
		
		output = exp(-0.44*x)*(2.0*cos(x)**2 - 1.76*cos(x)*sin(x) - 2.0*sin(x)**2 + 0.1936*sin(x)**2) - cmplx(0.0_8,1.0_8)*cos(x)
	end function d2funcTest
	
	!>
	!! This is neccesary only for CNFunction_test()
	!!
	function funcTest2( x ) result( output )
		real(8), intent(in) :: x
		complex(8) :: output
		
		output = sin(x) + cmplx(0.0_8,1.0_8)*cos(x)
	end function funcTest2
	
	!>
	!! @test Testing the CNFunction class
	!! @brief Testing the CNFunction class
	!!
	subroutine CNFunction_test()
		type(Grid) :: xGrid
		type(Grid) :: xGrid2
		type(IFStream) :: ifile
		type(OFStream) :: ofile
		type(CNFunction) :: nFunc
		type(CNFunction) :: nFunc2
		type(CNFunction) :: nFunc3
		complex(8) :: value
		complex(8), allocatable :: data(:)
		integer :: i
		
		call xGrid.init( 1.0_8, 10.0_8, 100 )
		call xGrid.show()
		
		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		! Test from function
		write(*,*) "---"
		write(*,*) "Testing from function"
		write(*,*) "---"
		
		nFunc = CNFunction( xGrid, func=funcTest )
		call nFunc.show()
		call nFunc.save( "salida1", format=BLKS_FORMAT )
		
		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		! Test for copy constructor
		write(*,*) "---"
		write(*,*) "Testing copy constructor"
		write(*,*) "---"
		
		call nFunc2.copy( nFunc )
		call nFunc2.show()
		
! 		call ofile.init( "salida2" )
! 		call nFunc.toFStream( ofile )
! 		call ofile.close()
		
		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		! Test from array
		write(*,*) "---"
		write(*,*) "Testing from array"
		write(*,*) "---"
		
		allocate( data(xGrid.nPoints) )
		do i=1,xGrid.nPoints
			data(i) = funcTest( xGrid.data(i) )
		end do
		
		nFunc = CNFunction( xGrid, fArray=data )
		call nFunc.show()
		call nFunc.save( "salida3", format=BLKS_FORMAT )
		
		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		! Test from IFStream
		write(*,*) "---"
		write(*,*) "Testing from IFStream"
		write(*,*) "---"
		
		call ifile.init( "data/formats/TWO_COLUMNS" )
		nFunc = CNFunction( ifile, columns=[1,2] )
		call nFunc.show()
		call ifile.close()
		call nFunc.save( "salidaF0", format=BLKS_FORMAT )
		
! 		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 		! Test operators
! 		write(*,*) "---"
! 		write(*,*) "Testing operators"
! 		write(*,*) "---"
! 		
! 		call xGrid.init( 1.0_8, 10.0_8, 100 )
! 		call nFunc.fromFunction( xGrid, func=funcTest )
! 		call nFunc2.fromFunction( xGrid, func=funcTest2 )
! 		call nFunc.save( "salidaF1" )
! 		call nFunc2.save( "salidaF2" )
! 		
! 		nFunc3 = nFunc+nFunc2
! 		call nFunc3.save( "salidaF1aF2" )
! 		
! 		nFunc3 = nFunc+3.0_8
! 		call nFunc3.save( "salidaF1a3.0" )
! 		
! 		nFunc3 = nFunc-nFunc2
! 		call nFunc3.save( "salidaF1mF2" )
! 		
! 		nFunc3 = nFunc-3.0_8
! 		call nFunc3.save( "salidaF1m3.0" )
! 		
! 		nFunc3 = nFunc*nFunc2
! 		call nFunc3.save( "salidaF1pF2" )
! 		
! 		nFunc3 = nFunc*3.0_8
! 		call nFunc3.save( "salidaF1p3.0" )
! 		
! 		nFunc3 = nFunc/nFunc2
! 		call nFunc3.save( "salidaF1dF2" )
! 		
! 		nFunc3 = nFunc/3.0_8
! 		call nFunc3.save( "salidaF1d3.0" )
! 		
! 		nFunc3 = nFunc**nFunc2
! 		call nFunc3.save( "salidaF1ppF2" )
! 		
! 		nFunc3 = nFunc**2.0_8
! 		call nFunc3.save( "salidaF1pp3.0" )

		write(*,*) "===================================================================="
		write(*,*) " TESTING RESIZE"
		write(*,*) "===================================================================="
		write(*,*) ""
		
		call xGrid.init( 1.0_8, 10.0_8, 10 )
		nFunc = CNFunction( xGrid, func=funcTest )
		
		write(*,*) ""
		write(*,*) " Testing resize grid 5, dir = -1"
		write(*,*) "---------------------------------"
		call nFunc.resize( 5, -1 )
		call nFunc.show()
		do i=1,nFunc.nPoints()
			write(*,"(I5,F10.5,5X,2F10.5)") i, nFunc.x(i), nFunc.at(i)
		end do
		
		write(*,*) ""
		write(*,*) " Testing resize grid -5, dir = -1"
		write(*,*) "----------------------------------"
		call nFunc.resize( -5, -1 )
		call nFunc.show()
		do i=1,nFunc.nPoints()
			write(*,"(I5,F10.5,5X,2F10.5)") i, nFunc.x(i), nFunc.at(i)
		end do
		
		write(*,*) ""
		write(*,*) "===================================================================="
		write(*,*) ""
		
		call xGrid.init( 1.0_8, 10.0_8, 10 )
		nFunc = CNFunction( xGrid, func=funcTest )
		
		write(*,*) ""
		write(*,*) " Testing resize grid 5, dir = 0"
		write(*,*) "--------------------------------"
		call nFunc.resize( 5, 0 )
		call nFunc.show()
		do i=1,nFunc.nPoints()
			write(*,"(I5,F10.5,5X,2F10.5)") i, nFunc.x(i), nFunc.at(i)
		end do
		
		write(*,*) ""
		write(*,*) " Testing resize grid -5, dir = 0"
		write(*,*) "---------------------------------"
		call nFunc.resize( -5, 0 )
		call nFunc.show()
		do i=1,nFunc.nPoints()
			write(*,"(I5,F10.5,5X,2F10.5)") i, nFunc.x(i), nFunc.at(i)
		end do
		
		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		! Testing interpolation
		write(*,*) "---"
		write(*,*) "Testing interpolation"
		write(*,*) "---"
		
		! plot "salidaFuncExact.dat" w l, "" u 1:3 w l, "salidaFunc.dat" w p pt 5, "" u 1:3 w lp, "salidaFunc2.dat" w p, "" u 1:3 w p lc rgb "magenta"
		call xGrid.init( 1.0_8, 10.0_8, 1000 )
		nFunc = CNFunction( xGrid, func=funcTest )
		call nFunc.save( "salidaFuncExact.dat" )
		
		call xGrid.init( 1.0_8, 10.0_8, 21 )
		nFunc = CNFunction( xGrid, func=funcTest )
		call nFunc.save( "salidaFunc.dat" )
		
		call xGrid2.init( -2.0_8, 13.0_8, 41 )
		nFunc2 = nFunc.interpolate( xGrid2 )
		call nFunc2.save( "salidaFunc2.dat" )

	end subroutine CNFunction_test
	
end module CNFunction_

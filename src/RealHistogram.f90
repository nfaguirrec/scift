!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                                                                                   !!
!!  This file is part of SciFT project                                               !!
!!  Copyright (c) 2013-2013 Nestor F. Aguirre (nfaguirrec@gmail.com)                 !!
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
module RealHistogram_
	use GOptions_
	use Math_
	use String_
	use IOStream_
	use RealList_
	use Grid_
	use RNFunction_
	implicit none
	private
	
	public :: &
		RealHistogram_test
		
	type, public, extends( RealList ) :: RealHistogram
		integer, private :: rule
		integer, private :: algorithm
		
		type(RNFunction) :: counts
		type(RNFunction) :: density
		
		! Used in algorithm = Histogram_RUNNING
		real(8), private :: n
		real(8), private :: s1
		real(8), private :: s2
		
		contains
			generic :: init => initRealHistogram
			generic :: assignment(=) => copyRealHistogram
			
			procedure :: initRealHistogram
			procedure :: copyRealHistogram
			final :: destroyRealHistogram
			procedure :: str
			procedure :: show
			
			procedure :: setRule
			generic :: add => addValue, addFArray, addFile
			procedure, private :: addValue
			procedure, private :: addFArray
			procedure, private :: addFile
			
			procedure :: build
			procedure :: mean
			procedure :: stdev
			procedure :: var
			procedure :: mode
			procedure :: median
			procedure :: minimum
			procedure :: maximum
			procedure :: skewness
			procedure :: stderr
			procedure :: summation
			
	end type RealHistogram
	
	contains
	
	!>
	!! @brief Constructor
	!! @see http://stat.ethz.ch/R-manual/R-devel/library/graphics/html/hist.html
	!!
	subroutine initRealHistogram( this, rule, algorithm )
		class(RealHistogram) :: this
		integer, optional :: rule
		integer, optional :: algorithm
		
		integer :: ruleEff
		integer :: algorithmEff
		
		ruleEff = Histogram_SQUAREROOT
		if( present(rule) ) ruleEff = rule
		
		algorithmEff = Histogram_STORING
		if( present(algorithm) ) then
			if( algorithm == Histogram_STORING .or. algorithm == Histogram_RUNNING ) then
				algorithmEff = algorithm
			else
				call GOptions_error( "Wrong value for paramenter algorithm. Possible values Histogram_RUNNING,Histogram_STORING", "RealHistogram%initRealHistogram()" )		
			end if
		end if
		
		this%rule = ruleEff
		this%algorithm = algorithmEff
		
		this%n = 0
		this%s1 = 0
		this%s2 = 0
		
		call this%initList()
	end subroutine initRealHistogram
	
	!>
	!! @brief Copy constructor
	!!
	subroutine copyRealHistogram( this, other )
		class(RealHistogram), intent(out) :: this
		class(RealHistogram), intent(in) :: other
		
		call this%copyList( other )

		this%rule = other%rule
		this%algorithm = other.algorithm
		
		this%counts = other%counts
		this%density = other%density
		
		this%n = other.n
		this%s1 = other.s1
		this%s2 = other.s2
	end subroutine copyRealHistogram
	
	!>
	!! @brief Destructor
	!!
	subroutine destroyRealHistogram( this )
		type(RealHistogram) :: this
		
		! @warning Hay que verificar que el desructor de la clase padre se llama automaticamente
! 		call this%destroyList()
	end subroutine destroyRealHistogram
	
	!>
	!! @brief Convert to string
	!!
	function str( this, formatted, prefix ) result( output )
		class(RealHistogram) :: this 
		character(:), allocatable :: output
		logical, optional :: formatted
		character(*), optional :: prefix
		
		logical :: effFormatted
		character(:), allocatable :: effPrefix
		
		integer :: fmt
		character(200) :: fstr
		
		effFormatted = .false.
		if( present(formatted) ) effFormatted = formatted
		
		effPrefix = ""
		if( present(prefix) ) effPrefix = prefix
		
		output = ""
		
		if( .not. effFormatted ) then
#define ITEMS(l,v) output = trim(output)//effPrefix//trim(l)//trim(adjustl(v))
#define ITEMI(l,v) output = trim(output)//l; write(fstr, "(I20)") v; output = trim(output)//trim(adjustl(fstr))
#define ITEMR(l,v) output = trim(output)//l; write(fstr, "(F20.6)") v; output = trim(output)//trim(adjustl(fstr))
#define ITEML(l,v) output = trim(output)//l; write(fstr, "(L3)") v; output = trim(output)//trim(adjustl(fstr))
		
			output = trim(output)//"<RealHistogram:"
! 			ITEMI( "min=", this%min )
			ITEMI( "size=", this%size() )
#undef ITEMS
#undef ITEMI
#undef ITEMR
#undef ITEML
			output = trim(output)//">"
		else
#define LINE(l) output = trim(output)//effPrefix//l//new_line('')
#define ITEMS(l,v) output = trim(output)//effPrefix//l; write(fstr, "(x,a)") trim(v); output = trim(output)//trim(fstr)//new_line('')
#define ITEMI(l,v) output = trim(output)//effPrefix//l; write(fstr, "(i20)") v; output = trim(output)//trim(fstr)//new_line('')
#define ITEMR(l,v) output = trim(output)//effPrefix//l; write(fstr, "(f20.6)") v; output = trim(output)//trim(fstr)//new_line('')

			LINE("RealHistogram")
			LINE("---------")
! 			ITEMI( "min=", this%min )
! 			ITEMR( ",size=", this%size )
			LINE("")
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
		class(RealHistogram) :: this
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
	!! @brief
	!!
	subroutine setRule( this, rule )
		class(RealHistogram) :: this
		integer, intent(in), optional :: rule
		
		this%rule = rule
	end subroutine setRule
	
	!>
	!! @brief
	!!
	subroutine addValue( this, value )
		class(RealHistogram) :: this
		real(8), intent(in) :: value
		
		if( this%algorithm == Histogram_RUNNING ) then
		
			this%s1 = this%s1 + value
			this%s2 = this%s2 + value**2
			this%n = this%n + 1
			
		else if( this%algorithm == Histogram_STORING ) then
		
			call this%append( value )
			
		end if
	end subroutine addValue
	
	!>
	!! @brief
	!!
	subroutine addFArray( this, array )
		class(RealHistogram) :: this
		real(8), intent(in) :: array(:)
		
		integer :: i
		
		do i=1,size(array)
			call this%addValue( array(i) )
		end do

	end subroutine addFArray
	
	!>
	!! @brief
	!!
	subroutine addFile( this, iFileName, column, tol, cComments )
		class(RealHistogram) :: this
		character(*), intent(in) :: iFileName
		integer, optional, intent(in) :: column
		real(8), optional, intent(in) :: tol
		character(*), optional, intent(in) :: cComments
		
		type(IFStream) :: ifile
		integer :: columnEff
		character(:), allocatable :: cCommentsEff
		type(String) :: buffer
		character(20), allocatable :: tokens(:)
		integer :: i
		real(8), allocatable :: data(:)
		integer :: nData
		real(8) :: stepSize
		
		columnEff = 1
		if( present(column) ) then
			columnEff = column
		end if
		
		cCommentsEff = "#"
		if( present(cComments) ) then
			cCommentsEff = cComments
		end if
		
		call ifile%init( trim(iFileName) )
		
		!! En el peor de los casos cada
		!! línea es un valor
		allocate( data(ifile.numberOfLines) )
		
		nData = 1
		do while( .not. ifile.eof() )
			buffer = ifile%readLine( cCommentsEff )
			
			call buffer.split( tokens, " " )
			
			if( columnEff <= size(tokens) ) then
				if( len(trim(tokens(columnEff))) /= 0 ) then
					read( tokens(columnEff),* ) data(nData)
					nData = nData + 1
				end if
			end if
		end do
		
		call this%addFArray( data(1:nData-1) )
		
		deallocate( data )
		call ifile.close()
	end subroutine addFile
	
	!>
	!! @brief
	!!
	subroutine build( this, nBins, binsPrecision, min, max )
		class(RealHistogram) :: this
		integer, optional, intent(in) :: nBins
		integer, optional, intent(in) :: binsPrecision
		real(8), optional, intent(in) :: min
		real(8), optional, intent(in) :: max
		
		integer :: EffNBins
		integer :: EffbinsPrecision
		real(8) :: EffMin
		real(8) :: EffMax
		
		real(8) :: windowWidth
		
		class(RealListIterator), pointer :: iter
		integer :: i, j
		real(8) :: rrange, h, x, norm
		type(Grid) :: xGrid
		integer, allocatable :: counts(:)
		
		if( this%algorithm == Histogram_RUNNING ) then
			call GOptions_error( "Method not available with algorithm = Histogram_RUNNING, use algorithm = Histogram_STORING", "RealHistogram.build()" )
		end if
		
		EffNBins = -1
		if( present(nBins) ) EffNBins = nBins
		
		EffbinsPrecision = -1
		if( present(binsPrecision) ) EffbinsPrecision = binsPrecision
		
		EffMin = this%minimum()
		if( present(min) ) EffMin = min
		
		EffMax = this%maximum()
		if( present(max) ) EffMax = max
		
		rrange = EffMax - EffMin
		
		if( EffNBins == -1 ) then
			!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
			! http://en.wikipedia.org/wiki/Histogram
			select case( this%rule )
				
				! @todo Histogram_SQUAREROOT no funciona, hay que revisar esta linea. Por el momento es completamente
				!       satisfactorio Histogram_STURGES
				case( Histogram_SQUAREROOT )
					EffNBins = ceiling( sqrt( real(this%size(),8) ) )
					
				! @todo Hay que ver si es adecuado utilizar STURGES para los casos dressing
				case( Histogram_STURGES, Histogram_GAUSSIAN_DRESSING, Histogram_LORENTZIAN_DRESSING )
					EffNBins = ceiling( log( real(this%size(),8) )/log(2.0_8) + 1.0_8 )
					
				case( Histogram_RICE )
					EffNBins = ceiling( 2.0_8*real(this%size(),8)**(1.0_8/3.0_8) )
				
				case( Histogram_DOANE )
					! Necesita tener implementado skewness
					stop "### ERROR ### RealHistogram.build(): Histogram_DOANE rule is not implemented"
				
				case( Histogram_SCOTT )
					EffNBins = ceiling( rrange*real(this%size(),8)**(1.0_8/3.0_8)/3.5_8/this%stdev() )
				
				case( Histogram_FREEDMAN_DIACONIS )
					! Necesita tener implementado el rango intercuartil
					stop "### ERROR ### RealHistogram.build(): Histogram_FREEDMAN_DIACONIS rule is not implemented"
					
				case default
					stop "### ERROR ### RealHistogram.build(): UNKNOWN rule is not implemented"
					
			end select
		end if
		
! 		if( EffbinsPrecision /= -1 ) then
! 			h = Math_fixPrecision( rrange/real(EffNBins,8), EffbinsPrecision )
! 			EffMin = Math_fixPrecision( EffMin-0.10_8*h, EffbinsPrecision )
! 			mmax = Math_fixPrecision( EffMax+0.10_8*h, EffbinsPrecision )
! 		else
! 			h = rrange/real(EffNBins,8)
! 			EffMin = EffMin-0.10_8*h
! 			mmax = EffMax+0.10_8*h
! 		end if

		h = rrange/real(EffNBins,8)
		
! 		h = (EffMax-h-EffMin)/real(EffNBins-1,8)
		
! 		write(6,*) "EffNBins = ", EffNBins
! 		write(6,*) "h = ", h
! 		write(6,*) "EffMin = ", EffMin, EffMin
! 		write(6,*) "EffMax = ", EffMax, EffMax
		
		if( abs(rrange) <= h ) return
		
! 		call xGrid%init( EffMin, EffMax-h, stepSize=h ) ! El valor de X es el inicio del intervalo
! 		call xGrid%init( EffMin+0.5_8*h, EffMax-0.5_8*h, stepSize=h )   ! El valor de X es el centro del intervalo
! 		write(*,*) "Inicializando ", EffMin, EffMax, h, EffNBins
		call xGrid%initDefault( EffMin, EffMax, stepSize=h )   ! El valor de X es el centro del intervalo
! 		call xGrid%show()
		
		if( this%rule == Histogram_GAUSSIAN_DRESSING .or. this%rule == Histogram_LORENTZIAN_DRESSING ) then
			
			windowWidth = 3.0_8*h
			call this%counts%init( xGrid, 0.0_8 )
			
			
			do i=1,xGrid%nPoints
				
				iter => this%begin
				do while( associated(iter) )
				
					! Solo fue necesario para una aplicación particular
! 					if( iter%data > 0.7_8 ) then
! 						windowWidth = 20.0_8*h
! 					end if
					
					if( abs(xGrid%at(i)-iter%data) < 6.0_8*windowWidth ) then
						if( this%rule == Histogram_GAUSSIAN_DRESSING ) then
							this%counts%fArray(i) = this%counts%fArray(i) + Math_gaussian( xGrid%at(i), iter%data, 1.0_8, windowWidth )
						else if( this%rule == Histogram_LORENTZIAN_DRESSING ) then
							this%counts%fArray(i) = this%counts%fArray(i) + Math_lorentzian( xGrid%at(i), iter%data, 1.0_8, windowWidth )
						end if
					end if
					
					iter => iter.next
				end do
				
			end do
			
			this%density = this%counts
			call this%density%normalize()
		else
			allocate( counts(xGrid%nPoints) )
			
			counts = 0
			j = 1
			iter => this%begin
			do while( associated(iter) )
	! 			i = floor( ( iter%data - EffMin )/h + 1.0_8 )
				i = int( ( iter%data - EffMin )/h ) + 1
				
				if( i>this%size() .or. i<1 ) then
					write(*,*) "Fuera de los limites", i, EffNBins, this%size(), h
					stop
				end if
				
				counts(i) = counts(i) + 1
				
! 				write(6,"(i5,2f10.5,i5)") j, xGrid%at(j), iter%data, i
				
				iter => iter.next
				j = j + 1
			end do
			
			if( sum(counts) /= this%size() ) then
				write(*,*) "### ERROR ### RealHistogram.build(). Problems in sampling. sum(counts) /= this%size() (", sum(counts), ", ", this%size(), ")"
				stop
			end if
			
			call this%counts%init( xGrid, real(counts,8) )
			call this%density%init( xGrid, real(counts,8)*h/real(this%size(),8) )
			
			deallocate( counts )
		end if
		
	end subroutine build
	
	!>
	!! @brief
	!!
	function mean( this, weights ) result( output )
		class(RealHistogram), intent(in) :: this
		real(8), optional, intent(in) :: weights(:)
		real(8) :: output
		
		class(RealListIterator), pointer :: iter
		integer :: i
		
		if( this%algorithm == Histogram_RUNNING ) then
			
			if( present(weights) ) then
				call GOptions_error( "Method not available with algorithm = Histogram_RUNNING, use algorithm = Histogram_STORING", "RealHistogram.mean( weights )" )
			end if
			
			output = this%s1/this%n
			
		else if( this%algorithm == Histogram_STORING ) then
		
			if( present(weights) ) then
				if( size(weights) < this%size() ) then
					write(*,"(A,I5,A,I5,A)") "### ERROR ### RealHistogram.mean(). Inconsistent size for weights. size(weights) /= this%size() (", size(weights), ", ", this%size(), ")"
					stop
				end if
				
				output = 0.0_8
				iter => this%begin
				i = 1
				do while( associated(iter) )
					output = output + iter%data*weights(i)
					
					iter => iter.next
					i = i+1
				end do
				
				output = output/sum(weights(1:i-1))
			else
				output = 0.0_8
				iter => this%begin
				i = 1
				do while( associated(iter) )
					output = output + iter%data
					
					iter => iter.next
					i = i+1
				end do
				
				output = output/real(this%size(),8)
			end if
			
		end if
	end function mean
	
	!>
	!! @brief
	!!
	function stdev( this, weights ) result( output )
		class(RealHistogram), intent(in) :: this
		real(8), optional, intent(in) :: weights(:)
		real(8) :: output
		
		if( this%algorithm == Histogram_RUNNING ) then
			if( present(weights) ) then
				call GOptions_error( "Method not available with algorithm = Histogram_RUNNING, use algorithm = Histogram_STORING", "RealHistogram.stdev( weights )" )
			end if
			
			output = sqrt((this%n*this%s2-this%s1**2)/real(this%n,8)/real(this%n-1.0_8,8))
			
		else if( this%algorithm == Histogram_STORING ) then
		
			output = sqrt( this%var( weights ) )
			
		end if

	end function stdev
	
	!>
	!! @brief
	!!
	function var( this, weights ) result( output )
		class(RealHistogram), intent(in) :: this
		real(8), optional, intent(in) :: weights(:)
		real(8) :: output
		
		class(RealListIterator), pointer :: iter
		real(8) :: mean
		integer :: i
		
		if( this%algorithm == Histogram_RUNNING ) then
			call GOptions_error( "Method not available with algorithm = Histogram_RUNNING, use algorithm = Histogram_STORING", "RealHistogram.var()" )
		end if
		
		if( this%size() == 1 ) then
			output = 0.0_8
			return
		end if

		if( present(weights) ) then
			if( size(weights) < this%size() ) then
				write(*,"(A,I5,A,I5,A)") "### ERROR ### RealHistogram.var(). Inconsistent size for weights. size(weights) /= this%size() (", size(weights), ", ", this%size(), ")"
				stop
			end if
			
			mean = this%mean( weights )
			
			output = 0.0_8
			iter => this%begin
			i = 1
			do while( associated(iter) )
				output = output + weights(i)*( iter%data - mean )**2
				
				iter => iter.next
				i = i+1
			end do
			
			! Approximation of the unbiased weighted covariance matrix, but without Bessel correction
			output = output/sum(weights(1:i-1))
		else
			mean = this%mean()
			
			output = 0.0_8
			iter => this%begin
			do while( associated(iter) )
				output = output + ( iter%data - mean )**2
				
				iter => iter.next
			end do
			
			! Bessel's correction
			! n->n-1
			! standard deviation of the sample (considered as the entire population) -> Corrected sample standard deviation
			! Standard deviation of the population -> sample standard deviation
			! sigma -> s
			output = output/real(this%size()-1,8)
		end if
	end function var
	
	!>
	!! @brief
	!!
	function mode( this ) result( output )
		class(RealHistogram), intent(in) :: this
		real(8) :: output
		
		stop "### ERROR ### RealHistogram.mode(): This function is unimplemented yet"
	end function mode
	
	!>
	!! @brief
	!!
	function median( this ) result( output )
		class(RealHistogram), intent(in) :: this
		real(8) :: output
		
		stop "### ERROR ### RealHistogram.median(): This function is unimplemented yet"
	end function median
	
	!>
	!! @brief
	!!
	function minimum( this ) result( output )
		class(RealHistogram), intent(in) :: this
		real(8) :: output
		
		class(RealListIterator), pointer :: iter
		
		! @todo se puede poner otro atributo que vaya almacenando el minimio
		if( this%algorithm == Histogram_RUNNING ) then
			call GOptions_error( "Method not available with algorithm = Histogram_RUNNING, use algorithm = Histogram_STORING", "RealHistogram.build()" )
		end if
		
		output = Math_INF
		iter => this%begin
		do while( associated(iter) )
			if( iter%data < output ) then
				output = iter%data
			end if
			
			iter => iter.next
		end do
	end function minimum
	
	!>
	!! @brief
	!!
	function maximum( this ) result( output )
		class(RealHistogram), intent(in) :: this
		real(8) :: output
		
		class(RealListIterator), pointer :: iter
		
		! @todo se puede poner otro atributo que vaya almacenando el maximum
		if( this%algorithm == Histogram_RUNNING ) then
			call GOptions_error( "Method not available with algorithm = Histogram_RUNNING, use algorithm = Histogram_STORING", "RealHistogram.build()" )
		end if
		
		output = -Math_INF
		iter => this%begin
		do while( associated(iter) )
			if( iter%data > output ) then
				output = iter%data
			end if
			
			iter => iter.next
		end do
	end function maximum
	
	!>
	!! @brief
	!!
	function skewness( this ) result( output )
		class(RealHistogram), intent(in) :: this
		real(8) :: output
		
		stop "### ERROR ### RealHistogram.skewness(): This function is unimplemented yet"
	end function skewness
	
	!>
	!! @brief
	!!
	function stderr( this ) result( output )
		class(RealHistogram), intent(in) :: this
		real(8) :: output
		
		if( this%algorithm == Histogram_RUNNING ) then
		
			output = this%stdev()/sqrt( this%n )
			
		else if( this%algorithm == Histogram_STORING ) then
		
			output = this%stdev()/sqrt( real(this%size(),8) )
			
		end if

	end function stderr
	
	!>
	!! @brief
	!!
	function summation( this ) result( output )
		class(RealHistogram), intent(in) :: this
		real(8) :: output
		
		class(RealListIterator), pointer :: iter
		
		if( this%algorithm == Histogram_RUNNING ) then
		
			output = this%s1
			
		else if( this%algorithm == Histogram_STORING ) then
		
			output = 0.0_8
			iter => this%begin
			do while( associated(iter) )
				output = output + iter%data
				
				iter => iter.next
			end do
			
		end if

	end function summation
	
	!>
	!! @brief Test method
	!!
	subroutine RealHistogram_test()
		type(RealHistogram) :: histogram
		type(RealHistogram) :: histogramRunning
		type(RNFunction) :: nFunc
		integer :: i
		
! 		call histogram%init()
! 		call histogram%init( Histogram_SQUAREROOT )
		call histogram%init( Histogram_STURGES )
! 		call histogram%init( Histogram_RICE )
! 		call histogram%init( Histogram_SCOTT )
		
		call histogram%add( [24.15162_8, 19.56235_8, 27.82564_8, 23.38200_8, 25.19829_8, 25.26511_8, 23.81071_8, 22.70389_8] )
		call histogram%add( [23.21883_8, 25.35600_8, 28.41117_8, 22.08219_8, 19.55053_8, 23.63690_8, 27.07390_8, 25.11683_8] )
		call histogram%add( [24.07832_8, 22.04728_8, 29.07267_8, 23.84218_8, 24.07261_8, 23.97873_8, 25.67417_8, 23.89337_8] )
		call histogram%add( [23.49143_8, 26.14219_8, 22.87863_8, 21.59113_8, 23.56555_8, 26.42314_8, 23.51600_8, 26.27489_8] )
		call histogram%add( [21.07893_8, 20.48072_8, 24.90150_8, 23.17327_8, 23.81940_8, 25.11435_8, 26.52324_8, 18.73398_8] )
		call histogram%add( [24.09926_8, 23.07400_8, 26.71212_8, 21.77789_8, 25.51567_8, 25.13831_8, 22.11752_8, 22.47796_8] )
		call histogram%add( [25.39945_8, 26.71204_8, 25.67166_8, 22.52061_8, 23.62552_8, 26.00762_8, 25.37902_8, 26.28057_8] )
		call histogram%add( [22.61389_8, 24.06349_8, 24.33601_8, 21.97826_8, 26.48619_8, 25.47802_8, 26.89355_8, 26.07590_8] )
		call histogram%add( [21.74619_8, 21.99553_8, 23.40948_8, 25.48071_8, 23.02762_8, 22.70441_8, 25.03438_8, 25.67790_8] )
		call histogram%add( [24.68533_8, 21.26442_8, 24.89509_8, 24.71221_8, 25.12706_8, 26.05145_8, 20.59260_8, 22.63209_8] )
		call histogram%add( [23.35024_8, 26.70019_8, 21.51930_8, 24.98537_8, 24.94632_8, 19.42552_8, 27.00687_8, 21.65142_8] )
		call histogram%add( [25.00371_8, 23.40407_8, 21.82391_8, 24.25161_8, 24.28748_8, 24.17388_8, 21.20663_8, 26.66869_8] )
		call histogram%add( [22.89491_8, 24.81186_8, 25.14049_8, 22.61879_8] )
		
		call histogramRunning%init( algorithm=Histogram_RUNNING )
		call histogramRunning%add( [24.15162_8, 19.56235_8, 27.82564_8, 23.38200_8, 25.19829_8, 25.26511_8, 23.81071_8, 22.70389_8] )
		call histogramRunning%add( [23.21883_8, 25.35600_8, 28.41117_8, 22.08219_8, 19.55053_8, 23.63690_8, 27.07390_8, 25.11683_8] )
		call histogramRunning%add( [24.07832_8, 22.04728_8, 29.07267_8, 23.84218_8, 24.07261_8, 23.97873_8, 25.67417_8, 23.89337_8] )
		call histogramRunning%add( [23.49143_8, 26.14219_8, 22.87863_8, 21.59113_8, 23.56555_8, 26.42314_8, 23.51600_8, 26.27489_8] )
		call histogramRunning%add( [21.07893_8, 20.48072_8, 24.90150_8, 23.17327_8, 23.81940_8, 25.11435_8, 26.52324_8, 18.73398_8] )
		call histogramRunning%add( [24.09926_8, 23.07400_8, 26.71212_8, 21.77789_8, 25.51567_8, 25.13831_8, 22.11752_8, 22.47796_8] )
		call histogramRunning%add( [25.39945_8, 26.71204_8, 25.67166_8, 22.52061_8, 23.62552_8, 26.00762_8, 25.37902_8, 26.28057_8] )
		call histogramRunning%add( [22.61389_8, 24.06349_8, 24.33601_8, 21.97826_8, 26.48619_8, 25.47802_8, 26.89355_8, 26.07590_8] )
		call histogramRunning%add( [21.74619_8, 21.99553_8, 23.40948_8, 25.48071_8, 23.02762_8, 22.70441_8, 25.03438_8, 25.67790_8] )
		call histogramRunning%add( [24.68533_8, 21.26442_8, 24.89509_8, 24.71221_8, 25.12706_8, 26.05145_8, 20.59260_8, 22.63209_8] )
		call histogramRunning%add( [23.35024_8, 26.70019_8, 21.51930_8, 24.98537_8, 24.94632_8, 19.42552_8, 27.00687_8, 21.65142_8] )
		call histogramRunning%add( [25.00371_8, 23.40407_8, 21.82391_8, 24.25161_8, 24.28748_8, 24.17388_8, 21.20663_8, 26.66869_8] )
		call histogramRunning%add( [22.89491_8, 24.81186_8, 25.14049_8, 22.61879_8] )
		
		! ! http://en.wikipedia.org/wiki/Descriptive_statistics
		! ! http://personality-project.org/r/html/describe.html
		! ! http://www.statmethods.net/stats/descriptives.html

		! # Obtenido mediante: BMI<-rnorm(n=100, m=24.2, sd=2.2)
		! BMI <- c(
		!    24.15162, 19.56235, 27.82564, 23.38200, 25.19829, 25.26511, 23.81071, 22.70389,
		!    23.21883, 25.35600, 28.41117, 22.08219, 19.55053, 23.63690, 27.07390, 25.11683,
		!    24.07832, 22.04728, 29.07267, 23.84218, 24.07261, 23.97873, 25.67417, 23.89337,
		!    23.49143, 26.14219, 22.87863, 21.59113, 23.56555, 26.42314, 23.51600, 26.27489,
		!    21.07893, 20.48072, 24.90150, 23.17327, 23.81940, 25.11435, 26.52324, 18.73398,
		!    24.09926, 23.07400, 26.71212, 21.77789, 25.51567, 25.13831, 22.11752, 22.47796,
		!    25.39945, 26.71204, 25.67166, 22.52061, 23.62552, 26.00762, 25.37902, 26.28057,
		!    22.61389, 24.06349, 24.33601, 21.97826, 26.48619, 25.47802, 26.89355, 26.07590,
		!    21.74619, 21.99553, 23.40948, 25.48071, 23.02762, 22.70441, 25.03438, 25.67790,
		!    24.68533, 21.26442, 24.89509, 24.71221, 25.12706, 26.05145, 20.59260, 22.63209,
		!    23.35024, 26.70019, 21.51930, 24.98537, 24.94632, 19.42552, 27.00687, 21.65142,
		!    25.00371, 23.40407, 21.82391, 24.25161, 24.28748, 24.17388, 21.20663, 26.66869,
		!    22.89491, 24.81186, 25.14049, 22.61879 )
		! 
		! > mean(BMI)
		! [1] 24.06056
		! 
		! > sd(BMI)
		! [1] 2.030379
		! 
		! > summary(BMI)
		!    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
		!   18.73   22.69   24.09   24.06   25.42   29.07
		! 
		! b <- seq(18.7,30.24,1.044444)
		! > b
		!  [1] 18.70000 19.74444 20.78889 21.83333 22.87778 23.92222 24.96666 26.01111
		!  [9] 27.05555 28.10000 29.14444 30.18888
		! 
		! > histinfo<-hist(BMI,breaks=b)
		! > b
		!  [1] 18.70000 19.74444 20.78889 21.83333 22.87778 23.92222 24.96666 26.01111
		!  [9] 27.05555 28.10000 29.14444 30.18888
		! 
		! > histinfo<-hist(BMI,breaks=b)
		! > histinfo
		! $breaks
		!  [1] 18.70000 19.74444 20.78889 21.83333 22.87778 23.92222 24.96666 26.01111
		!  [9] 27.05555 28.10000 29.14444 30.18888
		! 
		! $counts
		!  [1]  4  2  9 12 19 16 20 14  2  2  0
		! 
		! $intensities
		!  [1] 0.03829789 0.01914894 0.08617025 0.11489367 0.18191497 0.15319155
		!  [7] 0.19148944 0.13404261 0.01914894 0.01914894 0.00000000
		! 
		! $density
		!  [1] 0.03829789 0.01914894 0.08617025 0.11489367 0.18191497 0.15319155
		!  [7] 0.19148944 0.13404261 0.01914894 0.01914894 0.00000000
		! 
		! $mids
		!  [1] 19.22222 20.26667 21.31111 22.35555 23.40000 24.44444 25.48889 26.53333
		!  [9] 27.57777 28.62222 29.66666

		! @todo Parece que solo funciona con la combinación Histogram_STURGES y binsPrecision=1
		call histogram.build( binsPrecision=3 )
! 		call histogram.build()
		
		write(*,"(A20,I15)")   "    size = ", histogram%size()
		write(*,"(A20,2F15.5)") "    mean = ", histogram.mean(), histogramRunning.mean()
		write(*,"(A20,2F15.5)") "   stdev = ", histogram.stdev(), histogramRunning.stdev()
		write(*,"(A20,F15.5)") " minimum = ", histogram%minimum()
		write(*,"(A20,F15.5)") " maximum = ", histogram%maximum()
		write(*,"(A20,2F15.5)") "  stderr = ", histogram.stderr(), histogramRunning.stderr()
		
		! plot "./counts.dat" w boxes, "./counts.dat" w p pt 7
		call histogram%save("histData.dat")
		call histogram%counts%save("counts.dat")
		call histogram%density%save("density.dat")
		
		do i=1,1000000
			call histogram%add( 1.456_8 )
			call histogramRunning%add( 1.456_8 )
		end do
		
		write(*,"(A)")   ""
		write(*,"(A20,I15)")   "    size = ", histogram%size()
		write(*,"(A20,2F15.5)") "    mean = ", histogram.mean(), histogramRunning.mean()

! 		write(*,"(A20,F15.5)") " mode = ", histogram.mode()
! 		write(*,"(A20,F15.5)") "stdev = ", histogram.median()
! 		mode = histogram.mode()
! 		median = histogram.skewness()
		
		write(*,*) "LORENTZIAN DRESSING"
		write(*,*) "-------------------"
		call histogram%clear()
		call histogram%init( Histogram_LORENTZIAN_DRESSING )
		call histogram%add( "data/formats/ONE_COLUMN" )
		call histogram%show()
		call histogram.build( nBins=10000 )
		call histogram%density%save("density.dat")
		
	end subroutine RealHistogram_test
	
end module RealHistogram_

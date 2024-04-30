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
module StringHistogram_
	use GOptions_
	use String_
	use StringList_
	use StringIntegerMap_
	use StringIntegerPair_
	use StringRealPair_
	use StringRealMap_
	implicit none
	private
	
	public :: &
		StringHistogram_test
		
	type, public, extends( StringList ) :: StringHistogram
		integer, private :: algorithm
		
		type(StringIntegerMap) :: counts
		type(StringRealMap) :: density
		
		! Used in algorithm = Histogram_RUNNING
		type(StringRealMap) :: rCounts
		integer :: totalCounts
		
		contains
			generic :: init => initStringHistogram
			generic :: assignment(=) => copyStringHistogram
			
			procedure :: initStringHistogram
			procedure :: copyStringHistogram
			final :: destroyStringHistogram
			procedure :: str
			procedure :: show
			
			procedure :: countsBegin
			procedure :: densityBegin
			generic :: pair => countsPair, densityPair
			procedure, private :: countsPair
			procedure, private :: densityPair
			
			generic :: add => addValue, addFArray
			procedure, private :: addValue
			procedure, private :: addFArray
			
			procedure :: build
			procedure :: mean
			procedure :: stdev
			procedure :: mode
			procedure :: median
			procedure :: skewness
			procedure :: stderr
			
	end type StringHistogram
	
	contains
	
	!>
	!! @brief Constructor
	!!
	subroutine initStringHistogram( this, algorithm )
		class(StringHistogram) :: this
		integer, optional :: algorithm
		
		integer :: algorithmEff
		
		algorithmEff = Histogram_STORING
		if( present(algorithm) ) then
			if( algorithm == Histogram_STORING .or. algorithm == Histogram_RUNNING ) then
				algorithmEff = algorithm
			else
				call GOptions_error( "Wrong value for paramenter algorithm. Possible values Histogram_RUNNING,Histogram_STORING", "StringHistogram.initStringHistogram()" )		
			end if
		end if
		
		this.algorithm = algorithmEff
		call this.counts.clear()
		call this.density.clear()
		
		call this.rCounts.clear()
		this.totalCounts = 0
		
		this.StringList = StringList()
	end subroutine initStringHistogram
	
	!>
	!! @brief Copy constructor
	!!
	subroutine copyStringHistogram( this, other )
		class(StringHistogram), intent(out) :: this
		type(StringHistogram), intent(in) :: other

		this.algorithm = other.algorithm

		this.counts = other.counts
		this.density = other.density

		this.rCounts = other.rCounts
		this.totalCounts = other.totalCounts
	end subroutine copyStringHistogram
	
	!>
	!! @brief Destructor
	!!
	subroutine destroyStringHistogram( this )
		type(StringHistogram) :: this
		
		! @warning Hay que verificar que el desructor de la clase padre se llama automaticamente
! 		call this.destroyList()
	end subroutine destroyStringHistogram
	
	!>
	!! @brief Convert to string
	!!
	function str( this, formatted, prefix ) result( output )
		class(StringHistogram) :: this 
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
#define RFMT(v) int(log10(max(abs(v),1.0)))+merge(1,2,v>=0)
#define ITEMS(l,v) output = trim(output)//effPrefix//trim(l)//trim(adjustl(v))
#define ITEMI(l,v) output = trim(output)//l; fmt = RFMT(v); write(fstr, "(i<fmt>)") v; output = trim(output)//trim(fstr)
#define ITEMR(l,v) output = trim(output)//l; fmt = RFMT(v); write(fstr, "(f<fmt+7>.6)") v; output = trim(output)//trim(fstr)
		
			output = trim(output)//"<StringHistogram:"
! 			ITEMI( "min=", this.min )
! 			ITEMR( ",size=", this.size )
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

			LINE("StringHistogram")
			LINE("---------")
! 			ITEMI( "min=", this.min )
! 			ITEMR( ",size=", this.size )
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
		class(StringHistogram) :: this
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
	!! @brief Returns a pointers to the first element of the counts
	!!
	subroutine countsBegin( this, iter )
		class(StringHistogram) :: this
		class(StringIntegerMapIterator), pointer :: iter
		
		iter => this.counts.begin
	end subroutine countsBegin
	
	!>
	!! @brief Returns a pointers to the first element of the density
	!!
	subroutine densityBegin( this, iter )
		class(StringHistogram) :: this
		class(StringRealMapIterator), pointer :: iter
		
		if( this.algorithm == Histogram_RUNNING ) then
		
			iter => this.rCounts.begin
			
		else if( this.algorithm == Histogram_STORING ) then
		
			iter => this.density.begin
			
		end if

	end subroutine densityBegin
	
	!>
	!! @brief Returns a pointers to the first element of the density
	!!
	function countsPair( this, iter ) result( output )
		class(StringHistogram) :: this
		class(StringIntegerMapIterator), pointer :: iter
		type(StringIntegerPair) :: output
		
		if( this.algorithm == Histogram_RUNNING ) then
		
			output = this.counts.pair( iter )
			
		else if( this.algorithm == Histogram_STORING ) then
			
			if( this.counts.isEmpty() ) then
				call GOptions_error( "Run first build() Method", "StringHistogram.densityPair()" )
			end if
			
			output = this.counts.pair( iter )
		end if
		
		
	end function countsPair
	
	!>
	!! @brief Returns a pointers to the first element of the density
	!!
	function densityPair( this, iter ) result( output )
		class(StringHistogram) :: this
		class(StringRealMapIterator), pointer :: iter
		type(StringRealPair) :: output
		
		if( this.algorithm == Histogram_RUNNING ) then
		
			output = this.rCounts.pair( iter )
			output.second = output.second/real(this.totalCounts,8)
			
		else if( this.algorithm == Histogram_STORING ) then
			
			if( this.density.isEmpty() ) then
				call GOptions_error( "Run first build() Method", "StringHistogram.densityPair()" )		
			end if
			
			output = this.density.pair( iter )
		end if

	end function densityPair
	
	!>
	!! @brief
	!!
	subroutine addValue( this, value )
		class(StringHistogram) :: this
		type(String), intent(in) :: value
		
		integer :: cValue
		integer :: cValueR
		
		if( this.algorithm == Histogram_RUNNING ) then
		
			cValue = this.counts.at( value, defaultValue=0 )
			call this.counts.set( value, cValue+1 )
			
			cValueR = this.rCounts.at( value, defaultValue=0.0_8 )
			call this.rCounts.set( value, cValueR+1.0_8 )
			
			this.totalCounts = this.totalCounts + 1
			
		else if( this.algorithm == Histogram_STORING ) then
		
			call this.append( value )
			
		end if
		
	end subroutine addValue
	
	!>
	!! @brief
	!!
	subroutine addFArray( this, array )
		class(StringHistogram) :: this
		character(*), intent(in) :: array(:)
		
		integer :: i
		
		do i=1,size(array)
			call this.addValue( FString_toString( array(i) ) )
		end do
		
	end subroutine addFArray
	
	!>
	!! @brief
	!!
	subroutine build( this )
		class(StringHistogram) :: this
		
		class(StringListIterator), pointer :: iter
		integer :: cValue
		
		if( this.algorithm == Histogram_RUNNING ) then
			call GOptions_error( "Method not available with algorithm = Histogram_RUNNING, use algorithm = Histogram_STORING", "StringHistogram.build()" )
		end if
		
		call this.counts.clear()
		call this.density.clear()
		
		iter => this.begin
		do while( associated(iter) )
			cValue = this.counts.at( iter.data, defaultValue=0 )
			
			call this.counts.set( iter.data, cValue+1 )
			call this.density.set( iter.data, real(cValue+1,8)/real(this.size(),8) )
			
			iter => iter.next
		end do
	end subroutine build
	
	!>
	!! @brief
	!!
	function mean( this ) result( output )
		class(StringHistogram), intent(in) :: this
		real(8) :: output
		
		stop "### ERROR ### StringHistogram.mean(): This function is unimplemented yet"
! 		class(RealListIterator), pointer :: iter
! 		
! 		output = 0.0_8
! 		iter => this.begin
! 		do while( associated(iter) )
! 			output = output + iter.data
! 			
! 			iter => iter.next
! 		end do
! 		
! 		output = output/real(this.size(),8)
	end function mean
	
	!>
	!! @brief
	!!
	function stdev( this ) result( output )
		class(StringHistogram), intent(in) :: this
		real(8) :: output
		
		stop "### ERROR ### StringHistogram.stdev(): This function is unimplemented yet"
! 		class(RealListIterator), pointer :: iter
! 		real(8) :: mean
! 		
! 		if( this.size() == 1 ) then
! 			output = 0.0_8
! 			return
! 		end if
! 		
! 		mean = this.mean()
! 		
! 		output = 0.0_8
! 		iter => this.begin
! 		do while( associated(iter) )
! 			output = output + ( iter.data - mean )**2
! 			
! 			iter => iter.next
! 		end do
! 		
! 		! Bessel's correction
! 		! n->n-1
! 		! standard deviation of the sample (considered as the entire population) -> Corrected sample standard deviation
! 		! Standard deviation of the population -> sample standard deviation
! 		! sigma -> s
! 		output = sqrt( output/real(this.size()-1,8) )
	end function stdev
	
	!>
	!! @brief
	!!
	function mode( this ) result( output )
		class(StringHistogram), intent(in) :: this
		real(8) :: output
		
		stop "### ERROR ### StringHistogram.mode(): This function is unimplemented yet"
	end function mode
	
	!>
	!! @brief
	!!
	function median( this ) result( output )
		class(StringHistogram), intent(in) :: this
		real(8) :: output
		
		stop "### ERROR ### StringHistogram.median(): This function is unimplemented yet"
	end function median
	
	!>
	!! @brief
	!!
	function skewness( this ) result( output )
		class(StringHistogram), intent(in) :: this
		real(8) :: output
		
		stop "### ERROR ### StringHistogram.skewness(): This function is unimplemented yet"
	end function skewness
	
	!>
	!! @brief
	!!
	function stderr( this ) result( output )
		class(StringHistogram), intent(in) :: this
		real(8) :: output
		
		output = this.stdev()/sqrt( real(this.size(),8) )
	end function stderr
	
	!>
	!! @brief Test method
	!!
	subroutine StringHistogram_test()
		type(StringHistogram) :: histogram
		type(StringHistogram) :: histogramRunning
		integer :: i
		
		class(StringRealMapIterator), pointer :: iter, iterRunning
		type(StringRealPair) :: pair, pairRunning
		
		call histogram.init()
		call histogram.add( ["A", "A", "T", "U", "T", "U", "P", "A", "C", "Z", "U"] )
		call histogram.add( ["B", "F", "G", "O", "T", "Q", "W", "T", "S", "X", "Q"] )
		call histogram.add( ["Y", "F", "I", "E", "U", "W", "H", "A", "D", "C", "W"] )
		call histogram.add( ["I", "F", "W", "O", "L", "R", "S", "H", "F", "V", "E"] )
		call histogram.add( ["W", "R", "T", "E", "I", "S", "V", "K", "G", "B", "R"] )
		call histogram.add( ["U", "I", "T", "U", "S", "G", "R", "X", "H", "N", "T"] )
		call histogram.add( ["I", "I", "P", "O", "N", "X", "C", "U", "J", "M", "Y"] )
		call histogram.add( ["D", "G", "D", "V", "I", "V", "B", "D", "K", "I", "U"] )
		call histogram.add( ["A", "P", "B", "I", "D", "H", "J", "G", "L", "R", "I"] )
		call histogram.add( ["S", "R", "Q", "M", "L", "S", "D", "J", "I", "U", "O"] )
		call histogram.build()
		
		call histogramRunning.init( algorithm=Histogram_RUNNING )
		call histogramRunning.add( ["A", "A", "T", "U", "T", "U", "P", "A", "C", "Z", "U"] )
		call histogramRunning.add( ["B", "F", "G", "O", "T", "Q", "W", "T", "S", "X", "Q"] )
		call histogramRunning.add( ["Y", "F", "I", "E", "U", "W", "H", "A", "D", "C", "W"] )
		call histogramRunning.add( ["I", "F", "W", "O", "L", "R", "S", "H", "F", "V", "E"] )
		call histogramRunning.add( ["W", "R", "T", "E", "I", "S", "V", "K", "G", "B", "R"] )
		call histogramRunning.add( ["U", "I", "T", "U", "S", "G", "R", "X", "H", "N", "T"] )
		call histogramRunning.add( ["I", "I", "P", "O", "N", "X", "C", "U", "J", "M", "Y"] )
		call histogramRunning.add( ["D", "G", "D", "V", "I", "V", "B", "D", "K", "I", "U"] )
		call histogramRunning.add( ["A", "P", "B", "I", "D", "H", "J", "G", "L", "R", "I"] )
		call histogramRunning.add( ["S", "R", "Q", "M", "L", "S", "D", "J", "I", "U", "O"] )
		
		write(*,"(A20,I15)")   "    size = ", histogram.size()
! 		write(*,"(A20,F15.5)") "    mean = ", histogram.mean()
! 		write(*,"(A20,F15.5)") "   stdev = ", histogram.stdev()
! 		write(*,"(A20,F15.5)") "  stderr = ", histogram.stderr()
! 		
		! plot "./counts.out" w boxes, "./counts.out" w p pt 7
		call histogram.counts.save("counts.out")
		call histogram.density.save("density.out")
		
		call histogram.densityBegin( iter )
		call histogramRunning.densityBegin( iterRunning )
		do while( associated(iter) )
			pair = histogram.pair( iter )
			pairRunning = histogramRunning.pair( iterRunning )
			write(*,"(A15,F15.5,5X,A15,F15.5)") pair.first.fstr, pair.second, pairRunning.first.fstr, pairRunning.second
			
			iter => iter.next
			iterRunning => iterRunning.next
		end do
		
! 		do i=1,1000000
! 			call histogram.add( 1.456_8 )
! 		end do
! 		
! 		write(*,"(A)")   ""
! 		write(*,"(A20,I15)")   "    size = ", histogram.size()
! 		write(*,"(A20,F15.5)") "    mean = ", histogram.mean()
! 
! ! 		write(*,"(A20,F15.5)") " mode = ", histogram.mode()
! ! 		write(*,"(A20,F15.5)") "stdev = ", histogram.median()
! ! 		mode = histogram.mode()
! ! 		median = histogram.skewness()
		
	end subroutine StringHistogram_test
	
end module StringHistogram_

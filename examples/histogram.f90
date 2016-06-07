!>
!! @brief Test program
!!
program main
	use GOptions_
	use CommandLineParser_
	use RealList_
	use RNFunction_
	use RealHistogram_

	implicit none
	type(CommandLineParser) :: parser
	character(255) :: iFileName
	character(255) :: oFileName
	real(8) :: minValue, maxValue
	integer :: nBins
	character(255) :: buffer
	
	type(RealHistogram) :: hist
	
	if( command_argument_count() < 2 ) then
		write(*,*) "usage:"
		write(*,*) "   histogram -i iFileName -o oFileName [-min value] [-max value] [-nbins value]"
		write(*,*) ""
		stop
	end if
	
	iFileName = parser.getString( "-i" )
	oFileName = parser.getString( "-o" )
	
	call hist.init( Histogram_LORENTZIAN_DRESSING )
	call hist.add( iFileName )
	
	minValue = parser.getReal( "-min", def=hist.minimum() )
	maxValue = parser.getReal( "-max", def=hist.maximum() )
	nBins = parser.getInteger( "-nbins", def=-1 )
	
	call hist.build( nBins=nBins, min=minValue, max=maxValue )
	call hist.density.save( oFileName )
end program main

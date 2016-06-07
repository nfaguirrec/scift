!>
!! @brief Test program
!!
program main
	use GOptions_
	use RealList_
	use RNFunction_
	use RealHistogram_

	implicit none
	character(255) :: iFileName
	character(255) :: oFileName
	integer :: argc
	character(255) :: buffer
	
	type(RealHistogram) :: hist
	
	argc = command_argument_count()
	
	if( argc < 2 ) then
		write(*,*) "usage:"
		write(*,*) "   n1df.func iFileName oFileName [min] [max] [nBins]"
		write(*,*) ""
		stop
	end if
	
	call get_command_argument( 1, iFileName )
	call get_command_argument( 2, oFileName )
	
	call hist.init( Histogram_LORENTZIAN_DRESSING )
	call hist.add( iFileName )
	call hist.build( nBins=10000 )
	call hist.density.save( oFileName )
end program main

!>
!! @brief Test program
!!
program main
	use GOptions_
	use Math_
	use IOStream_
	use String_
	use CommandLineParser_
	use Grid_
	use RNFunction_
	use NIntegrator_
	implicit none
	
	type(String) :: iFileName
	type(String) :: oFileName
	type(String) :: filter
	
	real(8) :: t, ta, tb
	
	type(Grid) :: tgrid
	type(IFStream) :: ifile
	type(RNFunction) :: nFunc, nFunc2, nFuncWindow, oFunc
	type(NIntegrator) :: integrator
	type(String) :: strBuffer
	type(CommandLineParser) :: parser
	
	character(5), allocatable :: tokens(:)
	integer :: columns(2)
	integer :: i
	
	iFileName = parser.getString( "-i" )
	oFileName = parser.getString( "-o" )
	filter = parser.getString( "-f" )
	
	strBuffer = parser.getString( "-c", def="1,2" )
	call strBuffer.split( tokens, "," )
	columns = [ FString_toInteger(tokens(1)), FString_toInteger(tokens(2)) ]
	
	call ifile.init( iFileName.fstr )
	call nFunc.fromFStream( ifile, columns=columns )
	call ifile.close()
	
! 	call oFunc.init( nFunc.xGrid, value=0.0_8 )
	call oFunc.init( nFunc.xGrid )
	
	select case( trim(filter.fstr) )
		case( ">0" )
			oFunc.fArray = merge( nFunc.fArray, 0.0_8, nFunc.fArray > 0.0_8 )
		case default
			write(*,*) "### ERROR ### Filter "//trim(filter.fstr)//" is not supported"
			stop
	end select
	
	call oFunc.save( oFileName.fstr )
	
end program main

!>
!! @brief Test program
!!
program main
	use GOptions_
	use IOStream_
	use String_
	use CommandLineParser_
	use Spline_
	use RNFunction_
	use NIntegrator_
	implicit none
	
	type(String) :: iFileName
	type(IFStream) :: ifile
	type(RNFunction) :: nFunc
	type(RNFunction) :: nFuncSmooth
	type(RNFunction) :: dnFunc
	type(Spline) :: nFuncSpline
	type(NIntegrator) :: integrator
	type(String) :: strBuffer
	type(CommandLineParser) :: parser
	
	integer :: argc
	character(5), allocatable :: tokens(:)
	integer :: columns(2)
	integer :: smoothFactor
	integer :: i
	real(8) :: a, b
	integer :: idMethod
	real(8) :: value
	
	argc = command_argument_count()
	
	if( argc < 2 ) then
		write(*,"(X,A)") "Usage:"
		write(*,"(X,A)") "   n1df.integrate -i ifile [-c columns] [-s smoothFactor] [-a lowerLimit] [-b upperLimit] [-m method]"
		write(*,"(X,A)") "                                   1,2                 1          min(x)          max(x)       BOOLE"
		write(*,"(X,A)") ""
		write(*,"(X,A)") "                                                                                               SIMPSON"
		write(*,"(X,A)") "                                                                                               SIMPSON38"
		write(*,"(X,A)") "                                                                                               TRAPEZOIDAL"
		write(*,"(X,A)") "                                                                                               FIXED_QUADRATURE"
		write(*,"(X,A)") "                                                                                               ADAPTIVE_QUADRATURE"
		stop
	end if
	
	iFileName = parser.getString( "-i" )
	
	strBuffer = parser.getString( "-c", def="1,2" )
	call strBuffer.split( tokens, "," )
	columns = [ FString_toInteger(tokens(1)), FString_toInteger(tokens(2)) ]
	
	smoothFactor = parser.getInteger( "-s", def=1 )
	
	call ifile.init( iFileName.fstr )
	call nFunc.fromFStream( ifile, columns=columns )
	call ifile.close()
	
	a = parser.getReal( "-a", def=nFunc.min() )
	b = parser.getReal( "-b", def=nFunc.max() )
	
	strBuffer = parser.getString( "-m", def="BOOLE" )
	
	select case( trim(strBuffer.fstr) )
		case( "SIMPSON" )
			idMethod = NIntegrator_SIMPSON
		case( "EXTSIMPSON" )
			idMethod = NIntegrator_EXTSIMPSON
		case( "SIMPSON38" )
			idMethod = NIntegrator_SIMPSON38
		case( "TRAPEZOIDAL" )
			idMethod = NIntegrator_TRAPEZOIDAL
		case( "FIXED_QUADRATURE" )
			idMethod = NIntegrator_FIXED_QUADRATURE
		case( "QUADRATURE" )
			idMethod = NIntegrator_QUADRATURE
		case( "ADAPTIVE_QUADRATURE" )
			idMethod = NIntegrator_ADAPTIVE_QUADRATURE
		case( "BOOLE" )
			idMethod = NIntegrator_BOOLE
		case default
			idMethod = -1
	end select
	
	if( nFunc.xGrid.isEquallyspaced ) then
		call integrator.init( nFunc, idMethod )
	else
		call nFuncSpline.init( nFunc )
		nFuncSmooth = nFuncSpline.smooth( smoothFactor )
		
		call integrator.init( nFuncSmooth, idMethod )
! 		write(*,*) integrator.evaluate( a, b )
	end if
	
	value = integrator.evaluate( a, b )
	
	! Para evitar que imprima en formato raro, por ejemplo +E002
	if( abs(value) > 1d-98 ) then
		write(*,"(E20.10)") value
	else
		write(*,"(E20.10)") 0.0_8
	end if
	
! 	if( .not. nFunc.xGrid.isEquallyspaced ) then
! 		write(*,*) ""
! 		write(*,*) ""
! 		call nFuncSmooth.save( oFileName.fstr )
! 	end if
	
end program main

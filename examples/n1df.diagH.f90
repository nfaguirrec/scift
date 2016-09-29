!>
!! @brief Test program
!!
program main
	use IOStream_
	use String_
	use UnitsConverter_
	use CommandLineParser_
	use RNFunction_
	use Spline_
	use FourierGridDiagonalization_
	implicit none
	
	type(String) :: fileName
	type(String) :: smoothOFile
	type(IFStream) :: ifile
	type(OFStream) :: ofile
	type(RNFunction) :: nFunc
	type(RNFunction) :: nFuncSmooth
	type(Spline) :: nFuncSpline
	type(FourierGridDiagonalization) :: solver
	type(CommandLineParser) :: parser
	character(5), allocatable :: tokens(:)
	type(String) :: buffer
	integer :: columns(2)
	real(8) :: units(2)
	real(8) :: reducedMass
	integer :: smoothFactor
	integer :: nStates
	integer :: task
	
	integer :: i
	
	parser.usage = &
	"$ boundStates -i fileName  [ -c col1:col2 ] [ -m mass ] [ -n nstates ] [ -s smooth ]"//ENDL//ENDL &
	//"OPTIONS"//ENDL//ENDL &
	//"-i fileName"//ENDL &
	//"   Input file name in columns format in a.u."//ENDL &
	//"-c columns"//ENDL &
	//"   Columns to use ( default 1:2 )"//ENDL &
	//"-m reduced mass"//ENDL &
	//"   reduced mass in a.u. ( default 1.0 for e- )"//ENDL &
	//"-n nstates"//ENDL &
	//"   Maximum number of states ( default 10 )"//ENDL &
	//"-so smoothOFile"//ENDL &
	//"   smooth output file ( default smooth.dat )"//ENDL &
	//"-s sfactor"//ENDL &
	//"   Smooth factor ( default 10 )"//ENDL &
	//"-t task"//ENDL &
	//"   Task i.e. eigenvalues (0) or eigenfunctions/eigenfunctions (1) ( default 0 )"//ENDL
	
	fileName = parser.getString( "-i" )
	
	smoothOFile = parser.getString( "-so", def="smooth.dat" )
	
	buffer = parser.get( "-c", def="1:2" )
	call buffer.split( tokens, ":" )
	columns = [ FString_toInteger(tokens(1)), FString_toInteger(tokens(2)) ]
	
	reducedMass = parser.getReal( "-m", def=1.0_8 )
	nStates = parser.getInteger( "-n", def=10 )
	smoothFactor = parser.getInteger( "-s", def=10 )
	task = parser.getInteger( "-s", def=0 )
	
	write(*,"(A,A)")        "# -------------- INPUT PARAMETERS -------------- "
	write(*,"(A)")          "#"
	write(*,"(A,A)")        "# fileName      (-i) = ", trim(fileName.fstr)
	write(*,"(A,A20)")      "# columns       (-c) = ", trim(FString_fromInteger(columns(1)))//":"//trim(FString_fromInteger(columns(2)))
	write(*,"(A,F20.5,A5)") "# reducedMass   (-m) = ", reducedMass, "a.u."
	write(*,"(A,I20)")      "# nStates       (-n) = ", nStates
	write(*,"(A,A)")        "# smoothOFile  (-so) = ", smoothOFile.fstr
	write(*,"(A,I20)")      "# smoothFactor  (-s) = ", smoothFactor
	write(*,"(A,I20)")      "# task          (-t) = ", task
	write(*,"(A)")          "#"
	
	call ifile.init( fileName.fstr )
	call nFunc.fromFStream( ifile, columns=columns )
	call ifile.close()
	
	write(*,"(A,A)")        "# -------------- GRID INFORMATION -------------- "
	write(*,"(A)")          "#"
	write(*,"(A,F20.5)")    "# min = ", nFunc.min()
	write(*,"(A,F20.5)")    "# max = ", nFunc.max()
	write(*,"(A,F20.5)")    "# stepSize = ", nFunc.stepSize()
	write(*,"(A,I20)")      "# nPoints = ", nFunc.nPoints()
	write(*,"(A,L10)")      "# equallySpaced = ", nFunc.isEquallyspaced()
	write(*,"(A)")          "#"
	write(*,*) ""
	
	call nFuncSpline.init( nFunc )
	nFuncSmooth = nFuncSpline.smooth( smoothFactor )
	call nFuncSmooth.save( smoothOFile.fstr )
	
	call solver.init( nFuncSmooth, rMass=reducedMass )
	
	if( task == 1 ) then
		call solver.run( task=FourierGridDiagonalization_EIGENFUNCTIONS, nStates=nStates, type=0 )
	else
		call solver.run( task=FourierGridDiagonalization_EIGENVALUES, nStates=nStates, type=0 )
	end if
	
	write(*,*) ""
	write(*,"(A5,A20,A20)") "#  i ", "EigenValues"
	write(*,"(A5,A20,A20)") "#    ", "       a.u."
	write(*,"(A5,A20,A20)") "# ---", "-----------"
	do i=1,nStates
		if ( solver.eigenValues(i) < 0.0_8 ) then
			write(*,"(I5,F20.10)") i, solver.eigenValues(i)
			
			if( task == 1 ) then
! 				call solver.cEigenFunctions(i).save("wf"//trim(FString_fromInteger(i))//".dat")
				call solver.rEigenFunctions(i).save("wf"//trim(FString_fromInteger(i))//".dat")
			end if
		end if
	end do

end program main

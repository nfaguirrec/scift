!>
!! @brief Test program
!!
program boundStates
	use IOStream_
	use String_
	use UnitsConverter_
	use CommandLineParser_
	use RNFunction_
	use Spline_
	use ThrularNumerovMethod_
	implicit none
	
	type(String) :: fileName
	type(String) :: smoothOFile
	type(IFStream) :: ifile
	type(OFStream) :: ofile
	type(RNFunction) :: nFunc
	type(RNFunction) :: nFuncSmooth
	type(Spline) :: nFuncSpline
	type(ThrularNumerovMethod) :: solver
	type(CommandLineParser) :: parser
	character(5), allocatable :: tokens(:)
	type(String) :: buffer
	integer :: columns(2)
	real(8) :: units(2)
	real(8) :: reducedMass
	integer :: smoothFactor
	integer :: nStates
	
	integer :: i
	
	parser.usage = &
	"$ boundStates -i fileName -c col1:col2 [ -m 17.48445 ] [ -n 10 ] [ -s 10 ]"//ENDL//ENDL &
	//"OPTIONS"//ENDL//ENDL &
	//"-i fileName"//ENDL &
	//"   Input file name in columns format"//ENDL &
	//"-c columns"//ENDL &
	//"   Columns to use ( default 1:2 )"//ENDL &
	//"-m reduced mass"//ENDL &
	//"   reduced mass in a.m.u. ( default 17.48445, for Cl2 )"//ENDL &
	//"-n nstates"//ENDL &
	//"   Maximum number of states ( default 10 )"//ENDL &
	//"-so smoothOFile"//ENDL &
	//"   smooth output file ( default smooth.dat )"//ENDL &
	//"-s sfactor"//ENDL &
	//"   Smooth factor ( default 10 )"//ENDL
	
	fileName = parser.getString( "-i" )
	
	smoothOFile = parser.getString( "-so", def="smooth.dat" )
	
	buffer = parser.get( "-c" )
	call buffer.split( tokens, ":" )
	columns = [ FString_toInteger(tokens(1)), FString_toInteger(tokens(2)) ]
	
	reducedMass = parser.getReal( "-m", def=0.5_8*34.9689_8 )*amu
	nStates = parser.getInteger( "-n", def=10 )
	smoothFactor = parser.getInteger( "-s", def=10 )
	
	write(*,"(A,A20)")      "# fileName      (-i) = ", trim(fileName.fstr)
	write(*,"(A,A20)")      "# columns       (-c) = ", trim(FString_fromInteger(columns(1)))//":"//trim(FString_fromInteger(columns(2)))
	write(*,"(A,F20.5,A5)") "# reducedMass   (-m) = ", reducedMass/amu, "amu"
	write(*,"(A,I20)")      "# nStates       (-n) = ", nStates
	write(*,"(A,A20)")      "# smoothOFile  (-so) = ", smoothOFile.fstr
	write(*,"(A,I20)")      "# smoothFactor  (-s) = ", smoothFactor
	write(*,*) ""
	
	call ifile.init( fileName.fstr )
! 	call nFunc.fromFStream( ifile, columns=columns, units=[angs,cm1] )
	call nFunc.fromFStream( ifile, columns=columns )
! 	call nFunc.show()
	call ifile.destroy()
	
	call nFuncSpline.init( nFunc )
	nFuncSmooth = nFuncSpline.smooth( smoothFactor )
! 	call nFuncSmooth.show()
! 	call nFuncSmooth.save( smoothOFile.fstr, units=[angs,cm1] )
	call nFuncSmooth.save( smoothOFile.fstr )
	
	call solver.init( nFuncSmooth, nStates=nStates, rMass=reducedMass )
	call solver.run()
	
	write(*,*) ""
	write(*,"(A5,A20,A20)") "#  i ", "EigenValues"
! 	write(*,"(A5,A20,A20)") "#    ", "       cm-1"
	write(*,"(A5,A20,A20)") "# ---", "-----------"
	do i=1,solver.nStates
			if ( solver.eigenValue(i) < 0.0_8 ) then
! 				write(*,"(I5,F20.10)") i, solver.eigenValue(i)/cm1
				write(*,"(I5,F20.10)") i, solver.eigenValue(i)
				call solver.eigenFunction(i).save("wf"//trim(FString_fromInteger(i))//".dat")
			end if
	end do

end program boundStates

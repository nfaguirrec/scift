!>
!! @brief Test program
!!
program main
	use UnitsConverter_
	use AtomicElementsDB_
	use String_
	use Matrix_
	use Molecule_
	implicit none
	
	character(1000) :: sBuffer
	type(String) :: iFileName1, iFileName2
	type(Molecule) :: mol1, mol2
	real(8) :: thr, alpha
	type(Matrix) :: I1, I2, Idiff, Iaver
	real(8) :: value
	logical :: debug
	
	integer :: i
	real(8) :: averMass, linearR, maxI
	real(8) :: R1, R2
	real(8) :: Ivalue(3)
	
	if( command_argument_count() < 2 ) then
		write(*,*) "Usage: molecule.compare file1 file2 [ debug ] [ thr ] [alpha]"
		write(*,*) "                                      false     0.92    1.1  "
		stop
	end if
	
	call get_command_argument( 1, sBuffer )
	iFileName1 = sBuffer
	
	call get_command_argument( 2, sBuffer )
	iFileName2 = sBuffer
	
	debug = .false.
	call get_command_argument( 3, sBuffer )
	if( len_trim(sBuffer) /= 0 ) debug = FString_toLogical( sBuffer )
	
	thr = 0.92_8
	call get_command_argument( 4, sBuffer )
	if( len_trim(sBuffer) /= 0 ) thr = FString_toReal(sBuffer)
	
	alpha = 1.1_8
	call get_command_argument( 5, sBuffer )
	if( len_trim(sBuffer) /= 0 ) alpha = FString_toReal(sBuffer)
	
	write(*,"(A,F10.3)") "Similarity threshold = ", thr
	write(*,"(A,F10.3)") "Bond tolerance scale factor = ", alpha
	
	call mol1.init( iFileName1.fstr )
	call mol2.init( iFileName2.fstr )
	
	write(*,"(A)", advance="no") "Chemical formula ... "
	if( mol1.compareFormula( mol2, debug=debug ) ) then
		write(*,*) "OK"
	else
		write(*,*) "Failed"
	end if
	
	write(*,"(A)", advance="no") "Geometry ... "
	if( mol1.compareGeometry( mol2, useMassWeight=.true., thr=thr, debug=debug ) ) then
		write(*,*) "OK"
	else
		write(*,*) "Failed"
	end if
	
	write(*,"(A)", advance="no") "Connectivity ... "
	if( mol1.compareConnectivity( mol2, alpha=alpha, thr=thr, debug=debug ) ) then
		write(*,*) "OK"
	else
		write(*,*) "Failed"
	end if
	
end program main

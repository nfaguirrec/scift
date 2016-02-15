!>
!! @brief Test program
!!
program main
	use UnitsConverter_
	use String_
	use CommandLineParser_
	use Molecule_
	implicit none
	
	type(CommandLineParser) :: parser
	type(String) :: iFileName
	real(8) :: x, y, z
	type(Molecule) :: mol
	
	iFileName = parser.getString( "-i" )
	x = parser.getReal( "-x", def=0.0_8 )*angs
	y = parser.getReal( "-y", def=0.0_8 )*angs
	z = parser.getReal( "-z", def=0.0_8 )*angs
	
	call mol.init( iFileName.fstr )
	
	call mol.setCenter( [x,y,z] )
	
	call mol.save()
end program main

!>
!! @brief Test program
!!
program main
	use String_
	use CommandLineParser_
	use Molecule_
	implicit none
	
	type(CommandLineParser) :: parser
	type(String) :: iFileName
	real(8) :: alpha, beta, gamma
	logical :: random
	type(Molecule) :: mol
	
	iFileName = parser.getString( "-i" )
! 	alpha = parser.getReal( "-alpha", def=0.0_8 )
! 	beta = parser.getReal( "-beta", def=0.0_8 )
! 	gamma = parser.getReal( "-gamma", def=0.0_8 )
! 	random = parser.getLogical( "-random", def=.false. )
	
	call mol.init( iFileName.fstr )
	call mol.orient()
	
! 	if( .not. random ) then
! 		call mol.rotate( alpha, beta, gamma )
! 	else
! 		call mol.rotate( random=.true. )
! 	end if
	
	call mol.save()
end program main

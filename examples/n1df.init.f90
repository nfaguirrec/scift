!>
!! @brief Test program
!!
program main
	use GOptions_
	use String_
	use CommandLineParser_
	use RNFunction_
	use CNFunction_

	implicit none
	type(CommandLineParser) :: parser
	type(String) :: iFileRef
	type(String) :: oFileName
	real(8) :: rInitValue
	complex(8) :: cInitValue
	
	integer :: fileTypeA
	type(RNFunction) :: rFunc, rA
	type(CNFunction) :: cFunc, cA
	
	type(String) :: strBuffer
	character(5), allocatable :: tokens(:)
	
	if( command_argument_count() < 4 ) then
		write(*,"(A)") "usage:"
		write(*,"(A)") "   n1df.init -o oFileName ( -b iFileRef | -xgrid xmin,xmax,nPoints ) [-init value]"
		write(*,"(A)") "                                                                             0.0  "
		write(*,"(A)") ""
		stop
	end if
	
	oFileName = parser.getString( "-o" )
	iFileRef = parser.getString( "-b", def=FString_NULL )
	
	if( iFileRef /= FString_NULL ) then
		fileTypeA = CNFunction_checkTypeN1DF( iFileRef.fstr )
		if( fileTypeA == 0 ) then
			call rA.init( iFileRef.fstr )
		else if( fileTypeA == 1 ) then
			call cA.init( iFileRef.fstr )
		else
			write(0,*) "### ERROR ### unknown format for "//trim(iFileRef.fstr)
			stop
		end if
		
		if( fileTypeA == 0 ) then
			
			rInitValue = parser.getReal( "-init", def=0.0_8 )
			call rFunc.init( rA.xGrid, value=rInitValue )
			
		else if( fileTypeA == 1 ) then
			
			CInitValue = parser.getReal( "-init", def=0.0_8 )
			call cFunc.init( cA.xGrid, value=cInitValue )
			
		end if
	else
		fileTypeA = 0
		
		strBuffer = parser.getString( "-xgrid" )
		call strBuffer.split( tokens, "," )
		
		rInitValue = parser.getReal( "-init", def=0.0_8 )
		call rFunc.init( FString_toReal(tokens(1)), FString_toReal(tokens(2)), nPoints=FString_toInteger(tokens(3)), value=rInitValue )
	end if
	
	!---------------------------------------------
	! Saving AB
	!---------------------------------------------
	if( fileTypeA == 0 ) then
		call rFunc.save( oFileName.fstr )
	else if( fileTypeA == 1 ) then
		call cFunc.save( oFileName.fstr )
	end if
	
	if( allocated(tokens) ) deallocate( tokens )
	
end program main

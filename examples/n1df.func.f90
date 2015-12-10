!>
!! @brief Test program
!!
program main
	use MathParser_
	use RNFunction_
	use CNFunction_

	implicit none
	character(1000) :: formula
	character(255) :: fileNameA
	character(255) :: fileNameFuncA
	character(1) :: oper
	real(8) :: prefactor
	integer :: argc
	character(255) :: buffer
	
	integer :: fileTypeA
	type(RNFunction) :: rFunc, rA, rFuncA
	type(CNFunction) :: cFunc, cA, cFuncA
	type(MathParser) :: parser
	
	argc = command_argument_count()
	
	if( argc < 4 ) then
		write(*,*) "usage:"
		write(*,*) "   n1df.func "//achar(39)//"f(x)"//achar(39)//" "//achar(39)//"oper"//achar(39)//" fileA fileFuncA [prefactor]"
		write(*,*) ""
		write(*,*) "   fA = f(x) .oper. a*A"
		stop
	end if
	
	call get_command_argument( 1, formula )
	call get_command_argument( 2, oper )
	call get_command_argument( 3, fileNameA )
	call get_command_argument( 4, fileNameFuncA )
	
	prefactor = 1.0_8
	select case( argc )
		case( 5 )
			call get_command_argument( 5, buffer )
			read(buffer,*) prefactor
	end select
	
	!---------------------------------------------
	! Loading A
	!---------------------------------------------
	fileTypeA = CNFunction_checkTypeN1DF( fileNameA )
	if( fileTypeA == 0 ) then
		call rA.init( fileNameA )
	else if( fileTypeA == 1 ) then
		call cA.init( fileNameA )
	else
		write(0,*) "### ERROR ### unknown format for "//trim(fileNameA)
		stop
	end if
	
	call parser.init()
	
	if( fileTypeA == 0 ) then
		call parser.parseFunction( formula, [ 'x' ]  )
		call rFunc.init( rA.xGrid, evaluateFormulaR )
	else if( fileTypeA == 1 ) then
		call parser.parseFunction( formula, [ 'x' ]  )
		call cFunc.init( cA.xGrid, evaluateFormulaC )
	end if
	
	!---------------------------------------------
	! Operations
	!---------------------------------------------
	if( fileTypeA == 0 ) then
		rA = rA*prefactor
		
		select case( trim(oper) )
			case( '+' )
				rFuncA = rFunc + rA
			case( '-' )
				rFuncA = rFunc - rA
			case( '*' )
				rFuncA = rFunc * rA
			case( '/' )
				rFuncA = rFunc / rA
		end select

	else if( fileTypeA == 1 ) then
		cA = cA*prefactor
		
		select case( trim(oper) )
			case( '+' )
				cFuncA = cFunc + cA
			case( '-' )
				cFuncA = cFunc - cA
			case( '*' )
				cFuncA = cFunc * cA
			case( '/' )
				cFuncA = cFunc / cA
		end select
	end if
	
	!---------------------------------------------
	! Saving AB
	!---------------------------------------------
	if( fileTypeA == 0 ) then
		call rFuncA.save( fileNameFuncA )
	else if( fileTypeA == 1 ) then
		call cFuncA.save( fileNameFuncA )
	end if
	
	contains
	
	!>
	!!
	!!
	function evaluateFormulaR( x ) result( output )
		real(8), intent(in) :: x
		real(8) :: output
		
		output = parser.evaluateFunction( [ x ] )
	end function evaluateFormulaR
	
	!>
	!!
	!!
	function evaluateFormulaC( x ) result( output )
		real(8), intent(in) :: x
		complex(8) :: output
		
		output = cmplx( parser.evaluateFunction( [ x ] ), 0.0_8 )
	end function evaluateFormulaC

end program main

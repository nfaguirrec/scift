!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!  This file is part of scift (Scientific Fortran Tools).
!!  Copyright (C) by authors (2000-2013)
!!  
!!  Authors (alphabetic order):
!!    * Aguirre N.F. (nfaguirrec@gmail.com)  (2011-2013)
!!    * Roland Schmehl (roland.schmehl@alumni.uni-karlsruhe.de)  (2000-2008)
!!  
!!  Contributors (alphabetic order):
!!  
!!  Redistribution and use in source and binary forms, with or
!!  without modification, are permitted provided that the
!!  following conditions are met:
!!  
!!   * Redistributions of binary or source code must retain
!!     the above copyright notice and this list of conditions
!!     and/or other materials provided with the distribution.
!!   * All advertising materials mentioning features or use of
!!     this software must display the following acknowledgement:
!!     
!!     This product includes software from scift
!!     (Scientific Fortran Tools) project and its contributors.
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!!**********************************************************************************
!!                                                                                 !
!!    (2000-2008) Roland Schmehl                                                   !
!!                roland.schmehl@alumni.uni-karlsruhe.de                           !
!!                                                                                 !
!!    This software is distributable under the BSD license. See the terms of the   !
!!    BSD license in the documentation provided with this software.                !
!!                                                                                 !
!!    Fortran 90 function parser v1.1                                              !
!!    -------------------------------                                              !
!!                                                                                 !
!!    This function parser module is intended for applications where a set of      !
!!    mathematical fortran-style expressions is specified at runtime and is then   !
!!    evaluated for a large number of variable values. This is done by compiling   !
!!    the set of function strings into byte code, which is interpreted efficiently !
!!    for the various variable values.                                             !
!!                                                                                 !
!!    The source code is available from http://fparser.sourceforge.net             !
!!                                                                                 !
!!    Please send comments, corrections or questions to the author:                !
!!    Roland Schmehl <roland.schmehl@alumni.uni-karlsruhe.de>                      !
!!                                                                                 !
!!    The function parser concept is based on a C++ class library written by       !
!!    Juha Nieminen <warp@iki.fi> available from:                                  !
!!    http://warp.povusers.org/FunctionParser/                                     !
!!                                                                                 !
!!**********************************************************************************

!>
!! @brief
!!
module MathParser_
	implicit none
	private
	
	public :: &
		MathParser_test
		
	real(8), parameter :: ZERO = 0.0_8
	
	integer, parameter :: C_IMMED    = 1
	integer, parameter :: C_NEG      = 2
	integer, parameter :: C_ADD      = 3 
	integer, parameter :: C_SUB      = 4 
	integer, parameter :: C_MUL      = 5 
	integer, parameter :: C_DIV      = 6 
	integer, parameter :: C_POW      = 7 
	
	integer, parameter :: C_ABS      = 8
	integer, parameter :: C_EXP      = 9
	integer, parameter :: C_LOG10    = 10
	integer, parameter :: C_LOG      = 11
	integer, parameter :: C_SQRT     = 12
	integer, parameter :: C_SINH     = 13
	integer, parameter :: C_COSH     = 14
	integer, parameter :: C_TANH     = 15
	integer, parameter :: C_SIN      = 16
	integer, parameter :: C_COS      = 17
	integer, parameter :: C_TAN      = 18
	integer, parameter :: C_ASIN     = 19
	integer, parameter :: C_ACOS     = 20
	integer, parameter :: C_ATAN     = 21
	integer, parameter :: C_SGN      = 22
	integer, parameter :: C_CEIL     = 23
	integer, parameter :: C_FLOOR    = 24
	integer, parameter :: C_USTEP    = 25
	integer, parameter :: C_UBOX     = 26
	
	integer, parameter :: C_VARBEGIN = 27
	
	character(1), parameter :: STR_OPERATORS(C_ADD:C_POW) &
		= [ '+', '-', '*', '/', '^' ]
	
	character(6), parameter :: STR_FUNCTIONS(C_ABS:C_UBOX) &
		= [ 'abs   ', 'exp   ', 'log10 ', 'log   ', 'sqrt  ', 'sinh  ', 'cosh  ', &
		    'tanh  ', 'sin   ', 'cos   ', 'tan   ', 'asin  ', 'acos  ', 'atan  ', &
		    'sgn   ', 'ceil  ', 'floor ', 'ustep ', 'ubox  ' ]
		     
	character(*), parameter :: ERROR_MESSAGE(4) &
		 = [ 'Division by zero', 'Argument of SQRT negative', &
		   'Argument of LOG negative', 'Argument of ASIN or ACOS illegal' ]
	
	type, public :: MathParser
		integer, private, pointer :: bytecode(:)
		integer, private :: bytecodeSize
		real(8), private, pointer :: immed(:)
		integer, private :: immedSize
		real(8), private, pointer :: stack(:)
		integer, private :: stackSize
		integer, private :: stackPtr
		
		integer, private :: error ! =0: no error occured, >0: evaluation error
		
		integer, private, allocatable :: funcPosition(:)
		
		contains
			generic :: init => initDefault
			procedure :: initDefault
! 			generic :: assignment(=) => copyMathParser
! 			procedure :: copy => copyMathParser
! 			procedure :: copyMathParser
			final :: destroy
			
			procedure :: compile
			procedure :: compileSubstr
			procedure :: parseFunction
			procedure :: evaluateFunction
			procedure :: errorMessage
	end type MathParser
	
	integer, allocatable :: ipos(:) ! Associates function strings
	
	contains
	
	!>
	!! @brief Constructor
	!!
	subroutine initDefault( this )
		class(MathParser) :: this
		
		nullify( this.bytecode )
		nullify( this.immed )
		nullify( this.stack )
	end subroutine initDefault
	
	!>
	!! @brief Copy constructor
	!!
! 	subroutine copyMathParser( this, other )
! 			class(MathParser), intent(inout) :: this
! 			class(MathParser), intent(in) :: other
! 			
! 			write(*,*) "### ERROR ### MathParser.copyMathParser(). Copy constructor is not implemented yet"
! 			stop
! 	end subroutine copyMathParser
	
	!>
	!! @brief Destructor
	!!
	subroutine destroy( this )
		type(MathParser) :: this
		
		if( associated(this.bytecode) ) deallocate( this.bytecode )
		if( associated(this.immed) ) deallocate( this.immed )
		if( associated(this.stack) ) deallocate( this.stack )
		if( allocated(this.funcPosition) ) deallocate( this.funcPosition )
	end subroutine destroy
	
	!>
	!! Parse ith function string funcStr and compile it into bytecode
	!!
	!! @param funcStr Function string
	!! @param var Array with variable names
	!!
	subroutine parseFunction( this, funcStr, var )
		class(MathParser) :: this
		character(*), intent(in) :: funcStr
		character(*), intent(in) :: var(:)
		
		character(len(funcStr)) :: func ! Function string, local use
		
		allocate( ipos(len_trim(funcStr)) ) ! Char. positions in orig. string
		func = funcStr ! Local copy of function string
		call replace( '**', '^ ', func ) ! Exponent into 1-Char. format
		call removeSpaces( func ) ! Condense function string
		call checkSyntax( this, func, funcStr, var )
		deallocate( ipos )
		
		call compile( this, func, var ) ! Compile into bytecode
	end subroutine parseFunction
  
	!>
	!! Evaluate bytecode of ith function for the values passed in array Val(:)
	!!
	!! @param val Variable values
	!!
	function evaluateFunction( this, val ) result( res )
		class(MathParser) :: this
		real(8), intent(in) :: val(:)
		real(8) :: res
		integer :: IP ! Instruction pointer
		integer :: DP ! Data pointer
		integer :: SP ! Stack pointer
		
		DP = 1
		SP = 0
		do IP=1,this.ByteCodeSize
			select case( this.ByteCode(IP) )
				case( C_IMMED )
					SP=SP+1
					this.stack(SP) = this.Immed(DP)
					DP = DP+1
				case( C_NEG )
					this.stack(SP) = -this.stack(SP)
				case( C_ADD )
					this.stack(SP-1) = this.stack(SP-1)+this.stack(SP)
					SP = SP-1
				case( C_SUB )
					this.stack(SP-1) = this.stack(SP-1)-this.stack(SP)
					SP = SP-1
				case( C_MUL )
					this.stack(SP-1) = this.stack(SP-1)*this.stack(SP)
					SP = SP-1
				case( C_DIV )
					if( this.stack(SP) == ZERO ) then
						this.error = 1
						res = ZERO
						return
					endif
					this.stack(SP-1) = this.stack(SP-1)/this.stack(SP)
					SP = SP-1
				case( C_POW )
					this.stack(SP-1) = this.stack(SP-1)**this.stack(SP)
					SP = SP-1
				case( C_ABS )
					this.stack(SP) = abs( this.stack(SP) )
				case( C_EXP )
					this.stack(SP) = exp( this.stack(SP) )
				case ( C_LOG10 )
					if( this.stack(SP) <= ZERO ) then
						this.error = 3
						res = ZERO
						return
					endif
					this.stack(SP) = log10( this.stack(SP) )
				case( C_LOG )
					if( this.stack(SP) <= ZERO ) then
						this.error = 3
						res = ZERO
						return
					endif
					this.stack(SP) = log( this.stack(SP) )
				case( C_SQRT )
					if( this.stack(SP) < ZERO ) then
						this.error = 3
						res = ZERO
						return
					endif
					this.stack(SP) = sqrt( this.stack(SP) )
				case( C_SINH )
					this.stack(SP) = sinh( this.stack(SP) )
				case( C_COSH )
					this.stack(SP) = cosh( this.stack(SP) )
				case( C_TANH )
					this.stack(SP) = tanh( this.stack(SP) )
				case( C_SIN )
					this.stack(SP) = sin( this.stack(SP) )
				case( C_COS )
					this.stack(SP) = cos( this.stack(SP) )
				case( C_TAN )
					this.stack(SP) = tan( this.stack(SP) )
				case( C_ASIN )
					if( (this.stack(SP)<-1.0_8) .or. (this.stack(SP)>1.0_8) ) then
						this.error = 4
						res = ZERO
						return
					endif
					this.stack(SP) = asin( this.stack(SP) )
				case( C_ACOS )
					if( (this.stack(SP)<-1.0_8) .or. (this.stack(SP)>1.0_8) ) then
						this.error = 4
						res = ZERO
						return
					endif
					this.stack(SP) = acos( this.stack(SP) )
				case( C_ATAN )
					this.stack(SP) = atan( this.stack(SP) )
				case( C_SGN )
					this.stack(SP) = sign( 1.0_8, this.stack(SP) )
				case( C_CEIL )
					this.stack(SP) = ceiling( this.stack(SP) )
				case( C_FLOOR )
					this.stack(SP) = floor( this.stack(SP) )
#define ustep(x) 0.5_8*( 1.0_8 + tanh( 1.0d12*(x) ) )
				case( C_USTEP )
					this.stack(SP) = ustep( this.stack(SP) )
				case( C_UBOX )
					this.stack(SP) = ustep( this.stack(SP)+0.5_8 ) - ustep( this.stack(SP)-0.5_8 )
#undef ustep
				case  default
					SP = SP+1
					this.stack(SP) = val( this.ByteCode(IP)-C_VARBEGIN+1 )
			end select
		end do
		
		this.error = 0
		res = this.stack(1)
	end function evaluateFunction
	
	!>
	!! Check syntax of function string,  returns 0 if syntax is ok
	!!
	!! @param func Function string without spaces
	!! @param funcStr Original function string
	!! @param var Array with variable names
	!!
	subroutine checkSyntax( this, func, funcStr, var )
		class(MathParser) :: this
		character(*), intent(in) :: func
		character(*), intent(in) :: funcStr
		character(*), dimension(:), intent(in) :: var
		
		integer :: n
		character(1) :: c
		real(8) :: r
		logical :: err
		integer :: parCounter ! Parenthesis counter
		integer :: j,ib,in,lFunc
		
		j = 1
		parCounter = 0
		lFunc = len_trim(func)
		
		do while( .true. )
			if( j > lFunc) call parseErrMsg( j, funcStr )
			c = func(j:j)
			
			! Check for valid operand (must appear)
			if( c == '-' .OR. c == '+' ) then ! Check for leading - or +
				j = j+1
				if( j > lFunc) then
					call parseErrMsg(j, funcStr, 'Missing operand' )
				end if
				c = func(j:j)
				if( any(c == STR_OPERATORS)) call parseErrMsg( j, funcStr, 'Multiple operators' )
			end if
			n = MathFunctionIndex (func(j:))
			if( n > 0) then ! Check for math function
				j = j+len_trim(STR_FUNCTIONS(n))
				if( j > lFunc) call parseErrMsg( j, funcStr, 'Missing function argument' )
				c = func(j:j)
				if( c /= '(' ) call parseErrMsg( j, funcStr, 'Missing opening parenthesis' )
			end if
			if( c == '(' ) then ! Check for opening parenthesis
				parCounter = parCounter+1
				j = j+1
				CYCLE
			end if
			if( scan(c,'0123456789.' ) > 0 ) then ! Check for number
				r = strToReal(func(j:),ib,in,err)
				if( err) call parseErrMsg( j, funcStr, 'Invalid number format:  '//func(j+ib-1:j+in-2))
				j = j+in-1
				if( j > lFunc) exit
				c = func(j:j)
			else ! Check for variable
				n = variableIndex(func(j:),var,ib,in)
				if( n == 0) call parseErrMsg( j, funcStr, 'Invalid element: '//func(j+ib-1:j+in-2))
				j = j+in-1
				if( j > lFunc) exit
				c = func(j:j)
			end if
			do while (c == ')' ) ! Check for closing parenthesis
				parCounter = parCounter-1
				if( parCounter < 0) call parseErrMsg( j, funcStr, 'Mismatched parenthesis' )
				if( func(j-1:j-1) == '(' ) call parseErrMsg (j-1, funcStr, 'Empty parentheses' )
				j = j+1
				if( j > lFunc) exit
				c = func(j:j)
			end do
				
			! Now, we have a legal operand: A legal operator or end of string must follow
			if( j > lFunc) exit
			if( any(c == STR_OPERATORS)) then ! Check for multiple operators
				if( j+1 > lFunc) call parseErrMsg( j, funcStr)
				if( any(func(j+1:j+1) == STR_OPERATORS)) call parseErrMsg (j+1, funcStr, 'Multiple operators' )
			else ! Check for next operand
				call parseErrMsg( j, funcStr, 'Missing operator' )
			end if
			
			! Now, we have an operand and an operator: the next loop will check for another 
			! operand (must appear)
			j = j+1
		end do
		
		if( parCounter > 0 ) then
			call parseErrMsg( j, funcStr, 'Missing )' )
		end if
	end subroutine checkSyntax
	
	!>
	!! Return error message
	!!
	function errorMessage( this ) result (msg)
		class(MathParser) :: this
		character(len(ERROR_MESSAGE)) :: msg
		
		if( this.error < 1 .or. this.error > size(ERROR_MESSAGE) ) then
			msg = ''
		else
			msg = ERROR_MESSAGE(this.error)
		endif
	end function errorMessage
	
	!>
	!! Print error message and terminate program
	!!
	subroutine parseErrMsg( j, funcStr, msg )
		integer, intent(in) :: j
		character(*), intent(in) :: funcStr       ! Original function string
		character(*), optional, intent(in) :: msg
		
		integer :: k
		
		if( present(msg) ) then
			write(*,*) '*** Error in syntax of function string: '//msg
		else
			write(*,*) '*** Error in syntax of function string:'
		endif
		
		write(*,'(a)') ' '//trim(funcStr)
		
		do k=1,ipos(j)
			write(*,'(A)',advance='no') ' '! Advance to the jth position
		end do
		
		write(*,'(A)') '^'
		stop
	end subroutine parseErrMsg
	
	!>
	!! @brief Return operator index
	!!
	function operatorIndex( c ) result( n )
		character(1), intent(in) :: c
		integer :: n
		
		integer :: j
		
		n = 0
		do j=C_ADD,C_POW
			if( c == STR_OPERATORS(j) ) then
				n = j
				exit
			end if
		end do
	end function operatorIndex
	
	!>
	!! @brief Return index of math function beginnig at 1st position of string str
	!!
	integer function mathFunctionIndex( str )
		character(*), intent(in) :: str
		
		integer :: j, k
		character(len(STR_FUNCTIONS)) :: lcFunc
		
		mathFunctionIndex = 0
		do j=C_ABS,C_UBOX ! Check all math functions
			k = min( len_trim(STR_FUNCTIONS(j)), len(str) )   
			
			call toLowercase( str(1:k), lcFunc )
			
			if( lcFunc(1:k) == STR_FUNCTIONS(j) ) then ! Compare lower case letters
				mathFunctionIndex = j ! Found a matching function
				exit
			end if
		end do
	end function mathFunctionIndex
	
	!>
	!! Return index of variable at begin of string str (returns 0 if no variable found)
	!!
	!! @param str String
	!! @param var Array with variable names
	!! @param n Index of variable
	!! @param ibegin Start position of variable name
	!! @param inext Position of character after name
	!!
	function variableIndex( str, var, ibegin, inext ) result( n )
		character(*), intent(in) :: str
		character(*), intent(in) :: var(:)
		integer, optional, intent(out) :: ibegin
		integer, optional, intent(out) :: inext
		integer :: n
		
		integer :: j,ib,in,lstr
		
		n = 0
		lstr = len_trim(str)
		if( lstr > 0 ) then
			do ib=1,lstr ! Search for first character in str
				if ( str(ib:ib) /= ' ' ) exit ! When lstr>0 at least 1 char in str
			end do
			
			do in=ib,lstr ! Search for name terminators
				if( scan(str(in:in),'+-*/^) ') > 0 ) exit
			end do
			
			do j=1,size(var)
				if( str(ib:in-1) == var(j) ) then
					n = j ! Variable name found
					exit
				end if
			end do
		end if
		
		if( present(ibegin) ) ibegin = ib
		if( present(inext) ) inext  = in
	end function variableIndex
	
	!>
	!! @brief Remove Spaces from string, remember positions of characters in old string
	!!
	subroutine removeSpaces( str )
		character(*), intent(inout) :: str
		
		integer :: k
		integer :: lstr
		
		lstr = len_trim(str)
		ipos = (/ (k,k=1,lstr) /)
		
		k = 1
		do while( str(k:lstr) /= ' ' )                             
			if( str(k:k) == ' ' ) then
				str(k:lstr)  = str(k+1:lstr)//' '                  ! Move 1 character to left
				ipos(k:lstr) = (/ ipos(k+1:lstr), 0 /)             ! Move 1 element to left
				k = k-1
			end if
			k = k+1
		end do
	end subroutine removeSpaces
	
	!>
	!! Replace ALL appearances of character set ca in string str by character set cb
	!!
	subroutine replace( ca, cb, str )
		character(*), intent(in) :: ca
		character(len(ca)), intent(in) :: cb ! LEN(ca) must be LEN(cb)
		character(*), intent(inout) :: str
		
		integer :: j,lca
		
		lca = len(ca)
		do j=1,len_trim(str)-lca+1
			if( str(j:j+lca-1) == ca ) str(j:j+lca-1) = cb
		end do
	end subroutine replace
	
	!>
	!! Compile i-th function string F into bytecode
	!!
	!! @param i Function identifier
	!! @param f Function string
	!! @param var Array with variable names
	!!
	subroutine compile( this, f, var )
		class(MathParser) :: this
		character(*), intent(in) :: f
		character(*), intent(in) :: var(:)
		
		integer :: istat
		
		if( associated(this.bytecode) ) then
			deallocate( this.bytecode )
			deallocate( this.immed )
			deallocate( this.stack )
		end if
		
		this.bytecodeSize = 0
		this.immedSize = 0
		this.stackSize = 0
		this.stackPtr = 0
		
		call compileSubstr( this, f, 1, len_trim(f), var ) ! compile string to determine size
		
		allocate( this.bytecode( this.bytecodeSize ), stat=istat )
		allocate( this.immed( this.immedSize ), stat=istat )
		allocate( this.stack( this.stackSize ), stat=istat )
		
		if( istat /= 0 ) then
			write(*,*) '*** MathParser error: Memmory allocation for byte code failed'
			stop
		else
			this.bytecodeSize = 0
			this.immedSize = 0
			this.stackSize = 0
			this.stackPtr = 0
			
			call compileSubstr( this, f, 1, len_trim(f), var ) ! Compile string into bytecode
		end if
	end subroutine compile
	
	!>
	!! Compile function string f into bytecode
	!!
	!! @param f Function substring
	!! @param b Begin position substring
	!! @param e End position substring
	!! @param var Array with variable names
	!!
	recursive subroutine compileSubstr( this, f, b, e, var )
		class(MathParser) :: this
		character(*), intent(in) :: f
		integer, intent(in) :: b,e
		character(*), intent(in) :: var(:)
		
		integer :: n
		integer :: b2,j,k,io
		character(*), parameter :: calpha = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'
		
		! Check for special cases of substring
		if( f(b:b) == '+' ) then ! Case 1: f(b:e) = '+...'
! 			write(*,*)'1. f(b:e) = "+..."'
			call compileSubstr( this, f, b+1, e, var )
			return
		elseif( completelyEnclosed( f, b, e ) ) then ! Case 2: f(b:e) = '(...)'
! 			write(*,*)'2. f(b:e) = "(...)"'
			call compileSubstr( this, f, b+1, e-1, var )
			return
		elseif( scan(f(b:b),calpha) > 0 ) then
			n = mathFunctionIndex( f(b:e) )
			if( n > 0 ) then
				b2 = b + index( f(b:e), '(' ) - 1
				if( completelyEnclosed( f, b2, e ) ) then ! Case 3: f(b:e) = 'fcn(...)'
! 					write(*,*)'3. f(b:e) = "fcn(...)"'
					call compileSubstr( this, f, b2+1, e-1, var )
					call addCompiledByte( this, n )
					return
				end if
			end if
		elseif( f(b:b) == '-' ) then
			if( completelyEnclosed(f, b+1, e) ) then ! Case 4: f(b:e) = '-(...)'
! 			        write(*,*)'4. f(b:e) = "-(...)"'
				call compileSubstr( this, f, b+2, e-1, var )
				call addCompiledByte( this, C_NEG )
				return
			elseif( scan( f(b+1:b+1), calpha ) > 0 ) then
				n = mathFunctionIndex( f(b+1:e) )
				if( n > 0 ) then
					b2 = b+index(f(b+1:e),'(')
					if( completelyEnclosed( f, b2, e ) ) then ! Case 5: f(b:e) = '-fcn(...)'
! 						write(*,*)'5. f(b:e) = "-fcn(...)"'
						call compileSubstr( this, f, b2+1, e-1, var)
						call addCompiledByte( this, n )
						call addCompiledByte( this, C_NEG )
						return
					end if
				end if
			endif
		end if
		
		! Check for operator in substring: check only base level (k=0), exclude expr. in ()
		do io=C_ADD,C_POW ! Increasing priority +-*/^
			k = 0
			do j=e,b,-1
				if( f(j:j) == ')' ) then
					k = k+1
				elseif( f(j:j) == '(' ) then
					k = k-1
				end if
					if( k == 0 .and. f(j:j) == STR_OPERATORS(io) .and. isBinaryOperator(j, F) ) then
						if( any(f(j:j) == STR_OPERATORS(C_MUL:C_POW)) .and. f(b:b) == '-' ) then ! Case 6: F(b:e) = '-...Op...' with Op > -
! 						write(*,*)'6. f(b:e) = "-...Op..." with Op > -'
						call compileSubstr( this, f, b+1, e, var )
						call addCompiledByte( this, C_NEG )
						return
					else ! Case 7: f(b:e) = '...BinOp...'
! 						write(*,*)'7. Binary operator',f(j:j)
						call compileSubstr( this, f, b, j-1, var )
						call compileSubstr( this, f, j+1, e, var )
						call addCompiledByte( this, operatorIndex(STR_OPERATORS(io)) )
						this.StackPtr = this.StackPtr - 1
						return
					end if
				end if
			end do
		end do
		
		! check for remaining items, i.e. variables or explicit numbers
		b2 = b
		if( f(b:b) == '-' ) b2 = b2+1
		n = mathItemIndex( this, f(b2:e), var )
! 		write(*,*)'8. AddCompiledByte ', n
		call addCompiledByte( this, n )
		this.stackPtr = this.stackPtr + 1
		if( this.StackPtr > this.StackSize ) this.stackSize = this.stackSize + 1
		if( b2 > b ) call addCompiledByte( this, C_NEG )
	end subroutine compileSubstr
	
	!>
	!! Add compiled byte to bytecode
	!!
	subroutine addCompiledByte( this, b )
		class(MathParser) :: this
		integer, intent(in) :: b ! Value of byte to be added
		
		this.ByteCodeSize = this.ByteCodeSize + 1
		IF( associated(this.ByteCode) ) this.byteCode( this.byteCodeSize ) = b
	end subroutine addCompiledByte
	
	!>
	!! Return math item index, if item is real number, enter it into Comp-structure
	!!
	function mathItemIndex( this, f, var ) result( n )
		class(MathParser) :: this
		character(*), intent(in) :: f ! Function substring
		character(*), intent(in) :: var(:) ! Array with variable names
		integer :: n ! Byte value of math item
		
		n = 0
		if( scan( f(1:1), '0123456789.' ) > 0 ) then ! Check for begin of a number
			this.immedSize = this.immedSize + 1
			if( associated(this.immed) ) then
				this.immed( this.immedSize ) = strToReal( f )
			end if
			n = C_IMMED
		else                                                     ! Check for a variable
			n = variableIndex( f, var )
			if( n > 0 ) n = C_VARBEGIN+n-1
		end if
	end function mathItemIndex
	
	!>
	!! Check if function substring F(b:e) is completely enclosed by a pair of parenthesis
	!!
	function completelyEnclosed( f, b, e ) result( res )
		character(*), intent(in) :: f ! Function substring
		integer, intent(in) :: b, e ! First and last pos. of substring
		
		logical :: res
		integer :: j,k
		
		res = .false.
		if( f(b:b) == '(' .and. f(e:e) == ')' ) then
			k = 0
			do j=b+1,e-1
				if( f(j:j) == '(' ) then
					k = k+1
				else if( f(j:j) == ')' ) then
					k = k-1
				end if
				if (k < 0) exit
			end do
			if (k == 0) res=.true. ! All opened parenthesis closed
		end if
	end function completelyEnclosed

	!>
	!! Check if operator str(j:j) in string str is binary operator
	!! Special cases already covered elsewhere:( that is corrected in v1.1 )
	!! operator character str(j:j) is first character of string (j=1)
	!!
	!! @param j Position of Operator
	!! @param str String
	!!
	!! @return res Result
	!!
	function isBinaryOperator( j, str ) result( res )
		integer, intent(in) :: j
		character(*), intent(in) :: str
		logical :: res
		
		integer :: k
		logical :: dFlag
		logical :: pFlag
		
		res = .true.
		
		if( str(j:j) == '+' .or. str(j:j) == '-' ) then ! Plus or minus sign:
			if( j == 1 ) then ! leading unary operator ?
				res = .false.
			else if ( scan( str(j-1:j-1), '+-*/^(' ) > 0 ) then ! other unary operator ?
				res = .false.
			else if ( scan( str(j+1:j+1), '0123456789' ) > 0 .and. & ! in exponent of real number ?
				scan( str(j-1:j-1), 'eEdD' ) > 0 ) then
				
				dFlag=.false.; pFlag=.false.
				
				k = j-1
				do while( k > 1 ) ! step to the left in mantissa 
					k = k-1
					
					if( scan( str(k:k), '0123456789' ) > 0 ) then
						dFlag = .true.
					else if( str(k:k) == '.' ) then
						if (pFlag) then
							exit ! exit: 2nd appearance of '.'
						else
							pFlag = .true. ! mark 1st appearance of '.'
						endif
					else
						exit ! all other characters
					end if
				end do
				if( dFlag .and. ( k == 1 .or. scan( str(k:k), '+-*/^(' ) > 0 ) ) res = .false.
			end if
		end if
	end function isBinaryOperator
	
	!>
	!! @brief Get real number from string - Format: [blanks][+|-][nnn][.nnn][e|E|d|D[+|-]nnn]
	!!
	!! @param str String to transform
	!! @param ibegin Start position of real number (OPTIONAL)
	!! @param 1st character after real number (OUTPUT,OPTIONAL)
	!! @param error Error flag (OPTIONAL)
	!!
	!! @return res String converted to real
	!!
	function strToReal( str, ibegin, inext, error ) result ( res )
		character(*), intent(in) :: str
		integer, optional, intent(out) :: ibegin
		integer, optional, intent(out) :: inext
		logical, optional, intent(out) :: error
		real(8) :: res
		
		integer :: ib,in,istat
		logical :: bFlag  ! .T. at begin of number in str
		logical :: inMan  ! .T. in mantissa of number
		logical :: pFlag  ! .T. after 1st '.' encountered
		logical :: eFlag  ! .T. at exponent identifier 'eEdD'
		logical :: inExp  ! .T. in exponent of number
		logical :: dInMan ! .T. if at least 1 digit in mant.
		logical :: dInExp ! .T. if at least 1 digit in exp.
		logical :: err    ! Local error flag
		
		bFlag=.true.
		inMan=.false.
		pFlag=.false.
		eFlag=.false.
		inExp=.false.
		dInMan=.false.
		dInExp=.false.
		
		ib = 1
		in = 1
		
		do while( in <= len_trim(str) )
			select case( str(in:in) )
				case( ' ' ) ! Only leading blanks permitted
					ib = ib+1
					
					if( inman .or. eflag .or. inexp ) then
						exit
					end if
				CASE( '+', '-' ) ! Permitted only
					if( bFlag ) then
						inMan=.true.; bFlag=.false. ! at beginning of mantissa
					else if( eFlag ) then
						inExp=.true.; eFlag=.false. ! at beginning of exponent
					else
						exit ! - otherwise STOP
					endif
				case( '0' : '9' ) ! Mark
					if( bFlag ) then
						inMan=.true.; bFlag=.false. ! beginning of mantissa
					else if( eFlag ) then
						inExp=.true.; eFlag=.false. ! beginning of exponent
					endif
					
					if( inMan ) dInMan=.true. ! Mantissa contains digit
					if( inExp ) dInExp=.true. ! Exponent contains digit
				case( '.' )
					if( bFlag ) then
						pFlag=.true. ! mark 1st appearance of '.'
						inMan=.true.; bFlag=.false. ! mark beginning of mantissa
					else if( inMan .and. .not. pFlag ) then
						pFlag=.true. ! mark 1st appearance of '.'
					else
						exit ! otherwise STOP
					end if
				CASE( 'e', 'E', 'd', 'D' ) ! Permitted only
					if( inMan ) then
						eFlag=.true.; inMan=.false. ! following mantissa
					else
						exit ! otherwise STOP
					endif
				case default
					exit ! STOP at all other characters
			end select
			in = in+1
		end do
		
		err = ( ib > in-1 ) .or. ( .not. dInMan ) .or. ( (eFlag .or. inExp) .and. .not. dInExp )
		
		if( err ) then
			res = 0.0_8
		else
			read( str(ib:in-1), *, iostat=istat ) res
			err = istat /= 0
		end if
		
		if( present(ibegin) ) ibegin = ib
		if( present(inext) )  inext = in
		if( present(error) )  error = err
	end function strToReal
	
	!>
	!! @brief Transform upper case letters in str into lower case letters
	!!
	subroutine toLowercase( str, lcStr )
		character(*), intent(in) :: str
		character(*), intent(out) :: lcStr
		
		character(*), parameter :: lc = 'abcdefghijklmnopqrstuvwxyz'
		character(*), parameter :: uc = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
		
		integer :: j,k
		
		lcStr = str
		do j=1,len_trim(str)
			k = index( uc, str(j:j) )
			if (k > 0) lcStr(j:j) = lc(k:k)
		end do
	end subroutine toLowercase
	
	!>
	!! @brief Test method
	!!
	subroutine MathParser_test()
		type(MathParser) :: parser
		character(255) :: func
		character(6) :: var(5)
		real(8) :: val(5)
		
		character(10), allocatable :: cVar(:)
		real(8), allocatable :: cVal(:)
		
		allocate( cVar(2) )
		cVar = [ 'r', 's ' ]
		
		allocate( cVal(2) )
		cVal = [ 2.0_8, 3.0_8 ]
		
		call parser.init()
		
		func = "-0.5*12*cos(0.5)*(a**2+2)*sin(b*0.5+0.6)*1e-2+0.5*x+r+s"
		var  = [ 'x', 'a', 'b', cVar ]
		val  = [ 2.0_8, 3.0_8, 4.0_8, cVal ]
		
		call parser.parseFunction( func, var )
		write(*,"(A60,A6,F10.6)") trim(func), "  ==> ", parser.evaluateFunction( val )
		
		call parser.parseFunction( "0.5*10.8", ["x"] )
		write(*,"(A60,A6,F10.6)") "0.5*10.8", "  ==> ", parser.evaluateFunction( [0.0_8] )
		
		func = "sgn(-1.456)"
		call parser.parseFunction( func, var )
		write(*,"(A60,A6,F10.6)") trim(func), "  ==> ", parser.evaluateFunction( [0.0_8] )
		
		func = "ustep(1.456)"
		call parser.parseFunction( func, var )
		write(*,"(A60,A6,F10.6)") trim(func), "  ==> ", parser.evaluateFunction( [0.0_8] )
		
		func = "ustep(-1.456)"
		call parser.parseFunction( func, var )
		write(*,"(A60,A6,F10.6)") trim(func), "  ==> ", parser.evaluateFunction( [0.0_8] )
		
		func = "ustep(0.0)"
		call parser.parseFunction( func, var )
		write(*,"(A60,A6,F10.6)") trim(func), "  ==> ", parser.evaluateFunction( [0.0_8] )
		
		func = "ubox(-0.6)"
		call parser.parseFunction( func, var )
		write(*,"(A60,A6,F10.6)") trim(func), "  ==> ", parser.evaluateFunction( [0.0_8] )
		
		func = "ubox(-0.5)"
		call parser.parseFunction( func, var )
		write(*,"(A60,A6,F10.6)") trim(func), "  ==> ", parser.evaluateFunction( [0.0_8] )
		
		func = "ubox(0.0)"
		call parser.parseFunction( func, var )
		write(*,"(A60,A6,F10.6)") trim(func), "  ==> ", parser.evaluateFunction( [0.0_8] )
		
		func = "ubox(0.3)"
		call parser.parseFunction( func, var )
		write(*,"(A60,A6,F10.6)") trim(func), "  ==> ", parser.evaluateFunction( [0.0_8] )
		
		func = "ubox(0.5)"
		call parser.parseFunction( func, var )
		write(*,"(A60,A6,F10.6)") trim(func), "  ==> ", parser.evaluateFunction( [0.0_8] )
		
		func = "ubox(0.6)"
		call parser.parseFunction( func, var )
		write(*,"(A60,A6,F10.6)") trim(func), "  ==> ", parser.evaluateFunction( [0.0_8] )
		
	end subroutine MathParser_test
	
end module MathParser_

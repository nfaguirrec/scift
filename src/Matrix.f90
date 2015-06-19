!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!  This file is part of scift (Scientific Fortran Tools).
!!  Copyright (C) by authors (2010-2013)
!!  
!!  Authors (alphabetic order):
!!    * Aguirre N.F. (nfaguirrec@gmail.com)  (2010-2013)
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

!>
!! @brief
!!
module Matrix_
	use ieee_arithmetic   ! http://fortranwiki.org/fortran/show/ieee_arithmetic
	
	use Math_
	use RandomUtils_
	implicit none
	private
	
	enum, BIND(c)
		enumerator :: UNKNOWN_MATRIX
		enumerator :: SYMMETRIC_MATRIX
		enumerator :: SQUARE_MATRIX
		enumerator :: COLUMN_MATRIX
		enumerator :: ROW_MATRIX
	end enum
	
	public :: &
! 		Matrix_lMultiplicationByReal, & ! NO FUNCIONA LA MULTIPLICACION POR IZQUIERDA
		Matrix_get, &
		Matrix_trace, &
		Matrix_norm2, &
		Matrix_columnVector, &
		Matrix_test
	
	type, public :: Matrix
		integer :: nRows
		integer :: nCols
		real(8), allocatable :: data(:,:)
		integer :: type = UNKNOWN_MATRIX
		
		contains
			generic :: init => initDefaultMatrix, fromArray
			generic :: assignment(=) => copyMatrix
			
			procedure :: initDefaultMatrix
			procedure :: fromArray
			procedure :: fromArrayDiag
			procedure :: identity
			procedure :: random
			procedure :: columnVector
			procedure :: rowVector
			
			procedure :: copyMatrix
			final :: destroyMatrix
			procedure :: str
			procedure :: show
			procedure :: set
			procedure :: get
			procedure :: column
			procedure :: row
			
			procedure :: isZero
			procedure :: isDiagonal
			
			procedure :: rAddition
			procedure :: rSubtraction
			procedure :: rMultiplication
			procedure :: rDivision
			procedure :: rExponentiation
			
			procedure :: rAdditionByReal
			procedure :: rSubtractionByReal
			procedure :: rMultiplicationByReal
			procedure :: rDivisionByReal
			procedure :: rExponentiationByReal
			
			generic :: operator(+) => rAddition, rAdditionByReal
			generic :: operator(-) => rSubtraction, rSubtractionByReal
			generic :: operator(*) => rMultiplication, rMultiplicationByReal
			generic :: operator(/) => rDivision, rDivisionByReal
			generic :: operator(**) => rExponentiation, rExponentiationByReal
			
			procedure :: eigen
			procedure :: inverse
			procedure :: determinant
			procedure, private :: inverse2x2
			procedure, private :: inverse3x3
			procedure, private :: determinant2x2			
			procedure, private :: determinant3x3
			procedure :: trace
			procedure :: norm2
			procedure :: diagonal
			procedure :: transpose => trans
			procedure, private :: trans
			procedure :: linearize
			procedure :: projectionOntoNewAxes
	end type Matrix
	
!     interface assignment(=)
!        module procedure c_to_s_assign, s_to_c_assign
!     end interface
    
!     interface operator(*)
! ! 		module procedure Matrix_lMultiplicationByReal
! 		function Matrix_lMultiplicationByReal( constant, M ) result( output )
! 			import Matrix
! 			real(8), intent(in) :: constant
! 			type(Matrix), intent(in) :: M
! 			type(Matrix) :: output
! 		end function Matrix_lMultiplicationByReal
!     end interface operator(*)
	
	contains
	
	!>
	!! @brief Constructor
	!!
	subroutine initDefaultMatrix( this, nRows, nCols, val )
		class(Matrix) :: this
		integer, intent(in) :: nRows
		integer, intent(in) :: nCols
		real(8), optional, intent(in) :: val
		
		this.nRows = nRows
		this.nCols = nCols
		
		if( this.nRows == this.nCols ) then
			this.type = SQUARE_MATRIX
		else if( this.nCols == 1 ) then
			this.type = COLUMN_MATRIX
		else if( this.nRows == 1 ) then
			this.type = ROW_MATRIX
		else
			this.type = UNKNOWN_MATRIX
		end if
		
		if( allocated(this.data) ) deallocate(this.data)
		allocate( this.data(nRows,nCols) )
		
		this.data = 0.0_8
		if( present(val) ) this.data = val
	end subroutine initDefaultMatrix

	!>
	!! @brief Constructor
	!!
	subroutine fromArray( this, arr )
		class(Matrix) :: this
		real(8), intent(in) :: arr(:,:)
		
		call this.initDefaultMatrix( size(arr,dim=1), size(arr,dim=2) )
		this.data = arr
	end subroutine fromArray
	
	!>
	!! @brief Constructor
	!!
	subroutine fromArrayDiag( this, arr )
		class(Matrix) :: this
		real(8), intent(in) :: arr(:)
		
		integer :: i
		
		call this.initDefaultMatrix( size(arr), size(arr) )
		
		do i=1,size(arr)
			this.data(i,i) = arr(i)
		end do
	end subroutine fromArrayDiag
	
	!>
	!! @brief Constructor
	!!
	subroutine identity( this, nRows, nCols )
		class(Matrix) :: this
		integer, intent(in) :: nRows
		integer, intent(in) :: nCols
		
		integer i, j
		
		call this.initDefaultMatrix( nRows, nCols )
		
		do i=1,this.nRows
			this.data(i,i) = 1.0_8
		end do
	end subroutine identity
	
	!>
	!! @brief Constructor
	!!
	subroutine random( this, nRows, nCols, symmetric )
		class(Matrix) :: this
		integer, intent(in) :: nRows
		integer, intent(in) :: nCols
		logical, optional, intent(in) :: symmetric
		
		integer :: i, j, n, clock
		integer, dimension(:), allocatable :: seed
		logical :: effSymmetric
		
		effSymmetric = .false.
		if( present(symmetric) ) effSymmetric = symmetric
		
		call this.initDefaultMatrix( nRows, nCols )
		
		call RandomUtils_init()
		
		if( effSymmetric ) then
			do i=1,nRows
				do j=i,nCols
					call random_number( this.data(i,j) )
					this.data(j,i) = this.data(i,j)
				end do
			end do
			! @todo Hay que implementar el type como bits para que la matriz pueda ser simétrica y cuadrada por ejemplo
		else
			do i=1,nRows
				do j=1,nCols
					call random_number( this.data(i,j) )
				end do
			end do
		end if
	end subroutine random
	
	!>
	!! @brief Constructor
	!!
	subroutine columnVector( this, nElems, val, values )
		class(Matrix) :: this
		integer, intent(in) :: nElems
		real(8), optional, intent(in) :: val
		real(8), optional, intent(in) :: values(:)
		
		if( present(val) ) then
			call this.initDefaultMatrix( nElems, 1, val )
		else if( present(values) ) then
			call this.initDefaultMatrix( nElems, 1 )
			this.data(:,1) = values(1:nElems)
		else
			call this.initDefaultMatrix( nElems, 1 )
		end if
	end subroutine columnVector
	
	!>
	!! @brief Constructor
	!!
	subroutine rowVector( this, nElems, val, values )
		class(Matrix) :: this
		integer, intent(in) :: nElems
		real(8), optional, intent(in) :: val
		real(8), optional, intent(in) :: values(:)
		
		if( present(val) ) then
			call this.initDefaultMatrix( 1, nElems, val )
		else if( present(values) ) then
			call this.initDefaultMatrix( 1, nElems )
			this.data(1,:) = values(1:nElems)
		else
			call this.initDefaultMatrix( 1, nElems )
		end if
	end subroutine rowVector
	
	!>
	!! @brief Copy constructor
	!!
	subroutine copyMatrix( this, other )
		class(Matrix), intent(inout) :: this
		class(Matrix), intent(in) :: other
		
		this.nRows = other.nRows
		this.nCols = other.nCols
		
		if( allocated(this.data) ) deallocate(this.data)
		allocate( this.data(other.nRows,other.nCols) )
		
		this.data = other.data
		this.type = other.type
	end subroutine copyMatrix
	
	!>
	!! @brief Destructor
	!!
	subroutine destroyMatrix( this )
		type(Matrix) :: this
		
		this.nRows = 0
		this.nCols = 0
		if( allocated(this.data) ) deallocate(this.data)
	end subroutine destroyMatrix
	
	!>
	!! @brief
	!! @todo Este metodo solo funciona para matrices reales simetricas, así que hay que evaluar esto antes de hacerlo
	!!
	subroutine eigen( this, eValues, eVals, eVecs )
		class(Matrix), intent(in) :: this
		real(8), allocatable, optional, intent(inout) :: eValues(:)
		type(Matrix), optional, intent(inout) :: eVals
		type(Matrix), optional, intent(inout) :: eVecs
		
		type(Matrix) :: eVecsBuffer
		real(8), allocatable :: eValuesBuffer(:)
		real(8), allocatable :: workSpace(:)
		integer :: i, ssize, info
		
		if( this.type /= SQUARE_MATRIX ) then
			write(*,*) "### ERROR ### Matrix.eigen: matrix not square"
			stop
		end if
		
		ssize = this.nRows
		eVecsBuffer = this
		
		allocate( eValuesBuffer(ssize) )
		allocate( workSpace( 3*ssize-1 ) )
		
		! Compute the eigen values and eigen vectors using the upper elements of the symmetric matrix
		call dsyev( 'V', 'L', ssize, eVecsBuffer.data, ssize, eValuesBuffer, workSpace, 3*ssize-1, info )
		
		! Utilizando el método dsyev con ( 'V', 'U', ... ) me ha dado problemas para diagonalizar esta matrix
		! 
		!     48749.900676       0.000000  -48749.900676
		!         0.000000   97499.801352       0.000000
		!    -48749.900676       0.000000   48749.900676
		!
		! dando valores propios correctos, pero vectores propios totalmente errados que no son ni ortogonales
		!
		! 	     -0.707107      -0.408248       0.577350
		!         0.000000      -0.816497      -0.577350
		!        -0.707107       0.408248      -0.577350
		!
		! sin embargo utilizando el método dsyev con ( 'V', 'L', ... ) esto se corrige, pero no se el porqué,
		! dando su valor correcto
		!
		!         0.707107       0.000000       0.707107
		!        -0.000000       1.000000       0.000000
		!         0.707107       0.000000      -0.707107
		
		if ( info /= 0 ) then
			write(*,*) "### ERROR ### Matrix.eigen: values matrix failed"
			stop
		end if
		
		if( present(eValues) ) then
			if( allocated(eValues) ) deallocate(eValues)
			allocate(eValues(ssize))
			eValues = eValuesBuffer
		end if
		
		if( present(eVals) ) then
			call eVals.init( ssize, ssize, 0.0_8 )
			do i=1,ssize
				call eVals.set( i, i, eValuesBuffer(i) )
			end do
		end if
		
		if( present(eVecs) ) then
			eVecs = eVecsBuffer
		end if
		
		deallocate( workSpace )
		deallocate( eValuesBuffer )
	end subroutine eigen
! 	subroutine eigen( this, eVecs, eVals )
! 		class(Matrix), intent(in) :: this
! 		type(Matrix), intent(inout) :: eVecs
! 		real(8), allocatable, intent(inout) :: eVals(:)
! 		
! 		real(8), allocatable :: workSpace(:)
! 		integer :: ssize, info
! 		
! 		if( this.type /= SQUARE_MATRIX ) then
! 			write(*,*) "### ERROR ### Matrix.eigen: matrix not square"
! 			stop
! 		end if
! 		
! 		ssize = this.nRows
! 		eVecs = this
! 		
! 		if( allocated(eVals) ) deallocate(eVals)
! 		allocate(eVals(ssize))
! 		
! 		allocate( workSpace( 3*ssize-1 ) )
! 		
! 		! Compute the eigen values and eigen vectors using the upper elements of the symmetric matrix
! 		call dsyev( 'V', 'U', ssize, eVecs.data, ssize, eVals, workSpace, 3*ssize-1, info )
! 		
! 		if ( info /= 0 ) then
! 			write(*,*) "### ERROR ### Matrix.eigen: values matrix failed"
! 			stop
! 		end if
! 		
! 		deallocate( workSpace )
! 	end subroutine eigen
	
	!>
	!! @brief
	!!
	function inverse( this ) result( inv )
		class(Matrix), intent(in) :: this
		type(Matrix) :: inv
		
		integer, allocatable :: pivotInd(:)
		real(8), allocatable :: workSpace(:)
! 		real(8), allocatable :: test(:,:)
		integer :: i, ssize, info
		real(8) :: trace
		
		if( this.type /= SQUARE_MATRIX ) then
			write(*,*) "### ERROR ### Matrix.inverse: This operation only is defined for square matrices"
			stop
		end if
		
		ssize = this.nRows
		
		if( ssize == 2 ) then
			inv = this.inverse2x2()
			return
		else if( ssize == 3 ) then
			inv = this.inverse3x3()
			return
		end if
		
		inv = this
		
		allocate( pivotInd( ssize ) )
		
		!! Factorizacion LU
		call dgetrf( ssize, ssize, inv.data, ssize, pivotInd, info )
		if ( info /= 0 ) then
			write(*,*) "### ERROR ### Matrix.inverse: Get Matrix LU factorization failed"
			write(*,*) "A = "
			call this.show( formatted=.true. )
			stop
		end if
		
		allocate( workSpace( ssize ) )
		
		!! Invierte la matriz
		call dgetri( ssize, inv.data, ssize, pivotInd, workSpace, ssize, info )
		if ( info /= 0 ) then
			write(*,*) "### ERROR ### Matrix.inverse: Get Inverse Matrix failed"
			write(*,*) "A = "
			call this.show( formatted=.true. )
			stop
		end if
		
! 		allocate( test( ssize, ssize ) )
! 		test = matmul(matrix,inv)
! 		
! 		trace = 0.0_8
! 		do i=1,ssize
! 			trace = trace + test(i,i)
! 		end do
! 		
! 		if( abs( sum(test)-trace ) > 1e-10 ) stop "Get Inverse Matrix failed 2 A*A^-1"
		
		!! libera memoria 
		deallocate(workSpace)
		deallocate(pivotInd)
! 		deallocate(test)
	end function inverse
	
	!>
	!! @brief Returns the inverse for a 3x3 matrix
	!! Taken from http://www.cg.info.hiroshima-cu.ac.jp/~miyazaki/knowledge/teche23.html
	!!
	function inverse2x2( this ) result( inv )
		class(Matrix) :: this
		type(Matrix) :: inv
		
		type(Matrix) :: I
		real(8) :: detA
		
		if( this.isDiagonal() ) then
			call inv.init( 2, 2, val=0.0_8 )
			
			inv.data(1,1) = 1.0_8/this.data(1,1)
			inv.data(2,2) = 1.0_8/this.data(2,2)
			
			return
		end if
		
		detA = this.determinant2x2()
		
		if( abs(detA) < 1d-16 ) then
			write(*,*) "### WARNING ### Matrix.inverse2x2: Singular matrix detected ( detA < 1e-16 )"
			write(*,*) "                It will try to fix by Tikhonov regularization ( lambda = 1d-3 )"
			call I.identity( 2, 2 )
			this = this + I*1d-3  ! Tikhonov regularization
		end if
		
		if( abs(detA) < 1d-16 ) then
			write(*,*) "### ERROR ### Matrix.inverse2x2: Singular matrix detected ( detA < 1e-16 )"
			write(*,*) "A = "
			call this.show( formatted=.true. )
			write(*,*) ""
			write(*,*) "det(A) = ", detA
			stop
		end if
		
		call inv.init( 2, 2 )
		
#define a(i,j) this.data(i,j)
#define b(i,j) inv.data(i,j)
		b(1,1) =  a(2,2)
		b(1,2) = -a(1,2)
		b(2,1) = -a(2,1)
		b(2,2) =  a(1,1)
#undef a
#undef b
		
		inv = inv/detA
	end function inverse2x2
	
	!>
	!! @brief Returns the inverse for a 3x3 matrix
	!! Taken from http://www.cg.info.hiroshima-cu.ac.jp/~miyazaki/knowledge/teche23.html
	!!
	function inverse3x3( this ) result( inv )
		class(Matrix) :: this
		type(Matrix) :: inv
		
		type(Matrix) :: I
		real(8) :: detA
		
		if( this.isDiagonal() ) then
			call inv.init( 3, 3, val=0.0_8 )
			
			inv.data(1,1) = 1.0_8/this.data(1,1)
			inv.data(2,2) = 1.0_8/this.data(2,2)
			inv.data(3,3) = 1.0_8/this.data(3,3)
			
			return
		end if
		
		detA = this.determinant3x3()
		
		if( abs(detA) < 1d-16 ) then
			write(*,*) "### WARNING ### Matrix.inverse3x3: Singular matrix detected ( detA < 1e-16 )"
			write(*,*) "                It will try to fix by Tikhonov regularization ( lambda = 1d-3 )"
			call I.identity( 3, 3 )
			this = this + I*1d-3  ! Tikhonov regularization
		end if
		
		if( abs(detA) < 1d-16 ) then
			write(*,*) "### ERROR ### Matrix.inverse3x3: Singular matrix detected ( detA < 1e-16 )"
			write(*,*) "A = "
			call this.show( formatted=.true. )
			write(*,*) ""
			write(*,*) "det(A) = ", detA
			stop
		end if
		
		call inv.init( 3, 3 )
		
#define a(i,j) this.data(i,j)
#define b(i,j) inv.data(i,j)
		b(1,1) = a(2,2)*a(3,3) - a(2,3)*a(3,2)
		b(1,2) = a(1,3)*a(3,2) - a(1,2)*a(3,3)
		b(1,3) = a(1,2)*a(2,3) - a(1,3)*a(2,2)
		b(2,1) = a(2,3)*a(3,1) - a(2,1)*a(3,3)
		b(2,2) = a(1,1)*a(3,3) - a(1,3)*a(3,1)
		b(2,3) = a(1,3)*a(2,1) - a(1,1)*a(2,3)
		b(3,1) = a(2,1)*a(3,2) - a(2,2)*a(3,1)
		b(3,2) = a(1,2)*a(3,1) - a(1,1)*a(3,2)
		b(3,3) = a(1,1)*a(2,2) - a(1,2)*a(2,1)
#undef a
#undef b
		
		inv = inv/detA
	end function inverse3x3
	
	!>
	!! @brief
	!!
	function determinant( this ) result( det )
		class(Matrix), intent(in) :: this
		real(8) :: det
		
		integer :: ssize
		
		ssize = this.nRows
		
		if( ssize == 2 ) then
			det= this.determinant2x2()
		else if( ssize == 3 ) then
			det = this.determinant3x3()
		else
			write(*,*) "### ERROR ### Matrix.determinant() size>3 is not implemented yet"
			stop
		end if
	end function determinant
	
	!>
	!! @brief Returns the determinant for a 2x2 matrix
	!! Taken from http://www.cg.info.hiroshima-cu.ac.jp/~miyazaki/knowledge/teche23.html
	!!
	pure function determinant2x2( this ) result( output )
		class(Matrix), intent(in) :: this
		real(8) :: output
		
#define a(i,j) this.data(i,j)
		output = a(1,1)*a(2,2)-a(1,2)*a(2,1)
#undef a
	end function determinant2x2
	
	!>
	!! @brief Returns the determinant for a 2x2 matrix
	!! Taken from http://www.cg.info.hiroshima-cu.ac.jp/~miyazaki/knowledge/teche23.html
	!!
	pure function determinant3x3( this ) result( output )
		class(Matrix), intent(in) :: this
		real(8) :: output
		
#define a(i,j) this.data(i,j)
		output=a(1,1)*a(2,2)*a(3,3) &
			  +a(2,1)*a(3,2)*a(1,3) &
			  +a(3,1)*a(1,2)*a(2,3) &
			  -a(1,1)*a(3,2)*a(2,3) &
			  -a(3,1)*a(2,2)*a(1,3) &
			  -a(2,1)*a(1,2)*a(3,3)
#undef a
	end function determinant3x3
	
	!>
	!! @brief Factoriza la matriz
	!!
	!! @todo Hay que tener cuidado con el valor de numberOfRows y numberOfColumns
	!!       ya que en el caso de matrices cuadradas no hay problema ( DIM=1 o DIM=2 ?)
	
! Loadable Function: [l, u, p] = lu (a)
! Compute the LU decomposition of a, using subroutines from LAPACK. The result is returned in a permuted form, according to the optional return value p. For example, given the matrix a = [1, 2; 3, 4],
!  	
! [l, u, p] = lu (a)

	!<
! 	function Matrix_factorizeLU( this, L, U, pivotIndices, printFormatFlags ) result ( output )
! 		implicit none
! 		type(Matrix), intent(inout) :: this
! 		type(Matrix) :: output
! 		type(Matrix), intent(inout), optional :: L
! 		type(Matrix), intent(inout), optional :: U
! 		integer, allocatable, intent(inout), optional :: pivotIndices(:)
! 		integer,intent(in),optional :: printFormatFlags
! 		
! 		integer :: numberOfRows
! 		integer :: numberOfColumns
! 		integer :: infoProcess
! 		integer, allocatable :: pivotIndicesTmp(:)
! 		integer :: methodTmp
! 		integer :: i
! 		integer :: j
! 		
! 		!! Determina variables y parametros requeridos para el calculo
! 		numberOfRows = size( this.values, DIM=1 )
! 		numberOfColumns = size( this.values, DIM=1 )
! 		
! 		call Matrix_copyConstructor( output, this )
! 		
! 		if( .not. present( pivotIndices ) ) then
! 			allocate( pivotIndicesTmp( min( numberOfRows, numberOfColumns )  )  )
! 			
! 			call dgetrf( &
! 				numberOfRows, &
! 				numberOfColumns, &
! 				output.values, &
! 				numberOfRows, &
! 				pivotIndicesTmp, &
! 				infoProcess )
! 				
! 			deallocate( pivotIndicesTmp )
! 		else
! 			call dgetrf( &
! 				numberOfRows, &
! 				numberOfColumns, &
! 				output.values, &
! 				numberOfRows, &
! 				pivotIndices, &
! 				infoProcess )
! 		end if
! 		
! 		if( present(L) ) then
! 			call Matrix_setNull( L )
! 			
! 			do i=1, numberOfRows
! 				do j=1, i-1
! 					L.values(i, j) = output.values(i, j)
! 				end do
! 				
! 				L.values(i, i) = 1.0_8
! 			end do
! 		end if
! 		
! 		if( present(U) ) then
! 			call Matrix_setNull( U )
! 			
! 			do i=1, numberOfRows
! 				do j=i, numberOfColumns
! 					U.values(i, j) = output.values(i, j)
! 				end do
! 			end do
! 		end if
! 			
! 		!! Determina la ocurrencia de errores
! 		if(.not.present(printFormatFlags) .or. (present(printFormatFlags) .and. printFormatFlags/=WITHOUT_MESSAGES) ) then
! 
! 			if ( infoProcess /= 0 )  then
! 			
! 				call Matrix_exception(WARNING, "Factorization failed", "Class object Matrix in the factorizeLU() function" )
! 
! 			end if
! 
! 		end if
! 		
! 	end function Matrix_factorizeLU
	
	!>
	!! @brief  Retorna la traza de la matriz
	!!
	function trace( this ) result ( output )
		class(Matrix) , intent(in) :: this
		real(8) :: output
		
		integer :: i
		
		output = 0.0_8
		do i=1,this.nCols
			output = output + this.data( i, i )
		end do
	end function trace
	
	!>
	!! @brief  Returns the norm2
	!!
	real(8) function norm2( this )
		class(Matrix) , intent(in) :: this
		
		type(Matrix) :: U
		
		U = this.transpose()*this
		norm2 = sqrt( U.trace() )
	end function norm2
	
	!>
	!! @brief Returns the diagonal representation of a Matrix
	!!
	function diagonal( this ) result( diag )
		class(Matrix), intent(in) :: this
		type(Matrix) :: diag
		
		integer :: i
		
		select case( this.type )
			case( COLUMN_MATRIX )
				call diag.init( this.nRows, this.nRows )
				
				do i=1,this.nRows
					call diag.set( i, i, this.get(i,1) )
				end do
				
			case( ROW_MATRIX )
				call diag.init( this.nCols, this.nCols )
				
				do i=1,this.nCols
					call diag.set( i, i, this.get(1,i) )
				end do
				
			case( SQUARE_MATRIX )
				call diag.init( this.nCols, this.nCols )
				
				do i=1,this.nCols
					call diag.set( i, i, this.get(i,i) )
				end do
				
			case default
				write(*,*) "### ERROR ### Matrix.diagonal: The procedure is not defined for this matrix type"
		end select
	end function diagonal
	
	!>
	!! @brief Returns the transpose matrix
	!!
	function trans( this ) result( output )
		class(Matrix), intent(in) :: this
		type(Matrix) :: output
		
		call output.init( this.nCols, this.nRows )
		output.data = transpose( this.data )
	end function trans

	!>
	!! @brief Convert to string
	!!
	function str( this, formatted, prefix, precision ) result( output )
		class(Matrix) :: this 
		character(:), allocatable :: output
		logical, optional :: formatted
		character(*), optional :: prefix
		integer, optional :: precision
		
		integer :: ncolEff = 10
		
		logical :: effFormatted
		character(:), allocatable :: effPrefix
		integer :: effPrecision
		
		integer :: fmt
		character(20000) :: fstr
		integer :: i, j, k, upper
		
		integer :: auxColNum
		integer :: lowerLimit
		integer :: upperLimit
		
		integer :: maxIPart
		
		effFormatted = .false.
		if( present(formatted) ) effFormatted = formatted
		
		effPrefix = ""
		if( present(prefix) ) effPrefix = prefix
		
		effPrecision = 6
		if( present(precision) ) effPrecision = precision
		
		output = ""
		
		if( .not. effFormatted ) then
#define RFMT(v) int(log10(max(abs(v),1.0)))+merge(1,2,v>=0)

#define ITEMS(l,v) output = trim(output)//effPrefix//trim(l)//trim(adjustl(v))
#define ITEMI(l,v) output = trim(output)//l; fmt = RFMT(v); write(fstr, "(i<fmt>)") v; output = trim(output)//trim(fstr)
#define ITEMR(l,v) output = trim(output)//l; fmt = RFMT(v); write(fstr, "(f<fmt+7>.6)") v; output = trim(output)//trim(fstr)
		
			output = trim(output)//"<Matrix:"
! 			ITEMI( "min=", this.min )
! 			ITEMR( ",size=", this.size )
#undef ITEMS
#undef ITEMI
#undef ITEMR
			output = trim(output)//">"
		else
#define LINE(l) output = trim(output)//effPrefix//l//new_line('')
#define ITEMS(l,v) output = trim(output)//effPrefix//l; write(fstr, "(x,a)") trim(v); output = trim(output)//trim(fstr)//new_line('')
#define ITEMI(l,v) output = trim(output)//effPrefix//l; write(fstr, "(i10)") v; output = trim(output)//trim(fstr)//new_line('')
#define ITEMR(l,v) output = trim(output)//effPrefix//l; write(fstr, "(f10.5)") v; output = trim(output)//trim(fstr)//new_line('')

! 			LINE("Matrix")
! 			LINE("---------")
! 			LINE("")
! 			maxIPart = RFMT( maxval( this.data, mask=( this.data .lt. Math_Inf .and. .not. IEEE_IS_NAN(this.data) ) ) )
			maxIPart = min( RFMT( maxval( this.data, mask=( IEEE_IS_FINITE(this.data) ) ) ), 15 )
			
			do k=1, ceiling( (this.nCols*1.0)/(ncolEff*1.0) )
			
				lowerLimit = ncolEff*(k-1)+1
				upperLimit = ncolEff*k
				auxColNum = ncolEff
				
				if ( upperLimit > this.nCols ) then
					auxColNum =  ncolEff-upperLimit+this.nCols
					upperLimit = this.nCols
				end if
				
				if( k /= 1 ) then
					LINE("")
				end if
				
! 				if( present( columnKeys ) ) then
! 					if( tmpFlags == WITH_COLUMN_KEYS .or. tmpFlags == WITH_BOTH_KEYS ) then
! 						write (6,"(21X,<auxColNum>A15)") ( columnKeys(i), i = lowerLimit, upperLimit )
! 					end if
! 				else
! 					if( tmpFlags /= WITHOUT_KEYS ) then
! 						if( tmpFlags == WITH_COLUMN_KEYS .or. tmpFlags == WITH_BOTH_KEYS ) then
! 							write(fstr,"(I5,<upper>F10.4)") i, ( this.data(i,k), k=ncolEff*(j-1)+1,upper )
							write (fstr,"(5X,<auxColNum>I<maxIPart+5+effPrecision>)") ( i,i=lowerLimit,upperLimit )
							output = trim(output)//trim(fstr)//new_line('')
! 						end if
! 					end if
! 				end if
					
! 				LINE("")
				
! 				if( present( rowKeys ) ) then
! 				
! 					if( tmpFlags == WITH_ROW_KEYS .or. tmpFlags == WITH_BOTH_KEYS ) then
! 						write (6,"(A18,<auxColNum>F15.6)") ( rowKeys(i), ( this.values(i,j), j=lowerLimit,upperLimit ), i = 1, this.nRows )
! 					else
! 						write (6,"(5X,<auxColNum>F15.6)") ( ( this.values(i,j), j=lowerLimit,upperLimit ), i = 1, this.nRows )
! 					end if
! 					
! 				else
! 					if( tmpFlags /= WITHOUT_KEYS ) then
					
! 						if( ( tmpFlags == WITH_ROW_KEYS .or. tmpFlags == WITH_BOTH_KEYS ) .and. tmpFlags /= WITHOUT_KEYS ) then
					do i=1,this.nRows
						write (fstr,"(I5,<auxColNum>F<maxIPart+5+effPrecision>.<effPrecision>)") i, ( this.data(i,j), j=lowerLimit,upperLimit )
						
						if( i /= this.nRows ) then
							output = trim(output)//trim(fstr)//new_line('')
						else
							output = trim(output)//trim(fstr)
						end if
					end do
! 							write (fstr,"(I5,<auxColNum>F15.6)") ( i, ( this.data(i,j), j=lowerLimit,upperLimit ), i=1,this.nRows )
! 							output = trim(output)//trim(fstr)//new_line('')
! 						else
! 							write (6,"(5X,<auxColNum>F15.6)") ( ( this.values(i,j), j=lowerLimit,upperLimit ), i = 1, this.nRows )
! 						end if
						
! 					else
! 					
! 						write (fstr,"(5X,<auxColNum>F15.6)") ( ( this.data(i,j), j=lowerLimit,upperLimit ), i=1,this.nRows )
! 						output = trim(output)//trim(fstr)//new_line('')
! 
! 					end if
! 				end if
				
				if( k /= ceiling( (this.nCols*1.0)/(ncolEff*1.0) ) ) then
					LINE("")
				end if
				
			end do
#undef RFMT

#undef LINE
#undef ITEMS
#undef ITEMI
#undef ITEMR
		end if
	end function str
	
	!>
	!! @brief Show 
	!!
	subroutine show( this, unit, formatted, precision )
		class(Matrix) :: this
		integer, optional, intent(in) :: unit
		logical, optional :: formatted
		integer, optional :: precision
		
		integer :: effunit
		logical :: effFormatted
		
		effFormatted = .false.
		if( present(formatted) ) effFormatted = formatted
		
		effunit = 6
		if( present(unit) ) effunit = unit
		
		write(effunit,"(a)") trim(str(this,effFormatted,precision=precision))
	end subroutine show
	
	!>
	!! @brief Show 
	!!
	subroutine set( this, i, j, value )
		class(Matrix) :: this
		integer, intent(in) :: i, j
		real(8), intent(in) :: value
		
		this.data(i,j) = value
	end subroutine set
	
	!>
	!! @brief 
	!!
	function get( this, i, j ) result( output )
		class(Matrix), intent(in) :: this
		integer, intent(in) :: i, j
		real(8) :: output
		
		output = this.data(i,j)
	end function get
	
	!>
	!! @brief 
	!!
	function column( this, i ) result( output )
		class(Matrix), intent(in) :: this
		integer, intent(in) :: i
		type(Matrix) :: output
		
		call output.init( this.nRows, 1 )
		output.data(:,1) = this.data(:,i)
	end function column
	
	!>
	!! @brief 
	!!
	function row( this, i ) result( output )
		class(Matrix), intent(in) :: this
		integer, intent(in) :: i
		type(Matrix) :: output
		
		call output.init( 1, this.nCols )
		output.data(1,:) = this.data(i,:)
	end function row
	
	!>
	!! @brief
	!!
	function isZero( this, tol ) result( output )
		class(Matrix), intent(in) :: this
		real(8), optional, intent(in) :: tol
		logical :: output
		
		real(8) :: effTol
		
		integer i, j
		
		effTol = 1d-16
		if( present(tol) ) effTol = tol
		
		if( sum( this.data )/size(this.data) > effTol ) then
			output = .false.
		else
			output = .true.
		end if
	end function isZero
	
	!>
	!! @brief
	!!
	function isDiagonal( this, tol ) result( output )
		class(Matrix), intent(in) :: this
		real(8), optional, intent(in) :: tol
		logical :: output
		
		real(8) :: effTol
		
		integer i, j
		
		effTol = 1d-16
		if( present(tol) ) effTol = tol
		
		do i=2,this.nCols
			do j=i+1,this.nRows
				if( abs(this.data(i,j)-this.data(j,i)) > effTol ) then
					output = .false.
					return
				end if
			end do
		end do
		
		output = .true.
	end function isDiagonal
	
	!>
	!! @brief
	!!
	function rAddition( this, other ) result( output )
		class(Matrix), intent(in) :: this
		class(Matrix), intent(in) :: other
		type(Matrix) :: output
		
		if( this.nCols /= other.nCols .or. this.nRows /= other.nRows ) then
			write(*,*) "## ERROR ## Matrix.rAddition: The matrices have not the same size"
			stop
		end if
		
		call output.copyMatrix( this )
		output.data = this.data + other.data
	end function rAddition
	
	!>
	!! @brief
	!!
	function rAdditionByReal( this, constant ) result( output )
		class(Matrix), intent(in) :: this
		real(8), intent(in) :: constant
		type(Matrix) :: output
		
		call output.copyMatrix( this )
		output.data = this.data + constant
	end function rAdditionByReal
	
	!>
	!! @brief
	!!
	function rSubtraction( this, other ) result( output )
		class(Matrix), intent(in) :: this
		class(Matrix), intent(in) :: other
		type(Matrix) :: output
		
		if( this.nCols /= other.nCols .or. this.nRows /= other.nRows ) then
			write(*,*) "## ERROR ## Matrix.rSubtraction: The matrices have not the same size"
			stop
		end if
		
		call output.copyMatrix( this )
		output.data = this.data - other.data
	end function rSubtraction
	
	!>
	!! @brief
	!!
	function rSubtractionByReal( this, constant ) result( output )
		class(Matrix), intent(in) :: this
		real(8), intent(in) :: constant
		type(Matrix) :: output
		
		call output.copyMatrix( this )
		output.data = this.data - constant
	end function rSubtractionByReal
	
	!>
	!! @brief
	!!
	function rMultiplication( this, other ) result( output )
		class(Matrix), intent(in) :: this
		class(Matrix), intent(in) :: other
		type(Matrix) :: output
		
		if( this.nCols /= other.nRows ) then
			write(*,*) "## ERROR ## Matrix.rMultiplication: matrices with incompatible sizes"
			stop
		end if
		
		call output.init( this.nRows, other.nCols )
		output.data = matmul( this.data, other.data )
	end function rMultiplication
	
	!>
	!! @brief
	!!
	function rMultiplicationByReal( this, constant ) result( output )
		class(Matrix), intent(in) :: this
		real(8), intent(in) :: constant
		type(Matrix) :: output
		
		call output.copyMatrix( this )
		output.data = this.data*constant
	end function rMultiplicationByReal
	
	!>
	!! @brief
	!!
	function rDivision( this, other ) result( output )
		class(Matrix), intent(in) :: this
		class(Matrix), intent(in) :: other
		type(Matrix) :: output
		
		if( this.nCols /= other.nCols .or. this.nRows /= other.nRows ) then
			write(*,*) "## ERROR ## Matrix.rDivision: The matrices have not the same size"
			stop
		end if
		
		output = other.inverse()
		output.data = matmul( this.data, output.data )
	end function rDivision
	
	!>
	!! @brief
	!!
	function rDivisionByReal( this, constant ) result( output )
		class(Matrix), intent(in) :: this
		real(8), intent(in) :: constant
		type(Matrix) :: output
		
		call output.copyMatrix( this )
		output.data = this.data/constant
	end function rDivisionByReal
	
	!>
	!! @brief
	!!
	function rExponentiation( this, other ) result( output )
		class(Matrix), intent(in) :: this
		class(Matrix), intent(in) :: other
		type(Matrix) :: output
		
		write(*,*) "## ERROR ## Matrix.rExponentiation is not implemented"
		stop
	end function rExponentiation
	
	!>
	!! @brief
	!!
	function rExponentiationByReal( this, constant ) result( output )
		class(Matrix), intent(in) :: this
		real(8), intent(in) :: constant
		type(Matrix) :: output
		
		write(*,*) "## ERROR ## Matrix.rExponentiation is not implemented"
		stop
	end function rExponentiationByReal
	
	!>
	!! @brief
	!!
! 	subroutine linearize( this, Ml, bw )
	subroutine linearize( this, Ml )
		class(Matrix), intent(in) :: this
		real(8), allocatable :: Ml(:)
! 		integer, intent(in), optional :: bw ! bandWidth
		
! 		integer :: bwEff
! 		integer :: nsize
		integer :: effSize
		integer :: i,j,n
		
! 		if( present(bw) ) then
! 			bwEff = bw
! 		else
! 			bwEff = 20
! 		end if
		
! 		nsize = size(M,dim=1)

		! Number of different elements in the matrix
! 		if( this.isSymmetric() ) then
! 			effSize = nsize*(nsize+1)/2
! 		else
! 			effSize = nsize
! 		end if
		! @todo hay que programar el type de la matriz en bits para que esta pueda ser
		!       a la vez simetrica y cuadrada
		
		effSize = this.nCols*(this.nCols+1)/2 ! <== Esto es para una matriz cuadrada simetrica
		allocate( Ml(effSize) )
		
		! + Esto es para una matriz cuadrada simetrica
		! |
		! v
		n=1
		do i=1,this.nCols
			do j=i,this.nRows
				Ml(n) = this.data(i,j)
				n=n+1
			end do
		end do
		
! 		nsize = size(M,dim=1)
! 		nHalfSize = bwEff*(2*nsize-bwEff+1)/2
! 		
! 		allocate( Ml(nHalfSize) )
! 		
! 		n=1
! 		do i=1,nsize
! 			do j=i,min(i+bwEff-1,nsize)
! 				Ml(n) = M(i,j)
! 				n=n+1
! 			end do
! 		end do

	end subroutine linearize
	
	!>
	!! @brief
	!! 
	!!      n
	!!      --
	!! V' = \ proj   ( V ) ui 
	!!      /     u_i
	!!      --
	!!     i=1
	!!
	!!               T
	!!              V * u
	!! proj  (V) = -------
	!!     u        | u |
	!!
	function projectionOntoNewAxes( this, axes ) result( Vprime )
		class(Matrix), intent(in) :: this
		type(Matrix), intent(in) :: axes
		type(Matrix) :: Vprime
		
		type(Matrix) :: u
		integer :: i
		
		if( this.type == COLUMN_MATRIX ) then
			Vprime = this
			
			do i=1,axes.nCols
				u = axes.column(i)
				call Vprime.set( i, 1, Matrix_get( this.transpose()*u, 1, 1 )/u.norm2() )
			end do
		else
			write(*,*) "### ERROR ### The Matrix.projectionOntoNewAxes method is only implemented for column matrices"
		end if
	end function projectionOntoNewAxes
	
	!>
	!! @brief  Returns the i,j-th element of M
	!!
	real(8) function Matrix_get( M, i, j )
		type(Matrix) , intent(in) :: M
		integer :: i, j
		
		Matrix_get = M.data(i,j)
	end function Matrix_get
	
	!>
	!! @brief  Returns the trace
	!!
	real(8) function Matrix_trace( M )
		type(Matrix) , intent(in) :: M
		
		Matrix_trace = M.trace()
	end function Matrix_trace
	
	!>
	!! @brief  Returns the norm2
	!!
	real(8) function Matrix_norm2( M )
		type(Matrix) , intent(in) :: M
		
		Matrix_norm2 = sqrt( Matrix_trace( M.transpose()*M ) )
	end function Matrix_norm2
	
	!>
	!! @brief  Returns the matrix representation for the column vector
	!!
	function Matrix_columnVector( nElems, val, values ) result( output )
		integer, intent(in) :: nElems
		real(8), optional, intent(in) :: val
		real(8), optional, intent(in) :: values(:)
		type(Matrix) :: output
		
		call output.columnVector( nElems, val, values )
	end function Matrix_columnVector
	
	!>
	!! @brief
	!!
	function Matrix_lMultiplicationByReal( constant, M ) result( output )
		real(8), intent(in) :: constant
		type(Matrix), intent(in) :: M
		type(Matrix) :: output
		
		call output.copyMatrix( M )
		output.data = M.data*constant
	end function Matrix_lMultiplicationByReal
	
	!>
	!! @brief Test method
	!!
	subroutine Matrix_test()
		type(Matrix) :: A, B, C
		real(8), allocatable :: lA(:)
		
		write(*,*) ""
		write(*,*) "Testing contructors"
		write(*,*) "==================="
		write(*,*) "Initialization of a matrix 12x12 with all values equal to 1.0"
		write(*,*) ""
		call A.init( 12, 12, 1.0_8 )
		call A.show( 6, .true. )
		write(*,*) ""
		write(*,*) "Initialization of a matrix 12x12 with random values"
		write(*,*) ""
		call B.random( 12, 12, symmetric=.true. )
		call B.show( 6, .true. )
		
		write(*,*) ""
		write(*,*) "Initialization with elements Inf"
		write(*,*) ""
		call B.random( 4, 4, symmetric=.true. )
		B.data(1,1) = Math_Inf
		B.data(2,2) = Math_Inf
		call B.show( 6, .true. )
		
		write(*,*) ""
		write(*,*) "Initialization with elements NaN"
		write(*,*) ""
		call B.random( 4, 4, symmetric=.true. )
		B.data(1,1) = Math_Inf
		B.data(2,2) = Math_NAN
		call B.show( 6, .true. )
		
! 		call A.init( 12, 12, 1.0_8 )
! 		call A.show( 6, .true. )
! 		write(*,*) ""
! 		write(*,*) "Initialization of a matrix 12x12 with random values"
! 		write(*,*) ""
! 		call B.random( 12, 12, symmetric=.true. )
! 		call B.show( 6, .true. )
		
		write(*,*) ""
		write(*,*) "Testing inverse"
		write(*,*) "==============="
		write(*,*) ""
		write(*,*) "B ="
		call B.show( 6, .true. )
		C = B.inverse()
		write(*,*) ""
		write(*,*) "B^{-1} ="
		call C.show( 6, .true. )
		A = B*C
		write(*,*) ""
		write(*,*) "B*B^{-1} ="
		call A.show( 6, .true. )
		
		write(*,*) ""
		write(*,*) "Testing inverse for small matrices"
		write(*,*) "=================================="
		write(*,*) ""
		write(*,*) "B ="
		call B.init(3,3)
		B.data(1,:) = [ -0.76675, 0.00000, -0.64195 ]
		B.data(2,:) = [  0.00000, 1.00000,  0.00000 ]
		B.data(3,:) = [  0.64195, 0.00000, -0.76675 ]
		call B.show( 6, .true. )
		C = B.inverse()
		write(*,*) ""
		write(*,*) "B^{-1} ="
		call C.show( 6, .true. )
		A = B*C
		write(*,*) ""
		write(*,*) "B*B^{-1} ="
		call A.show( 6, .true. )
		
		write(*,*) ""
		write(*,*) "B ="
		call B.init(2,2)
		B.data(1,:) = [ -0.76675, -0.84195 ]
		B.data(2,:) = [  0.64195, -2.76675 ]
		call B.show( 6, .true. )
		C = B.inverse()
		write(*,*) ""
		write(*,*) "B^{-1} ="
		call C.show( 6, .true. )
		A = B*C
		write(*,*) ""
		write(*,*) "B*B^{-1} ="
		call A.show( 6, .true. )
		
		write(*,*) ""
		write(*,*) "Approximately singular matrix "
		write(*,*) "B ="
		call B.init(3,3)
		B.data(1,:) = [  63534.983702,     97467.773498,    -54790.146796 ]
		B.data(2,:) = [  97467.773498,    212028.341745,     27139.351856 ]
		B.data(3,:) = [ -54790.146796,     27139.351856,    245051.310679 ]
		write(*,*) ""
		write(*,*) "det(B) =", B.determinant3x3()
		write(*,*) ""
		call B.show( 6, .true. )
		C = B.inverse()
		write(*,*) ""
		write(*,*) "B^{-1} ="
		call C.show( 6, .true. )
		A = B*C
		write(*,*) ""
		write(*,*) "B*B^{-1} ="
		call A.show( 6, .true. )
		
		write(*,*) ""
		write(*,*) "Testing operators"
		write(*,*) "================="
		write(*,*) ""
		A = A*2.0_8
		write(*,*) "A*2.0 ="
		call A.show( 6, .true. )
		
		call B.random( 12, 12, symmetric=.true. )
		write(*,*) ""
		write(*,*) "B ="
		call B.show( 6, .true. )
		A = B/B
		write(*,*) ""
		write(*,*) "B/B ="
		call A.show( 6, .true. )
		
		write(*,*) ""
		write(*,*) "Testing linearization"
		write(*,*) "====================="
		write(*,*) ""
		call A.random( 3, 3, symmetric=.true. )
		write(*,*) ""
		write(*,*) "A ="
		call A.show( 6, .true. )
		call A.linearize( lA )
		write(*,*) ""
		write(*,*) "linearize(A) ="
		write(*,"(5X,<size(lA)>F10.6)") lA
		deallocate(lA)
		
		write(*,*) ""
		write(*,*) "Testing eigenvalues and eigenvectors"
		write(*,*) "===================================="
		
		!-----------------------------------------------------------------------------
		! Ejemplo tomado de
		!
		! http://software.intel.com/sites/products/documentation/doclib/mkl_sa/11/mkl_lapack_examples/dsyev_ex.f.htm
		! *
		! * Matrix
		! *    1.96  -6.49  -0.47  -7.20  -0.65
		! *   -6.49   3.80  -6.39   1.50  -6.34
		! *   -0.47  -6.39   4.17  -1.51   2.67
		! *   -7.20   1.50  -1.51   5.70   1.80
		! *   -0.65  -6.34   2.67   1.80  -7.10
		! *
		! * Eigenvalues
		! * -11.07  -6.23   0.86   8.87  16.09
		! *
		! * Eigenvectors (stored columnwise)
		! *  -0.30  -0.61   0.40  -0.37   0.49
		! *  -0.51  -0.29  -0.41  -0.36  -0.61
		! *  -0.08  -0.38  -0.66   0.50   0.40
		! *   0.00  -0.45   0.46   0.62  -0.46
		! *  -0.80   0.45   0.17   0.31   0.16
		!-----------------------------------------------------------------------------
		
		call A.init(5,5)
		A.data(1,:) = [  1.96,  -6.49,  -0.47,  -7.20,  -0.65 ]
		A.data(2,:) = [ -6.49,   3.80,  -6.39,   1.50,  -6.34 ]
		A.data(3,:) = [ -0.47,  -6.39,   4.17,  -1.51,   2.67 ]
		A.data(4,:) = [ -7.20,   1.50,  -1.51,   5.70,   1.80 ]
		A.data(5,:) = [ -0.65,  -6.34,   2.67,   1.80,  -7.10 ]
		
		write(*,*) ""
		write(*,*) "A ="
		call A.show( 6, .true. )
		
		call A.eigen( eVecs=B, eVals=C )
		
		write(*,*) ""
		write(*,*) "eigenvectors ="
		call B.show( 6, .true. )
		
		write(*,*) ""
		write(*,*) "eigenvalues ="
		call C.show( 6, .true. )
		
		write(*,*) ""
		write(*,*) "Testing projection for column vectors"
		write(*,*) "====================================="
		call A.columnVector( 3, values=[0.5_8,0.7071_8,0.5_8] )
		write(*,*) ""
		write(*,*) "A = "
		call A.show( formatted=.true. )
		
		! These axis are equivalent to a rotation with alpha=45º and beta=45º
		call B.init(3,3)
		B.data(1,:) = [  0.5000, 0.5000, -0.7071 ]
		B.data(2,:) = [ -0.7071, 0.7071,  0.0000 ]
		B.data(3,:) = [  0.5000, 0.5000,  0.7071 ]
		
		write(*,*) ""
		write(*,*) "axes = "
		call B.show( formatted=.true. )
		
		C = A.projectionOntoNewAxes( B )
		
		write(*,*) ""
		write(*,*) "projection = "
		call C.show( formatted=.true. )
		
		write(*,*) ""
		write(*,*) "Testing info methods"
		write(*,*) "===================="
		call A.random( 3, 3, symmetric=.true. )
		write(*,*) ""
		write(*,*) "A = "
		call A.show( formatted=.true. )
		write(*,*) "A.isDiagonal() = ", A.isDiagonal()
		
		call A.init( 3, 3, val=1d-12 )
		write(*,*) ""
		write(*,*) "A = "
		call A.show( formatted=.true. )
		write(*,*) "A.isZero() = ", A.isZero()
		write(*,*) "A.isZero( tol=1d-12 ) = ", A.isZero( tol=1d-12 )
		
	end subroutine Matrix_test
	
end module Matrix_

! 	subroutine Matrix_show( this, rowKeys, columnKeys, flags )
! 		implicit none
! 		type(Matrix), intent(in) :: this
! 		character(*), intent(in), optional :: rowKeys(:)
! 		character(*), intent(in), optional :: columnKeys(:)
! 		integer, intent(in), optional :: flags
! 		
! 		integer :: auxColNum
! 		integer :: columns
! 		integer :: rows
! 		integer :: i
! 		integer :: j
! 		integer :: k
! 		integer :: lowerLimit
! 		integer :: upperLimit
! 		integer :: tmpFlags
! 		
! 		tmpFlags = WITHOUT_KEYS
! 		if( present(flags) ) then
! 			tmpFlags = flags
! 		end if
! 		
! 		rows = size( this.values, DIM=1 )
! 		columns = size( this.values, DIM=2 )
! 				
! 		if( present( rowKeys ) ) then
! 			if( size( rowKeys ) < rows ) then
! 			
! 				call Matrix_exception(WARNING, "The size of row keys is low than number of matrix rows", &
! 					 "Class object Matrix in the show() function" )
! 				
! 			end if
! 		end if
! 		
! 		if( present( columnKeys ) ) then
! 			if( size( columnKeys ) < columns ) then
! 			
! 				call Matrix_exception(WARNING, "The size of column keys is low than number of matrix columns", &
! 					"Class object Matrix in the show() function" )
! 				
! 			end if
! 		end if
! 		
! 		do k=1, ceiling( (columns * 1.0)/(APMO_instance.FORMAT_NUMBER_OF_COLUMNS * 1.0 ) )
! 		
! 			lowerLimit = APMO_instance.FORMAT_NUMBER_OF_COLUMNS * ( k - 1 ) + 1
! 			upperLimit = APMO_instance.FORMAT_NUMBER_OF_COLUMNS * ( k )
! 			auxColNum = APMO_instance.FORMAT_NUMBER_OF_COLUMNS
! 			
! 			if ( upperLimit > columns ) then
! 				auxColNum =  APMO_instance.FORMAT_NUMBER_OF_COLUMNS -  upperLimit + columns
! 				upperLimit = columns
! 			end if
! 			
! 			if( present( columnKeys ) ) then
! 
! 				if( tmpFlags == WITH_COLUMN_KEYS .or. tmpFlags == WITH_BOTH_KEYS ) then
! 					write (6,"(21X,<auxColNum>A15)") ( columnKeys(i), i = lowerLimit, upperLimit )
! 				end if
! 				
! 			else
! 			
! 				if( tmpFlags /= WITHOUT_KEYS ) then
! 					if( tmpFlags == WITH_COLUMN_KEYS .or. tmpFlags == WITH_BOTH_KEYS ) then
! 						write (6,"(5X,<auxColNum>I15)") ( i,i=lowerLimit,upperLimit )
! 					end if
! 				end if
! 				
! 			end if
! 				
! 			print *,""
! 			
! 			if( present( rowKeys ) ) then
! 			
! 				if( tmpFlags == WITH_ROW_KEYS .or. tmpFlags == WITH_BOTH_KEYS ) then
! 					write (6,"(A18,<auxColNum>F15.6)") ( rowKeys(i), ( this.values(i,j), j=lowerLimit,upperLimit ), i = 1, rows )
! 				else
! 					write (6,"(5X,<auxColNum>F15.6)") ( ( this.values(i,j), j=lowerLimit,upperLimit ), i = 1, rows )
! 				end if
! 				
! 			else
! 				if( tmpFlags /= WITHOUT_KEYS ) then
! 				
! 					if( ( tmpFlags == WITH_ROW_KEYS .or. tmpFlags == WITH_BOTH_KEYS ) .and. tmpFlags /= WITHOUT_KEYS ) then
! 						write (6,"(I5,<auxColNum>F15.6)") ( i, ( this.values(i,j), j=lowerLimit,upperLimit ), i = 1, rows )
! 					else
! 						write (6,"(5X,<auxColNum>F15.6)") ( ( this.values(i,j), j=lowerLimit,upperLimit ), i = 1, rows )
! 					end if
! 					
! 				else
! 				
! 					write (6,"(5X,<auxColNum>F15.6)") ( ( this.values(i,j), j=lowerLimit,upperLimit ), i = 1, rows )
! 
! 				end if
! 			end if
! 			
! 			print *,""
! 			
! 		end do
! 		
! 	end subroutine Matrix_show


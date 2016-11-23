module FourierGridDiagonalization_
	use GOptions_
	use Math_
	use Grid_
	use RNFunction_
	use CNFunction_
	implicit none
	private
	
	public :: &
		FourierGridDiagonalization_test
		
	integer, parameter, public :: FourierGridDiagonalization_EIGENVALUES = 0
	integer, parameter, public :: FourierGridDiagonalization_EIGENFUNCTIONS = 1
	
	type, public :: FourierGridDiagonalization
		real(8), private :: rMass
		type(RNFunction), private :: potential
		
		real(8), allocatable :: eigenValues(:)
		type(RNFunction), allocatable :: rEigenFunctions(:)
		type(CNFunction), allocatable :: cEigenFunctions(:)
		
		contains
			procedure :: init
			final :: destroy
			procedure :: clear
			procedure :: str
			procedure :: show
			procedure :: nStates
			procedure :: run
			procedure :: runReal
			procedure :: runComplex
	end type FourierGridDiagonalization
		
	contains
		
	!>
	!! @brief Contructor
	!!
	subroutine init( this, potential, rMass )
		implicit none
		class(FourierGridDiagonalization) :: this
		class(RNFunction), intent(in) :: potential
		real(8), optional, intent(in) :: rMass
		
		call this.clear()
		
		this.rMass = 1.0_8
		if( present(rMass) ) this.rMass = rMass
		
		this.potential = potential
	end subroutine init
	
	!>
	!! @brief Destructor
	!!
	subroutine destroy( this )
		type(FourierGridDiagonalization) :: this
		
		call this.clear()
	end subroutine destroy
	
	!>
	!! @brief Destructor
	!!
	subroutine clear( this )
		class(FourierGridDiagonalization) :: this
		
		this.rMass = 1.0_8
		
! 		call this.potential.clear()
		if( allocated(this.eigenValues) ) deallocate( this.eigenValues )
		if( allocated(this.rEigenFunctions) ) deallocate( this.rEigenFunctions )
	end subroutine clear
	
	!>
	!! @brief String representation of the object
	!!
	function str( this ) result( output )
		implicit none
		class(FourierGridDiagonalization) :: this 
		character(len=200) :: output
		
		integer :: fmt
		character(len=200) :: strBuffer
		
		output = ""
		
		output = trim(output)//"<FourierGridDiagonalization:"
		
		output = trim(output)//this.potential.str()
		
		output = trim(output)//",rMass="
		fmt = int(log10(this.rMass+1.0))+1
		write(strBuffer, "(f<fmt+7>.6)") this.rMass
		output = trim(output)//trim(strBuffer)
		
		output = trim(output)//">"
	end function str
	
	!>
	!! @brief Write the string representation of the object
	!        in a selected unit
	!!
	subroutine show( this, unit )
		class(FourierGridDiagonalization) :: this
		integer, optional, intent(in) :: unit
		
		integer :: effunit
		
		if( present(unit) ) then
			effunit = unit
		else
			effunit = 6
		end if
		
		write(effunit,"(a)") trim(this.str())
	end subroutine show
	
	!>
	!! @brief Returns the number of eigenvalues located after run()
	!!
	function nStates( this ) result( output )
		class(FourierGridDiagonalization), intent(in) :: this
		integer :: output
		
		if( allocated(this.eigenValues) ) then
			output = size(this.eigenValues)
		else
			output = -1
			call GOptions_warning( "The method run() has to be called before (returning -1)", "FourierGridDiagonalization.nStates()" )
		end if
	end function nStates
	
	!>
	!! @brief Starts the numerical method
	!!
	subroutine run( this, task, nStates, iRange, vRange, abstol, type )
		class(FourierGridDiagonalization) :: this
		integer, optional, intent(in) :: task
		integer, optional, intent(in) :: nStates
		integer, optional, intent(in) :: iRange(2)
		real(8), optional, intent(in) :: vRange(2)
		real(8), optional, intent(in) :: abstol
		integer, optional, intent(in) :: type
		
		integer :: effType
		
		effType = 0
		if( present(type) ) effType = type
		
		if( effType == 1 ) then
			call this.runComplex( task, nStates, iRange, vRange, abstol )
		else
			call this.runReal( task, nStates, iRange, vRange, abstol )
		end if
	end subroutine run
	
	!>
	!! @brief Starts the numerical method
	!!        The Fourier Grid Hamiltonian Method for Calculating Vibrational Energy Levels of Triatomic Molecules
	!!        http://onlinelibrary.wiley.com/doi/10.1002/qua.22547/pdf
	!!        The Fourier grid Hamiltonian method for bound state eigenvalues and eigenfunctions
	!!        http://scitation.aip.org/content/aip/journal/jcp/91/6/10.1063/1.456888
	!!
	subroutine runReal( this, task, nStates, iRange, vRange, abstol )
		class(FourierGridDiagonalization) :: this
		integer, optional, intent(in) :: task
		integer, optional, intent(in) :: nStates
		integer, optional, intent(in) :: iRange(2)
		real(8), optional, intent(in) :: vRange(2)
		real(8), optional, intent(in) :: abstol
		
		integer :: effIRange(2)
		real(8) :: effVRange(2)
		real(8) :: effAbstol
		
		integer :: nPoints
		
		integer :: nEigenFound
		real(8), allocatable :: eigenValues(:)
		real(8), allocatable :: eigenVectors(:,:)
		real(8), allocatable :: H(:,:)
		
		character(1) :: charTask
		character(1) :: charRange
		real(8), allocatable :: work(:)
		integer, allocatable :: iwork(:)
		integer, allocatable :: ifail(:)
		integer :: info
		
		integer :: i, j
		real(8) :: dr, L, mass
		
		! Default values: 10 states
		charRange = "I"
		effIRange = [ 1, 10 ]
		effVRange = [ 0.0_8, 0.0_8 ]
		if( present(nStates) ) then
			charRange = "I"
			
			effIRange = [ 1, nStates ]
		else if( present(iRange) ) then
			charRange = "I"
			
			effIRange = iRange
		else if( present(vRange) ) then
			charRange = "V"
			
			effVRange = vRange
		end if
		
		charTask = "N"
		if( present(task) ) then
			if( task == FourierGridDiagonalization_EIGENVALUES ) then
				charTask = "N"
			else if( task == FourierGridDiagonalization_EIGENFUNCTIONS ) then
				charTask = "V"
			end if
		end if
		
		effAbstol = 1.0e-10
		if( present(abstol) ) effAbstol = abstol
		
		nPoints = this.potential.nPoints()
		
		allocate( eigenValues(nPoints) )
		allocate( eigenVectors(nPoints,nPoints) )
		
		allocate( H(nPoints,nPoints) )
		
		allocate( work(8*nPoints) )
		allocate( iwork(5*nPoints) )
		allocate( ifail(nPoints) )
		
		L = this.potential.xGrid.lenght()
		dr = this.potential.xGrid.stepSize
		mass = this.rMass
		
		H = 0.0_8
		
		do i=1,nPoints-1
			do j=i+1,nPoints
	! 			H(i,j) = (-1.0_8)**(i-j)*( 2.0_8*Math_PI/(2.0_8*L*sin(Math_PI*(i-j)/nPoints)) )**2/mass
				H(i,j) = (-1.0_8)**(i-j)*( Math_PI/dr/nPoints/sin(Math_PI*(i-j)/nPoints) )**2/mass
				H(j,i) = H(i,j)
			end do
		end do
		
		do i=1,nPoints
! 			H(i,i) = (2.0_8*Math_PI)**2*( (nPoints-1)*(nPoints-2)/6.0_8 + 1.0_8 )/( 4.0_8*mass*L**2 ) + this.potential.at( i )
			H(i,i) = ( Math_PI/dr/nPoints )**2*( nPoints**2+2 )/6.0_8/mass + this.potential.at( i )
! 			H(i,i) = (Math_PI**2/6.0_8)/( mass*dr**2 ) + this.potential.at( i )
		end do
		
! 		call dsyevx( 'V', 'A', 'U', nPoints, H, nPoints, &
! 						0.0_8, 0.0_8, &
! 						0, 0, &
! 						1.0e-10, &
! 						nEigenFound, eigenValues, eigenVectors, nPoints, &
! 						work, size(work), iwork, ifail, info )
			
! 		call dsyevx( 'V', 'I', 'U', nPoints, H, nPoints, &
! 				0.0_8, 0.0_8, &
! 				1, min(effNStates,nPoints), &
! 				effAbstol, &
! 				nEigenFound, eigenValues, eigenVectors, nPoints, &
! 				work, size(work), iwork, ifail, info )

	! 	call dsyevx( 'V', 'V', 'U', nPoints, H, nPoints, &
	! 					-0.30_8, 0.30_8, &
	! 					0, 0, &
	! 					1.0e-10, &
	! 					nEigenFound, eigenValues, eigenVectors, nPoints, &
	! 					work, size(work), iwork, ifail, info )
	
		call dsyevx( charTask, charRange, 'U', nPoints, H, nPoints, &
				effVRange(1), effVRange(2), &
				effIRange(1), effIRange(2), &
				effAbstol, &
				nEigenFound, eigenValues, eigenVectors, nPoints, &
				work, size(work), iwork, ifail, info )
		
		if( info /= 0 ) then
			call GOptions_error( "Diagonalization failed", "FourierGridDiagonalization.run()" )
		end if
		
		if( allocated(this.eigenValues) ) deallocate( this.eigenValues )
		allocate( this.eigenValues( nEigenFound ) )
		
		if( allocated(this.rEigenFunctions) ) deallocate( this.rEigenFunctions )
		allocate( this.rEigenFunctions( nEigenFound ) )
		
		this.eigenValues(1:nEigenFound) = eigenValues(1:nEigenFound)
		do i=1,nEigenFound
			call this.rEigenFunctions(i).fromGridArray( this.potential.xGrid, eigenVectors(:,i) )
		end do
		
		deallocate( work )
		deallocate( iwork )
		deallocate( ifail )
		
		deallocate( H )
		deallocate( eigenValues )
		deallocate( eigenVectors )

	end subroutine runReal
	
	!>
	!! @brief Starts the numerical method
	!!        The Fourier Grid Hamiltonian Method for Calculating Vibrational Energy Levels of Triatomic Molecules
	!!        http://onlinelibrary.wiley.com/doi/10.1002/qua.22547/pdf
	!!
	subroutine runComplex( this, task, nStates, iRange, vRange, abstol )
		class(FourierGridDiagonalization) :: this
		integer, optional, intent(in) :: task
		integer, optional, intent(in) :: nStates
		integer, optional, intent(in) :: iRange(2)
		real(8), optional, intent(in) :: vRange(2)
		real(8), optional, intent(in) :: abstol
		
		integer :: effIRange(2)
		real(8) :: effVRange(2)
		real(8) :: effAbstol
		
		integer :: nPoints
		
		integer :: nEigenFound
		real(8), allocatable :: eigenValues(:)
		complex(8), allocatable :: eigenVectors(:,:)
		complex(8), allocatable :: H(:,:)
		
		character(1) :: charTask
		character(1) :: charRange
		integer, allocatable :: nonzeroElements(:)
		complex(8), allocatable :: work(:)
		real(8), allocatable :: rwork(:)
		integer, allocatable :: iwork(:)
		integer, allocatable :: ifail(:)
		integer :: info
		
		integer :: i, j
		real(8) :: dr, L, mass
		
		! Default values: 10 states
		charRange = "I"
		effIRange = [ 1, 10 ]
		effVRange = [ 0.0_8, 0.0_8 ]
		if( present(nStates) ) then
			charRange = "I"
			
			effIRange = [ 1, nStates ]
		else if( present(iRange) ) then
			charRange = "I"
			
			effIRange = iRange
		else if( present(vRange) ) then
			charRange = "V"
			
			effVRange = vRange
		end if
		
		charTask = "N"
		if( present(task) ) then
			if( task == FourierGridDiagonalization_EIGENVALUES ) then
				charTask = "N"
			else if( task == FourierGridDiagonalization_EIGENFUNCTIONS ) then
				charTask = "V"
			end if
		end if
		
		effAbstol = 1.0e-10
		if( present(abstol) ) effAbstol = abstol
		
		nPoints = this.potential.nPoints()
		
		allocate( eigenValues(nPoints) )
		allocate( eigenVectors(nPoints,nPoints) )
		
		allocate( H(nPoints,nPoints) )
		
		allocate( nonzeroElements(2*nPoints) )
		allocate( work(2*nPoints) )
		allocate( rwork(24*nPoints) )
		allocate( iwork(10*nPoints) )
		allocate( ifail(nPoints) )
		
		L = this.potential.xGrid.lenght()
		dr = this.potential.xGrid.stepSize
		mass = this.rMass
		
		H = 0.0_8
		
		do i=1,nPoints-1
			do j=i+1,nPoints
	! 			H(i,j) = (-1.0_8)**(i-j)*( 2.0_8*Math_PI/(2.0_8*L*sin(Math_PI*(i-j)/nPoints)) )**2/mass
				H(i,j) = (-1.0_8)**(i-j)*( Math_PI/dr/nPoints/sin(Math_PI*(i-j)/nPoints) )**2/mass
				H(j,i) = H(i,j)
			end do
		end do
		
		do i=1,nPoints
! 			H(i,i) = (2.0_8*Math_PI)**2*( (nPoints-1)*(nPoints-2)/6.0_8 + 1.0_8 )/( 4.0_8*mass*L**2 ) + this.potential.at( i )
			H(i,i) = ( Math_PI/dr/nPoints )**2*( nPoints**2+2 )/6.0_8/mass + this.potential.at( i )
! 			H(i,i) = (Math_PI**2/6.0_8)/( mass*dr**2 ) + this.potential.at( i )
		end do
		
		call zheevr( charTask, charRange, 'U', nPoints, H, nPoints, &
				effVRange(1), effVRange(2), &
				effIRange(1), effIRange(2), &
				effAbstol, &
				nEigenFound, eigenValues, eigenVectors, nPoints, nonzeroElements, &
				work, size(work), rwork, size(rwork), iwork, size(iwork), info )
				
		if( info /= 0 ) then
			call GOptions_error( "Diagonalization failed", "FourierGridDiagonalization.run()" )
		end if
		
		if( allocated(this.eigenValues) ) deallocate( this.eigenValues )
		allocate( this.eigenValues( nEigenFound ) )
		
		if( allocated(this.cEigenFunctions) ) deallocate( this.cEigenFunctions )
		allocate( this.cEigenFunctions( nEigenFound ) )
		
		this.eigenValues(1:nEigenFound) = eigenValues(1:nEigenFound)
		do i=1,nEigenFound
			call this.cEigenFunctions(i).fromGridArray( this.potential.xGrid, eigenVectors(:,i) )
		end do
		
		deallocate( work )
		deallocate( iwork )
		deallocate( ifail )
		
		deallocate( H )
		deallocate( eigenValues )
		deallocate( eigenVectors )

	end subroutine runComplex
	
	!>
	! This is neccesary only for NFunction_test()
	!!
	function funcTest( x ) result( output )
		real(8), intent(in) :: x
		real(8) :: output
		
		output = 5.0_8*( exp(2.0_8*(2.0_8-x))-2.0_8*exp(2.0_8-x) )
	end function funcTest
	
	!>
	! Test
	!!
	subroutine FourierGridDiagonalization_test()
		type(Grid) :: rGrid
		type(RNFunction) :: potential
		type(FourierGridDiagonalization) :: solver
		integer :: i
		
		call rGrid.init( 1.0_8, 30.0_8, 1000 )
		call rGrid.show()
		
		call potential.fromFunction( rGrid, funcTest )
		call potential.show()
! 		call potential.save( "morse.out" )
		
		call solver.init( potential, rMass=5.0_8 )
		call solver.run( nStates=10 )
		
		write(*,*) "solver.nStates() = ", solver.nStates()
		do i=1,solver.nStates()
			write(*,"(I5,F20.10)") i, solver.eigenValues(i)
		end do
		
! 		call solver.eigenfunction(7).save( "salida" )
	end subroutine FourierGridDiagonalization_test

end module FourierGridDiagonalization_

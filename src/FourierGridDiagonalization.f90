module FourierGridDiagonalization_
	use Math_
	use RNFunction_
	implicit none
	private
	
	type, public :: FourierGridDiagonalization
		real(8), private :: rMass
		type(RNFunction), private :: potential
		
		integer :: nStates
		real(8), allocatable :: eigenValues(:)
		type(RNFunction), allocatable :: eigenFunctions(:)
		
		contains
			procedure :: init
			final :: destroy
			procedure :: clear
			procedure :: str
			procedure :: show
			procedure :: run
	end type FourierGridDiagonalization
		
	contains
		
	!>
	!! @brief Contructor
	!!
	subroutine init( this, potential, nStates, rMass )
		implicit none
		class(FourierGridDiagonalization) :: this
		class(RNFunction), intent(in) :: potential
		integer, optional, intent(in) :: nStates
		real(8), optional, intent(in) :: rMass
		
		call this.clear()
		
		if( present(nStates) ) then
			this.nStates = nStates
		else
			this.nStates = 10
		end if
		
		if( present(rMass) ) then
			this.rMass = rMass
		else
			this.rMass = 1.0_8
		end if
		
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
		this.nStates = 10
		
! 		call this.potential.clear()
		if( allocated(this.eigenValues) ) deallocate( this.eigenValues )
		if( allocated(this.eigenFunctions) ) deallocate( this.eigenFunctions )
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
		
		output = trim(output)//",nStates="
		fmt = int(log10(float(this.nStates)+1.0))+1
		write(strBuffer, "(i<fmt>)") this.nStates
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
	!! @brief Starts the numerical method
	!!
	subroutine run( this, abstol )
		class(FourierGridDiagonalization) :: this
		real(8), optional, intent(in) :: abstol
		
		real(8) :: effAbstol
		
		integer :: nPoints
		
		integer :: nEigenFound
		real(8), allocatable :: eigenValues(:)
		real(8), allocatable :: eigenVectors(:,:)
		real(8), allocatable :: H(:,:)
		
		real(8), allocatable :: work(:)
		integer, allocatable :: iwork(:)
		integer, allocatable :: ifail(:)
		integer :: info
		
		integer :: i, j
		real(8) :: dr, L, mass
		
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
			
		call dsyevx( 'V', 'I', 'U', nPoints, H, nPoints, &
				0.0_8, 0.0_8, &
				1, min(this.nStates,nPoints), &
				effAbstol, &
				nEigenFound, eigenValues, eigenVectors, nPoints, &
				work, size(work), iwork, ifail, info )

	! 	call dsyevx( 'V', 'V', 'U', nPoints, H, nPoints, &
	! 					-0.30_8, 0.30_8, &
	! 					0, 0, &
	! 					1.0e-10, &
	! 					nEigenFound, eigenValues, eigenVectors, nPoints, &
	! 					work, size(work), iwork, ifail, info )
		
		if( info /= 0 ) then
			write(*,*) "### ERROR ### Diagonalization failed"
			stop
		end if
		
		if( allocated(this.eigenValues) ) deallocate( this.eigenValues )
		allocate( this.eigenValues( nEigenFound ) )
		
		if( allocated(this.eigenFunctions) ) deallocate( this.eigenFunctions )
		allocate( this.eigenFunctions( nEigenFound ) )
		
		this.nStates = nEigenFound
		this.eigenValues(1:nEigenFound) = eigenValues(1:nEigenFound)
		do i=1,nEigenFound
			call this.eigenFunctions(i).fromGridArray( this.potential.xGrid, eigenVectors(:,i) )
		end do
		
		deallocate( work )
		deallocate( iwork )
		deallocate( ifail )
		
		deallocate( H )
		deallocate( eigenValues )
		deallocate( eigenVectors )

	end subroutine run
	
end module FourierGridDiagonalization_

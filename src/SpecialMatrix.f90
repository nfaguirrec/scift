!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                                                                                   !!
!!  This file is part of SciFT project                                               !!
!!  Copyright (c) 2013-2013 Nestor F. Aguirre (nfaguirrec@gmail.com)                 !!
!!                                                                                   !!
!!  Redistribution and use in source and binary forms, with or without               !!
!!  modification, are permitted provided that the following conditions are met:      !!
!!                                                                                   !!
!!  1. Redistributions of source code must retain the above copyright notice, this   !!
!!     list of conditions and the following disclaimer.                              !!
!!  2. Redistributions in binary form must reproduce the above copyright notice,     !!
!!     this list of conditions and the following disclaimer in the documentation     !!
!!     and/or other materials provided with the distribution.                        !!
!!  3. Neither the name of the copyright holders nor the names of its contributors   !!
!!     may be used to endorse or promote products derived from this software         !!
!!     without specific prior written permission.                                    !!
!!                                                                                   !!
!!  The copyright holders provide no reassurances that the source code provided      !!
!!  does not infringe any patent, copyright, or any other intellectual property      !!
!!  rights of third parties.  The copyright holders disclaim any liability to any    !!
!!  recipient for claims brought against recipient by any third party for            !!
!!  infringement of that parties intellectual property rights.                       !!
!!                                                                                   !!
!!  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND  !!
!!  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED    !!
!!  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE           !!
!!  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR  !!
!!  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES   !!
!!  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;     !!
!!  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND      !!
!!  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT       !!
!!  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS    !!
!!  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                     !!
!!                                                                                   !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!>
!! @brief
!!
module SpecialMatrix_
	use UnitsConverter_
	use String_
	use Math_
	use RandomUtils_
	use Matrix_
	implicit none
	private
	
	! @todo Debería tener todo esto implementado http://www.ee.ic.ac.uk/hp/staff/dmb/matrix/special.html
	
	public :: &
		SpecialMatrix_identity, &
		SpecialMatrix_unitaryColumnMatrix, &
		SpecialMatrix_xRotation, &
		SpecialMatrix_yRotation, &
		SpecialMatrix_zRotation, &
		SpecialMatrix_rotation, &
		SpecialMatrix_randomRotation, &
		SpecialMatrix_rotationTransform, &
		SpecialMatrix_test
		
	contains
	
	!>
	!! @brief Identity matrix
	!!
	function SpecialMatrix_identity( nRows, nCols ) result( I )
		integer, intent(in) :: nRows, nCols
		type(Matrix) :: I
		
		call I%identity( nRows, nCols )
	end function SpecialMatrix_identity
	
	!>
	!! @brief Gives a 3D unitary vector in spherical coordinates oriented by the angles theta and phi
	!! @param theta  0 <= theta <= pi
	!! @param phi    0 <=   phi <= 2pi
	!!
	function SpecialMatrix_unitaryColumnMatrix( theta, phi ) result( U )
		real(8), intent(in) :: theta
		real(8), intent(in) :: phi
		type(Matrix) :: U
		
		call U.columnVector( 3 )
		call U%set( 1, 1, sin(theta)*cos(phi) )
		call U%set( 2, 1, sin(theta)*sin(phi) )
		call U%set( 3, 1, cos(theta) )
	end function SpecialMatrix_unitaryColumnMatrix

	!>
	!! @brief Gives a rotation matrix around of x axis by "theta" radians
	!!
	function SpecialMatrix_xRotation( theta ) result( Rx )
		real(8), intent(in) :: theta
		type(Matrix) :: Rx
		
		call Rx%identity( 3, 3 )
		call Rx%set( 2, 2, cos(theta) )
		call Rx%set( 2, 3, sin(theta) )
		call Rx%set( 3, 2,-sin(theta) )
		call Rx%set( 3, 3, cos(theta) )
	end function SpecialMatrix_xRotation
	
	!>
	!! @brief Gives a rotation matrix around of y axis by theta radians
	!!
	function SpecialMatrix_yRotation( theta ) result( Ry )
		real(8), intent(in) :: theta
		type(Matrix) :: Ry
		
		call Ry%identity( 3, 3 )
		call Ry%set( 1, 1, cos(theta) )
		call Ry%set( 1, 3,-sin(theta) )
		call Ry%set( 3, 1, sin(theta) )
		call Ry%set( 3, 3, cos(theta) )
	end function SpecialMatrix_yRotation
	
	!>
	!! @brief Gives a rotation matrix around of z axis by "theta" radians
	!!
	function SpecialMatrix_zRotation( theta ) result( Rz )
		real(8), intent(in) :: theta
		type(Matrix) :: Rz
		
		call Rz%identity( 3, 3 )
		call Rz%set( 1, 1, cos(theta) )
		call Rz%set( 1, 2, sin(theta) )
		call Rz%set( 2, 1,-sin(theta) )
		call Rz%set( 2, 2, cos(theta) )
	end function SpecialMatrix_zRotation
	
	!>
	!! @brief Gives a rotation matrix that represents a rotation in 3D by radians about the origin.
	!! http://www.easyspin.org/documentation/eulerangles.html
	!! @param alpha   0 <= alpha <  2pi  ----> equivalent to   phi en ZYZ
	!! @param beta    0 <= beta  <=  pi  ----> equivalent to theta en ZYZ
	!! @param gamma   0 <= gamma <  2pi
	!! @param convention ZYZ, YXZ, XYZ, ZXZ, ZYZ, default ZYZ
	!!
	function SpecialMatrix_rotation( alpha, beta, gamma, convention ) result( R )
		real(8), intent(in) :: alpha
		real(8), intent(in) :: beta
		real(8), intent(in) :: gamma
		character(3), optional, intent(in) :: convention
		type(Matrix) :: R
		
		character(3) :: effConvention
		
		effConvention = "ZYZ"
		if( present(convention) ) effConvention = convention
		
		select case( effConvention )
			case( "YXZ" )
				R = SpecialMatrix_zRotation( gamma )* &
					SpecialMatrix_xRotation(  beta )* &
					SpecialMatrix_yRotation( alpha )
			case( "ZYX" )
				R = SpecialMatrix_xRotation( gamma )* &
					SpecialMatrix_yRotation(  beta )* &
					SpecialMatrix_zRotation( alpha )
			case( "XYZ" )
				R = SpecialMatrix_zRotation( gamma )* &
					SpecialMatrix_yRotation(  beta )* &
					SpecialMatrix_xRotation( alpha )
			case( "ZXZ" )
				R = SpecialMatrix_zRotation( gamma )* &
					SpecialMatrix_xRotation(  beta )* &
					SpecialMatrix_zRotation( alpha )
			case( "ZYZ" )
				R = SpecialMatrix_zRotation( gamma )* &
					SpecialMatrix_yRotation(  beta )* &
					SpecialMatrix_zRotation( alpha )
		end select
	end function SpecialMatrix_rotation
	
	!>
	!! @brief Gives a random rotation matrix that represents a
	!!        rotation in 3D by radians about the origin.
	!!
	function SpecialMatrix_randomRotation( convention, alpha, beta, gamma, fr ) result( R )
		character(3), optional, intent(in) :: convention
		real(8), optional, intent(out) :: alpha
		real(8), optional, intent(out) :: beta
		real(8), optional, intent(out) :: gamma
		integer, optional, intent(in) :: fr
		type(Matrix) :: R
		
		integer :: effFr
		real(8) :: effAlpha
		real(8) :: effBeta
		real(8) :: effGamma
		
		effFr = 3
		if( present(fr) ) effFr = fr
		
		if( effFr > 3 .or. effFr < 0 ) then
			write(6,"(A)") "### ERROR ### SpecialMatrix_randomRotation()"
			write(6,"(A)") "Number of degrees of freedom should be grater than 0 and lower than 3, it have been selected "//trim(FString_fromInteger(effFr))
			stop
		end if
		
! 		if( effFr/=3 .and. effFr/=2 .and. effFr/=0 ) then
! 			write(6,"(A)") "### ERROR ### SpecialMatrix_randomRotation()"
! 			write(6,"(A)") "Number of degrees of freedom should be 3, 2 or 0, it have been selected "//trim(FString_fromInteger(effFr))
! 			stop
! 		end if
		
		call RandomUtils_init()
		
		select case( effFr )
			case(3)
				effAlpha = RandomUtils_uniform( [0.0_8, 2.0_8*Math_PI] ) ! phi
				effBeta  = RandomUtils_uniform( [0.0_8, Math_PI] )  ! theta
				effGamma = RandomUtils_uniform( [0.0_8, 2.0_8*Math_PI] )
				
				R = SpecialMatrix_rotation( effAlpha, effBeta, effGamma, convention )
				
			case(2)
				effAlpha = RandomUtils_uniform( [0.0_8, 2.0_8*Math_PI] ) ! phi
				effBeta  = RandomUtils_uniform( [0.0_8, Math_PI] )  ! theta
				effGamma = 0.0_8  ! Condon and Shortley phase convention
				
				R = SpecialMatrix_rotation( effAlpha, effBeta, effGamma, convention )
				
			case(1)
				effAlpha = 0.0_8 ! phi
				effBeta  = RandomUtils_uniform( [0.0_8, 2.0_8*Math_PI] )  ! theta
				effGamma = 0.0_8  ! Condon and Shortley phase convention
				
				R = SpecialMatrix_rotation( effAlpha, effBeta, effGamma, convention )
				
			case default
				
				R = SpecialMatrix_identity( 3, 3 )
		end select
		
		if( present(alpha) ) alpha = effAlpha
		if( present(beta) )   beta = effBeta
		if( present(gamma) ) gamma = effGamma
	end function SpecialMatrix_randomRotation
	
	!>
	!! @brief Gives a rotation about the origin that transforms the
	!!        axes "axesBegin" to the direction of the axes "axesEnd".
	!!
	function SpecialMatrix_rotationTransform( axesBegin, axesEnd, angles ) result( R )
		type(Matrix), intent(in) :: axesBegin
		type(Matrix), intent(in) :: axesEnd
		real(8), optional, intent(out) :: angles(3)
		type(Matrix) :: R
		
		type(Matrix) :: u
		real(8) :: rThetaPhi(3)
		
		call u.columnVector( 3, values=axesBegin%data(:,3) )
		u = u.projectionOntoNewAxes( axesEnd )
		rThetaPhi = Math_cart2Spher( u%data(:,1) )
		
		R = SpecialMatrix_rotation( rThetaPhi(3), rThetaPhi(2), 0.0_8 ) ! Ry(beta)*Rz(alpha)
		
		if( present(angles) ) then
			angles(1) = rThetaPhi(3) ! alpha
			angles(2) = rThetaPhi(2) ! beta
		end if
		
		call u.columnVector( 3, values=axesBegin%data(:,1) )
		u = u.projectionOntoNewAxes( axesEnd )
		
		u = R*u
		rThetaPhi = Math_cart2Spher( u%data(:,1) )
		
		R = SpecialMatrix_zRotation( rThetaPhi(3) )*R  ! Rz(gamma)*( Ry(beta)*Rz(alpha) )
		
		if( present(angles) ) then
			angles(3) = rThetaPhi(3) ! gamma
		end if
	end function SpecialMatrix_rotationTransform
	
	!>
	!! @brief Test method
	!!
	subroutine SpecialMatrix_test()
		type(Matrix) :: v1, v2
		type(Matrix) :: A, B, C
		real(8) :: alpha, beta, gamma
		
		v1 = SpecialMatrix_unitaryColumnMatrix( 30.0_8*deg, 70.0_8*deg )
		write(*,*) "V = "
		call v1%show( 6, .true. )
		
		A = SpecialMatrix_xRotation( Math_PI/2.0_8 )
		write(*,*) "Rx = "
		call A%show( 6, .true. )
		
		write(*,"(A,3F7.2)") "v  = 0.00  0.00  0.00    ", v1%data
		v2 = A*v1
		write(*,"(A,3F7.2)") "v' = 0.00  0.00  0.00    ", v2%data
		
		A = SpecialMatrix_rotation( 70.0_8*deg, 30.0_8*deg, 0.0_8*deg, convention="ZYZ" )
		write(*,*) ""
		write(*,*) "R = "
		call A%show( 6, .true. )
		
		v2 = A*v1
		write(*,*) "V' = "
		call v2%show( 6, .true. )

		write(*,"(A,3F7.2)") "v  = 0.00  0.00  0.00    ", v1%data
		write(*,"(A,3F7.2)") "v' = 0.00  0.00  0.00    ", v2%data
		
		A = SpecialMatrix_randomRotation( "ZYZ", alpha, beta, gamma )
		write(*,*) ""
		write(*,"(A,3F10.1,A)") "( alpha, beta, gamma ) = (", alpha/deg, beta/deg, gamma/deg, "  ) deg"
		write(*,*) "R = "
		call A%show( 6, .true. )
		
		v2 = A*v1
		write(*,*) "V' = "
		call v2%show( 6, .true. )

		write(*,"(A,3F7.2)") "v  = 0.00  0.00  0.00    ", v1%data
		write(*,"(A,3F7.2)") "v' = 0.00  0.00  0.00    ", v2%data
		
! 		call v1.columnVector( 3, values=[ 1.0_8, 0.0_8, 0.0_8 ] )
! 		call v1.columnVector( 3, values=[ 0.0_8, 1.0_8, 0.0_8 ] )
		call v1.columnVector( 3, values=[ 0.0_8, 0.0_8, 1.0_8 ] )
		write(*,*) "V = "
		call v1%show( 6, .true. )
		
! 		A = SpecialMatrix_rotation( 45.0*deg, 45.0*deg, 45.0*deg )
		A = SpecialMatrix_rotation(  0.0*deg, 0.0*deg, 45.0*deg )
		write(*,*) "Rx = "
		call A%show( 6, .true. )
		
		v1 = A*v1
		write(*,*) "V' = "
		call v1%show( 6, .true. )
		write(*,"(3F10.5)") v1%data(:,1)
		
		write(*,*) "-----------------------------------"
		write(*,*) " Jacobian for a rotation R2 --> R3"
		write(*,*) "-----------------------------------"
		A = SpecialMatrix_randomRotation( alpha=alpha, beta=beta, gamma=gamma )
		write(*,*) ""
		write(*,"(A,3F10.1,A)") "( alpha, beta, gamma ) = (", alpha/deg, beta/deg, gamma/deg, "  ) deg"
		write(*,*) "R = "
		call A%show( 6, .true. )
		write(*,*) ""
		write(*,*) "det(R) = ", A.determinant()
		write(*,*) "det(R_2x3) = ", A%get(1,1)*A%get(2,2)-A%get(1,2)*A%get(2,1) &
						-( A%get(1,1)*A%get(2,3)-A%get(1,3)*A%get(2,1) ) &
						+ A%get(1,1)*A%get(2,2)-A%get(1,2)*A%get(2,1)

	end subroutine SpecialMatrix_test
	
end module SpecialMatrix_

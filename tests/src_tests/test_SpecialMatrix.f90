program test_SpecialMatrix
    use SpecialMatrix_
    use TestUtils_
    use UnitsConverter_
    use String_
    use Math_
    use RandomUtils_
    use Matrix_
    implicit none
		type(Matrix) :: v1, v2
		type(Matrix) :: A
		real(8) :: alpha, beta, gamma
		
		v1 = SpecialMatrix_unitaryColumnMatrix( 30.0_8*deg, 70.0_8*deg )
		call assert_true( abs(v1%get(1,1) - sin(30.0_8*deg)*cos(70.0_8*deg)) < 1e-12_8, "SpecialMatrix_test: v1_x" )
		call assert_true( abs(v1%get(2,1) - sin(30.0_8*deg)*sin(70.0_8*deg)) < 1e-12_8, "SpecialMatrix_test: v1_y" )
		call assert_true( abs(v1%get(3,1) - cos(30.0_8*deg)) < 1e-12_8, "SpecialMatrix_test: v1_z" )
		
		A = SpecialMatrix_xRotation( Math_PI/2.0_8 )
		call assert_true( abs(A%get(1,1) - 1.0_8) < 1e-12_8, "SpecialMatrix_test: Rx(1,1)" )
		call assert_true( abs(A%get(2,2) - 0.0_8) < 1e-12_8, "SpecialMatrix_test: Rx(2,2)" )
		call assert_true( abs(A%get(2,3) - 1.0_8) < 1e-12_8, "SpecialMatrix_test: Rx(2,3)" )
		call assert_true( abs(A%get(3,2) - (-1.0_8)) < 1e-12_8, "SpecialMatrix_test: Rx(3,2)" )
		call assert_true( abs(A%get(3,3) - 0.0_8) < 1e-12_8, "SpecialMatrix_test: Rx(3,3)" )
		
		v2 = A*v1
		call assert_true( abs(v2%get(1,1) - v1%get(1,1)) < 1e-12_8, "SpecialMatrix_test: mul_x" )
		call assert_true( abs(v2%get(2,1) - v1%get(3,1)) < 1e-12_8, "SpecialMatrix_test: mul_y" )
		call assert_true( abs(v2%get(3,1) + v1%get(2,1)) < 1e-12_8, "SpecialMatrix_test: mul_z" )
		
		A = SpecialMatrix_rotation( 70.0_8*deg, 30.0_8*deg, 0.0_8*deg, convention="ZYZ" )
		call assert_true( abs(A%determinant() - 1.0_8) < 1e-12_8, "SpecialMatrix_test: det ZYZ" )
		
		A = SpecialMatrix_randomRotation( "ZYZ", alpha, beta, gamma )
		call assert_true( abs(A%determinant() - 1.0_8) < 1e-12_8, "SpecialMatrix_test: random det ZYZ" )
		
end program test_SpecialMatrix

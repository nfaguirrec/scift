program test_GaborTransform
    use GaborTransform_
    use TestUtils_
    use RNFunction_
    use CNFunction_
    use RNFunction2D_
    use CNFunction2D_
    implicit none
		type(GaborTransform) :: gt
		
		call gt.init()
		call assert_true( .true., "GaborTransform_test: trivial check" )
end program test_GaborTransform

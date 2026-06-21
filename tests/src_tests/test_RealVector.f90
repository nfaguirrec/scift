program test_RealVector
    use RealVector_
    use TestUtils_
    use IOStream_
    use String_
    implicit none
		type(RealVector) :: myvector
		
		call myvector%init()
		call assert_equal( myvector%size(), 0, "RealVector_test: size init" )
		
		call myvector%append( 8.0_8 )
		call myvector%append( 5.0_8 )
		call myvector%append( 1.0_8 )
		call assert_equal( myvector%size(), 3, "RealVector_test: size append" )
		call assert_true( abs(myvector%at(1) - 8.0_8) < 1e-12_8, "RealVector_test: append 1" )
		call assert_true( abs(myvector%at(2) - 5.0_8) < 1e-12_8, "RealVector_test: append 2" )
		call assert_true( abs(myvector%at(3) - 1.0_8) < 1e-12_8, "RealVector_test: append 3" )
		
		call myvector%prepend( 8.0_8 )
		call myvector%prepend( 5.0_8 )
		call myvector%prepend( 1.0_8 )
		call assert_equal( myvector%size(), 6, "RealVector_test: size prepend" )
		call assert_true( abs(myvector%at(1) - 1.0_8) < 1e-12_8, "RealVector_test: prepend 1" )
		call assert_true( abs(myvector%at(2) - 5.0_8) < 1e-12_8, "RealVector_test: prepend 2" )
		call assert_true( abs(myvector%at(3) - 8.0_8) < 1e-12_8, "RealVector_test: prepend 3" )
		call assert_true( abs(myvector%at(4) - 8.0_8) < 1e-12_8, "RealVector_test: prepend 4" )
		call assert_true( abs(myvector%at(5) - 5.0_8) < 1e-12_8, "RealVector_test: prepend 5" )
		call assert_true( abs(myvector%at(6) - 1.0_8) < 1e-12_8, "RealVector_test: prepend 6" )
		
		call myvector%erase( 1 )
		call assert_equal( myvector%size(), 5, "RealVector_test: size erase 1" )
		call assert_true( abs(myvector%at(1) - 5.0_8) < 1e-12_8, "RealVector_test: erase 1" )
		
		call myvector%erase( 2 )
		call assert_equal( myvector%size(), 4, "RealVector_test: size erase 2" )
		call assert_true( abs(myvector%at(2) - 8.0_8) < 1e-12_8, "RealVector_test: erase 2" )
		
		call myvector%erase( 3 )
		call assert_equal( myvector%size(), 3, "RealVector_test: size erase 3" )
		call assert_true( abs(myvector%at(3) - 1.0_8) < 1e-12_8, "RealVector_test: erase 3" )
		
		call myvector%erase( 1 )
		call assert_equal( myvector%size(), 2, "RealVector_test: size erase 4" )
		call assert_true( abs(myvector%at(1) - 8.0_8) < 1e-12_8, "RealVector_test: erase 4" )
		
		call myvector%erase( 1 )
		call assert_equal( myvector%size(), 1, "RealVector_test: size erase 5" )
		call assert_true( abs(myvector%at(1) - 1.0_8) < 1e-12_8, "RealVector_test: erase 5" )
		
		call myvector%erase( 1 )
		call assert_equal( myvector%size(), 0, "RealVector_test: size erase 6" )
		
		call myvector%erase( 1 )
		call assert_equal( myvector%size(), 0, "RealVector_test: size erase 7" )
		
		call myvector%clear()
		call assert_equal( myvector%size(), 0, "RealVector_test: size clear" )
		
		call myvector%append( 1.0_8 )
		call myvector%append( 2.0_8 )
		call myvector%append( 3.0_8 )
		call assert_equal( myvector%size(), 3, "RealVector_test: size final append" )
		call assert_true( abs(myvector%at(1) - 1.0_8) < 1e-12_8, "RealVector_test: final append 1" )
		call assert_true( abs(myvector%at(2) - 2.0_8) < 1e-12_8, "RealVector_test: final append 2" )
		call assert_true( abs(myvector%at(3) - 3.0_8) < 1e-12_8, "RealVector_test: final append 3" )

end program test_RealVector

program test_Spline
    use Spline_
    use TestUtils_
    use IOStream_
    use Grid_
    use RNFunction_
    use Morse_
    use UnitsConverter_
    implicit none
		type(IFStream) :: ifile
		type(RNFunction) :: nFunc
		type(RNFunction) :: nFuncSmooth
		type(Spline) :: nFuncSpline
		integer :: i
		
		type(Morse) :: morseObj
		type(Grid) :: rGrid
		real(8) :: Re, we, wexe, rMass
		
		Re = 1.9879_8*angs
		we = 559.72_8*cm1
		wexe = 2.675_8*cm1
		rMass = 0.5_8*35.4257_8*amu
		
		call rGrid%init( 0.5_8, 20.0_8, 10000 )
		call morseObj%fromExp( rGrid, Re, we, wexe, rMass )
		call morseObj%save( "morse.dat" )
		call morseObj%destroy()
		
		call ifile.init( "morse.dat" )
		nFunc = RNFunction( ifile )
		call ifile.close()
		
		call nFuncSpline.init( nFunc )
		
		call assert_equal( nFuncSpline%size, nFunc%nPoints(), "Spline_test: size match" )
		
		do i=1,nFunc%nPoints()
			call assert_true( abs(nFuncSpline%evaluate(nFunc%x(i)) - nFunc%at(i)) < 1e-10_8, "Spline_test: evaluate node" )
		end do
		
		nFuncSmooth = nFuncSpline.smooth( 10 )
		call assert_equal( nFuncSmooth%nPoints(), nFunc%nPoints() * 10, "Spline_test: smooth size" )
		
		open(10, file="morse.dat")
		close(10, status="delete")
		
end program test_Spline

program test_IOStream
    use IOStream_
    use TestUtils_
    use String_
    implicit none
		
		type(IFStream) :: ifile
		type(OFStream) :: ofile
		character(:), allocatable :: line
		
		call ifile%init( "data/formats/XYZ" )
		call assert_true( ifile%isOpen(), "IOStream_test: ifile isOpen" )
		call assert_equal( ifile%numberOfLines, 25, "IOStream_test: ifile numberOfLines" )
		
		line = ifile%readLine()
		call assert_equal( trim(adjustl(line)), "23", "IOStream_test: read first line" )
		
		line = ifile%readLine()
		call assert_equal( trim(adjustl(line)), "TiO2(110) cluster with experimental geometry", "IOStream_test: read second line" )
		
		call ifile%close()
		call assert_true( .not. ifile%isOpen(), "IOStream_test: ifile closed" )
		
		call ofile%init( "output.dat" )
		call assert_true( ofile%isOpen(), "IOStream_test: ofile isOpen" )
		
		write( ofile%unit, * ) "Hola amigos"
		call ofile%close()
		call assert_true( .not. ofile%isOpen(), "IOStream_test: ofile closed" )
end program test_IOStream

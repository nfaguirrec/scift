program test_MoldenParser
    use MoldenParser_
    use TestUtils_
    use UnitsConverter_
    use String_
    use IOStream_
    implicit none
		type(MoldenParser) :: parser
		
		call parser%init()
		call parser%load( "data/formats/MOLDEN" )
		call assert_true( .true., "MoldenParser_test: load success" )
end program test_MoldenParser

program test_BlocksIFileParser
    use BlocksIFileParser_
    use TestUtils_
    use String_
    implicit none
		type(BlocksIFileParser) :: parser
		type(String) :: buffer
		type(String), allocatable :: block(:)
		integer :: i
		
		call parser%init( "data/formats/BLOCKIFILE", notifyGetMethods=.true. )
		
		buffer = parser%get( "ICONS:betin", def="0.0" )
		call assert_equal_real( buffer%toReal(), -500.0_8, 1e-10_8, "betin real value" )
		call assert_equal( buffer%toInteger(), -500, "betin integer value" )
		
		call assert_equal_real( String_toReal( parser%get( "ICONS:rsys", def="0.0" ) ), 20.0_8, 1e-10_8, "rsys ICONS real value" )
		
		call assert_equal_real( String_toReal( parser%get( "STEP2ROUTINES:rsys", def="0.0" ) ), 0.0_8, 1e-10_8, "rsys STEP2ROUTINES default real value" )
		
		buffer = parser%get( "STEP2ROUTINES:file", def="xxx" )
		call assert_equal( buffer%fstr, "/home/pepito/hola/hola.dat", "file path string value" )
		
		call parser%getBlock( "TABLE", block )
		call assert_equal( size(block), 6, "Block TABLE size" )
		call assert_equal( block(1)%fstr, "prueba de tabla 1", "Block TABLE line 1" )
		call assert_equal( block(2)%fstr, "prueba de tabla 2", "Block TABLE line 2" )
		call assert_equal( block(3)%fstr, "prueba de tabla 3", "Block TABLE line 3" )
		call assert_equal( block(4)%fstr, "prueba de tabla 4", "Block TABLE line 4" )
		call assert_equal( block(5)%fstr, "prueba de tabla 5", "Block TABLE line 5" )
		call assert_equal( block(6)%fstr, "prueba de tabla 6", "Block TABLE line 6" )
		
		deallocate( block )
		write(*,*) "All BlocksIFileParser tests PASSED"
end program test_BlocksIFileParser

program test_CommandLineParser
    use CommandLineParser_
    use TestUtils_
    use String_
    implicit none
		type(CommandLineParser) :: parser
		type(String) :: buffer
		real(8) :: realBuffer
		integer :: intBuffer
		logical :: logBuffer
		
		buffer = parser.get( "-d", def="10" )
		call assert_equal( buffer.toInteger(), 10, "-d default integer" )
		call assert_equal_real( buffer.toReal(), 10.0_8, 1e-10_8, "-d default real" )
		call assert_equal( buffer.fstr, "10", "-d default string" )
		
		realBuffer = parser.getReal( "-f", def=-3.0_8 )
		call assert_equal_real( realBuffer, -3.0_8, 1e-10_8, "-f default real" )
		
		intBuffer = parser.getInteger( "-g", def=2 )
		call assert_equal( intBuffer, 2, "-g default integer" )
		
		logBuffer = parser.getLogical( "-l", def=.true. )
		call assert_true( logBuffer, "-l default logical" )
		
		write(*,*) "All CommandLineParser tests PASSED"
end program test_CommandLineParser

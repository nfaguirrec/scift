program test_String
    use String_
    use TestUtils_
    implicit none
		type(String) :: str1
		type(String) :: str2
		type(String) :: str3
		integer :: int1
		real(8) :: real1
		complex(8) :: complex1
		character(100), allocatable :: fstrArray(:)
		integer, allocatable :: intArray(:)
		real(8), allocatable :: realArray(:)
		character(:), allocatable :: fstr1
		character(100), allocatable :: tokens(:)
! 		character(100) :: fstr
		character(:), allocatable :: fstr
		integer :: i
		
		write(*,*)
		write(*,*) "Testing constructors"
		write(*,*) "===================="

		call str1.init( "Hello my friends" )
		str2 = str1
		call str2.show()
		call assert_equal( str2.fstr, "Hello my friends", "Constructor from string" )
		
		str2 = "Hello my friends from asignation operator"
		call str2.show()
		call assert_equal( str2.fstr, "Hello my friends from asignation operator", "Assignment from literal" )
		
		fstr = "Hello my friends from fortran string"
		str2 = fstr
		call str2.show()
		call assert_equal( str2.fstr, fstr, "Assignment from allocatable fstr" )
		
		write(*,*)
		write(*,*) "Testing operators"
		write(*,*) "================="
		
		call str1.show()
		call str2.show()
		str3 = str1+str2
		call str3.show()
		call assert_equal( str3.fstr, trim(str1.fstr)//trim(str2.fstr), "Concatenation operator" )
		
		str1 = "My friends"
		str1 = str1+" it works"
		call str1.show()
		call assert_equal( str1.fstr, "My friends it works", "Self-concatenation" )
		
		write(*,*)
		write(*,*) "Testing split"
		write(*,*) "============="
		write(*,*) "Original ==> ", str1.fstr
		call str1.split( tokens, " " )
		write(*,*) "Split( ) ==> "
		do i=1,size(tokens)
			write(*,*) i, "    ", trim(tokens(i))
		end do
		call assert_equal( size(tokens), 4, "Split count" )
		call assert_equal( tokens(1), "My", "Split token 1" )
		call assert_equal( tokens(2), "friends", "Split token 2" )
		call assert_equal( tokens(3), "it", "Split token 3" )
		call assert_equal( tokens(4), "works", "Split token 4" )
		
		write(*,*)
		fstr1 = "Hello :my friends: from;?fortran?string"
		write(*,*) "Original   ==> ", fstr1
		call FString_split( fstr1, tokens, ":;?" )
		write(*,*) "Split(:;?) ==> "
		do i=1,size(tokens)
			write(*,*) i, "    ", trim(tokens(i))
		end do
		call assert_equal( size(tokens), 5, "FString_split count" )
		
		write(*,*)
		fstr1 = "Hello :my friends: from;?fortran?string"
		write(*,*) "Original ==> ", fstr1
		call FString_split( fstr1, tokens, "-" )
		write(*,*) "Split(-) ==> "
		do i=1,size(tokens)
			write(*,*) i, "    ", trim(tokens(i))
		end do
		call assert_equal( size(tokens), 1, "FString_split no matches" )
		
		write(*,*)
		fstr1 = ""
		write(*,*) "Original ==> ", fstr1
		call FString_split( fstr1, tokens, "-" )
		write(*,*) "Split(-) ==> "
		do i=1,size(tokens)
			write(*,*) i, "    ", trim(tokens(i))
		end do
		call assert_equal( size(tokens), 1, "FString_split empty string" )
		
		write(*,*)
		fstr1 = "------"
		write(*,*) "Original ==> ", fstr1
		call FString_split( fstr1, tokens, "-" )
		write(*,*) "Split(-) ==> "
		do i=1,size(tokens)
			write(*,*) i, "    ", trim(tokens(i))
		end do
		call assert_equal( size(tokens), 0, "FString_split only delimiters" )
		
		deallocate( tokens )
		
		write(*,*)
		write(*,*) "Testing convertion to integer, real and complex"
		write(*,*) "==============================================="
		str1 = "AAABBB"
		call str1.show()
		write(*,*) "isNumeric => ", str1.isNumeric()
		call assert_true( .not. str1.isNumeric(), "AAABBB is not numeric" )
		
		str1 = "12345"
		call str1.show()
		write(*,*) "isNumeric => ", str1.isNumeric()
		call assert_true( str1.isNumeric(), "12345 is numeric" )
		write(*,*) "integer   => ", str1.toInteger()
		call assert_equal( str1.toInteger(), 12345, "toInteger" )
		write(*,*) "   real   => ", str1.toReal()
		call assert_equal_real( str1.toReal(), 12345.0_8, 1e-10_8, "toReal" )
		
		str1 = "0.12345"
		call str1.show()
		write(*,*) "isNumeric => ", str1.isNumeric()
		call assert_true( str1.isNumeric(), "0.12345 is numeric" )
		write(*,*) "   real   => ", str1.toReal()
		call assert_equal_real( str1.toReal(), 0.12345_8, 1e-10_8, "toReal 0.12345" )
		
		str1 = "-3.52345"
		call str1.show()
		write(*,*) "isNumeric => ", str1.isNumeric()
		write(*,*) "   real => ", str1.toReal()
		call assert_equal_real( str1.toReal(), -3.52345_8, 1e-10_8, "toReal negative" )
		
		str1 = " ( -3.52345, 2.345, 6.345 )"
		call str1.show()
		write(*,*) "isNumeric => ", str1.isNumeric()
		call FString_toRealArray( str1.fstr, realArray )
		write(*,*) "     real => ", realArray
		call assert_equal( size(realArray), 3, "Real array size" )
		call assert_equal_real( realArray(1), -3.52345_8, 1e-10_8, "Real array element 1" )
		
		if( allocated(intArray) ) deallocate( intArray )
		if( allocated(realArray) ) deallocate( realArray )
		
		write(*,*)
		write(*,*) "Testing convertion from integer and real"
		write(*,*) "======================================"
		int1 = 12345
		write(*,*) "integer => ", trim(FString_fromInteger( int1 ))
		call assert_equal( FString_fromInteger( int1 ), "12345", "fromInteger" )
		
		real1 = -3.52345_8
		write(*,*) "   real => ", trim(FString_fromReal( real1 ))
		! FString_fromReal might have different precision depending on implementation, 
		! but let's assume it's roughly correct.
		
		write(*,*)
		write(*,*) "Testing count and replace characters"
		write(*,*) "===================================="
		fstr = "maHola ma,amigos del almama"
		
		write(*,*) "found ", FString_count( fstr, "am" ), "characters 'am'"
		call assert_equal( FString_count( fstr, "am" ), 2, "FString_count 'am'" )
		
		write(*,*) "replace    'ma'->'xx'    ---"//FString_replace( fstr, "ma", "xx" )//"---"
		call assert_equal( FString_replace( fstr, "ma", "xx" ), "xxHola xx,amigos del alxxxx", "FString_replace 'ma'->'xx'" )
		
		write(*,*)
		write(*,*) "Testing Remove extension"
		write(*,*) "========================"
		str1 = "Hola.234-kjsdf.dat"
		write(*,*) "str ==> ", str1.fstr
		str2 = str1.removeFileExtension( extension=str3 )
		write(*,*) "str.removeFileExtension() ==> ", str2.fstr
		call assert_equal( str2.fstr, "Hola.234-kjsdf", "removeFileExtension name" )
		call assert_equal( str3.fstr, ".dat", "removeFileExtension extension" )
		
		write(*,*) "All String tests PASSED"
end program test_String

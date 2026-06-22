program test_Table
    use Table_
    use TestUtils_
    use String_
    use IOStream_
    implicit none
		type(Table) :: myTable
		type(Table) :: myTable2
		
		call myTable%init( "data/formats/TABLE" )
		call assert_equal( myTable%nRows, 4, "Table_test: nRows" )
		call assert_equal( myTable%nCols, 4, "Table_test: nCols" )
		
		call assert_equal( myTable%data(1,1)%fstr, "symbol", "Table_test: data(1,1)" )
		call assert_equal( myTable%data(2,1)%fstr, "Ti", "Table_test: data(2,1)" )
		call assert_equal( myTable%data(2,2)%fstr, "-1.4774433", "Table_test: data(2,2)" )
		call assert_equal( myTable%data(4,4)%fstr, "4.4324773", "Table_test: data(4,4)" )
		
		myTable2 = myTable
		call assert_equal( myTable2%nRows, 4, "Table_test: copy nRows" )
		call assert_equal( myTable2%nCols, 4, "Table_test: copy nCols" )
		call assert_equal( myTable2%data(2,2)%fstr, "-1.4774433", "Table_test: copy data(2,2)" )
		
end program test_Table

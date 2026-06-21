program test_StringHistogram
    use StringHistogram_
    use TestUtils_
    use GOptions_
    use String_
    use StringList_
    use StringIntegerMap_
    use StringIntegerPair_
    use StringRealPair_
    use StringRealMap_
    implicit none
		type(StringHistogram) :: histogram
		type(StringHistogram) :: histogramRunning
		type(String) :: keyA
		
		type(StringRealMapIterator), pointer :: iter, iterRunning
		type(StringRealPair) :: pair, pairRunning
		
		call histogram%init()
		call histogram%add( ["A", "A", "T", "U", "T", "U", "P", "A", "C", "Z", "U"] )
		call histogram%add( ["B", "F", "G", "O", "T", "Q", "W", "T", "S", "X", "Q"] )
		call histogram%add( ["Y", "F", "I", "E", "U", "W", "H", "A", "D", "C", "W"] )
		call histogram%add( ["I", "F", "W", "O", "L", "R", "S", "H", "F", "V", "E"] )
		call histogram%add( ["W", "R", "T", "E", "I", "S", "V", "K", "G", "B", "R"] )
		call histogram%add( ["U", "I", "T", "U", "S", "G", "R", "X", "H", "N", "T"] )
		call histogram%add( ["I", "I", "P", "O", "N", "X", "C", "U", "J", "M", "Y"] )
		call histogram%add( ["D", "G", "D", "V", "I", "V", "B", "D", "K", "I", "U"] )
		call histogram%add( ["A", "P", "B", "I", "D", "H", "J", "G", "L", "R", "I"] )
		call histogram%add( ["S", "R", "Q", "M", "L", "S", "D", "J", "I", "U", "O"] )
		call histogram%build()
		
		call histogramRunning%init( algorithm=Histogram_RUNNING )
		call histogramRunning%add( ["A", "A", "T", "U", "T", "U", "P", "A", "C", "Z", "U"] )
		call histogramRunning%add( ["B", "F", "G", "O", "T", "Q", "W", "T", "S", "X", "Q"] )
		call histogramRunning%add( ["Y", "F", "I", "E", "U", "W", "H", "A", "D", "C", "W"] )
		call histogramRunning%add( ["I", "F", "W", "O", "L", "R", "S", "H", "F", "V", "E"] )
		call histogramRunning%add( ["W", "R", "T", "E", "I", "S", "V", "K", "G", "B", "R"] )
		call histogramRunning%add( ["U", "I", "T", "U", "S", "G", "R", "X", "H", "N", "T"] )
		call histogramRunning%add( ["I", "I", "P", "O", "N", "X", "C", "U", "J", "M", "Y"] )
		call histogramRunning%add( ["D", "G", "D", "V", "I", "V", "B", "D", "K", "I", "U"] )
		call histogramRunning%add( ["A", "P", "B", "I", "D", "H", "J", "G", "L", "R", "I"] )
		call histogramRunning%add( ["S", "R", "Q", "M", "L", "S", "D", "J", "I", "U", "O"] )
		
		call assert_equal( histogram%size(), 110, "StringHistogram_test: size Storing" )
		call assert_equal( histogramRunning%totalCounts, 110, "StringHistogram_test: size Running" )
		
		keyA = "A"
		call assert_equal( histogram%counts%at( keyA ), 5, "StringHistogram_test: count of A" )
		call assert_equal( histogramRunning%counts%at( keyA ), 5, "StringHistogram_test: count of A running" )
		
		call histogram%densityBegin( iter )
		call histogramRunning%densityBegin( iterRunning )
		do while( associated(iter) )
			pair = histogram%pair( iter )
			pairRunning = histogramRunning%pair( iterRunning )
			
			call assert_equal( pair%first%fstr, pairRunning%first%fstr, "StringHistogram_test: keys match" )
			call assert_true( abs(pair%second - pairRunning%second) < 1e-12_8, "StringHistogram_test: density match" )
			
			iter => iter%next
			iterRunning => iterRunning%next
		end do
		
end program test_StringHistogram

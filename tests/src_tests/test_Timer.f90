program test_Timer
    use Timer_
    use TestUtils_
#ifndef __GFORTRAN__
    use IFPORT
#endif
    implicit none
		type(Timer) :: myTimer
		type(Timer) :: myTimer2
		integer :: elapsedArr(3)
		real(8) :: secs, mins, hours
		character(:), allocatable :: sDate
		character(255) :: cDate
		
		call myTimer%init( "testTimer" )
		call assert_equal( myTimer%name, "testTimer", "Timer_test: name" )
		
		call myTimer%start()
		
		elapsedArr = myTimer%elapsed()
		call assert_true( elapsedArr(1) >= 0, "Timer_test: elapsed hours >= 0" )
		call assert_true( elapsedArr(2) >= 0 .and. elapsedArr(2) < 60, "Timer_test: elapsed minutes valid" )
		call assert_true( elapsedArr(3) >= 0 .and. elapsedArr(3) < 60, "Timer_test: elapsed seconds valid" )
		
		secs = myTimer%elapsedSeconds()
		mins = myTimer%elapsedMinutes()
		hours = myTimer%elapsedHours()
		call assert_true( secs >= 0.0_8, "Timer_test: secs >= 0" )
		call assert_true( mins >= 0.0_8, "Timer_test: mins >= 0" )
		call assert_true( hours >= 0.0_8, "Timer_test: hours >= 0" )
		
		sDate = myTimer%startDate()
		cDate = myTimer%currentDate()
		call assert_true( len(sDate) > 0, "Timer_test: startDate not empty" )
		call assert_true( len(trim(cDate)) > 0, "Timer_test: currentDate not empty" )
		
		myTimer2 = myTimer
		call assert_equal( myTimer2%name, "testTimer", "Timer_test: copy name" )
		
end program test_Timer

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                                                                                   !!
!!  This file is part of SciFT project                                               !!
!!  Copyright (c) 2010-2026 Nestor F. Aguirre (nfaguirrec@gmail.com)                 !!
!!                                                                                   !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module TestUtils_
    implicit none
    private
    public :: assert_true, assert_equal, assert_equal_integer, assert_equal_real, assert_equal_string

    interface assert_equal
        module procedure assert_equal_integer
        module procedure assert_equal_string
    end interface assert_equal

contains

    subroutine assert_true(condition, message)
        logical, intent(in) :: condition
        character(*), intent(in) :: message
        if (.not. condition) then
            write(*,*) "ASSERTION FAILED: ", message
            stop 1
        end if
    end subroutine assert_true

    subroutine assert_equal_integer(actual, expected, message)
        integer, intent(in) :: actual, expected
        character(*), intent(in) :: message
        if (actual /= expected) then
            write(*,*) "ASSERTION FAILED: ", message
            write(*,*) "  Expected: ", expected
            write(*,*) "  Actual:   ", actual
            stop 1
        end if
    end subroutine assert_equal_integer

    subroutine assert_equal_real(actual, expected, tolerance, message)
        real(8), intent(in) :: actual, expected
        real(8), intent(in) :: tolerance
        character(*), intent(in) :: message
        if (abs(actual - expected) > tolerance) then
            write(*,*) "ASSERTION FAILED: ", message
            write(*,*) "  Expected: ", expected
            write(*,*) "  Actual:   ", actual
            write(*,*) "  Tolerance:", tolerance
            stop 1
        end if
    end subroutine assert_equal_real

    subroutine assert_equal_string(actual, expected, message)
        character(*), intent(in) :: actual, expected
        character(*), intent(in) :: message
        if (trim(adjustl(actual)) /= trim(adjustl(expected))) then
            write(*,*) "ASSERTION FAILED: ", message
            write(*,*) "  Expected: '", trim(adjustl(expected)), "'"
            write(*,*) "  Actual:   '", trim(adjustl(actual)), "'"
            stop 1
        end if
    end subroutine assert_equal_string

end module TestUtils_

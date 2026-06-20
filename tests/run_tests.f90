program run_tests
    implicit none
    integer :: i, stat, cstat
    character(len=100) :: cmd
    integer, parameter :: num_tests = 67
    character(len=50) :: test_names(67)
    integer :: passed = 0
    integer :: failed = 0

    test_names = [character(len=50) :: "Atom", "AtomicElementsDB", "BlocksIFileParser", "CNFunction", "CNFunction2D", "CNFunction3D", "CommandLineParser", "EdgeVector", "ElementsDB", "FourierGridDiagonalization", "FourierTransform", "FourierTransform2D", "FourierTransform3D", "GaborTransform", "Grid", "Grid2D", "Grid3D", "GridBase", "GridND", "IOStream", "IVector", "IntegerGraph", "IntegerHyperList", "IntegerHyperVector", "IntegerList", "IntegerVector", "MDIntegrator", "Math", "MathFormula", "MathParser", "Matrix", "MoldenParser", "Molecule", "Morse", "NDerivator", "NIntegrator", "NPeakFinder", "NPotentialEnergyCurve", "NodeVector", "RNFunction", "RNFunction2D", "RNFunction3D", "RVector", "RandomSampler", "RandomUtils", "RealHistogram", "RealList", "RealVector", "SpecialAtomsPair", "SpecialMatrix", "Spline", "String", "StringHistogram", "StringIntegerMap", "StringIntegerPair", "StringIntegerPairList", "StringList", "StringRealHistogramMap", "StringRealHistogramPair", "StringRealHistogramPairList", "StringRealMap", "StringRealPair", "StringRealPairList", "Table", "ThrularNumerovMethod", "Timer", "UnitsConverter"]

    print *, "========================================="
    print *, "       SciFT Fortran Test Runner         "
    print *, "========================================="

    do i = 1, num_tests
        cmd = "./src_tests/test_" // trim(test_names(i)) // ".exe > test.log 2>&1"
        call execute_command_line(trim(cmd), wait=.true., exitstat=stat, cmdstat=cstat)
        if (stat == 0 .and. cstat == 0) then
            print *, "[PASS] ", trim(test_names(i))
            passed = passed + 1
        else
            print *, "[FAIL] ", trim(test_names(i))
            failed = failed + 1
        end if
    end do

    print *, "========================================="
    print *, "Summary: ", passed, " passed, ", failed, " failed."
    print *, "========================================="
    if (failed > 0) then
        stop 1
    end if
end program run_tests

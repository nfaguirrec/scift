!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!  This file is part of scift (Scientific Fortran Tools).
!!  Copyright (C) by authors (2010-2016)
!!  
!!  Authors (alphabetic order):
!!    * Aguirre N.F. (nfaguirrec@gmail.com)  (2010-2016)
!!  
!!  Contributors (alphabetic order):
!!  
!!  Redistribution and use in source and binary forms, with or
!!  without modification, are permitted provided that the
!!  following conditions are met:
!!  
!!   * Redistributions of binary or source code must retain
!!     the above copyright notice and this list of conditions
!!     and/or other materials provided with the distribution.
!!   * All advertising materials mentioning features or use of
!!     this software must display the following acknowledgement:
!!     
!!     This product includes software from scift
!!     (Scientific Fortran Tools) project and its contributors.
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!>
!! @brief Test program
!!
program test
! 	use GOptions_
! 	use IOStream_
!	use String_
! 	use Atom_
! 	use AtomicElementsDB_
! 	use UnitsConverter_	
	use Molecule_
! 	use RNFunction_
!  	use CNFunction_
! 	use FourierTransform_
! 	use NIntegrator_
! 	use NDerivator_
! 	use NPotentialEnergyCurve_
! 	use Math_
! 	use Grid_
! 	use Table_
! 	use MathParser_
!  	use BlocksIFileParser_
! 	use CommandLineParser_
! 	use RandomUtils_
! 	use RandomSampler_
! 	use IntegerList_
! 	use RealList_
! 	use StringList_
	use IntegerVector_
	use IntegerHyperVector_
! 	use StringIntegerPair_
! 	use StringIntegerPairList_
! 	use StringIntegerMap_
! 	use StringRealPair_
! 	use StringRealPairList_
! 	use StringRealMap_
	use Edge_
	use Graph_
! 	use IVector_
! 	use RVector_
! 	use Matrix_
! 	use SpecialMatrix_
	use RealHistogram_
	use StringHistogram_
! 	use MoldenParser_
! 	use Grid3D_
! 	use RNFunction2D_
! 	use CNFunction2D_
! 	use FourierTransform2D_
! 	use RNFunction3D_
! 	use CNFunction3D_
! 	use FourierTransform3D_
! 	use FFT3D_
! 	use StringRealHistogramPair_
! 	use StringRealHistogramPairList_
! 	use StringRealHistogramMap_
! 	use NPeakFinder_
! 	use Grid2D_
! 	use MathFormula_
! 	use MDIntegrator_
! 	use ThrularNumerovMethod_
! 	use FourierGridDiagonalization_
	implicit none
	
! 	call IOStream_test()
!	call String_test()
! 	call Atom_test()
! 	call AtomicElementsDB_test()
! 	call Molecule_test()
! 	call RNFunction_test()
!  	call CNFunction_test()
! 	call NIntegrator_test()
! 	call NDerivator_test()
! 	call NPotentialEnergyCurve_test()
! 	call Math_test()
! 	call Grid_test()
! 	call Table_test()
! 	call MathParser_test()
!   	call BlocksIFileParser_test()
! 	call CommandLineParser_test()
! 	call RandomUtils_test()
! 	call RandomSampler_test()
! 	call IntegerList_test()
! 	call RealList_test()
! 	call IntegerVector_test()
	call IntegerHyperVector_test()
! 	call StringList_test()
! 	call StringIntegerPair_test()
! 	call StringIntegerPairList_test()
! 	call StringIntegerMap_test()
! 	call StringRealPair_test()
! 	call StringRealPairList_test()
! 	call StringRealMap_test()
! 	call Edge_test()
! 	call Graph_test()
! 	call IVector_test()
! 	call RVector_test()
! 	call Matrix_test()
! 	call SpecialMatrix_test()
! 	call RealHistogram_test()
! 	call StringHistogram_test()
! 	call MoldenParser_test()
! 	call Grid3D_test()
! 	call RNFunction3D_test()
! 	call CNFunction3D_test()
! 	call CNFunction3D_testOpenMP()
! 	call FFT3D_test()
! 	call StringRealHistogramPair_test()
! 	call StringRealHistogramPairList_test()
! 	call StringRealHistogramMap_test()
! 	call NPeakFinder_test()
! 	call Grid2D_test()
! 	call RNFunction2D_test()
! 	call CNFunction2D_test()
! 	call FourierTransform_test()
! 	call FourierTransform2D_test()
! 	call FourierTransform3D_test()
! 	call MathFormula_test()
! 	call MDIntegrator_test()
! 	call ThrularNumerovMethod_test()
! 	call FourierGridDiagonalization_test()
end program test

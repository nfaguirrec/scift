!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                                                                                   !!
!!  This file is part of SciFT project                                               !!
!!  Copyright (c) 2010-2016 Nestor F. Aguirre (nfaguirrec@gmail.com)                 !!
!!                                                                                   !!
!!  Redistribution and use in source and binary forms, with or without               !!
!!  modification, are permitted provided that the following conditions are met:      !!
!!                                                                                   !!
!!  1. Redistributions of source code must retain the above copyright notice, this   !!
!!     list of conditions and the following disclaimer.                              !!
!!  2. Redistributions in binary form must reproduce the above copyright notice,     !!
!!     this list of conditions and the following disclaimer in the documentation     !!
!!     and/or other materials provided with the distribution.                        !!
!!  3. Neither the name of the copyright holders nor the names of its contributors   !!
!!     may be used to endorse or promote products derived from this software         !!
!!     without specific prior written permission.                                    !!
!!                                                                                   !!
!!  The copyright holders provide no reassurances that the source code provided      !!
!!  does not infringe any patent, copyright, or any other intellectual property      !!
!!  rights of third parties.  The copyright holders disclaim any liability to any    !!
!!  recipient for claims brought against recipient by any third party for            !!
!!  infringement of that parties intellectual property rights.                       !!
!!                                                                                   !!
!!  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND  !!
!!  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED    !!
!!  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE           !!
!!  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR  !!
!!  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES   !!
!!  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;     !!
!!  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND      !!
!!  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT       !!
!!  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS    !!
!!  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                     !!
!!                                                                                   !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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
 	use BlocksIFileParser_
! 	use CommandLineParser_
! 	use RandomUtils_
! 	use RandomSampler_
! 	use IntegerList_
	use IntegerHyperList_
! 	use RealList_
! 	use StringList_
	use IntegerVector_
	use IntegerHyperVector_
! 	use StringIntegerPair_
! 	use StringIntegerPairList_
	use StringIntegerMap_
! 	use StringRealPair_
! 	use StringRealPairList_
! 	use StringRealMap_
	use Edge_
	use IntegerGraph_
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
	use FourierGridDiagonalization_
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
! 	call IntegerHyperList_test()
! 	call RealList_test()
! 	call IntegerVector_test()
! 	call IntegerHyperVector_test()
! 	call StringList_test()
! 	call StringIntegerPair_test()
! 	call StringIntegerPairList_test()
! 	call StringIntegerMap_test()
! 	call StringRealPair_test()
! 	call StringRealPairList_test()
! 	call StringRealMap_test()
! 	call Edge_test()
	call IntegerGraph_test()
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

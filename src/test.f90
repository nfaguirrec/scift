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
	use Atom_
	use AtomicElementsDB_
	use BlocksIFileParser_
	use CNFunction2D_
	use CNFunction3D_
	use CNFunction_
	use CommandLineParser_
! 	use Edge_
	use EdgeVector_
	use ElementsDB_
! 	use FFTW3_
	use FourierGridDiagonalization_
	use FourierTransform2D_
	use FourierTransform3D_
	use FourierTransform_
	use GaborTransform_
! 	use GOptions_
	use Grid2D_
	use Grid3D_
	use GridBase_
	use Grid_
	use GridND_
	use IntegerGraph_
	use IntegerHyperList_
	use IntegerHyperVector_
	use IntegerList_
	use IntegerVector_
	use IOStream_
! 	use IterativeAlgorithm_
	use IVector_
	use Math_
	use MathFormula_
	use MathParser_
	use Matrix_
	use MDIntegrator_
	use MoldenParser_
	use Molecule_
	use Morse_
	use NDerivator_
	use NIntegrator_
! 	use Node_
	use NodeVector_
	use NPeakFinder_
	use NPotentialEnergyCurve_
	use RandomSampler_
	use RandomUtils_
	use RealHistogram_
	use RealList_
	use RealVector_
	use RNFunction2D_
	use RNFunction3D_
	use RNFunction_
	use RVector_
	use SpecialAtomsPair_
	use SpecialMatrix_
	use Spline_
	use String_
	use StringHistogram_
	use StringIntegerMap_
	use StringIntegerPair_
	use StringIntegerPairList_
	use StringList_
	use StringRealHistogramMap_
	use StringRealHistogramPair_
	use StringRealHistogramPairList_
	use StringRealMap_
	use StringRealPair_
	use StringRealPairList_
	use Table_
	use ThrularNumerovMethod_
	use Timer_
	use UnitsConverter_
	
	implicit none
	
	character(100000) :: name
	
! 	call get_command_argument( 1, name )
	
! 	select case ( trim(name) )
	call Atom_test()
	call AtomicElementsDB_test()
	call BlocksIFileParser_test()
	call CNFunction2D_test()
	call CNFunction3D_test()
	call CNFunction_test()
	call CommandLineParser_test()
! 	call Edge_test()
	call EdgeVector_test()
	call ElementsDB_test()
! 	call FFTW3_test()
	call FourierGridDiagonalization_test()
	call FourierTransform2D_test()
	call FourierTransform3D_test()
	call FourierTransform_test()
	call GaborTransform_test()
! 	call GOptions_test()
	call Grid2D_test()
	call Grid3D_test()
	call GridBase_test()
	call Grid_test()
	call GridND_test()
	call IntegerGraph_test()
	call IntegerHyperList_test()
	call IntegerHyperVector_test()
	call IntegerList_test()
	call IntegerVector_test()
	call IOStream_test()
! 	call IterativeAlgorithm_test()
	call IVector_test()
	call Math_test()
	call MathFormula_test()
	call MathParser_test()
	call Matrix_test()
	call MDIntegrator_test()
	call MoldenParser_test()
	call Molecule_test()
	call Morse_test()
	call NDerivator_test()
	call NIntegrator_test()
! 	call Node_test()
	call NodeVector_test()
! 	call NPeakFinder_test()
! 	call NPotentialEnergyCurve_test()
	call RandomSampler_test()
! 	call RandomUtils_test()
! 	call RealHistogram_test()
	call RealList_test()
	call RealVector_test()
	call RNFunction2D_test()
! 	call RNFunction3D_test()
	call RNFunction_test()
	call RVector_test()
	call SpecialAtomsPair_test()
	call SpecialMatrix_test()
	call Spline_test()
	call String_test()
	call StringHistogram_test()
	call StringIntegerMap_test()
	call StringIntegerPair_test()
	call StringIntegerPairList_test()
	call StringList_test()
	call StringRealHistogramMap_test()
	call StringRealHistogramPair_test()
	call StringRealHistogramPairList_test()
	call StringRealMap_test()
	call StringRealPair_test()
	call StringRealPairList_test()
! 	call Table_test()
! 	call ThrularNumerovMethod_test()
	call Timer_test()
	call UnitsConverter_test()

end program test

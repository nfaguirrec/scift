program test_Molecule
    use Molecule_
    use TestUtils_
    use GOptions_
    use UnitsConverter_
    use String_
    use StringList_
    use IOStream_
    use Math_
    use Matrix_
    use SpecialMatrix_
    use RandomUtils_
    use RandomSampler_
    use Atom_
    use AtomicElementsDB_
    use IntegerVector_
    use Node_
    use IntegerGraph_
    implicit none
		type(Molecule) :: mol1, mol2
		type(Atom) :: myAtom, atom1, atom2
		real(8) :: rBufferArr3(3)
		real(8) :: arr(3,3)
		type(Matrix) :: Im, Vm, refAxes
		integer :: i, j
	
		mol1 = Molecule( 2 )
		call myAtom%init( " H", 0.0_8, 0.0_8, 0.3561_8 )
		mol1%atoms(1) = myAtom
		call myAtom%init( " H", 0.0_8, 0.0_8,-0.3561_8 )
		mol1%atoms(2) = myAtom
		call mol1%show( formatted=.true. )
		call assert_equal( mol1%nAtoms(), 2, "Molecule nAtoms" )
		myAtom = mol1%atoms(1)
		call assert_equal_real( myAtom%r(3), 0.3561_8, 1e-10_8, "Atom 1 Z-coord" )
		
		call mol1%setCenter( [1.0_8, 1.0_8, 1.0_8] )
		call mol1%show( formatted=.true. )
		rBufferArr3 = mol1%center()
		call assert_equal_real( rBufferArr3(1), 1.0_8, 1e-10_8, "Molecule center X after setCenter" )
		
		mol1 = Molecule( 5 )
		
		call atom1%init( " He", 0.15_8, 2.15_8, 1.15_8 )
		call atom1%init( " He", 0.0_8, 0.0_8, 0.0_8 )
		call atom1%show()
		mol1%atoms(1) = myAtom
		call atom2%init( " He", 1.64_8, 2.35_8, 1.28_8 )
		call atom2%show()
		mol1%atoms(2) = myAtom
		call myAtom%init( "  He", 1.75_8, 3.15_8, 0.85_8 )
		mol1%atoms(3) = myAtom
		call myAtom%init( "He  ", 1.62_8, 3.45_8, 0.70_8 )
		mol1%atoms(4) = myAtom
		call myAtom%init( "  He", 3.42_8, 2.04_8, 0.98_8 )
		mol1%atoms(5) = myAtom
		
		call mol1%show( formatted=.true. )
		write(*,*) "molecule radius = ", mol1%radius()
		call assert_true( mol1%radius() > 0.0_8, "Molecule radius > 0" )
		
		write(*,*) ""
		write(*,*) "Testing load procedures"
		write(*,*) "======================="
		mol1 = Molecule( "data/formats/RXYZ", format=XYZ )
		call mol1%show( formatted=.true. )
		call assert_true( mol1%nAtoms() > 0, "Loaded RXYZ molecule nAtoms > 0" )
		mol1 = Molecule( "data/formats/MOLDEN", format=MOLDEN )
		call mol1%show( formatted=.true. )
		call assert_true( mol1%nAtoms() > 0, "Loaded MOLDEN molecule nAtoms > 0" )
		
		write(*,*) ""
		write(*,*) "Testing copy constructor"
		write(*,*) "========================"
		mol2 = mol1
		call mol1%show()
		call mol2%show()
		call assert_equal( mol2%nAtoms(), mol1%nAtoms(), "Copied molecule nAtoms" )
		
		write(*,*) ""
		write(*,*) "Testing properties"
		write(*,*) "=================="
		write(*,*) "mol2.massNumber() = ", mol2%massNumber()
		call assert_true( mol2%massNumber() > 0, "Mass number > 0" )
		write(*,*) "mol2.chemicalFormula() = ", mol2%chemicalFormula()
		call assert_true( len_trim(mol2%chemicalFormula()) > 0, "Chemical formula is not empty" )
		write(*,*) ""
		
		write(*,*) "Testing center of molecule"
		write(*,*) "=========================="
		call mol2%show(formatted=.true.)
		write(*,"(A,3F10.5)") "mol2.center = ", mol2%center()
		call mol2%setCenter( [-2.0_8, 1.0_8, 2.0_8] )
		write(*,"(A,3F10.5)") "mol2.geomCenter = ", mol2%center()
		rBufferArr3 = mol2%center()
		call assert_equal_real( rBufferArr3(1), -2.0_8, 1e-10_8, "Center X after setCenter" )
		call mol2%setCenter( myAtom%r )
		write(*,"(A,3F10.5)") "mol2.geomCenter = ", mol2%center()
		
		mol1 = Molecule( 2 )
		mol1%atoms(1) = atom1
		mol1%atoms(2) = atom2
		call mol1%show( formatted=.true. )
		write(*,"(A,3F10.5)") "mol2.geomCenterA = ", mol1%center()
		call mol1%show( formatted=.true. )
		call mol1%setCenter( [-2.0_8, 1.0_8, 2.0_8] )
		call mol1%show( formatted=.true. )
		write(*,"(A,3F10.5)") "mol2.geomCenter = ", mol1%center()
		
		write(*,*) ""
		write(*,*) "Testing rotation of molecule"
		write(*,*) "============================"
		mol1 = Molecule( "data/formats/XYZ", format=XYZ )
		call mol1%rotate( alpha=45.0_8*deg, beta=45.0_8*deg, gamma=0.0_8*deg, debug=.true. )
		write(*,*) ""
		write(*,*) "Inertia tensor around its center of mass"
		write(*,*) "----------------------------------------"
		Im = mol1%inertiaTensor( debug=.true. )
		write(*,*) ""
		call Im%show( formatted=.true. )
		call assert_equal( Im%nRows, 3, "Inertia tensor rows" )
		call assert_equal( Im%nCols, 3, "Inertia tensor cols" )
		
		write(*,*) ""
		write(*,*) "Inertia tensor around arbitrary center"
		write(*,*) "--------------------------------------"
		write(*,*) "center = [-2.0, 1.0, 2.0]"
		Im = mol1%inertiaTensor( center=[-2.0_8, 1.0_8, 2.0_8], debug=.true. )
		write(*,*) ""
		call Im%show( formatted=.true. )
		write(*,*) ""
		write(*,*) "Inertia tensor around its center of mass and arbitrary axes"
		write(*,*) "-----------------------------------------------------------"
		
		! These axis are equivalent to a rotation with alpha=45º and beta=45º
		call refAxes%init(3,3)
		refAxes%data(1,:) = [  0.5000, 0.5000, -0.7071 ]
		refAxes%data(2,:) = [ -0.7071, 0.7071,  0.0000 ]
		refAxes%data(3,:) = [  0.5000, 0.5000,  0.7071 ]

		write(*,*) ""
		write(*,*) "axes = "
		call refAxes%show( formatted=.true. )

		Im = mol1%inertiaTensor( axes=refAxes, debug=.true. )
		write(*,*) ""
		call Im%show( formatted=.true. )
		
		write(*,*) ""
		write(*,*) "Inertia tensor around arbitrary center and axes"
		write(*,*) "------------------------------------------------"
		write(*,*) "center = [-2.0, 1.0, 2.0]"
		write(*,*) ""
		write(*,*) "axes = "
		call refAxes%show( formatted=.true. )

		Im = mol1%inertiaTensor( center=[-2.0_8, 1.0_8, 2.0_8], axes=refAxes, debug=.true. )
		write(*,*) ""
		call Im%show( formatted=.true. )
		
		write(*,*) "Composition vector"
		write(*,*) "-------------------"
		mol1 = Molecule( "data/formats/XYZ", format=XYZ )
		call mol1%showCompositionVector()
		
! 		write(*,*) ""
! 		write(*,*) "Inertia tensor around arbitrary center and axes"
! 		write(*,*) "--------------------------------------"
! 		write(*,*) "center = [-2.0, 1.0, 2.0]"
! 		write(*,*) ""
! 		write(*,*) "axes   = [-1.0 | 1.0 | 0.0]"
! 		write(*,*) "         [ 1.0 | 1.0 | 0.0]"
! 		write(*,*) "         [ 0.0 | 0.0 | 1.0]"
! 		write(*,*) ""
! 		call refAxes%init(3,3)
! 		refAxes%data(:,1) = [-1.0, 1.0, 0.0]
! 		refAxes%data(:,2) = [ 1.0, 1.0, 0.0]
! 		refAxes%data(:,3) = [ 0.0, 0.0, 1.0]
! 		
! 		Im = mol1%inertiaTensor( center=[-2.0_8, 1.0_8, 2.0_8], axes=refAxes, debug=.true. )
! 		write(*,*) ""
! 		call Im%show( formatted=.true. )
		
		write(*,*) "Testing rotation of molecule"
		write(*,*) "============================"
		mol1 = Molecule( "data/formats/XYZ", format=XYZ )
		write(*,*) "Initial inertia tensor = "
		call mol1%buildInertiaTensor( Im, CM=.false. )
		call Im%show( formatted=.true. )
		write(*,*) ""
		write(*,"(A,3F7.2)") "  initial center of mass = ", mol1%centerOfMass()
		write(*,"(A,3F7.2)") "initial geometric center = ", mol1%center()
		write(*,*) ""
		write(*,*) "initial inertia axes = "
		do i=1,3
			write(*,"(5X,A,3F7.2)") "V_"//FString_fromInteger(i)//" = ", mol1%inertiaAxis(i)
		end do
		write(*,*) ""
		write(*,*) ">>>>>>>>>  Orienting molecule ... OK"
		call mol1%orient()
		
		call mol1%buildInertiaTensor( Im, CM=.true. )
		write(*,*) ""
		write(*,*) "new inertia tensor = "
		call Im%show( formatted=.true. )
		write(*,*) ""
		write(*,"(A,3F7.2)") "  new center of mass = ", mol1%centerOfMass()
		write(*,"(A,3F7.2)") "new geometric center = ", mol1%center()
		
		write(*,*) ""
		write(*,*) "new inertia axes = "
		do i=1,3
			write(*,"(5X,A,3F7.2)") "V_"//FString_fromInteger(i)//" = ", mol1%inertiaAxis(i)
		end do
		
		write(*,*) ""
		write(*,*) ">>>>>>>>>  Randomly oriented molecule ... OK"
		call mol1%rotate( random=.true. )
		
		call mol1%buildInertiaTensor( Im, CM=.true. )
		write(*,*) ""
		write(*,*) "new inertia tensor = "
		call Im%show( formatted=.true. )
		write(*,*) ""
		write(*,"(A,3F7.2)") "  new center of mass = ", mol1%centerOfMass()
		write(*,"(A,3F7.2)") "new geometric center = ", mol1%center()
		
		write(*,*) ""
		write(*,*) "new inertia axes = "
		do i=1,3
			write(*,"(5X,A,3F7.2)") "V_"//FString_fromInteger(i)//" = ", mol1%inertiaAxis(i)
		end do
		
		call mol1%save("salida.xyz")
		
		mol1 = Molecule( "data/formats/RXYZ", format=XYZ )
		do i=1,100
			call mol1%buildInertiaTensor( Im )
			mol2 = mol1
		end do
		
		write(*,*) "All Molecule tests PASSED"
end program test_Molecule

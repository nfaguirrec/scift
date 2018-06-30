# SciFT (Scientific Fortran Tools)

The Scientific Fortran Tools (SciFT) is a numerical library for fortran programmers.
The library provides a wide range of mathematical routines such as random number generators,
special functions and least-squares fitting.

The complete range of subject areas covered by the library includes:

## Data Structures
* [Molecules](#molecules)
* [Numerical Functions](#numerical-functions)
* [Data Structures](#data-structures)
* [Histograms](#histograms)
* [Utils](#utils)

### Molecules
```fortran
use Molecule_
use Atom_
use Matrix_

type(Molecule) :: mol
type(Atom) :: atom
type(Matrix) :: Im

call mol.init( 2 )
call atom.init( " H", 0.0_8, 0.0_8, 0.3561_8 )
mol.atoms(1) = atom
call atom.init( " H", 0.0_8, 0.0_8,-0.3561_8 )
mol.atoms(2) = atom

call mol.show( formatted=.true. )

write(*,*) ""
write(*,*) "Testing load procedures"
write(*,*) "======================="
call mol.load( "data/formats/XYZ", format=XYZ )
call mol.show( formatted=.true. )

write(*,*) ""
write(*,*) "Testing properties"
write(*,*) "=================="
write(*,*) "mol.massNumber() = ", mol.massNumber()
write(*,*) "mol.chemicalFormula() = ", mol.chemicalFormula()

write(*,*) ""
write(*,*) "Testing center of molecule"
write(*,*) "=========================="
call mol.show(formatted=.true.)
write(*,"(A,3F10.5)") "mol.center = ", mol.center()
call mol.setCenter( [-2.0_8, 1.0_8, 2.0_8] )
write(*,"(A,3F10.5)") "mol.geomCenter = ", mol.center()

write(*,*) ""
write(*,*) "Testing rotation of molecule"
write(*,*) "============================"
call mol.init( "data/formats/XYZ", format=XYZ )
call mol.rotate( alpha=45.0_8*deg, beta=45.0_8*deg, gamma=0.0_8*deg, debug=.true. )

write(*,*) ""
write(*,*) "Inertia tensor"
write(*,*) "=============="
Im = mol.inertiaTensor()
call Im.show( formatted=.true. )

```
### Numerical Functions
```fortran
function funcTest( x ) result( output )
        real(8), intent(in) :: x
        real(8) :: output
        
        output = exp(-0.44*x)*sin(x)**2.0_8
end function funcTest
```

```fortran
use Grid_
use RNFunction_

type(Grid) :: xGrid, xGrid2
type(RNFunction) :: nFunc

call xGrid.init( 1.0_8, 10.0_8, 21 )

write(*,*) ""
write(*,*) "Testing from function"
write(*,*) "====================="
call nFunc.fromFunction( xGrid, func=funcTest )
call nFunc.show()

write(*,*) "Testing interpolation"
write(*,*) "====================="
call xGrid2.init( -2.0_8, 13.0_8, 41 )
nFunc2 = nFunc.interpolate( xGrid2 )
call nFunc.show()
```
### Data Structures

```fortran
type(RealList) :: mylist
class(RealListIterator), pointer :: iter

call mylist.init()

call mylist.append( 8.0_8 )
call mylist.append( [ 1.0_8, 2.0_8] )
call mylist.prepend( 2.0_8 )

iter => mylist.begin
iter => iter.next
iter => iter.next
call mylist.insert( iter, 1.0_8 )

call mylist.erase( mylist.begin )

iter => mylist.begin
do while( associated(iter) )
		write(*,*) iter.data
		iter => iter.next
end do
```

```fortran
type(String) :: str
type(StringIntegerPair) :: pair
type(StringIntegerMap) :: mymap
class(StringIntegerMapIterator), pointer :: iter

call mymap.init()

str = "mystr1"
call mymap.insert( str, 45 )
str = "mystr2"
call mymap.insert( str, 3 )
str = "mystr3"
call mymap.insert( str, 8 )
str = "mystr4"
call mymap.insert( str, 9 )

! mymap = { "mystr1":45, "mystr2":3, "mystr3":8, "mystr4":8 }

str = "mystr2"
call mymap.erase( str )

! mymap = { "mystr1":45, "mystr3":8, "mystr4":8 }

str = "mystr3"
call mymap.set( str, 56 )

! mymap = { "mystr1":45, "mystr3":56, "mystr4":8 }

iter => mymap.begin
do while( associated(iter) )
	pair = this.pair( iter )
	write(*,*) pair.first.fstr, pair.second
	
	iter => iter.next
end do

! "mystr1"  45
! "mystr3"  56
! "mystr4"   8
```

```fortran
use Graph_

type(IntegerGraph) :: mygraph

call mygraph.init( directed=.false. )

!-------------
!     (1)     
!      |      
!     (2)     
!    /   \    
!  (3)   (4)  
!    \   /    
!     (5)
!-------------

call mygraph.newNode()
call mygraph.newNode()
call mygraph.newNode()
call mygraph.newNode()
call mygraph.newNode()

call mygraph.newEdges( 1, [2] )
call mygraph.newEdges( 2, [1,3,4] )
call mygraph.newEdges( 3, [2,5] )
call mygraph.newEdges( 4, [2,5] )
call mygraph.newEdges( 5, [3,4] )

write(*,*) ""
write(*,*) "Topological Indices"
write(*,*) "-------------------"
write(*,*) "Randic               = ", mygraph.randicIndex()
write(*,*) "Wiener               = ", mygraph.wienerIndex()
write(*,*) "Wiener               = ", mygraph.inverseWienerIndex()
write(*,*) "Balaban              = ", mygraph.balabanIndex()
write(*,*) "MolecularTopological = ", mygraph.molecularTopologicalIndex()
write(*,*) "Kirchhoff            = ", mygraph.kirchhoffIndex()
write(*,*) "KirchhoffSum         = ", mygraph.kirchhoffSumIndex()
write(*,*) "wienerSum            = ", mygraph.wienerSumIndex()
write(*,*) "JOmega               = ", mygraph.JOmegaIndex()

```
### Histograms

### Utils



## SciFT (Scientific Fortran Tools)

[![CI](https://github.com/nfaguirrec/scift/actions/workflows/ci.yml/badge.svg)](https://github.com/nfaguirrec/scift/actions/workflows/ci.yml)

The Scientific Fortran Tools (SciFT) is a library specially oriented to scientific purposes for Fortran programmers. The library provides a wide range of mathematical routines such as random number generators, special functions, and high-level classes to manipulate strings, files parsing, matrices, grid-based numerical functions, data structures (lists, vectors, maps, graphs), and molecules.

## Prerequisites

**System Requirements:**

SciFT is known to work on GNU/Linux. However, it should work on any POSIX-compliant system.

**Dependencies:**

- **[GNU Bash](https://www.gnu.org/software/bash/)**

- **[GNU Awk (gawk)](https://www.gnu.org/software/gawk/)** (version >= 4.0)

- **Fortran Compiler**:
  - **[GNU Fortran (gfortran)](https://gcc.gnu.org/wiki/GFortran)** (version >= 6.0) OR
  - **[Intel® Fortran Compiler (ifort/ifx)](https://software.intel.com/en-us/fortran-compilers)** (version >= 14.0.3)

- **Mathematical Libraries**:
  - For **gfortran**: **[FFTW3](https://www.fftw.org/)**, **[OpenBLAS](https://www.openblas.net/)**, and **[LAPACK](http://www.netlib.org/lapack/)** (e.g. installed via Conda).
  - For **Intel Compiler**: **[Intel® Math Kernel Library (Intel® MKL)](https://software.intel.com/en-us/mkl)**.


<!---**Recommended Dependencies:**--->

## Compiling SciFT

Download the .zip file from this page and extract the files,
```
$ unzip scift-master.zip 
Archive:  scift-master.zip
3e330bbcb711cb2275ef1ce06aa021de764662f2
   creating: scift-master/
  inflating: scift-master/LICENSE    
  inflating: scift-master/LICENSE.FortranParser  
  inflating: scift-master/Makefile
...

$ mv scift-master scift
```
or clone the repository using git
```
$ git clone https://github.com/nfaguirrec/scift.git
```
The following should be the content of the scift directory if previous steps were successful:
```
$ cd scift
$ ls
docs      examples  LICENSE.FortranParser  README.md     src    VERSION
doxyfile  LICENSE   Makefile               SCIFTvars.sh  utils
```

To build the library, use the modern **CMake** build system. CMake automatically handles Fortran module dependency tracking and finds math libraries (such as FFTW3, OpenBLAS, LAPACK, or Intel MKL) on your system or active Conda environments.

### Supported Build Configurations

Choose one of the following compilation configurations depending on your compiler and math library preferences:

#### 1. GNU Fortran (`gfortran`) + standard BLAS/LAPACK (e.g. OpenBLAS)
This is the default configuration on most Linux/Conda environments:
```bash
$ FC=gfortran cmake -B build_gfortran -S .
$ cmake --build build_gfortran -j$(nproc)
```

#### 2. GNU Fortran (`gfortran`) + Intel MKL
Use this configuration to link high-performance Intel MKL math libraries using `gfortran`:
```bash
$ FC=gfortran cmake -B build_gfortran_mkl -S . -DWITH_MKL=ON
$ cmake --build build_gfortran_mkl -j$(nproc)
```

#### 3. Intel Fortran (`ifort`/`ifx`) + standard BLAS/LAPACK (e.g. OpenBLAS)
Use this configuration to compile with Intel compiler tools linking standard LAPACK libraries:
```bash
$ FC=ifort cmake -B build_ifort -S . -DWITH_MKL=OFF
$ cmake --build build_ifort -j$(nproc)
```

#### 4. Intel Fortran (`ifort`/`ifx`) + Intel MKL
Use this configuration to build using Intel tools linking directly to Intel MKL (enabled automatically by default when configuring with `ifort` or `ifx`):
```bash
$ FC=ifort cmake -B build_ifort_mkl -S . -DWITH_MKL=ON
$ cmake --build build_ifort_mkl -j$(nproc)
```

*(Note: The CMake setup automatically sets `MKL_INTERFACE=lp64` when using Intel MKL to ensure 32-bit default integer compatibility, avoiding type signature mismatch issues during compilation.)*

## Installing SciFT

The basic environmental variables that SciFT needs can be loaded just adding the following command anywhere in the ~/.bashrc file:

```
source <PATH_TO_SCIFT>/SCIFTvars.sh <build>
```

## Running Tests

SciFT includes a comprehensive suite of unit tests for its classes and routines. To execute the complete test suite:
```bash
$ ctest --test-dir build
```

You can also run individual test suites directly from the build directory, for example:
```bash
$ ./build/tests/test_UnitsConverter
$ ./build/tests/test_ElementsDB
```

## Usage

The following are some examples of how to use some important classes available in SciFT

**class String**

The following block (`test.f90`) is an example of how to use the class String:

```fortran
program test
	use String_

	type(String) :: str1, str2
	integer :: int1
	character(100), allocatable :: tokens(:)
	integer :: i
	
	str1 = "Hello"
	
	str2 = str1+":fortran-string"
	write(*,*) trim(str2%fstr)
	
	call str2%split( tokens, ":-" )
	do i=1,size(tokens)
		write(*,*) i, "    ", trim(tokens(i))
	end do
	
	call str2%replace( ":", " string " )
	write(*,*) trim(str2%fstr)
	
	call str2%replace( "string", "fortran", wholeWords=.true. )
	write(*,*) trim(str2%fstr)
	
	str1 = str2%toUpper()
	write(*,*) trim(str1%fstr)
end program test
```
It is compiled and executed as follows:

- **Using Intel Compiler**:
  ```bash
  $ ifort test.f90 -o test -I${SCIFT_HOME}/modules -L${SCIFT_HOME}/src -lscift
  $ ./test
  ```

- **Using gfortran (with conda libraries)**:
  ```bash
  $ gfortran test.f90 -o test -I${SCIFT_HOME}/modules -L${SCIFT_HOME}/src -lscift -L${CONDA_PREFIX}/lib -lfftw3 -lopenblas -llapack
  $ ./test
  ```

For either compiler, the output is: 
```
 Hello:fortran-string
           1     Hello
           2     fortran
           3     string
 Hello string fortran-string
 Hello fortran fortran-string
 HELLO FORTRAN FORTRAN-STRING
```

**class RealList**

The following block (`test.f90`) is an example of how to use the class List (equivalent to list< double > in C++):

```fortran
program test
	use RealList_

	type(RealList) :: mylist
	type(RealListIterator), pointer :: iter

	mylist = RealList()

	call mylist%append( 8.0_8 )
	call mylist%append( 5.0_8 )
	call mylist%append( 1.0_8 )
	call mylist%prepend( 0.0_8 )
	call mylist%append( [ 10.0_8, 11.0_8, 12.0_8 ] )

	iter => mylist%begin
	iter => iter%next

	call mylist%insert( iter, 3.0_8 )

	iter => mylist%begin
	do while( associated(iter) )
		write(*,"(A,F6.3)", advance="no") " --> ", iter%data

		iter => iter%next
	end do
end program test
```
It is compiled an executed as follows:

```
$ ifort test.f90 -o test -I${SCIFT_HOME}/modules -L${SCIFT_HOME}/src -lscift
$ ./test 
 -->  0.000 -->  8.000 -->  3.000 -->  5.000 -->  1.000 --> 10.000 --> 11.000 --> 12.000
```

**class List (containing user-defined objects)**

The following block (`test.f90`) is an example of how to specialize and use the class List to contain MyData objects. Notice that the class MyData must have defined the == operator (equivalent to list< MyData > in C++):

```fortran
module MyData_
	implicit none
	private

	type, public :: MyData
		integer :: id
		character(2) :: name

		contains
			generic :: operator(==) => MyData_eq
			procedure :: MyData_eq
	end type MyData

	contains

	function MyData_eq( this, other ) result( output )
		class(MyData), intent(in) :: this, other
		logical :: output

		output = ( this%id == other%id .and. this%name == other%name )
	end function MyData_eq
end module MyData_

module MyList_
	use MyData_
	implicit none
	private

#define List MyList
#define ListIterator MyListIterator
#define __CLASS_ITEMLIST__ class(MyData)
#define __TYPE_ITEMLIST__ type(MyData)
#define ListBasicInterface
#include <List.h90>
#undef ListBasicInterface
#undef List
#undef ListIterator
#undef __CLASS_ITEMLIST__
#undef __TYPE_ITEMLIST__

end module MyList_

program test
	use MyData_
	use MyList_

	type(MyList) :: list
	type(MyListIterator), pointer :: iter

	list = MyList()

	call list%append( MyData(1,"a") )
	call list%append( MyData(5,"b") )
	call list%prepend( MyData(0,"c") )
	call list%append( [ MyData(0,"d"), MyData(1,"e"), MyData(2,"f") ] )

	iter => list%begin
	iter => iter%next

	call list%insert( iter, MyData(3,"g") )

	iter => list%begin
	do while( associated(iter) )
		write(*,"(A,I2,A3,A)", advance="no") " --> (", iter%data%id, iter%data%name, ")"

		iter => iter%next
	end do
end program test
```
It is compiled an executed as follows (notice it requires the Fortran preprocessor, -fpp):

```
$ ifort -fpp test.f90 -o test -I${SCIFT_HOME}/include -L${SCIFT_HOME}/src -lscift
$ ./test 
 --> ( 0 c ) --> ( 1 a ) --> ( 3 g ) --> ( 5 b ) --> ( 0 d ) --> ( 1 e ) --> ( 2 f )
```

**class StringIntegerMap**

The following block (`test.f90`) is an example of how to use the class Map (equivalent to map<string, int> in C++):

```fortran
program test
	use String_
	use StringIntegerPair_
	use StringIntegerMap_
	implicit none

	type(StringIntegerPair) :: pair
	type(StringIntegerMap) :: mymap
	class(StringIntegerMapIterator), pointer :: iter

	mymap = StringIntegerMap()

	call mymap%insert( String("John"), 27 )
	call mymap%insert( String("Marie"), 22 )
	call mymap%insert( String("Luna"), 24 )

	iter => mymap%begin
	do while( associated(iter) )
		pair = mymap%pair( iter )
		write(*,"(A15,A,I2)") pair%first%fstr, " --> ", pair%second

		iter => iter%next
	end do
end program test
```
It is compiled an executed as follows:

```
$ ifort test.f90 -o test -I${SCIFT_HOME}/modules -L${SCIFT_HOME}/src -lscift
$ ./test 
           John --> 27
           Luna --> 24
          Marie --> 22
```

**class Map (containing user-defined objects)**

The following block (`test.f90`) is an example of how to specialize and use the class Map to contain String keys and MyData values. Notice that the class MyData must have defined the == operator (equivalent to map< string, MyData > in C++):

```fortran
module MyData_
	implicit none
	private

	type, public :: MyData
		integer :: id
		character(2) :: name

		contains
			generic :: operator(==) => MyData_eq
			procedure :: MyData_eq
	end type MyData

	contains

	function MyData_eq( this, other ) result( output )
		class(MyData), intent(in) :: this, other
		logical :: output

		output = ( this%id == other%id .and. this%name == other%name )
	end function MyData_eq
end module MyData_

module MyPair_
	use String_
	use MyData_
	implicit none
	private

#define Pair MyPair
#define PairBasicInterface
#define __CLASS_ITEMFIRST__ class(String)
#define __TYPE_ITEMFIRST__  type(String)
#define __CLASS_ITEMSECOND__ class(MyData)
#define __TYPE_ITEMSECOND__  type(MyData)
#define __ADD_ATTRIBUTES__
#define __ADD_METHODS__
#include <Pair.h90>
#undef Pair
#undef PairBasicInterface
#undef __CLASS_ITEMFIRST__
#undef __TYPE_ITEMFIRST__
#undef __CLASS_ITEMSECOND__
#undef __TYPE_ITEMSECOND__
#undef __ADD_ATTRIBUTES__
#undef __ADD_METHODS__

end module MyPair_

module MyPairList_
	use String_
	use MyPair_
	implicit none
	private

#define List MyPairList
#define ListBasicInterface
#define ListIterator MyPairListIterator
#define __CLASS_ITEMLIST__ class(MyPair)
#define __TYPE_ITEMLIST__ type(MyPair)
#include <List.h90>
#undef List
#undef ListBasicInterface
#undef ListIterator
#undef __CLASS_ITEMLIST__
#undef __TYPE_ITEMLIST__

end module MyPairList_

module MyMap_
	use IOStream_
	use String_
	use MyData_
	use MyPair_
	use MyPairList_, MyMapIterator => MyPairListIterator
	implicit none
	private

	public :: MyMapIterator

#define Map MyMap
#define MapBasicInterface
#define __CLASS_MAPITERATOR__  type(MyMapIterator)
#define __MAPLIST__            MyPairList
#define __TYPE_MAPLIST__       type(MyPairList)
#define __TYPE_MAPPAIR__       type(MyPair)
#define __CLASS_MAPPAIR__      class(MyPair)
#define __CLASS_MAPVALUE__     class(MyData)
#define __TYPE_MAPVALUE__     type(MyData)
#define __ADD_METHODS__
#include <Map.h90>
#undef Map
#undef MapBasicInterface
#undef __CLASS_MAPITERATOR__
#undef __MAPLIST__
#undef __TYPE_MAPLIST__
#undef __TYPE_MAPPAIR__
#undef __CLASS_MAPPAIR__
#undef __CLASS_MAPVALUE__
#undef __TYPE_MAPVALUE__
#undef __ADD_METHODS__

end module MyMap_

program test
	use String_
	use MyData_
	use MyPair_
	use MyPairList_
	use MyMap_

	type(MyMap) :: mmap
	type(MyMapIterator), pointer :: iter
	type(MyPair) :: pair

	mmap = MyMap()

	call mmap%insert( String("John"),  MyData(1,"a") )
	call mmap%insert( String("Marie"), MyData(2,"b") )
	call mmap%insert( String("Luna"),  MyData(3,"c") )

	iter => mmap%begin
	do while( associated(iter) )
			pair = mmap%pair( iter )
			write(*,"(A15,A,I2,A3,A)") pair%first%fstr, &
				" --> (", pair%second%id, pair%second%name, ")"

			iter => iter%next
	end do
end program test
```
It is compiled an executed as follows (notice it requires the Fortran preprocessor, -fpp for ifort or -cpp for gfortran):

```
$ gfortran -cpp test.f90 -o test -I${SCIFT_HOME}/include -I${SCIFT_HOME}/modules -L${SCIFT_HOME}/src -lscift -ffree-line-length-none
$ ./test 
           John --> ( 1 a )
           Luna --> ( 3 c )
          Marie --> ( 2 b )

$ ifort -fpp test.f90 -o test -I${SCIFT_HOME}/include -I${SCIFT_HOME}/modules -L${SCIFT_HOME}/src -lscift
$ ./test
           John --> ( 1 a )
           Luna --> ( 3 c )
          Marie --> ( 2 b )
```

**class IntegerGraph**

The following block (`test.f90`) is an example of how to use the class Graph (equivalent to GTL:graph in C++):

```fortran
program test
	use IntegerGraph_
	
	type(IntegerGraph) :: mygraph
	call mygraph%init( directed=.false. )
	
	!        (1)    
	!         |     
	!        (2)    
	!       /   \   
	!     (3)   (4) 
	!       \   /   
	!        (5)
	
	call mygraph%newNodes( 5 )
	
	call mygraph%newEdges( 1, [2] )
	call mygraph%newEdges( 2, [1,3,4] )
	call mygraph%newEdges( 3, [2,5] )
	call mygraph%newEdges( 4, [2,5] )
	call mygraph%newEdges( 5, [3,4] )
	
	call mygraph%computeDijkstraPaths( 1 )
	write(*,*) "distance from 1 to 5 = ", mygraph%distance(5)
end program test
```
It is compiled an executed as follows (notice it requires math libraries):

```
$ gfortran test.f90 -o test -I${SCIFT_HOME}/modules -L${SCIFT_HOME}/src -lscift -L${CONDA_PREFIX}/lib -Wl,-rpath,${CONDA_PREFIX}/lib -lfftw3 -lopenblas -llapack
$ ./test 
 distance from 1 to 5 =    3.00000000000000

$ ifort test.f90 -o test -I${SCIFT_HOME}/modules -L${SCIFT_HOME}/src -lscift -qmkl
$ ./test
 distance from 1 to 5 =    3.00000000000000
```

**class Molecule**

The following block (`test.f90`) is an example of how to use the class Molecule. It creates a hydrogen molecule, rotates it, and then show it on screen in the XYZ format:

```fortran
program test
	use UnitsConverter_
	use Atom_
	use Molecule_
	implicit none
	
	type(Atom) :: atm
	type(Molecule) :: mol
	
	mol = Molecule( 2, name="Hydrogen molecule" )
	call atm%init( "H", 0.0_8, 0.0_8, 0.3561_8*angs ); mol%atoms(1) = atm
	call atm%init( "H", 0.0_8, 0.0_8,-0.3561_8*angs ); mol%atoms(2) = atm
	
	call mol%rotate( alpha=45.0*deg, beta=45.0*deg, gamma=0.0*deg )
	
	call mol%save()
end program test
```
It is compiled and executed as follows (notice it requires math libraries):

```
$ gfortran test.f90 -o test -I${SCIFT_HOME}/modules -L${SCIFT_HOME}/src -lscift -L${CONDA_PREFIX}/lib -Wl,-rpath,${CONDA_PREFIX}/lib -lfftw3 -lopenblas -llapack
$ ./test
           2
Hydrogen molecule
  H            0.17805000         -0.25180072          0.17805000
  H           -0.17805000          0.25180072         -0.17805000

$ ifort test.f90 -o test -I${SCIFT_HOME}/modules -L${SCIFT_HOME}/src -lscift -qmkl
$ ./test 
           2
Hydrogen molecule
  H            0.17805000         -0.25180072          0.17805000
  H           -0.17805000          0.25180072         -0.17805000
```

**class Matrix**

The following block (`test.f90`) is an example of how to use the class Matrix. It creates the matrix, diagonalizes it, and then show the results in a nice format.

```fortran
program test
	use Matrix_
	implicit none
	
	type(Matrix) :: A, B, C
	
	call A%init(5,5)
	A%data(1,:) = [  1.96,  -6.49,  -0.47,  -7.20,  -0.65 ]
	A%data(2,:) = [ -6.49,   3.80,  -6.39,   1.50,  -6.34 ]
	A%data(3,:) = [ -0.47,  -6.39,   4.17,  -1.51,   2.67 ]
	A%data(4,:) = [ -7.20,   1.50,  -1.51,   5.70,   1.80 ]
	A%data(5,:) = [ -0.65,  -6.34,   2.67,   1.80,  -7.10 ]
	
	write(*,*) ""
	write(*,*) "A ="
	call A%show( formatted=.true. )
	
	call A%eigen( eVecs=B, eVals=C )
	
	write(*,*) ""
	write(*,*) "eigenvectors ="
	call B%show( formatted=.true. )
	
	write(*,*) ""
	write(*,*) "eigenvalues ="
	call C%show( formatted=.true. )
end program test
```

It is compiled and executed as follows (notice it requires math libraries):

```
$ gfortran test.f90 -o test -I${SCIFT_HOME}/modules -L${SCIFT_HOME}/src -lscift -L${CONDA_PREFIX}/lib -Wl,-rpath,${CONDA_PREFIX}/lib -lfftw3 -lopenblas -llapack
$ ./test

 A =
                1           2           3           4           5
    1    1.960000   -6.490000   -0.470000   -7.200000   -0.650000
    2   -6.490000    3.800000   -6.390000    1.500000   -6.340000
    3   -0.470000   -6.390000    4.170000   -1.510000    2.670000
    4   -7.200000    1.500000   -1.510000    5.700000    1.800000
    5   -0.650000   -6.340000    2.670000    1.800000   -7.100000

 eigenvectors =
                1           2           3           4           5
    1   -0.298067   -0.607513   -0.402620   -0.374481    0.489637
    2   -0.507798   -0.287968    0.406586   -0.357169   -0.605255
    3   -0.081606   -0.384320    0.659966    0.500764    0.399148
    4   -0.003589   -0.446730   -0.455290    0.620365   -0.456375
    5   -0.804130    0.448032   -0.172458    0.310768    0.162248

 eigenvalues =
                 1            2            3            4            5
    1   -11.065575     0.000000     0.000000     0.000000     0.000000
    2     0.000000    -6.228747     0.000000     0.000000     0.000000
    3     0.000000     0.000000     0.864028     0.000000     0.000000
    4     0.000000     0.000000     0.000000     8.865457     0.000000
    5     0.000000     0.000000     0.000000     0.000000    16.094837

$ ifort test.f90 -o test -I${SCIFT_HOME}/modules -L${SCIFT_HOME}/src -lscift -qmkl
$ ./test 
 
 A =
                1           2           3           4           5
    1    1.960000   -6.490000   -0.470000   -7.200000   -0.650000
    2   -6.490000    3.800000   -6.390000    1.500000   -6.340000
    3   -0.470000   -6.390000    4.170000   -1.510000    2.670000
    4   -7.200000    1.500000   -1.510000    5.700000    1.800000
    5   -0.650000   -6.340000    2.670000    1.800000   -7.100000
 
 eigenvectors =
                1           2           3           4           5
    1   -0.298067   -0.607513   -0.402620   -0.374481    0.489637
    2   -0.507798   -0.287968    0.406586   -0.357169   -0.605255
    3   -0.081606   -0.384320    0.659966    0.500764    0.399148
    4   -0.003589   -0.446730   -0.455290    0.620365   -0.456375
    5   -0.804130    0.448032   -0.172458    0.310768    0.162248
 
 eigenvalues =
                 1            2            3            4            5
    1   -11.065575     0.000000     0.000000     0.000000     0.000000
    2     0.000000    -6.228747     0.000000     0.000000     0.000000
    3     0.000000     0.000000     0.864028     0.000000     0.000000
    4     0.000000     0.000000     0.000000     8.865457     0.000000
    5     0.000000     0.000000     0.000000     0.000000    16.094837
```

## Authors
* Nestor F. Aguirre ( nfaguirrec@gmail.com )

## Citing

[![DOI](https://zenodo.org/badge/81277582.svg)](https://zenodo.org/badge/latestdoi/81277582)

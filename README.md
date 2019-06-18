## SciFT (Scientific Fortran Tools)

The Scientific Fortran Tools (SciFT) is a library specially oriented to scientific purposes for Fortran programmers. The library provides a wide range of mathematical routines such as random number generators, special functions, and high-level classes to manipulate strings, files parsing, matrices, grid-based numerical functions, data structures (lists, vectors, maps, graphs), and molecules.

## Prerequisites

**System Requirements:**

SciFT is known to work on GNU/Linux. However, it should work on any POSIX-compliant system.

**Dependencies:**

- **[GNU Bash](https://www.gnu.org/software/bash/)**

- **[GNU Awk (gawk)](https://www.gnu.org/software/gawk/)** (version >= 4.0)

- **[Intel® Fortran Compiler](https://software.intel.com/en-us/fortran-compilers)** (version >= 14.0.3)<br>
  SciFT has not been tested with any other compiler.

- **[Intel® Math Kernel Library (Intel® MKL)](https://software.intel.com/en-us/mkl)**<br>
  SciFT has not been tested with any other math library.

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

Enter in the scift directory (`cd scift`) and modify the Makefile file (`src/Makefile`) if necessary.

To build the code just type make inside the main directory as follows:
```
$ source SCIFTvars.sh
$ make
Building dependencies for Atom.f90 ... OK
Building dependencies for AtomicElementsDB.f90 ... OK
Building dependencies for BlocksIFileParser.f90 ... OK
...
Building n3df.eval.f90 (0:00.79)
Building n3df.func.f90 (0:00.63)
Building n3df.oper.f90 (0:00.67)
```

## Installing SciFT

The basic environmental variables that SciFT needs can be loaded just adding the following command anywhere in the ~/.bashrc file:

```
source <PATH_TO_SCIFT>/SCIFTvars.sh
```

## Usage

**class String**

The following block (`test.f90`) is an example of how to use the class String:

```fortran
program test
	use String_

	type(String) :: str1, str2
	integer :: int1
	character(100), allocatable :: tokens(:)
	character(:), allocatable :: fstr
	integer :: i
	
	str1 = "Hello"
	
	str2 = str1+":fortran-string"
	write(*,*) trim(str2.fstr)
	
	call str2.split( tokens, ":-" )
	do i=1,size(tokens)
		write(*,*) i, "    ", trim(tokens(i))
	end do
	
	call str2.replace( ":", " string " )
	write(*,*) trim(str2.fstr)
	
	call str2.replace( "string", "fortran", wholeWords=.true. )
	write(*,*) trim(str2.fstr)
	
	str1 = str2.toUpper()
	write(*,*) trim(str1.fstr)
end program test
```
It is compiled an executed as follows:

```
$ ifort test.f90 -o test -I${SCIFT_HOME}/src -L${SCIFT_HOME}/src -lscift
$ ./test 
 Hello:fortran-string
           1     Hello
           2     fortran
           3     string
 Hello string fortran-string
 Hello fortran fortran-string
 HELLO FORTRAN FORTRAN-STRING
```

**class StringIntegerMap**

The following block (`test.f90`) is an example of how to use the class Map (equivalent to map<string, int> in C++):

```fortran
program test
	use String_
	use StringIntegerPair_
	use StringIntegerMap_
	implicit none
	
	type(String) :: str
	type(StringIntegerPair) :: pair
	type(StringIntegerMap) :: mymap
	class(StringIntegerMapIterator), pointer :: iter
	
	call mymap.init()
	
	str = "John" ; call mymap.insert( str, 27 )
	str = "Marie"; call mymap.insert( str, 22 )
	str = "Luna" ; call mymap.insert( str, 24 )
	
	iter => mymap.begin
	do while( associated(iter) )
		pair = mymap.pair( iter )
		write(*,"(A15,I10)") pair.first.fstr, pair.second
		
		iter => iter.next
	end do
end program test
```
It is compiled an executed as follows:

```
$ ifort test.f90 -o test -I${SCIFT_HOME}/src -L${SCIFT_HOME}/src -lscift
$ ./test 
           John        27
           Luna        24
          Marie        22
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
	
	call mol.init( 2, name="Hydrogen molecule" )
	call atm.init( "H", 0.0_8, 0.0_8, 0.3561_8*angs ); mol.atoms(1) = atm
	call atm.init( "H", 0.0_8, 0.0_8,-0.3561_8*angs ); mol.atoms(2) = atm
	
	call mol.rotate( alpha=45.0*deg, beta=45.0*deg, gamma=0.0*deg )
	
	call mol.save()
end program test
```
It is compiled and executed as follows (notice it requires the MKL library):

```
$ ifort test.f90 -o test -I${SCIFT_HOME}/src -L${SCIFT_HOME}/src -lscift -mkl
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
	
	call A.init(5,5)
	A.data(1,:) = [  1.96,  -6.49,  -0.47,  -7.20,  -0.65 ]
	A.data(2,:) = [ -6.49,   3.80,  -6.39,   1.50,  -6.34 ]
	A.data(3,:) = [ -0.47,  -6.39,   4.17,  -1.51,   2.67 ]
	A.data(4,:) = [ -7.20,   1.50,  -1.51,   5.70,   1.80 ]
	A.data(5,:) = [ -0.65,  -6.34,   2.67,   1.80,  -7.10 ]
	
	write(*,*) ""
	write(*,*) "A ="
	call A.show( formatted=.true. )
	
	call A.eigen( eVecs=B, eVals=C )
	
	write(*,*) ""
	write(*,*) "eigenvectors ="
	call B.show( formatted=.true. )
	
	write(*,*) ""
	write(*,*) "eigenvalues ="
	call C.show( formatted=.true. )
end program test
```

It is compiled and executed as follows (notice it requires the MKL library):

```
$ ifort test.f90 -o test -I${SCIFT_HOME}/src -L${SCIFT_HOME}/src -lscift -mkl
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

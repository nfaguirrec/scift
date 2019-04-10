## SciFT (Scientific Fortran Tools)

The Scientific Fortran Tools (SciFT) is a library specially oriented to scientific purposes for Fortran programmers. The library provides a wide range of mathematical routines such as random number generators, special functions, and high-level classes to manipulate strings, files parsing, matrices, grid-based numerical functions, data structures (lists, vectors, maps, graphs), and molecules.

The complete range of subject areas covered by the library includes:

## Data Structures
* [Molecules](#molecules)
* [Numerical Functions](#numerical-functions)
* [Data Structures](#data-structures)
* [Histograms](#histograms)
* [Utils](#utils)

## Prerequisites

**System Requirements:**

SciFT is known to work on GNU/Linux. However, it should work on any POSIX-compliant system.

**Dependencies:**

- **[GNU Bash](https://www.gnu.org/software/bash/)**

- **[GNU Awk (gawk)](https://www.gnu.org/software/gawk/)** (version >= 4.0)

- **[Intel® Fortran Compiler](https://software.intel.com/en-us/fortran-compilers)** (version >= 14.0.3)<br>
  M3C has not been tested with any other compiler.

- **[SciFT (Scientific Fortran Tools)](https://github.com/nfaguirrec/scift)**<br>
  The Scientific Fortran Tools (SciFT) is a numerical library for fortran programmers.

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
source <PATH_TO_M3C>/SCIFTvars.sh
```

## Authors
* Nestor F. Aguirre ( nfaguirrec@gmail.com )

## Citing

[![DOI](https://zenodo.org/badge/81277582.svg)](https://zenodo.org/badge/latestdoi/81277582)

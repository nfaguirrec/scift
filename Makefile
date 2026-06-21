MAKEFLAGS = -s
FC = ifort
GFCFLAGS = -w -I. -I/home/aguirre/Software/miniconda3/envs/python3.10/include -I/usr/include -cpp -O3 -g -fPIC -fdec-structure -ffree-line-length-none
GLDFLAGS = -L. -lscift -L/home/aguirre/Software/miniconda3/envs/python3.10/lib -L/usr/lib -Wl,-rpath,/home/aguirre/Software/miniconda3/envs/python3.10/lib -lfftw3 -lopenblas -llapack
IFCFLAGS = -w -I. -O3 -g -fPIC -fpp -mkl
ILDFLAGS = -L. -lscift
TLIB = 

ifeq ($(findstring gfortran,$(FC)),gfortran)
    FCFLAGS = $(GFCFLAGS)
    LDFLAGS = $(GLDFLAGS)
else
    FCFLAGS = $(IFCFLAGS)
    LDFLAGS = $(ILDFLAGS)
endif

all: build_src  $(TLIB) 

build_src: src
	make -C src
	echo 'Building source dir src'

clean:
	rm -f *.o *.mod *~ .deps $(TLIB) 2> /dev/null

distclean:
	rm -f *.o *.mod *~ .deps Makefile $(TLIB) 2> /dev/null

Makefile: .deps
	fmake

.deps:
	fmake

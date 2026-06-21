FC = gfortran

ifeq ($(findstring gfortran,$(FC)),gfortran)
    FCFLAGS = -w -I../src -I/home/aguirre/Software/miniconda3/envs/python3.10/include -cpp -g -ffree-line-length-none -fdec-structure
    LDFLAGS = -L../src -lscift -L/home/aguirre/Software/miniconda3/envs/python3.10/lib -Wl,-rpath,/home/aguirre/Software/miniconda3/envs/python3.10/lib -lfftw3 -lopenblas -llapack
else
    FCFLAGS = -static -static-intel -static-libgcc -mkl -traceback -w -fpp -g -I../src
    LDFLAGS = -L../src -lscift
endif
LIB_SCIFT = ../src/libscift.a

TEST_SRCS = $(wildcard src_tests/*.f90)
TEST_EXES = $(patsubst src_tests/%.f90,src_tests/%.exe,$(TEST_SRCS))

all: run_tests.exe $(TEST_EXES)

run_tests.exe: run_tests.f90 $(LIB_SCIFT)
	$(FC) $(FCFLAGS) -o $@ $<

src_tests/%.exe: src_tests/%.f90 $(LIB_SCIFT)
	$(FC) $(FCFLAGS) $< -o $@ $(LDFLAGS)

clean:
	rm -f run_tests.exe test.log
	rm -f src_tests/*.exe src_tests/*.o src_tests/*.mod

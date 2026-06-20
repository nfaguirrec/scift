FC = ifort
FCFLAGS = -static -static-intel -static-libgcc -mkl -traceback -w -fpp -g -I../src
LDFLAGS = -L../src -lscift
LIB_SCIFT = ../src/libscift.a

TEST_SRCS = $(wildcard src_tests/*.f90)
TEST_EXES = $(patsubst src_tests/%.f90,src_tests/%.exe,$(TEST_SRCS))

all: run_tests.exe $(TEST_EXES)

run_tests.exe: run_tests.f90 $(LIB_SCIFT)
	$(FC) -o $@ $<

src_tests/%.exe: src_tests/%.f90 $(LIB_SCIFT)
	$(FC) $(FCFLAGS) $< -o $@ $(LDFLAGS)

clean:
	rm -f run_tests.exe test.log
	rm -f src_tests/*.exe src_tests/*.o src_tests/*.mod

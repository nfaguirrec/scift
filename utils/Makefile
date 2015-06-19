MAKEFLAGS = -s
FC = ifort
FCFLAGS = -traceback -w -I. -g -fpp
LDFLAGS = 
TLIB = 

all:  $(TLIB) 

clean:
	rm -f *.o *.mod *~ .deps $(TLIB) 2> /dev/null

distclean:
	rm -f *.o *.mod *~ .deps Makefile $(TLIB) 2> /dev/null

Makefile: .deps
	fmake

.deps:
	fmake

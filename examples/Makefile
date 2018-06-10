MAKEFLAGS = -s
FC = ifort
FCFLAGS = -static -static-intel -static-libgcc -mkl -traceback -w -I. -fpp -g -I../src
LDFLAGS = -L../src -lscift
TLIB = 

all:  $(TLIB) boundStates histogram molecule.chemicalFormula molecule.compare molecule.fragments molecule.fv molecule.graph molecule.inertia molecule.isConnected molecule.isFragmentOf molecule.mass molecule.minMult molecule.move molecule.orient molecule.overlapping molecule.radius molecule.random molecule.rms molecule.rotate molecule.vrandom n1df.compare n1df.convert n1df.cumulative n1df.derivative n1df.diagH n1df.evaluate n1df.filter n1df.fourier n1df.fourierFilter n1df.func n1df.gabor n1df.gsmooth n1df.init n1df.innerProduct n1df.integrate n1df.interp n1df.join n1df.norm n1df.oper n1df.peakFinder n1df.resize n1df.smooth n1df.transitionElement n1df.translate n3df.convert n3df.eval n3df.func n3df.oper 

boundStates: boundStates.f90  
	fmake
	echo -n 'Building boundStates.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) boundStates.f90  -o boundStates $(LDFLAGS)

histogram: histogram.f90  
	fmake
	echo -n 'Building histogram.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) histogram.f90  -o histogram $(LDFLAGS)

molecule.chemicalFormula: molecule.chemicalFormula.f90  
	fmake
	echo -n 'Building molecule.chemicalFormula.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) molecule.chemicalFormula.f90  -o molecule.chemicalFormula $(LDFLAGS)

molecule.compare: molecule.compare.f90  
	fmake
	echo -n 'Building molecule.compare.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) molecule.compare.f90  -o molecule.compare $(LDFLAGS)

molecule.fragments: molecule.fragments.f90  
	fmake
	echo -n 'Building molecule.fragments.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) molecule.fragments.f90  -o molecule.fragments $(LDFLAGS)

molecule.fv: molecule.fv.f90  
	fmake
	echo -n 'Building molecule.fv.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) molecule.fv.f90  -o molecule.fv $(LDFLAGS)

molecule.graph: molecule.graph.f90  
	fmake
	echo -n 'Building molecule.graph.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) molecule.graph.f90  -o molecule.graph $(LDFLAGS)

molecule.inertia: molecule.inertia.f90  
	fmake
	echo -n 'Building molecule.inertia.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) molecule.inertia.f90  -o molecule.inertia $(LDFLAGS)

molecule.isConnected: molecule.isConnected.f90  
	fmake
	echo -n 'Building molecule.isConnected.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) molecule.isConnected.f90  -o molecule.isConnected $(LDFLAGS)

molecule.isFragmentOf: molecule.isFragmentOf.f90  
	fmake
	echo -n 'Building molecule.isFragmentOf.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) molecule.isFragmentOf.f90  -o molecule.isFragmentOf $(LDFLAGS)

molecule.mass: molecule.mass.f90  
	fmake
	echo -n 'Building molecule.mass.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) molecule.mass.f90  -o molecule.mass $(LDFLAGS)

molecule.minMult: molecule.minMult.f90  
	fmake
	echo -n 'Building molecule.minMult.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) molecule.minMult.f90  -o molecule.minMult $(LDFLAGS)

molecule.move: molecule.move.f90  
	fmake
	echo -n 'Building molecule.move.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) molecule.move.f90  -o molecule.move $(LDFLAGS)

molecule.orient: molecule.orient.f90  
	fmake
	echo -n 'Building molecule.orient.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) molecule.orient.f90  -o molecule.orient $(LDFLAGS)

molecule.overlapping: molecule.overlapping.f90  
	fmake
	echo -n 'Building molecule.overlapping.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) molecule.overlapping.f90  -o molecule.overlapping $(LDFLAGS)

molecule.radius: molecule.radius.f90  
	fmake
	echo -n 'Building molecule.radius.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) molecule.radius.f90  -o molecule.radius $(LDFLAGS)

molecule.random: molecule.random.f90  
	fmake
	echo -n 'Building molecule.random.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) molecule.random.f90  -o molecule.random $(LDFLAGS)

molecule.rms: molecule.rms.f90  
	fmake
	echo -n 'Building molecule.rms.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) molecule.rms.f90  -o molecule.rms $(LDFLAGS)

molecule.rotate: molecule.rotate.f90  
	fmake
	echo -n 'Building molecule.rotate.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) molecule.rotate.f90  -o molecule.rotate $(LDFLAGS)

molecule.vrandom: molecule.vrandom.f90  
	fmake
	echo -n 'Building molecule.vrandom.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) molecule.vrandom.f90  -o molecule.vrandom $(LDFLAGS)

n1df.compare: n1df.compare.f90  
	fmake
	echo -n 'Building n1df.compare.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) n1df.compare.f90  -o n1df.compare $(LDFLAGS)

n1df.convert: n1df.convert.f90  
	fmake
	echo -n 'Building n1df.convert.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) n1df.convert.f90  -o n1df.convert $(LDFLAGS)

n1df.cumulative: n1df.cumulative.f90  
	fmake
	echo -n 'Building n1df.cumulative.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) n1df.cumulative.f90  -o n1df.cumulative $(LDFLAGS)

n1df.derivative: n1df.derivative.f90  
	fmake
	echo -n 'Building n1df.derivative.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) n1df.derivative.f90  -o n1df.derivative $(LDFLAGS)

n1df.diagH: n1df.diagH.f90  
	fmake
	echo -n 'Building n1df.diagH.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) n1df.diagH.f90  -o n1df.diagH $(LDFLAGS)

n1df.evaluate: n1df.evaluate.f90  
	fmake
	echo -n 'Building n1df.evaluate.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) n1df.evaluate.f90  -o n1df.evaluate $(LDFLAGS)

n1df.filter: n1df.filter.f90  
	fmake
	echo -n 'Building n1df.filter.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) n1df.filter.f90  -o n1df.filter $(LDFLAGS)

n1df.fourier: n1df.fourier.f90  
	fmake
	echo -n 'Building n1df.fourier.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) n1df.fourier.f90  -o n1df.fourier $(LDFLAGS)

n1df.fourierFilter: n1df.fourierFilter.f90  
	fmake
	echo -n 'Building n1df.fourierFilter.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) n1df.fourierFilter.f90  -o n1df.fourierFilter $(LDFLAGS)

n1df.func: n1df.func.f90  
	fmake
	echo -n 'Building n1df.func.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) n1df.func.f90  -o n1df.func $(LDFLAGS)

n1df.gabor: n1df.gabor.f90  
	fmake
	echo -n 'Building n1df.gabor.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) n1df.gabor.f90  -o n1df.gabor $(LDFLAGS)

n1df.gsmooth: n1df.gsmooth.f90  
	fmake
	echo -n 'Building n1df.gsmooth.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) n1df.gsmooth.f90  -o n1df.gsmooth $(LDFLAGS)

n1df.init: n1df.init.f90  
	fmake
	echo -n 'Building n1df.init.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) n1df.init.f90  -o n1df.init $(LDFLAGS)

n1df.innerProduct: n1df.innerProduct.f90  
	fmake
	echo -n 'Building n1df.innerProduct.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) n1df.innerProduct.f90  -o n1df.innerProduct $(LDFLAGS)

n1df.integrate: n1df.integrate.f90  
	fmake
	echo -n 'Building n1df.integrate.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) n1df.integrate.f90  -o n1df.integrate $(LDFLAGS)

n1df.interp: n1df.interp.f90  
	fmake
	echo -n 'Building n1df.interp.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) n1df.interp.f90  -o n1df.interp $(LDFLAGS)

n1df.join: n1df.join.f90  
	fmake
	echo -n 'Building n1df.join.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) n1df.join.f90  -o n1df.join $(LDFLAGS)

n1df.norm: n1df.norm.f90  
	fmake
	echo -n 'Building n1df.norm.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) n1df.norm.f90  -o n1df.norm $(LDFLAGS)

n1df.oper: n1df.oper.f90  
	fmake
	echo -n 'Building n1df.oper.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) n1df.oper.f90  -o n1df.oper $(LDFLAGS)

n1df.peakFinder: n1df.peakFinder.f90  
	fmake
	echo -n 'Building n1df.peakFinder.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) n1df.peakFinder.f90  -o n1df.peakFinder $(LDFLAGS)

n1df.resize: n1df.resize.f90  
	fmake
	echo -n 'Building n1df.resize.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) n1df.resize.f90  -o n1df.resize $(LDFLAGS)

n1df.smooth: n1df.smooth.f90  
	fmake
	echo -n 'Building n1df.smooth.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) n1df.smooth.f90  -o n1df.smooth $(LDFLAGS)

n1df.transitionElement: n1df.transitionElement.f90  
	fmake
	echo -n 'Building n1df.transitionElement.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) n1df.transitionElement.f90  -o n1df.transitionElement $(LDFLAGS)

n1df.translate: n1df.translate.f90  
	fmake
	echo -n 'Building n1df.translate.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) n1df.translate.f90  -o n1df.translate $(LDFLAGS)

n3df.convert: n3df.convert.f90  
	fmake
	echo -n 'Building n3df.convert.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) n3df.convert.f90  -o n3df.convert $(LDFLAGS)

n3df.eval: n3df.eval.f90  
	fmake
	echo -n 'Building n3df.eval.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) n3df.eval.f90  -o n3df.eval $(LDFLAGS)

n3df.func: n3df.func.f90  
	fmake
	echo -n 'Building n3df.func.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) n3df.func.f90  -o n3df.func $(LDFLAGS)

n3df.oper: n3df.oper.f90  
	fmake
	echo -n 'Building n3df.oper.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) n3df.oper.f90  -o n3df.oper $(LDFLAGS)

clean:
	rm -f *.o *.mod *~ .deps boundStates histogram molecule.chemicalFormula molecule.compare molecule.fragments molecule.fv molecule.graph molecule.inertia molecule.isConnected molecule.isFragmentOf molecule.mass molecule.minMult molecule.move molecule.orient molecule.overlapping molecule.radius molecule.random molecule.rms molecule.rotate molecule.vrandom n1df.compare n1df.convert n1df.cumulative n1df.derivative n1df.diagH n1df.evaluate n1df.filter n1df.fourier n1df.fourierFilter n1df.func n1df.gabor n1df.gsmooth n1df.init n1df.innerProduct n1df.integrate n1df.interp n1df.join n1df.norm n1df.oper n1df.peakFinder n1df.resize n1df.smooth n1df.transitionElement n1df.translate n3df.convert n3df.eval n3df.func n3df.oper  2> /dev/null

distclean:
	rm -f *.o *.mod *~ .deps Makefile boundStates histogram molecule.chemicalFormula molecule.compare molecule.fragments molecule.fv molecule.graph molecule.inertia molecule.isConnected molecule.isFragmentOf molecule.mass molecule.minMult molecule.move molecule.orient molecule.overlapping molecule.radius molecule.random molecule.rms molecule.rotate molecule.vrandom n1df.compare n1df.convert n1df.cumulative n1df.derivative n1df.diagH n1df.evaluate n1df.filter n1df.fourier n1df.fourierFilter n1df.func n1df.gabor n1df.gsmooth n1df.init n1df.innerProduct n1df.integrate n1df.interp n1df.join n1df.norm n1df.oper n1df.peakFinder n1df.resize n1df.smooth n1df.transitionElement n1df.translate n3df.convert n3df.eval n3df.func n3df.oper  2> /dev/null

Makefile: .deps
	fmake

.deps:
	fmake

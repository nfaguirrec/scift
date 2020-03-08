MAKEFLAGS = -s
FC = ifort
FCFLAGS = -static -static-intel -static-libgcc -mkl -traceback -w -I. -fpp -g
LDFLAGS = -L. -lscift
TLIB = libscift.a

all: Atom.o AtomicElementsDB.o BlocksIFileParser.o CNFunction2D.o CNFunction3D.o CNFunction.o CommandLineParser.o Edge.o EdgeVector.o ElementsDB.o FFTW3.o FourierGridDiagonalization.o FourierTransform2D.o FourierTransform3D.o FourierTransform.o GaborTransform.o GOptions.o Grid2D.o Grid3D.o GridBase.o Grid.o GridND.o IntegerGraph.o IntegerHyperList.o IntegerHyperVector.o IntegerList.o IntegerVector.o IOStream.o IterativeAlgorithm.o IVector.o Math.o MathFormula.o MathParser.o Matrix.o MDIntegrator.o MoldenParser.o Molecule.o Morse.o NDerivator.o NIntegrator.o Node.o NodeVector.o NPeakFinder.o NPotentialEnergyCurve.o RandomSampler.o RandomUtils.o RealHistogram.o RealList.o RealVector.o RNFunction2D.o RNFunction3D.o RNFunction.o RVector.o SpecialAtomsPair.o SpecialMatrix.o Spline.o String.o StringHistogram.o StringIntegerMap.o StringIntegerPair.o StringIntegerPairList.o StringList.o StringRealHistogramMap.o StringRealHistogramPair.o StringRealHistogramPairList.o StringRealMap.o StringRealPair.o StringRealPairList.o Table.o ThrularNumerovMethod.o Timer.o UnitsConverter.o  $(TLIB) test 

Atom.o atom_.mod: Atom.f90 atomicelementsdb_.mod specialatomspair_.mod goptions_.mod timer_.mod unitsconverter_.mod iostream_.mod string_.mod
	fmake
	echo -n 'Building Atom.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) -c Atom.f90

AtomicElementsDB.o atomicelementsdb_.mod: AtomicElementsDB.f90 specialatomspair_.mod goptions_.mod timer_.mod unitsconverter_.mod iostream_.mod string_.mod
	fmake
	echo -n 'Building AtomicElementsDB.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) -c AtomicElementsDB.f90

BlocksIFileParser.o blocksifileparser_.mod: BlocksIFileParser.f90 string_.mod
	fmake
	echo -n 'Building BlocksIFileParser.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) -c BlocksIFileParser.f90

CNFunction2D.o cnfunction2d_.mod: CNFunction2D.f90 math_.mod rnfunction_.mod goptions_.mod timer_.mod grid_.mod string_.mod rnfunction2d_.mod iostream_.mod grid2d_.mod
	fmake
	echo -n 'Building CNFunction2D.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) -c CNFunction2D.f90

CNFunction3D.o cnfunction3d_.mod: CNFunction3D.f90 math_.mod rnfunction_.mod goptions_.mod timer_.mod commandlineparser_.mod rnfunction3d_.mod grid_.mod string_.mod iostream_.mod grid3d_.mod
	fmake
	echo -n 'Building CNFunction3D.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) -c CNFunction3D.f90

CNFunction.o cnfunction_.mod: CNFunction.f90 math_.mod goptions_.mod timer_.mod grid_.mod string_.mod iostream_.mod
	fmake
	echo -n 'Building CNFunction.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) -c CNFunction.f90

CommandLineParser.o commandlineparser_.mod: CommandLineParser.f90 string_.mod
	fmake
	echo -n 'Building CommandLineParser.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) -c CommandLineParser.f90

Edge.o edge_.mod: Edge.f90 string_.mod
	fmake
	echo -n 'Building Edge.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) -c Edge.f90

EdgeVector.o edgevector_.mod: EdgeVector.f90 edge_.mod string_.mod iostream_.mod
	fmake
	echo -n 'Building EdgeVector.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) -c EdgeVector.f90

ElementsDB.o elementsdb_.mod: ElementsDB.f90 goptions_.mod timer_.mod unitsconverter_.mod iostream_.mod string_.mod
	fmake
	echo -n 'Building ElementsDB.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) -c ElementsDB.f90

FFTW3.o fftw3_.mod: FFTW3.f90 
	fmake
	echo -n 'Building FFTW3.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) -c FFTW3.f90

FourierGridDiagonalization.o fouriergriddiagonalization_.mod: FourierGridDiagonalization.f90 math_.mod rnfunction_.mod goptions_.mod cnfunction_.mod fftw3_.mod timer_.mod fouriertransform_.mod grid_.mod randomutils_.mod string_.mod iostream_.mod matrix_.mod
	fmake
	echo -n 'Building FourierGridDiagonalization.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) -c FourierGridDiagonalization.f90

FourierTransform2D.o fouriertransform2d_.mod: FourierTransform2D.f90 math_.mod rnfunction_.mod cnfunction_.mod goptions_.mod cnfunction2d_.mod fftw3_.mod timer_.mod fouriertransform_.mod grid_.mod string_.mod iostream_.mod randomutils_.mod rnfunction2d_.mod grid2d_.mod
	fmake
	echo -n 'Building FourierTransform2D.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) -c FourierTransform2D.f90

FourierTransform3D.o fouriertransform3d_.mod: FourierTransform3D.f90 math_.mod cnfunction3d_.mod rnfunction_.mod cnfunction_.mod cnfunction2d_.mod goptions_.mod fftw3_.mod timer_.mod fouriertransform_.mod commandlineparser_.mod rnfunction3d_.mod grid_.mod rnfunction2d_.mod string_.mod iostream_.mod randomutils_.mod fouriertransform2d_.mod grid3d_.mod grid2d_.mod
	fmake
	echo -n 'Building FourierTransform3D.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) -c FourierTransform3D.f90

FourierTransform.o fouriertransform_.mod: FourierTransform.f90 math_.mod rnfunction_.mod goptions_.mod cnfunction_.mod fftw3_.mod timer_.mod grid_.mod iostream_.mod string_.mod randomutils_.mod
	fmake
	echo -n 'Building FourierTransform.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) -c FourierTransform.f90

GaborTransform.o gabortransform_.mod: GaborTransform.f90 math_.mod rnfunction_.mod goptions_.mod cnfunction_.mod cnfunction2d_.mod timer_.mod grid_.mod string_.mod iostream_.mod rnfunction2d_.mod grid2d_.mod
	fmake
	echo -n 'Building GaborTransform.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) -c GaborTransform.f90

GOptions.o goptions_.mod: GOptions.f90 timer_.mod string_.mod iostream_.mod
	fmake
	echo -n 'Building GOptions.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) -c GOptions.f90

Grid2D.o grid2d_.mod: Grid2D.f90 math_.mod goptions_.mod timer_.mod grid_.mod string_.mod iostream_.mod
	fmake
	echo -n 'Building Grid2D.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) -c Grid2D.f90

Grid3D.o grid3d_.mod: Grid3D.f90 math_.mod goptions_.mod timer_.mod grid_.mod string_.mod iostream_.mod
	fmake
	echo -n 'Building Grid3D.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) -c Grid3D.f90

GridBase.o gridbase_.mod: GridBase.f90 string_.mod iostream_.mod
	fmake
	echo -n 'Building GridBase.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) -c GridBase.f90

Grid.o grid_.mod: Grid.f90 math_.mod goptions_.mod timer_.mod string_.mod iostream_.mod
	fmake
	echo -n 'Building Grid.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) -c Grid.f90

GridND.o gridnd_.mod: GridND.f90 math_.mod goptions_.mod timer_.mod grid_.mod string_.mod iostream_.mod
	fmake
	echo -n 'Building GridND.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) -c GridND.f90

IntegerGraph.o integergraph_.mod: IntegerGraph.f90 edgevector_.mod nodevector_.mod math_.mod edge_.mod integervector_.mod stringintegermap_.mod integerhypervector_.mod randomutils_.mod string_.mod realvector_.mod iostream_.mod node_.mod matrix_.mod stringintegerpairlist_.mod stringintegerpair_.mod
	fmake
	echo -n 'Building IntegerGraph.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) -c IntegerGraph.f90

IntegerHyperList.o integerhyperlist_.mod: IntegerHyperList.f90 math_.mod integerlist_.mod randomutils_.mod string_.mod iostream_.mod matrix_.mod
	fmake
	echo -n 'Building IntegerHyperList.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) -c IntegerHyperList.f90

IntegerHyperVector.o integerhypervector_.mod: IntegerHyperVector.f90 math_.mod integervector_.mod randomutils_.mod string_.mod iostream_.mod matrix_.mod
	fmake
	echo -n 'Building IntegerHyperVector.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) -c IntegerHyperVector.f90

IntegerList.o integerlist_.mod: IntegerList.f90 string_.mod iostream_.mod
	fmake
	echo -n 'Building IntegerList.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) -c IntegerList.f90

IntegerVector.o integervector_.mod: IntegerVector.f90 string_.mod iostream_.mod
	fmake
	echo -n 'Building IntegerVector.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) -c IntegerVector.f90

IOStream.o iostream_.mod: IOStream.f90 string_.mod
	fmake
	echo -n 'Building IOStream.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) -c IOStream.f90

IterativeAlgorithm.o iterativealgorithm_.mod: IterativeAlgorithm.f90 
	fmake
	echo -n 'Building IterativeAlgorithm.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) -c IterativeAlgorithm.f90

IVector.o ivector_.mod: IVector.f90 
	fmake
	echo -n 'Building IVector.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) -c IVector.f90

Math.o math_.mod: Math.f90 
	fmake
	echo -n 'Building Math.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) -c Math.f90

MathFormula.o mathformula_.mod: MathFormula.f90 string_.mod mathparser_.mod
	fmake
	echo -n 'Building MathFormula.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) -c MathFormula.f90

MathParser.o mathparser_.mod: MathParser.f90 
	fmake
	echo -n 'Building MathParser.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) -c MathParser.f90

Matrix.o matrix_.mod: Matrix.f90 math_.mod randomutils_.mod
	fmake
	echo -n 'Building Matrix.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) -c Matrix.f90

MDIntegrator.o mdintegrator_.mod: MDIntegrator.f90 
	fmake
	echo -n 'Building MDIntegrator.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) -c MDIntegrator.f90

MoldenParser.o moldenparser_.mod: MoldenParser.f90 goptions_.mod timer_.mod unitsconverter_.mod string_.mod iostream_.mod
	fmake
	echo -n 'Building MoldenParser.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) -c MoldenParser.f90

Molecule.o molecule_.mod: Molecule.f90 edgevector_.mod nodevector_.mod atomicelementsdb_.mod math_.mod specialatomspair_.mod edge_.mod integervector_.mod stringintegermap_.mod goptions_.mod timer_.mod stringlist_.mod unitsconverter_.mod specialmatrix_.mod integerhypervector_.mod integergraph_.mod realvector_.mod string_.mod randomutils_.mod atom_.mod iostream_.mod node_.mod matrix_.mod stringintegerpairlist_.mod stringintegerpair_.mod randomsampler_.mod
	fmake
	echo -n 'Building Molecule.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) -c Molecule.f90

Morse.o morse_.mod: Morse.f90 math_.mod rnfunction_.mod goptions_.mod timer_.mod unitsconverter_.mod grid_.mod string_.mod iostream_.mod thrularnumerovmethod_.mod
	fmake
	echo -n 'Building Morse.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) -c Morse.f90

NDerivator.o nderivator_.mod: NDerivator.f90 math_.mod rnfunction_.mod goptions_.mod timer_.mod grid_.mod string_.mod iostream_.mod
	fmake
	echo -n 'Building NDerivator.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) -c NDerivator.f90

NIntegrator.o nintegrator_.mod: NIntegrator.f90 math_.mod rnfunction_.mod goptions_.mod timer_.mod grid_.mod string_.mod iostream_.mod
	fmake
	echo -n 'Building NIntegrator.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) -c NIntegrator.f90

Node.o node_.mod: Node.f90 string_.mod
	fmake
	echo -n 'Building Node.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) -c Node.f90

NodeVector.o nodevector_.mod: NodeVector.f90 string_.mod iostream_.mod node_.mod
	fmake
	echo -n 'Building NodeVector.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) -c NodeVector.f90

NPeakFinder.o npeakfinder_.mod: NPeakFinder.f90 math_.mod rnfunction_.mod integerlist_.mod goptions_.mod timer_.mod grid_.mod string_.mod iostream_.mod
	fmake
	echo -n 'Building NPeakFinder.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) -c NPeakFinder.f90

NPotentialEnergyCurve.o npotentialenergycurve_.mod: NPotentialEnergyCurve.f90 math_.mod rnfunction_.mod goptions_.mod morse_.mod timer_.mod unitsconverter_.mod grid_.mod string_.mod iostream_.mod spline_.mod thrularnumerovmethod_.mod
	fmake
	echo -n 'Building NPotentialEnergyCurve.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) -c NPotentialEnergyCurve.f90

RandomSampler.o randomsampler_.mod: RandomSampler.f90 math_.mod goptions_.mod timer_.mod unitsconverter_.mod iostream_.mod string_.mod randomutils_.mod
	fmake
	echo -n 'Building RandomSampler.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) -c RandomSampler.f90

RandomUtils.o randomutils_.mod: RandomUtils.f90 math_.mod
	fmake
	echo -n 'Building RandomUtils.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) -c RandomUtils.f90

RealHistogram.o realhistogram_.mod: RealHistogram.f90 math_.mod rnfunction_.mod goptions_.mod reallist_.mod timer_.mod grid_.mod string_.mod iostream_.mod
	fmake
	echo -n 'Building RealHistogram.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) -c RealHistogram.f90

RealList.o reallist_.mod: RealList.f90 string_.mod iostream_.mod
	fmake
	echo -n 'Building RealList.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) -c RealList.f90

RealVector.o realvector_.mod: RealVector.f90 string_.mod iostream_.mod
	fmake
	echo -n 'Building RealVector.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) -c RealVector.f90

RNFunction2D.o rnfunction2d_.mod: RNFunction2D.f90 math_.mod rnfunction_.mod goptions_.mod timer_.mod grid_.mod string_.mod iostream_.mod grid2d_.mod
	fmake
	echo -n 'Building RNFunction2D.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) -c RNFunction2D.f90

RNFunction3D.o rnfunction3d_.mod: RNFunction3D.f90 math_.mod rnfunction_.mod goptions_.mod timer_.mod grid_.mod string_.mod iostream_.mod grid3d_.mod
	fmake
	echo -n 'Building RNFunction3D.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) -c RNFunction3D.f90

RNFunction.o rnfunction_.mod: RNFunction.f90 math_.mod goptions_.mod timer_.mod grid_.mod string_.mod iostream_.mod
	fmake
	echo -n 'Building RNFunction.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) -c RNFunction.f90

RVector.o rvector_.mod: RVector.f90 
	fmake
	echo -n 'Building RVector.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) -c RVector.f90

SpecialAtomsPair.o specialatomspair_.mod: SpecialAtomsPair.f90 
	fmake
	echo -n 'Building SpecialAtomsPair.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) -c SpecialAtomsPair.f90

SpecialMatrix.o specialmatrix_.mod: SpecialMatrix.f90 math_.mod goptions_.mod timer_.mod unitsconverter_.mod iostream_.mod string_.mod randomutils_.mod matrix_.mod
	fmake
	echo -n 'Building SpecialMatrix.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) -c SpecialMatrix.f90

Spline.o spline_.mod: Spline.f90 math_.mod rnfunction_.mod goptions_.mod timer_.mod grid_.mod string_.mod iostream_.mod
	fmake
	echo -n 'Building Spline.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) -c Spline.f90

String.o string_.mod: String.f90 
	fmake
	echo -n 'Building String.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) -c String.f90

StringHistogram.o stringhistogram_.mod: StringHistogram.f90 stringrealmap_.mod stringintegermap_.mod goptions_.mod timer_.mod stringlist_.mod iostream_.mod string_.mod stringrealpairlist_.mod stringrealpair_.mod stringintegerpairlist_.mod stringintegerpair_.mod
	fmake
	echo -n 'Building StringHistogram.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) -c StringHistogram.f90

StringIntegerMap.o stringintegermap_.mod: StringIntegerMap.f90 string_.mod iostream_.mod stringintegerpairlist_.mod stringintegerpair_.mod
	fmake
	echo -n 'Building StringIntegerMap.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) -c StringIntegerMap.f90

StringIntegerPair.o stringintegerpair_.mod: StringIntegerPair.f90 string_.mod
	fmake
	echo -n 'Building StringIntegerPair.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) -c StringIntegerPair.f90

StringIntegerPairList.o stringintegerpairlist_.mod: StringIntegerPairList.f90 string_.mod iostream_.mod stringintegerpair_.mod
	fmake
	echo -n 'Building StringIntegerPairList.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) -c StringIntegerPairList.f90

StringList.o stringlist_.mod: StringList.f90 string_.mod iostream_.mod
	fmake
	echo -n 'Building StringList.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) -c StringList.f90

StringRealHistogramMap.o stringrealhistogrammap_.mod: StringRealHistogramMap.f90 math_.mod stringrealhistogrampairlist_.mod rnfunction_.mod realhistogram_.mod stringrealhistogrampair_.mod goptions_.mod reallist_.mod timer_.mod grid_.mod string_.mod iostream_.mod
	fmake
	echo -n 'Building StringRealHistogramMap.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) -c StringRealHistogramMap.f90

StringRealHistogramPair.o stringrealhistogrampair_.mod: StringRealHistogramPair.f90 math_.mod rnfunction_.mod realhistogram_.mod goptions_.mod reallist_.mod timer_.mod grid_.mod iostream_.mod string_.mod
	fmake
	echo -n 'Building StringRealHistogramPair.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) -c StringRealHistogramPair.f90

StringRealHistogramPairList.o stringrealhistogrampairlist_.mod: StringRealHistogramPairList.f90 math_.mod rnfunction_.mod realhistogram_.mod stringrealhistogrampair_.mod goptions_.mod reallist_.mod timer_.mod grid_.mod string_.mod iostream_.mod
	fmake
	echo -n 'Building StringRealHistogramPairList.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) -c StringRealHistogramPairList.f90

StringRealMap.o stringrealmap_.mod: StringRealMap.f90 string_.mod iostream_.mod stringrealpair_.mod stringrealpairlist_.mod
	fmake
	echo -n 'Building StringRealMap.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) -c StringRealMap.f90

StringRealPair.o stringrealpair_.mod: StringRealPair.f90 string_.mod
	fmake
	echo -n 'Building StringRealPair.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) -c StringRealPair.f90

StringRealPairList.o stringrealpairlist_.mod: StringRealPairList.f90 string_.mod iostream_.mod stringrealpair_.mod
	fmake
	echo -n 'Building StringRealPairList.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) -c StringRealPairList.f90

Table.o table_.mod: Table.f90 string_.mod iostream_.mod
	fmake
	echo -n 'Building Table.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) -c Table.f90

test: test.f90 StringRealHistogramMap.o Matrix.o Molecule.o CommandLineParser.o RNFunction3D.o StringRealMap.o RealList.o String.o MathFormula.o IVector.o UnitsConverter.o Morse.o Atom.o RNFunction2D.o FourierTransform.o NDerivator.o NodeVector.o BlocksIFileParser.o GridBase.o Timer.o MDIntegrator.o Grid3D.o ThrularNumerovMethod.o StringRealHistogramPair.o StringHistogram.o ElementsDB.o StringIntegerMap.o RealHistogram.o StringRealPair.o EdgeVector.o GridND.o StringIntegerPairList.o IntegerList.o RealVector.o FourierTransform3D.o Node.o Grid2D.o IntegerHyperVector.o StringList.o IntegerHyperList.o StringRealHistogramPairList.o FourierGridDiagonalization.o CNFunction.o RNFunction.o CNFunction3D.o Grid.o IntegerGraph.o NPotentialEnergyCurve.o NIntegrator.o AtomicElementsDB.o GaborTransform.o StringIntegerPair.o FourierTransform2D.o Edge.o StringRealPairList.o FFTW3.o NPeakFinder.o SpecialMatrix.o MoldenParser.o RandomSampler.o RandomUtils.o SpecialAtomsPair.o RVector.o MathParser.o CNFunction2D.o GOptions.o Spline.o Math.o Table.o IOStream.o IntegerVector.o stringrealhistogrammap_.mod matrix_.mod molecule_.mod commandlineparser_.mod rnfunction3d_.mod stringrealmap_.mod reallist_.mod string_.mod mathformula_.mod ivector_.mod unitsconverter_.mod morse_.mod atom_.mod rnfunction2d_.mod fouriertransform_.mod nderivator_.mod nodevector_.mod blocksifileparser_.mod gridbase_.mod timer_.mod mdintegrator_.mod grid3d_.mod thrularnumerovmethod_.mod stringrealhistogrampair_.mod stringhistogram_.mod elementsdb_.mod stringintegermap_.mod realhistogram_.mod stringrealpair_.mod edgevector_.mod gridnd_.mod stringintegerpairlist_.mod integerlist_.mod realvector_.mod fouriertransform3d_.mod node_.mod grid2d_.mod integerhypervector_.mod stringlist_.mod integerhyperlist_.mod stringrealhistogrampairlist_.mod fouriergriddiagonalization_.mod cnfunction_.mod rnfunction_.mod cnfunction3d_.mod grid_.mod integergraph_.mod npotentialenergycurve_.mod nintegrator_.mod atomicelementsdb_.mod gabortransform_.mod stringintegerpair_.mod fouriertransform2d_.mod edge_.mod stringrealpairlist_.mod fftw3_.mod npeakfinder_.mod specialmatrix_.mod moldenparser_.mod randomsampler_.mod randomutils_.mod specialatomspair_.mod rvector_.mod mathparser_.mod cnfunction2d_.mod goptions_.mod spline_.mod math_.mod table_.mod iostream_.mod integervector_.mod
	fmake
	echo -n 'Building test.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) test.f90 StringRealHistogramMap.o Matrix.o Molecule.o CommandLineParser.o RNFunction3D.o StringRealMap.o RealList.o String.o MathFormula.o IVector.o UnitsConverter.o Morse.o Atom.o RNFunction2D.o FourierTransform.o NDerivator.o NodeVector.o BlocksIFileParser.o GridBase.o Timer.o MDIntegrator.o Grid3D.o ThrularNumerovMethod.o StringRealHistogramPair.o StringHistogram.o ElementsDB.o StringIntegerMap.o RealHistogram.o StringRealPair.o EdgeVector.o GridND.o StringIntegerPairList.o IntegerList.o RealVector.o FourierTransform3D.o Node.o Grid2D.o IntegerHyperVector.o StringList.o IntegerHyperList.o StringRealHistogramPairList.o FourierGridDiagonalization.o CNFunction.o RNFunction.o CNFunction3D.o Grid.o IntegerGraph.o NPotentialEnergyCurve.o NIntegrator.o AtomicElementsDB.o GaborTransform.o StringIntegerPair.o FourierTransform2D.o Edge.o StringRealPairList.o FFTW3.o NPeakFinder.o SpecialMatrix.o MoldenParser.o RandomSampler.o RandomUtils.o SpecialAtomsPair.o RVector.o MathParser.o CNFunction2D.o GOptions.o Spline.o Math.o Table.o IOStream.o IntegerVector.o -o test $(LDFLAGS)

ThrularNumerovMethod.o thrularnumerovmethod_.mod: ThrularNumerovMethod.f90 math_.mod rnfunction_.mod goptions_.mod timer_.mod grid_.mod string_.mod iostream_.mod
	fmake
	echo -n 'Building ThrularNumerovMethod.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) -c ThrularNumerovMethod.f90

Timer.o timer_.mod: Timer.f90 
	fmake
	echo -n 'Building Timer.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) -c Timer.f90

UnitsConverter.o unitsconverter_.mod: UnitsConverter.f90 goptions_.mod timer_.mod iostream_.mod string_.mod
	fmake
	echo -n 'Building UnitsConverter.f90 '
	time -f "(%E)" $(FC) $(FCFLAGS) -c UnitsConverter.f90

$(TLIB): Atom.o AtomicElementsDB.o BlocksIFileParser.o CNFunction2D.o CNFunction3D.o CNFunction.o CommandLineParser.o Edge.o EdgeVector.o ElementsDB.o FFTW3.o FourierGridDiagonalization.o FourierTransform2D.o FourierTransform3D.o FourierTransform.o GaborTransform.o GOptions.o Grid2D.o Grid3D.o GridBase.o Grid.o GridND.o IntegerGraph.o IntegerHyperList.o IntegerHyperVector.o IntegerList.o IntegerVector.o IOStream.o IterativeAlgorithm.o IVector.o Math.o MathFormula.o MathParser.o Matrix.o MDIntegrator.o MoldenParser.o Molecule.o Morse.o NDerivator.o NIntegrator.o Node.o NodeVector.o NPeakFinder.o NPotentialEnergyCurve.o RandomSampler.o RandomUtils.o RealHistogram.o RealList.o RealVector.o RNFunction2D.o RNFunction3D.o RNFunction.o RVector.o SpecialAtomsPair.o SpecialMatrix.o Spline.o String.o StringHistogram.o StringIntegerMap.o StringIntegerPair.o StringIntegerPairList.o StringList.o StringRealHistogramMap.o StringRealHistogramPair.o StringRealHistogramPairList.o StringRealMap.o StringRealPair.o StringRealPairList.o Table.o ThrularNumerovMethod.o Timer.o UnitsConverter.o 
	echo 'Building library $(TLIB)'
	ar rvs $(TLIB) Atom.o AtomicElementsDB.o BlocksIFileParser.o CNFunction2D.o CNFunction3D.o CNFunction.o CommandLineParser.o Edge.o EdgeVector.o ElementsDB.o FFTW3.o FourierGridDiagonalization.o FourierTransform2D.o FourierTransform3D.o FourierTransform.o GaborTransform.o GOptions.o Grid2D.o Grid3D.o GridBase.o Grid.o GridND.o IntegerGraph.o IntegerHyperList.o IntegerHyperVector.o IntegerList.o IntegerVector.o IOStream.o IterativeAlgorithm.o IVector.o Math.o MathFormula.o MathParser.o Matrix.o MDIntegrator.o MoldenParser.o Molecule.o Morse.o NDerivator.o NIntegrator.o Node.o NodeVector.o NPeakFinder.o NPotentialEnergyCurve.o RandomSampler.o RandomUtils.o RealHistogram.o RealList.o RealVector.o RNFunction2D.o RNFunction3D.o RNFunction.o RVector.o SpecialAtomsPair.o SpecialMatrix.o Spline.o String.o StringHistogram.o StringIntegerMap.o StringIntegerPair.o StringIntegerPairList.o StringList.o StringRealHistogramMap.o StringRealHistogramPair.o StringRealHistogramPairList.o StringRealMap.o StringRealPair.o StringRealPairList.o Table.o ThrularNumerovMethod.o Timer.o UnitsConverter.o  > /dev/null 2> /dev/null

clean:
	rm -f *.o *.mod *~ .deps test  2> /dev/null

distclean:
	rm -f *.o *.mod *~ .deps Makefile test  2> /dev/null

Makefile: .deps
	fmake

.deps:
	fmake

set(test_names
  Atom
  AtomicElementsDB
  BlocksIFileParser
  CNFunction
  CNFunction2D
  CNFunction3D
  CommandLineParser
  EdgeVector
  ElementsDB
  FourierGridDiagonalization
  FourierTransform
  FourierTransform2D
  FourierTransform3D
  GaborTransform
  Grid
  Grid2D
  Grid3D
  GridBase
  GridND
  IOStream
  IVector
  IntegerGraph
  IntegerHyperList
  IntegerHyperVector
  IntegerList
  IntegerVector
  MDIntegrator
  Math
  MathFormula
  MathParser
  Matrix
  MoldenParser
  Molecule
  Morse
  NDerivator
  NIntegrator
  NPeakFinder
  NPotentialEnergyCurve
  NodeVector
  RNFunction
  RNFunction2D
  RNFunction3D
  RVector
  RandomSampler
  RandomUtils
  RealHistogram
  RealList
  RealVector
  SpecialAtomsPair
  SpecialMatrix
  Spline
  String
  StringHistogram
  StringIntegerMap
  StringIntegerPair
  StringIntegerPairList
  StringList
  StringRealHistogramMap
  StringRealHistogramPair
  StringRealHistogramPairList
  StringRealMap
  StringRealPair
  StringRealPairList
  Table
  ThrularNumerovMethod
  Timer
  UnitsConverter
)

# Copy tests data directory to the build tree so tests can access files relatively
file(COPY "${CMAKE_CURRENT_SOURCE_DIR}/data" DESTINATION "${CMAKE_CURRENT_BINARY_DIR}")

# Build and register individual test programs
foreach(name IN LISTS test_names)
  set(test_target "test_${name}")
  add_executable(${test_target} "test_${name}.f90")
  target_link_libraries(${test_target} PRIVATE scift)
  
  # Register the test executable with CTest
  add_test(NAME ${test_target}
    COMMAND ${test_target}
    WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
  )
endforeach()

set(scift_SOURCES
  Atom.f90
  AtomicElementsDB.f90
  BlocksIFileParser.f90
  CNFunction2D.f90
  CNFunction3D.f90
  CNFunction.f90
  CommandLineParser.f90
  Edge.f90
  EdgeVector.f90
  ElementsDB.f90
  FFTW3.f90
  FourierGridDiagonalization.f90
  FourierTransform2D.f90
  FourierTransform3D.f90
  FourierTransform.f90
  GaborTransform.f90
  GOptions.f90
  Grid2D.f90
  Grid3D.f90
  GridBase.f90
  Grid.f90
  GridND.f90
  IntegerGraph.f90
  IntegerHyperList.f90
  IntegerHyperVector.f90
  IntegerList.f90
  IntegerVector.f90
  IOStream.f90
  IterativeAlgorithm.f90
  IVector.f90
  Math.f90
  MathFormula.f90
  MathParser.f90
  Matrix.f90
  MDIntegrator.f90
  MoldenParser.f90
  Molecule.f90
  Morse.f90
  NDerivator.f90
  NIntegrator.f90
  Node.f90
  NodeVector.f90
  NPeakFinder.f90
  NPotentialEnergyCurve.f90
  RandomSampler.f90
  RandomUtils.f90
  RealHistogram.f90
  RealList.f90
  RealVector.f90
  RNFunction2D.f90
  RNFunction3D.f90
  RNFunction.f90
  RVector.f90
  SpecialAtomsPair.f90
  SpecialMatrix.f90
  Spline.f90
  String.f90
  StringHistogram.f90
  StringIntegerMap.f90
  StringIntegerPair.f90
  StringIntegerPairList.f90
  StringList.f90
  StringRealHistogramMap.f90
  StringRealHistogramPair.f90
  StringRealHistogramPairList.f90
  StringRealMap.f90
  StringRealPair.f90
  StringRealPairList.f90
  Table.f90
  TestUtils.f90
  ThrularNumerovMethod.f90
  Timer.f90
  UnitsConverter.f90
)

# Build the main library (static)
add_library(scift STATIC ${scift_SOURCES})

# Copy .h90 header templates to the build include directory at build time
file(GLOB H90_HEADERS "${CMAKE_CURRENT_SOURCE_DIR}/*.h90")
set(COPIED_H90_HEADERS "")

foreach(HEADER ${H90_HEADERS})
  get_filename_component(HEADER_NAME ${HEADER} NAME)
  set(DEST_FILE "${CMAKE_BINARY_DIR}/include/${HEADER_NAME}")
  
  add_custom_command(
    OUTPUT ${DEST_FILE}
    COMMAND ${CMAKE_COMMAND} -E copy ${HEADER} ${DEST_FILE}
    DEPENDS ${HEADER}
    COMMENT "Copying ${HEADER_NAME} to build include directory"
  )
  list(APPEND COPIED_H90_HEADERS ${DEST_FILE})
endforeach()

add_custom_target(copy_h90 ALL DEPENDS ${COPIED_H90_HEADERS})
add_dependencies(scift copy_h90)

# Target-specific compiler options
target_compile_options(scift PUBLIC
  $<$<Fortran_COMPILER_ID:GNU>:-w -cpp -fdec-structure -ffree-line-length-none>
  $<$<Fortran_COMPILER_ID:Intel>:-w -fpp>
  $<$<Fortran_COMPILER_ID:IntelLLVM>:-w -fpp>
)

# Target-specific include directories
target_include_directories(scift PUBLIC
  $<BUILD_INTERFACE:${CMAKE_Fortran_MODULE_DIRECTORY}>
  $<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/include>
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
  PRIVATE
  ${FFTW3_INCLUDE_DIRS}
)

# Target-specific link libraries
target_link_libraries(scift PUBLIC
  ${LAPACK_LIBRARIES}
  ${FFTW3_LIBRARIES}
)

# Enable Position Independent Code (good practice for library archives/shared object compatibility)
set_target_properties(scift PROPERTIES POSITION_INDEPENDENT_CODE ON)

# Installation rules
include(GNUInstallDirs)
install(TARGETS scift
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
)

install(FILES ${H90_HEADERS}
  DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
)

install(DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/
  DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
  FILES_MATCHING PATTERN "*.mod"
)


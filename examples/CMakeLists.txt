set(example_names
  boundStates
  histogram
  molecule.angles
  molecule.atomicOverlapping
  molecule.chemicalFormula
  molecule.compare
  molecule.dihedrals
  molecule.distances
  molecule.distort
  molecule.duplicate
  molecule.fragments
  molecule.fv
  molecule.graph
  molecule.inertia
  molecule.isConnected
  molecule.isFragmentOf
  molecule.mass
  molecule.merge
  molecule.minMult
  molecule.move
  molecule.orient
  molecule.overlapping
  molecule.radius
  molecule.random
  molecule.rms
  molecule.rotate
  molecule.vrandom
  n1df.compare
  n1df.convert
  n1df.convolution
  n1df.cumulative
  n1df.derivative
  n1df.diagH
  n1df.evaluate
  n1df.filter
  n1df.fourier
  n1df.fourierFilter
  n1df.func
  n1df.gabor
  n1df.gsmooth
  n1df.init
  n1df.innerProduct
  n1df.integrate
  n1df.interp
  n1df.join
  n1df.norm
  n1df.oper
  n1df.peakFinder
  n1df.resize
  n1df.smooth
  n1df.transitionElement
  n1df.translate
  n3df.convert
  n3df.eval
  n3df.func
  n3df.oper
)

# Copy all *.xyz, *.dat, *.gif, *.jmol data files to the build directory so examples can run with local files
file(GLOB example_data
  "${CMAKE_CURRENT_SOURCE_DIR}/*.xyz"
  "${CMAKE_CURRENT_SOURCE_DIR}/*.dat"
  "${CMAKE_CURRENT_SOURCE_DIR}/*.gif"
  "${CMAKE_CURRENT_SOURCE_DIR}/*.jmol"
)
foreach(file IN LISTS example_data)
  file(COPY ${file} DESTINATION "${CMAKE_CURRENT_BINARY_DIR}")
endforeach()

# Build examples and link them to scift
foreach(name IN LISTS example_names)
  add_executable(${name} ${name}.f90)
  target_link_libraries(${name} PRIVATE scift)
  
  # Configure runtime output directory for convenience
  set_target_properties(${name} PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
  )
endforeach()

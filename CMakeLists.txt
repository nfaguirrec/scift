cmake_minimum_required(VERSION 3.15)
project(SciFT LANGUAGES Fortran)

# Allow selection of standard build types (Release, Debug, RelWithDebInfo, MinSizeRel)
if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE "Release" CACHE STRING "Choose the type of build." FORCE)
endif()

message(STATUS "Fortran compiler: ${CMAKE_Fortran_COMPILER}")
message(STATUS "Fortran compiler ID: ${CMAKE_Fortran_COMPILER_ID}")
message(STATUS "Build type: ${CMAKE_BUILD_TYPE}")

# Automatically find dependencies under active Conda environments
if(DEFINED ENV{CONDA_PREFIX})
  message(STATUS "Found CONDA_PREFIX: $ENV{CONDA_PREFIX}, adding to search paths")
  list(APPEND CMAKE_PREFIX_PATH "$ENV{CONDA_PREFIX}")
endif()

# Find math libraries (BLAS/LAPACK or Intel MKL)
if(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
  set(WITH_MKL_DEFAULT ON)
else()
  set(WITH_MKL_DEFAULT OFF)
endif()
option(WITH_MKL "Use Intel MKL if available" ${WITH_MKL_DEFAULT})

if(WITH_MKL)
  set(MKL_INTERFACE "lp64" CACHE STRING "Use 32-bit integer LP64 MKL interface" FORCE)
  find_package(MKL CONFIG QUIET)
  if(MKL_FOUND)
    message(STATUS "Found Intel MKL CONFIG package")
    set(LAPACK_LIBRARIES MKL::MKL)
  else()
    message(STATUS "Intel MKL CONFIG package not found, falling back to FindLAPACK")
    find_package(LAPACK REQUIRED)
  endif()
else()
  find_package(LAPACK REQUIRED)
endif()

# Find FFTW3 (preferring PkgConfig under Unix/Conda, falling back to manual search)
find_package(PkgConfig QUIET)
if(PkgConfig_FOUND)
  pkg_check_modules(FFTW3 QUIET IMPORTED_TARGET fftw3)
  if(TARGET PkgConfig::FFTW3)
    set(FFTW3_LIBRARIES PkgConfig::FFTW3)
    set(FFTW3_FOUND TRUE)
  endif()
endif()

if(NOT FFTW3_FOUND)
  find_path(FFTW3_INCLUDE_DIR NAMES fftw3.h)
  find_library(FFTW3_LIBRARY NAMES fftw3)
  if(FFTW3_INCLUDE_DIR AND FFTW3_LIBRARY)
    set(FFTW3_FOUND TRUE)
    set(FFTW3_INCLUDE_DIRS ${FFTW3_INCLUDE_DIR})
    set(FFTW3_LIBRARIES ${FFTW3_LIBRARY})
  endif()
endif()

if(NOT FFTW3_FOUND)
  message(FATAL_ERROR "FFTW3 library not found.")
endif()

# Centralized Fortran Module Directory
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/modules)
include_directories(${CMAKE_Fortran_MODULE_DIRECTORY})

# Define targets
add_subdirectory(src)

# Enable testing framework
enable_testing()
add_subdirectory(tests)

# Compile examples
add_subdirectory(examples)

# Copy the utils directory to the build directory at build time
add_custom_target(copy_utils ALL
  COMMAND ${CMAKE_COMMAND} -E copy_directory
    "${CMAKE_CURRENT_SOURCE_DIR}/utils"
    "${CMAKE_CURRENT_BINARY_DIR}/utils"
  COMMENT "Copying utils directory to build tree"
)


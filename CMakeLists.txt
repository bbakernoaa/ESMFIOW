cmake_minimum_required(VERSION 3.18)

project(ESMFIOW
    VERSION 0.1.0
    DESCRIPTION "Fortran library wrapping ESMF I/O capabilities with netCDF CF convention support"
    LANGUAGES Fortran
)

# Set Fortran standard
set(CMAKE_Fortran_STANDARD 2008)
set(CMAKE_Fortran_STANDARD_REQUIRED ON)

# Set default build type
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release)
endif()

# Build options
option(BUILD_SHARED_LIBS "Build shared libraries" ON)
option(BUILD_EXAMPLES "Build examples" ON)
option(BUILD_TESTS "Build tests" ON)
option(BUILD_DOCS "Build documentation" ON)

# Find required dependencies
find_package(PkgConfig REQUIRED)

# Find ESMF
find_package(ESMF QUIET)
if(NOT ESMF_FOUND)
    # Try using pkg-config
    pkg_check_modules(ESMF REQUIRED esmf)
    if(ESMF_FOUND)
        set(ESMF_LIBRARIES ${ESMF_LIBRARIES})
        set(ESMF_INCLUDE_DIRS ${ESMF_INCLUDE_DIRS})
    else()
        message(FATAL_ERROR "ESMF library not found. Please ensure ESMF is installed and ESMFMKFILE is set.")
    endif()
endif()

# Find NetCDF-Fortran
find_package(PkgConfig REQUIRED)
pkg_check_modules(NETCDF_FORTRAN REQUIRED netcdf-fortran)
if(NOT NETCDF_FORTRAN_FOUND)
    message(FATAL_ERROR "NetCDF-Fortran library not found. Please install netcdf-fortran development package.")
endif()

# Include directories
include_directories(${ESMF_INCLUDE_DIRS})
include_directories(${NETCDF_FORTRAN_INCLUDE_DIRS})

# Compiler flags
if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -Wall -Wextra -pedantic")
    set(CMAKE_Fortran_FLAGS_DEBUG "-g -O0 -fcheck=all -fbacktrace")
    set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -march=native")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -warn all -check all")
    set(CMAKE_Fortran_FLAGS_DEBUG "-g -O0 -traceback")
    set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -xHost")
endif()

# Create the main library
add_library(esmfiow)

# Add source files
target_sources(esmfiow PRIVATE
    src/esmfiow.f90
    src/esmfiow_constants.f90
    src/esmfiow_cf_utils.f90
    src/esmfiow_io.f90
    src/esmfiow_grid.f90
    src/esmfiow_field.f90
)

# Set target properties
set_target_properties(esmfiow PROPERTIES
    FORTRAN_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/modules
    VERSION ${PROJECT_VERSION}
    SOVERSION ${PROJECT_VERSION_MAJOR}
)

# Link libraries
target_link_libraries(esmfiow PUBLIC
    ${ESMF_LIBRARIES}
    ${NETCDF_FORTRAN_LIBRARIES}
)

# Include directories for the library
target_include_directories(esmfiow PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src>
    $<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/modules>
    $<INSTALL_INTERFACE:include>
    ${ESMF_INCLUDE_DIRS}
    ${NETCDF_FORTRAN_INCLUDE_DIRS}
)

# Install targets
install(TARGETS esmfiow
    EXPORT esmfiow-targets
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
    RUNTIME DESTINATION bin
    INCLUDES DESTINATION include
)

# Install Fortran modules
install(DIRECTORY ${CMAKE_BINARY_DIR}/modules/
    DESTINATION include
    FILES_MATCHING PATTERN "*.mod"
)

# Install headers if any
install(DIRECTORY src/
    DESTINATION include
    FILES_MATCHING PATTERN "*.h" PATTERN "*.inc"
)

# Create and install config files
include(CMakePackageConfigHelpers)

configure_package_config_file(
    "${CMAKE_CURRENT_SOURCE_DIR}/cmake/esmfiow-config.cmake.in"
    "${CMAKE_CURRENT_BINARY_DIR}/esmfiow-config.cmake"
    INSTALL_DESTINATION lib/cmake/esmfiow
)

write_basic_package_version_file(
    "${CMAKE_CURRENT_BINARY_DIR}/esmfiow-config-version.cmake"
    VERSION ${PROJECT_VERSION}
    COMPATIBILITY SameMajorVersion
)

install(FILES
    "${CMAKE_CURRENT_BINARY_DIR}/esmfiow-config.cmake"
    "${CMAKE_CURRENT_BINARY_DIR}/esmfiow-config-version.cmake"
    DESTINATION lib/cmake/esmfiow
)

install(EXPORT esmfiow-targets
    FILE esmfiow-targets.cmake
    NAMESPACE esmfiow::
    DESTINATION lib/cmake/esmfiow
)

# Build examples
if(BUILD_EXAMPLES)
    add_subdirectory(example)
endif()

# Build tests
if(BUILD_TESTS)
    enable_testing()
    add_subdirectory(test)
endif()

# Build documentation
if(BUILD_DOCS)
    add_subdirectory(doc)
endif()

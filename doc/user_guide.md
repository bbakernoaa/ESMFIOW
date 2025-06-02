# ESMFIOW User Guide

## Table of Contents

1. [Introduction](#introduction)
2. [Installation](#installation)
3. [Integration Methods](#integration-methods)
4. [Basic Usage](#basic-usage)
5. [Advanced Features](#advanced-features)
6. [Examples](#examples)
7. [Performance Optimization](#performance-optimization)
8. [Troubleshooting](#troubleshooting)

## Introduction

The Earth System Model Framework I/O Wrapper (ESMFIOW) library provides a modern, efficient interface for handling I/O operations in Earth System Models. This guide will walk you through integrating ESMFIOW into your projects and using its advanced features for optimized data storage and retrieval.

### Key Benefits

- **Simplified API**: Easy-to-use interface that abstracts complex NetCDF4 operations
- **Advanced Compression**: Built-in support for multiple compression algorithms
- **ESMF Integration**: Seamless integration with Earth System Modeling Framework
- **Parallel I/O**: Optimized for both sequential and parallel execution
- **CF Compliance**: Automatic Climate and Forecast metadata handling

## Installation

### Prerequisites

Before installing ESMFIOW, ensure you have the following dependencies:

```bash
# Required dependencies
- Modern Fortran compiler (gfortran 9.0+, ifort 19.0+)
- NetCDF4 library with HDF5 support
- ESMF library (8.6.0 or later)
- MPI library (for parallel features)

# Optional dependencies
- CMake 3.15+ (for CMake build)
- Fortran Package Manager (fpm) (for fpm build)
```

### Building from Source

#### Option 1: CMake Build (Recommended)

```bash
# Clone the repository
git clone https://github.com/your-org/ESMFIOW.git
cd ESMFIOW

# Configure and build
mkdir build && cd build
cmake .. -DCMAKE_BUILD_TYPE=Release \
         -DBUILD_EXAMPLES=ON \
         -DBUILD_TESTS=ON
make -j4

# Install
make install
```

#### Option 2: Fortran Package Manager (fpm)

```bash
# Clone the repository
git clone https://github.com/your-org/ESMFIOW.git
cd ESMFIOW

# Build with fpm
fpm build --profile release

# Run tests
fpm test
```

### Environment Setup

Add the following to your environment:

```bash
# For CMake installation
export ESMFIOW_ROOT=/path/to/esmfiow/install
export LD_LIBRARY_PATH=$ESMFIOW_ROOT/lib:$LD_LIBRARY_PATH

# For module files
export ESMFIOW_MOD=$ESMFIOW_ROOT/include
```

## Integration Methods

### CMake Integration

Create a `CMakeLists.txt` file for your project:

```cmake
cmake_minimum_required(VERSION 3.15)
project(MyEarthSystemModel LANGUAGES Fortran)

# Find ESMFIOW
find_package(ESMFIOW REQUIRED)

# Create your executable
add_executable(my_model src/main.f90)

# Link ESMFIOW
target_link_libraries(my_model PRIVATE ESMFIOW::esmfiow)

# Set Fortran standard
set_property(TARGET my_model PROPERTY Fortran_STANDARD 2008)
```

### Manual Compilation

```bash
# Compile with gfortran
gfortran -I$ESMFIOW_MOD my_program.f90 -L$ESMFIOW_ROOT/lib -lesmfiow -lnetcdff -lnetcdf -lhdf5

# Compile with Intel Fortran
ifort -I$ESMFIOW_MOD my_program.f90 -L$ESMFIOW_ROOT/lib -lesmfiow -lnetcdff -lnetcdf -lhdf5
```

### FPM Integration

Add ESMFIOW as a dependency in your `fpm.toml`:

```toml
[dependencies]
esmfiow = { git = "https://github.com/your-org/ESMFIOW.git", tag = "v1.0.0" }
```

## Basic Usage

### Minimal Example

```fortran
program minimal_example
    use esmfiow
    implicit none

    integer :: rc
    type(esmfiow_file_handle) :: file_handle
    real, allocatable :: temperature(:,:)

    ! Initialize ESMFIOW
    call esmfiow_initialize(rc)
    if (rc /= ESMFIOW_SUCCESS) stop "Failed to initialize ESMFIOW"

    ! Create sample data
    allocate(temperature(100, 50))
    temperature = 273.15 + sin(real([(i, i=1,size(temperature))]))

    ! Create a new NetCDF file with compression
    call esmfiow_file_create("output.nc", file_handle, &
                           title="Temperature Data", &
                           rc=rc)
    if (rc /= ESMFIOW_SUCCESS) stop "Failed to create file"

    ! Write temperature field with automatic compression
    call esmfiow_write_field(file_handle, temperature, "temperature", &
                           long_name="Air Temperature", &
                           units="K", &
                           rc=rc)
    if (rc /= ESMFIOW_SUCCESS) stop "Failed to write field"

    ! Clean up
    call esmfiow_file_close(file_handle, rc)
    call esmfiow_finalize(rc)

    print *, "Successfully created output.nc with compressed temperature data"
end program minimal_example
```

### File Operations

#### Creating Files

```fortran
! Basic file creation
call esmfiow_file_create("data.nc", file_handle, rc=rc)

! File with metadata
call esmfiow_file_create("data.nc", file_handle, &
                       title="My Dataset", &
                       institution="My Organization", &
                       source="Model Output", &
                       rc=rc)

! File with custom compression settings
type(esmfiow_nc4_compression_settings) :: comp_settings
comp_settings%deflate = .true.
comp_settings%deflate_level = 6
comp_settings%shuffle = .true.
comp_settings%fletcher32 = .true.

call esmfiow_file_create("data.nc", file_handle, &
                       compression=comp_settings, &
                       rc=rc)
```

#### Opening Existing Files

```fortran
! Read-only access
call esmfiow_file_open("input.nc", file_handle, "r", rc=rc)

! Read-write access
call esmfiow_file_open("data.nc", file_handle, "a", rc=rc)
```

### Field I/O Operations

#### Writing Fields

```fortran
! 2D field
real :: temperature(100, 50)
call esmfiow_write_field(file_handle, temperature, "temperature", &
                       long_name="Air Temperature", &
                       units="K", &
                       rc=rc)

! 3D field with time dimension
real :: pressure(100, 50, 10)
call esmfiow_write_field(file_handle, pressure, "pressure", &
                       long_name="Atmospheric Pressure", &
                       units="Pa", &
                       dims=["lon", "lat", "time"], &
                       rc=rc)

! Field with custom attributes
call esmfiow_write_field(file_handle, temperature, "temp", &
                       long_name="Air Temperature", &
                       units="K", &
                       valid_range=[200.0, 350.0], &
                       fill_value=-999.0, &
                       rc=rc)
```

#### Reading Fields

```fortran
! Read entire field
real, allocatable :: data(:,:)
call esmfiow_read_field(file_handle, "temperature", data, rc=rc)

! Read field subset
integer :: start(2) = [10, 20]
integer :: count(2) = [50, 30]
call esmfiow_read_field(file_handle, "temperature", data, &
                      start=start, count=count, rc=rc)
```

## Advanced Features

### Custom Compression Settings

```fortran
type(esmfiow_nc4_compression_settings) :: compression
type(esmfiow_nc4_chunking_settings) :: chunking

! Configure compression
compression%deflate = .true.
compression%deflate_level = 9  ! Maximum compression
compression%shuffle = .true.   ! Improve compression ratio
compression%fletcher32 = .true. ! Add checksums

! Configure chunking for optimal access patterns
chunking%chunk_sizes = [50, 25]  ! Optimize for this access pattern
chunking%auto_chunking = .false.

call esmfiow_nc4_set_compression(file_handle, "temperature", &
                                compression, chunking, rc=rc)
```

### ESMF Grid Integration

```fortran
use ESMF
use esmfiow

type(ESMF_Grid) :: grid
type(ESMF_Field) :: field
type(esmfiow_file_handle) :: file_handle

! Create ESMF grid
grid = ESMF_GridCreateNoPeriDim(minIndex=[1,1], maxIndex=[100,50], rc=rc)

! Create ESMF field
field = ESMF_FieldCreate(grid, ESMF_TYPEKIND_R4, rc=rc)

! Write ESMF field directly
call esmfiow_esmf_write_field(file_handle, field, "temperature", rc=rc)

! Read into ESMF field
call esmfiow_esmf_read_field(file_handle, "temperature", field, rc=rc)
```

### Parallel I/O

```fortran
program parallel_example
    use mpi
    use esmfiow
    implicit none

    integer :: ierr, rank, size
    type(esmfiow_file_handle) :: file_handle
    real, allocatable :: local_data(:,:)

    ! Initialize MPI
    call MPI_Init(ierr)
    call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
    call MPI_Comm_size(MPI_COMM_WORLD, size, ierr)

    ! Initialize ESMFIOW with MPI
    call esmfiow_initialize(comm=MPI_COMM_WORLD, rc=rc)

    ! Create parallel file
    call esmfiow_file_create("parallel_output.nc", file_handle, &
                           parallel=.true., rc=rc)

    ! Each process writes its portion
    allocate(local_data(100/size, 50))
    ! ... populate local_data ...

    call esmfiow_write_field_parallel(file_handle, local_data, "data", &
                                     global_start=[rank*100/size+1, 1], &
                                     rc=rc)

    call esmfiow_file_close(file_handle, rc)
    call esmfiow_finalize(rc)
    call MPI_Finalize(ierr)
end program parallel_example
```

### Climate and Forecast (CF) Conventions

```fortran
! Automatic CF-compliant coordinate creation
call esmfiow_cf_add_coordinates(file_handle, &
                              lon_values=longitude, &
                              lat_values=latitude, &
                              time_values=time_array, &
                              time_units="days since 2000-01-01", &
                              rc=rc)

! Add CF global attributes
call esmfiow_cf_add_global_attributes(file_handle, &
                                    title="Model Output", &
                                    institution="Research Center", &
                                    conventions="CF-1.8", &
                                    rc=rc)

! CF-compliant variable attributes
call esmfiow_cf_add_variable_attributes(file_handle, "temperature", &
                                      standard_name="air_temperature", &
                                      long_name="Air Temperature", &
                                      units="K", &
                                      coordinates="lon lat time", &
                                      rc=rc)
```

## Examples

### Example 1: Weather Model Output

```fortran
program weather_model
    use esmfiow
    implicit none

    ! Model parameters
    integer, parameter :: nx = 360, ny = 180, nt = 24
    real, parameter :: missing_value = -999.0

    ! Variables
    type(esmfiow_file_handle) :: file_handle
    real :: temperature(nx, ny, nt)
    real :: pressure(nx, ny, nt)
    real :: humidity(nx, ny, nt)
    real :: longitude(nx), latitude(ny)
    real :: time(nt)
    integer :: rc, i, j, t

    ! Initialize library
    call esmfiow_initialize(rc)

    ! Generate sample data
    do i = 1, nx
        longitude(i) = -180.0 + (i-1) * 360.0 / nx
    end do

    do j = 1, ny
        latitude(j) = -90.0 + (j-1) * 180.0 / ny
    end do

    do t = 1, nt
        time(t) = real(t-1)  ! Hours since start
    end do

    ! Generate synthetic weather data
    do t = 1, nt
        do j = 1, ny
            do i = 1, nx
                temperature(i,j,t) = 273.15 + 20.0 * sin(latitude(j)*3.14159/180.0) + &
                                   5.0 * sin(time(t)*3.14159/12.0)
                pressure(i,j,t) = 101325.0 - 1000.0 * abs(latitude(j))/90.0
                humidity(i,j,t) = 0.5 + 0.3 * sin(longitude(i)*3.14159/180.0)
            end do
        end do
    end do

    ! Create output file with compression
    call esmfiow_file_create("weather_output.nc", file_handle, &
                           title="Weather Model Output", &
                           institution="Weather Service", &
                           source="Atmospheric Model v2.0", &
                           rc=rc)

    ! Add CF-compliant coordinates
    call esmfiow_cf_add_coordinates(file_handle, &
                                  lon_values=longitude, &
                                  lat_values=latitude, &
                                  time_values=time, &
                                  time_units="hours since 2024-01-01 00:00:00", &
                                  rc=rc)

    ! Write temperature field
    call esmfiow_write_field(file_handle, temperature, "temperature", &
                           standard_name="air_temperature", &
                           long_name="Air Temperature", &
                           units="K", &
                           dims=["lon ", "lat ", "time"], &
                           coordinates="lon lat time", &
                           fill_value=missing_value, &
                           rc=rc)

    ! Write pressure field
    call esmfiow_write_field(file_handle, pressure, "pressure", &
                           standard_name="air_pressure", &
                           long_name="Atmospheric Pressure", &
                           units="Pa", &
                           dims=["lon ", "lat ", "time"], &
                           coordinates="lon lat time", &
                           rc=rc)

    ! Write humidity field
    call esmfiow_write_field(file_handle, humidity, "humidity", &
                           standard_name="relative_humidity", &
                           long_name="Relative Humidity", &
                           units="1", &
                           dims=["lon ", "lat ", "time"], &
                           coordinates="lon lat time", &
                           valid_range=[0.0, 1.0], &
                           rc=rc)

    ! Close file and finalize
    call esmfiow_file_close(file_handle, rc)
    call esmfiow_finalize(rc)

    print *, "Successfully created weather_output.nc"
    print *, "File size optimized with automatic compression"
end program weather_model
```

### Example 2: Ocean Model with Custom Compression

```fortran
program ocean_model
    use esmfiow
    implicit none

    integer, parameter :: nx = 1440, ny = 720, nz = 50
    type(esmfiow_file_handle) :: file_handle
    type(esmfiow_nc4_compression_settings) :: compression
    type(esmfiow_nc4_chunking_settings) :: chunking
    real :: sea_surface_temperature(nx, ny)
    real :: salinity(nx, ny, nz)
    integer :: rc

    call esmfiow_initialize(rc)

    ! Generate ocean data
    call generate_ocean_data(sea_surface_temperature, salinity)

    ! Create file with high compression for large datasets
    call esmfiow_file_create("ocean_output.nc", file_handle, &
                           title="Ocean Model Output", rc=rc)

    ! Configure high compression for SST (2D field)
    compression%deflate = .true.
    compression%deflate_level = 9
    compression%shuffle = .true.
    compression%fletcher32 = .true.

    chunking%chunk_sizes = [180, 90]  ! Optimize for regional access
    chunking%auto_chunking = .false.

    call esmfiow_write_field(file_handle, sea_surface_temperature, "sst", &
                           long_name="Sea Surface Temperature", &
                           units="K", &
                           compression=compression, &
                           chunking=chunking, &
                           rc=rc)

    ! Configure different compression for 3D salinity field
    chunking%chunk_sizes = [90, 45, 10]  ! 3D chunking

    call esmfiow_write_field(file_handle, salinity, "salinity", &
                           long_name="Sea Water Salinity", &
                           units="psu", &
                           dims=["lon", "lat", "depth"], &
                           compression=compression, &
                           chunking=chunking, &
                           rc=rc)

    call esmfiow_file_close(file_handle, rc)
    call esmfiow_finalize(rc)

contains
    subroutine generate_ocean_data(sst, sal)
        real, intent(out) :: sst(:,:), sal(:,:,:)
        integer :: i, j, k

        ! Generate synthetic ocean data
        do j = 1, size(sst, 2)
            do i = 1, size(sst, 1)
                sst(i,j) = 273.15 + 25.0 * exp(-((j-size(sst,2)/2)**2)/(size(sst,2)/4)**2)
            end do
        end do

        do k = 1, size(sal, 3)
            do j = 1, size(sal, 2)
                do i = 1, size(sal, 1)
                    sal(i,j,k) = 35.0 + 2.0 * sin(real(i)/size(sal,1)*6.28) - real(k)/size(sal,3)*5.0
                end do
            end do
        end do
    end subroutine generate_ocean_data
end program ocean_model
```

### Example 3: Parallel Earth System Model

```fortran
program parallel_earth_system
    use mpi
    use esmfiow
    use ESMF
    implicit none

    ! MPI variables
    integer :: ierr, rank, nprocs

    ! ESMF variables
    type(ESMF_VM) :: vm
    type(ESMF_Grid) :: grid
    type(ESMF_Field) :: temp_field, precip_field

    ! ESMFIOW variables
    type(esmfiow_file_handle) :: file_handle
    integer :: rc

    ! Initialize MPI and ESMF
    call MPI_Init(ierr)
    call ESMF_Initialize(vm=vm, rc=rc)
    call ESMF_VMGet(vm, localPet=rank, petCount=nprocs, rc=rc)

    ! Initialize ESMFIOW
    call esmfiow_initialize(comm=MPI_COMM_WORLD, rc=rc)

    ! Create ESMF grid (distributed)
    grid = ESMF_GridCreateNoPeriDim( &
        minIndex=[1, 1], &
        maxIndex=[360, 180], &
        regDecomp=[nprocs, 1], &
        rc=rc)

    ! Create ESMF fields
    temp_field = ESMF_FieldCreate(grid, ESMF_TYPEKIND_R4, &
                                name="temperature", rc=rc)
    precip_field = ESMF_FieldCreate(grid, ESMF_TYPEKIND_R4, &
                                  name="precipitation", rc=rc)

    ! Initialize fields with model data
    call initialize_model_fields(temp_field, precip_field, rank)

    ! Create parallel output file
    call esmfiow_file_create("earth_system_output.nc", file_handle, &
                           title="Earth System Model Output", &
                           parallel=.true., &
                           rc=rc)

    ! Write ESMF fields directly (parallel I/O)
    call esmfiow_esmf_write_field(file_handle, temp_field, "temperature", &
                                standard_name="air_temperature", &
                                long_name="Air Temperature", &
                                units="K", &
                                rc=rc)

    call esmfiow_esmf_write_field(file_handle, precip_field, "precipitation", &
                                standard_name="precipitation_rate", &
                                long_name="Precipitation Rate", &
                                units="kg m-2 s-1", &
                                rc=rc)

    ! Cleanup
    call esmfiow_file_close(file_handle, rc)
    call ESMF_FieldDestroy(temp_field, rc=rc)
    call ESMF_FieldDestroy(precip_field, rc=rc)
    call ESMF_GridDestroy(grid, rc=rc)

    call esmfiow_finalize(rc)
    call ESMF_Finalize(rc=rc)
    call MPI_Finalize(ierr)

    if (rank == 0) then
        print *, "Parallel Earth System Model output complete"
        print *, "Data written by", nprocs, "processes"
    end if

contains
    subroutine initialize_model_fields(temp_field, precip_field, rank)
        type(ESMF_Field), intent(inout) :: temp_field, precip_field
        integer, intent(in) :: rank

        real, pointer :: temp_ptr(:,:), precip_ptr(:,:)
        integer :: i, j, rc

        ! Get field data pointers
        call ESMF_FieldGet(temp_field, farrayPtr=temp_ptr, rc=rc)
        call ESMF_FieldGet(precip_field, farrayPtr=precip_ptr, rc=rc)

        ! Initialize with rank-dependent data
        do j = 1, size(temp_ptr, 2)
            do i = 1, size(temp_ptr, 1)
                temp_ptr(i,j) = 273.15 + 20.0 + real(rank) * 0.1
                precip_ptr(i,j) = 1e-5 * (1.0 + real(rank) * 0.01)
            end do
        end do
    end subroutine initialize_model_fields
end program parallel_earth_system
```

## Performance Optimization

### Compression Strategy

Choose compression settings based on your use case:

```fortran
! For maximum compression (slower I/O)
compression%deflate_level = 9
compression%shuffle = .true.

! For balanced compression/speed
compression%deflate_level = 6
compression%shuffle = .true.

! For fastest I/O (minimal compression)
compression%deflate_level = 1
compression%shuffle = .false.
```

### Chunking Optimization

```fortran
! For time-series access (reading all times at one location)
chunking%chunk_sizes = [1, 1, unlimited_size]

! For spatial analysis (reading one time slice)
chunking%chunk_sizes = [full_nx, full_ny, 1]

! For regional analysis
chunking%chunk_sizes = [nx/4, ny/4, 1]
```

### Memory Management

```fortran
! For large datasets, process in chunks
integer, parameter :: chunk_size = 1000000
real, allocatable :: data_chunk(:)
integer :: start_idx, end_idx, total_size

total_size = nx * ny * nz
allocate(data_chunk(chunk_size))

do start_idx = 1, total_size, chunk_size
    end_idx = min(start_idx + chunk_size - 1, total_size)

    ! Process chunk
    call process_data_chunk(data_chunk(1:end_idx-start_idx+1))

    ! Write chunk
    call esmfiow_write_field_chunk(file_handle, data_chunk, &
                                 start=[start_idx], &
                                 count=[end_idx-start_idx+1], &
                                 rc=rc)
end do
```

## Troubleshooting

### Common Issues

#### 1. NetCDF Library Not Found

```bash
# Ensure NetCDF is properly installed
export NETCDF_ROOT=/path/to/netcdf
export LD_LIBRARY_PATH=$NETCDF_ROOT/lib:$LD_LIBRARY_PATH
export PKG_CONFIG_PATH=$NETCDF_ROOT/lib/pkgconfig:$PKG_CONFIG_PATH
```

#### 2. ESMF Integration Issues

```bash
# Ensure ESMF is properly configured
export ESMFMKFILE=/path/to/esmf/lib/esmf.mk
source $ESMFMKFILE
```

#### 3. Parallel I/O Problems

```fortran
! Check MPI initialization before ESMFIOW
call MPI_Initialized(flag, ierr)
if (.not. flag) then
    print *, "ERROR: MPI must be initialized before ESMFIOW"
    stop 1
end if
```

#### 4. Memory Issues with Large Arrays

```fortran
! Use allocatable arrays and check allocation
allocate(large_array(nx, ny, nz), stat=ierr)
if (ierr /= 0) then
    print *, "ERROR: Failed to allocate memory for array"
    print *, "Required size:", nx*ny*nz*4, "bytes"
    stop 1
end if
```

### Error Handling Best Practices

```fortran
! Always check return codes
call esmfiow_function(args, rc=rc)
if (rc /= ESMFIOW_SUCCESS) then
    print *, "ERROR in esmfiow_function, rc =", rc
    call esmfiow_error_message(rc, error_msg)
    print *, "Error message:", trim(error_msg)
    ! Handle error appropriately
end if

! Use error handling wrapper
subroutine check_error(rc, operation)
    integer, intent(in) :: rc
    character(*), intent(in) :: operation
    character(256) :: error_msg

    if (rc /= ESMFIOW_SUCCESS) then
        call esmfiow_error_message(rc, error_msg)
        print *, "ERROR in ", operation, ": ", trim(error_msg)
        stop 1
    end if
end subroutine check_error
```

### Performance Debugging

```fortran
! Enable timing information
call esmfiow_set_debug_level(ESMFIOW_DEBUG_TIMING)

! Profile compression performance
call system_clock(start_time)
call esmfiow_write_field(file_handle, data, "field", rc=rc)
call system_clock(end_time, clock_rate)
write_time = real(end_time - start_time) / real(clock_rate)
print *, "Write time:", write_time, "seconds"
```

## API Reference

For complete API documentation, see the [generated Doxygen documentation](doxygen/html/index.html).

## Support

For questions, bug reports, and feature requests:
- GitHub Issues: [ESMFIOW Issues](https://github.com/your-org/ESMFIOW/issues)
- Documentation: [Online Docs](https://your-org.github.io/ESMFIOW/)
- Email: esmfiow-support@your-org.edu

## License

ESMFIOW is released under the MIT License. See the [LICENSE](../LICENSE) file for details.

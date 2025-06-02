# ESMFIOW - ESMF I/O Wrapper Library

[![Build Status](https://img.shields.io/badge/build-passing-brightgreen)](https://github.com/username/esmfiow)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![Fortran](https://img.shields.io/badge/Fortran-2008+-blue.svg)](https://fortran-lang.org/)
[![Docker](https://img.shields.io/badge/Docker-Ready-blue.svg)](https://hub.docker.com/r/noaaepic/ubuntu22.04-intel-unified)

ESMFIOW is a high-level Fortran library that provides a simplified interface for ESMF (Earth System Modeling Framework) I/O capabilities with automatic netCDF CF (Climate and Forecast) convention compliance and NetCDF4 compression support.

## Features

- **CF Convention Compliance**: Automatic generation of CF-compliant netCDF files
- **NetCDF4 Compression**: Built-in support for chunking, compression, and filters
- **ESMF Integration**: Seamless integration with ESMF grids and fields
- **High-Level Interface**: Simplified API for common I/O operations
- **Parallel I/O Support**: MPI-enabled parallel file operations
- **Modular Design**: Clean separation of concerns with multiple modules
- **Docker Development**: Complete Docker-based development environment
- **Production Ready**: Comprehensive testing and CI/CD pipeline

## Quick Start

### Option 1: Docker Development (Recommended)

The easiest way to get started is using our Docker-based development environment with the NOAA EPIC image that includes Intel compilers and all required scientific libraries.

```bash
# Clone the repository
git clone https://github.com/username/esmfiow.git
cd esmfiow

# Set up Docker development environment
./scripts/setup-dev.sh docker-setup

# Quick build and test
./scripts/setup-dev.sh quick-start

# Interactive development shell
make docker-shell
```

### Option 2: Using Make with Docker

```bash
# Build the library
make docker-build

# Run tests
make docker-test

# Interactive development
make docker-shell

# Clean and rebuild
make docker-clean && make docker-build
```

### Option 3: Using Docker Script Directly

```bash
# Build in release mode
./scripts/docker-build.sh build

# Build in debug mode
./scripts/docker-build.sh --debug build

# Run tests
./scripts/docker-build.sh test

# Interactive shell
./scripts/docker-build.sh shell
```

### Option 4: Native Installation

#### Prerequisites

- Fortran compiler (Intel ifort recommended, gfortran 9.0+ supported)
- ESMF library (8.0+) with NetCDF support
- NetCDF-Fortran library (4.5+)
- HDF5 library (1.10+)
- CMake (3.12+)
- MPI library (for parallel support)

#### Native Build

```bash
git clone https://github.com/username/esmfiow.git
cd esmfiow

# Check your environment
./scripts/setup-dev.sh check

# Set up native environment (if needed)
./scripts/setup-dev.sh native-setup

# Build using Make
make build

# Or build using CMake directly
mkdir build && cd build
cmake .. -DCMAKE_BUILD_TYPE=Release
make -j4
make install
```

### Basic Usage

```fortran
program example
    use esmfiow
    implicit none

    integer :: rc
    type(esmfiow_file_handle) :: file_handle

    ! Initialize library
    call esmfiow_initialize(rc)

    ! Create CF-compliant file
    call esmfiow_file_create("output.nc", file_handle, &
                           title="My Climate Data", &
                           institution="My Institution", &
                           rc=rc)

    ! Write ESMF fields (implementation specific)
    ! call esmfiow_write_field(file_handle, my_field, rc)

    ! Clean up
    call esmfiow_file_close(file_handle, rc)
    call esmfiow_finalize(rc)

end program example
```

## Docker Development Environment

ESMFIOW includes a complete Docker-based development environment using the NOAA EPIC image, which provides:

- **Intel oneAPI Compilers**: ifort, icc, icpc with optimizations
- **Scientific Libraries**: Pre-installed ESMF, NetCDF4, HDF5, PNetCDF
- **MPI Support**: Intel MPI for parallel computing
- **Build Tools**: CMake, Make, and other development utilities

### Docker Environment Commands

| Command | Description |
|---------|-------------|
| `make docker-build` | Build library in Release mode |
| `make docker-debug` | Build library in Debug mode |
| `make docker-test` | Run comprehensive test suite |
| `make docker-shell` | Interactive development shell |
| `make docker-clean` | Clean build artifacts |
| `make help` | Show all available targets |

### Development Workflow

1. **Start Development Session**:
   ```bash
   # Enter interactive development environment
   make docker-shell
   
   # Inside container, you have access to:
   # - Intel compilers (ifort, icc, icpc)
   # - ESMF library with NetCDF support
   # - All build tools and scientific libraries
   ```

2. **Build and Test Cycle**:
   ```bash
   # Quick debug build and test
   make docker-debug && make docker-test
   
   # Release build
   make docker-build
   
   # Clean rebuild
   make docker-clean && make docker-build
   ```

3. **Advanced Docker Usage**:
   ```bash
   # Build with specific options
   ./scripts/docker-build.sh --debug --jobs 8 build
   
   # Run specific test categories
   ./scripts/docker-build.sh test
   
   # Pull latest base image
   ./scripts/docker-build.sh pull
   ```

### Docker Compose Alternative

For persistent development, you can use Docker Compose:

```bash
# Start development environment
docker-compose up -d esmfiow-dev

# Attach to running container
docker-compose exec esmfiow-dev bash

# Run tests in separate container
docker-compose run --rm esmfiow-test bash -c "cd build && ctest"
```

### VS Code Integration

For VS Code users, the Docker environment can be used with the Remote-Containers extension:

1. Open the project in VS Code
2. Install the "Remote - Containers" extension
3. Use Command Palette: "Remote-Containers: Reopen in Container"
4. The development environment will be automatically configured

## Library Structure

### Core Modules

- **`esmfiow`**: Main module providing the high-level interface
- **`esmfiow_constants`**: Library constants and parameter definitions
- **`esmfiow_cf_utils`**: CF convention utilities and validation
- **`esmfiow_io`**: File I/O operations and handle management
- **`esmfiow_grid`**: ESMF grid to netCDF conversion utilities
- **`esmfiow_field`**: ESMF field I/O operations

### Key Features

#### CF Convention Support
- Automatic global attributes
- Standard coordinate variables
- Proper units and metadata
- CF-compliant variable naming
- Missing value handling

#### ESMF Integration
- Direct support for ESMF grids
- ESMF field I/O operations
- Coordinate system mapping
- Regridding support (planned)

## API Reference

### File Operations

```fortran
! Create a new CF-compliant file
call esmfiow_file_create(filename, file_handle [, title, institution, ...], rc)

! Open existing file
call esmfiow_file_open(filename, file_handle [, readonly], rc)

! Close file
call esmfiow_file_close(file_handle, rc)
```

### Field Operations

```fortran
! Write ESMF field to file
call esmfiow_write_field(file_handle, field, variable_name, rc)

! Read field from file
call esmfiow_read_field(file_handle, variable_name, field, rc)
```

### Grid Operations

```fortran
! Write grid coordinates to file
call esmfiow_grid_to_netcdf(file_handle, grid, rc)

! Read grid from file
call esmfiow_grid_from_netcdf(file_handle, grid, rc)
```

## Examples

The `example/` directory contains several demonstration programs:

- **`example.f90`**: Basic file creation and management
- **`cf_example.f90`**: CF convention compliance demonstration
- **`grid_example.f90`**: Grid I/O operations

## Testing

Run the test suite:

```bash
# Using CMake
ctest --test-dir build --output-on-failure

# Using FPM
fpm test
```

## Dependencies

### Required
- ESMF (Earth System Modeling Framework) 8.0+
- NetCDF-Fortran 4.5+
- Fortran compiler with 2008 standard support

### Optional
- CMake 3.18+ (for CMake build)
- Fortran Package Manager (for fpm build)

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests for new functionality
5. Submit a pull request

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Acknowledgments

- ESMF development team for the Earth System Modeling Framework
- CF Convention committee for the Climate and Forecast metadata standards
- NetCDF developers for the network Common Data Form library

## Support

- Documentation: [GitHub Wiki](https://github.com/username/esmfiow/wiki)
- Issues: [GitHub Issues](https://github.com/username/esmfiow/issues)
- Discussions: [GitHub Discussions](https://github.com/username/esmfiow/discussions)

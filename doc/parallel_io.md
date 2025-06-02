# ESMFIOW Parallel I/O and Compression Documentation

## Overview

The ESMFIOW library provides high-level interfaces for efficient parallel I/O operations with NetCDF4 compression capabilities. This document describes the parallel I/O features, their implementation, and best practices for achieving optimal performance.

## Parallel I/O Features

ESMFIOW supports parallel I/O through integration with ESMF (Earth System Modeling Framework) and its underlying I/O capabilities. The key features include:

1. **Parallel File Creation**: Create NetCDF4 files that can be written to concurrently by multiple processes.
2. **Parallel Variable Access**: Efficiently read and write variables in parallel with minimal communication overhead.
3. **Compression with Parallel I/O**: Apply NetCDF4 compression to data written in parallel.
4. **Performance Optimization**: Built-in chunking and access pattern optimizations for various parallel workloads.
5. **ESMF Integration**: Seamless integration with ESMF Field, Grid, and Array objects.

## Architecture

The parallel I/O capabilities in ESMFIOW are implemented through a layered approach:

```
+----------------------------------+
|      Application Code            |
+----------------------------------+
|      ESMFIOW ESMF Integration    |
|   (esmfiow_esmf_io module)       |
+----------------------------------+
|   ESMFIOW NetCDF4 Compression    |
|   (esmfiow_netcdf4 module)       |
+----------------------------------+
|      ESMF I/O Layer              |
+----------------------------------+
|      NetCDF4/HDF5/PnetCDF        |
+----------------------------------+
```

## Key Components

### esmfiow_esmf_io Module

The `esmfiow_esmf_io` module provides high-level interfaces for ESMF integrated I/O with compression. The key functions include:

- `esmfiow_esmf_initialize`: Initialize ESMF for use with ESMFIOW
- `esmfiow_esmf_create_io_config`: Create an I/O configuration for ESMF Field I/O
- `esmfiow_esmf_io_write_field`: Write an ESMF Field to a NetCDF file with compression
- `esmfiow_esmf_io_read_field`: Read an ESMF Field from a NetCDF file

### esmfiow_netcdf4 Module

The `esmfiow_netcdf4` module provides lower-level interfaces for NetCDF4 compression and parallel I/O. Key functions include:

- `esmfiow_nc4_create_parallel_file`: Create a NetCDF4 file for parallel access
- `esmfiow_nc4_set_compression`: Set compression settings for a variable
- `esmfiow_nc4_optimize_variable`: Optimize variable chunking based on access pattern

## Using Parallel I/O with ESMFIOW

### Basic Usage

To use parallel I/O in ESMFIOW:

1. Initialize ESMF:
```fortran
call ESMF_Initialize(rc=rc)
```

2. Create an ESMFIOW I/O configuration:
```fortran
type(esmfiow_esmf_io_config) :: io_config
call esmfiow_esmf_create_io_config(io_config, "output.nc", rc=rc)
```

3. Enable parallel I/O:
```fortran
io_config%is_parallel = .true.
io_config%io_strategy = ESMFIOW_ESMF_IO_STRATEGY_NETCDF4
```

4. Set compression settings:
```fortran
io_config%compression%compression_type = ESMFIOW_NC4_COMPRESSION_DEFLATE
io_config%compression%deflate_level = 5
io_config%compression%shuffle = .true.
```

5. Write field data in parallel:
```fortran
call esmfiow_esmf_io_write_field(io_config, field, rc=rc)
```

### Optimizing Performance

For optimal parallel I/O performance:

1. **Chunking Strategy**:
   - Set `io_config%chunking%enable_chunking = .true.`
   - Choose chunking based on your access pattern: sequential vs. random
   - For domain-decomposed data, align chunks with decomposition boundaries

2. **Access Patterns**:
   - Use `ESMFIOW_ESMF_ACCESS_SEQUENTIAL` for time series or structured writes
   - Use `ESMFIOW_ESMF_ACCESS_RANDOM` for irregular or sparse access patterns

3. **Compression Levels**:
   - Lower deflate levels (1-3) provide good performance with reasonable compression
   - Higher levels (4-9) provide better compression but slower performance
   - Enable shuffle filter for improved compression of structured data

4. **Collective vs. Independent I/O**:
   - Collective operations are generally more efficient for large-scale parallel I/O
   - ESMFIOW uses collective operations by default in parallel mode

## Performance Considerations

### Compression vs. Speed Tradeoffs

There is an inherent tradeoff between compression level and I/O performance. Higher compression levels require more CPU time but produce smaller files. For large-scale parallel applications:

1. **I/O Bound Applications**: Use lower compression levels (1-3)
2. **Storage-Limited Applications**: Use higher compression levels (5-9)
3. **Balanced Approach**: Use level 4 with shuffle enabled

### Parallel Scalability

ESMFIOW's parallel I/O capabilities have been designed to scale with the number of processors:

1. **Small Process Counts** (1-64): Excellent scaling, nearly linear speedup
2. **Medium Process Counts** (64-512): Good scaling with proper chunking
3. **Large Process Counts** (512+): Performance dependent on file system and collective I/O implementation

The choice of underlying parallel file system (Lustre, GPFS, etc.) significantly impacts performance.

## Examples

See the `parallel_compression_example.f90` in the examples directory for a complete example demonstrating:

1. Sequential vs. parallel write performance
2. Sequential vs. parallel read performance
3. File size comparisons with different compression settings
4. Scaling performance on multiple processes

## Troubleshooting

### Common Issues

1. **Poor Performance with Default Chunking**:
   - Use `esmfiow_nc4_optimize_variable` to set custom chunking that matches your data access patterns

2. **Slow Compression**:
   - Lower the deflate level or disable compression during development/testing
   - Use shuffle without deflate for numeric data that doesn't compress well

3. **File Corruption with Parallel Writes**:
   - Ensure all processes call `ESMF_VMBarrier` before closing files
   - Check for errors from the `esmfiow_esmf_io_write_field` function

4. **Performance Limitations**:
   - Check your file system configuration for parallel I/O optimization
   - Use the `HDF5_CACHE_SIZE` environment variable to increase cache size

## Building Applications with ESMFIOW Parallel I/O

To build applications that use ESMFIOW's parallel I/O capabilities:

1. Include the appropriate modules:
```fortran
use esmfiow_constants
use esmfiow_netcdf4
use esmfiow_esmf_io
use ESMF
```

2. Link against ESMFIOW and ESMF:
```
-lesmfiow -lesmf
```

3. Compile with MPI support:
```
mpif90 -o my_app my_app.f90 -lesmfiow -lesmf
```

4. Run with MPI:
```
mpirun -np N ./my_app
```

## References

1. NetCDF4 Parallel I/O Documentation: https://www.unidata.ucar.edu/software/netcdf/docs/parallel_io.html
2. ESMF I/O Documentation: https://earthsystemmodeling.org/docs/release/latest/ESMF_refdoc/node5.html#SECTION05080000000000000000
3. HDF5 Parallel I/O: https://portal.hdfgroup.org/display/HDF5/Parallel+HDF5

## Version Information

This documentation applies to ESMFIOW version 1.0.0 and above, which is compatible with ESMF version 8.6.0 and above.

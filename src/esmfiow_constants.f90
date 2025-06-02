!> @file esmfiow_constants.f90
!> @brief ESMFIOW Constants Module
!>
!> This module defines constants used throughout the ESMFIOW library,
!> including status codes, compression settings, and ESMF integration parameters.
!>
!> @author ESMFIOW Development Team
!> @date June 2025
!> @defgroup esmfiow_constants Constants Module
!> @{

!> @brief Core constants for the ESMFIOW library
!>
!> This module defines all constants used throughout the ESMFIOW library,
!> including return codes, string lengths, precision settings, compression
!> constants, and ESMF integration parameters.
module esmfiow_constants
    use iso_fortran_env, only: int32, int64, real32, real64
    implicit none

    private
    public :: ESMFIOW_SUCCESS, ESMFIOW_ERROR, ESMFIOW_WARNING
    public :: ESMFIOW_MAX_STRING_LEN, ESMFIOW_MAX_NAME_LEN
    public :: ESMFIOW_REAL_KIND, ESMFIOW_INT_KIND
    public :: ESMFIOW_CF_VERSION
    public :: ESMFIOW_VERSION, ESMFIOW_ESMF_MIN_VERSION
    public :: ESMFIOW_PARALLEL_CAPABLE
    ! NetCDF4 compression constants
    public :: ESMFIOW_NC4_DEFAULT_DEFLATE_LEVEL, ESMFIOW_NC4_MAX_DEFLATE_LEVEL
    public :: ESMFIOW_NC4_DEFAULT_CHUNK_SIZE, ESMFIOW_NC4_MAX_CHUNK_SIZE
    public :: ESMFIOW_NC4_COMPRESSION_NONE, ESMFIOW_NC4_COMPRESSION_DEFLATE
    public :: ESMFIOW_NC4_COMPRESSION_SZIP, ESMFIOW_NC4_COMPRESSION_LZF
    public :: ESMFIOW_NC4_SHUFFLE_ON, ESMFIOW_NC4_SHUFFLE_OFF
    public :: ESMFIOW_NC4_FLETCHER32_ON, ESMFIOW_NC4_FLETCHER32_OFF
    ! ESMF integration constants
    public :: ESMFIOW_ESMF_ENABLED, ESMFIOW_USE_ESMF_LOGGING
    public :: ESMFIOW_ESMF_IO_STRATEGY_NETCDF4, ESMFIOW_ESMF_IO_STRATEGY_PNETCDF
    public :: ESMFIOW_ESMF_IO_STRATEGY_NETCDF_CLASSIC, ESMFIOW_ESMF_IO_STRATEGY_DEFAULT
    public :: ESMFIOW_ESMF_ACCESS_SEQUENTIAL, ESMFIOW_ESMF_ACCESS_RANDOM

    !> @name Return Codes
    !> @{

    !> @brief Operation completed successfully
    integer, parameter :: ESMFIOW_SUCCESS = 0

    !> @brief Operation failed
    integer, parameter :: ESMFIOW_ERROR = -1

    !> @brief Operation completed with warnings
    integer, parameter :: ESMFIOW_WARNING = 1
    !> @}

    !> @name String Lengths
    !> @{

    !> @brief Maximum length for general strings
    integer, parameter :: ESMFIOW_MAX_STRING_LEN = 256

    !> @brief Maximum length for names (variables, dimensions, etc.)
    integer, parameter :: ESMFIOW_MAX_NAME_LEN = 64
    !> @}

    !> @name Precision Settings
    !> @{

    !> @brief Default real precision (64-bit)
    integer, parameter :: ESMFIOW_REAL_KIND = real64

    !> @brief Default integer precision (32-bit)
    integer, parameter :: ESMFIOW_INT_KIND = int32
    !> @}

    !> @brief CF Convention version supported
    !> @details Climate and Forecast metadata conventions version
    character(len=*), parameter :: ESMFIOW_CF_VERSION = "CF-1.8"

    !> @name Version Information
    !> @{

    !> @brief ESMFIOW library version
    character(len=*), parameter :: ESMFIOW_VERSION = "1.0.0"

    !> @brief Minimum required ESMF version
    character(len=*), parameter :: ESMFIOW_ESMF_MIN_VERSION = "8.6.0"

    !> @brief Flag indicating parallel I/O capability
    logical, parameter :: ESMFIOW_PARALLEL_CAPABLE = .true.
    !> @}

    !> @name NetCDF4 Compression Constants
    !> @{

    !> @brief Default deflate level for NetCDF4 compression
    !> @details Lower value for better performance
    integer, parameter :: ESMFIOW_NC4_DEFAULT_DEFLATE_LEVEL = 1

    !> @brief Maximum deflate level for NetCDF4 compression
    !> @details Higher value for better compression ratio
    integer, parameter :: ESMFIOW_NC4_MAX_DEFLATE_LEVEL = 9

    !> @brief Default chunk size in bytes
    integer, parameter :: ESMFIOW_NC4_DEFAULT_CHUNK_SIZE = 1024

    !> @brief Maximum allowed chunk size
    integer, parameter :: ESMFIOW_NC4_MAX_CHUNK_SIZE = 2147483647
    !> @}

    !> @name Compression Types
    !> @{

    !> @brief No compression
    integer, parameter :: ESMFIOW_NC4_COMPRESSION_NONE = 0

    !> @brief DEFLATE compression (zlib)
    integer, parameter :: ESMFIOW_NC4_COMPRESSION_DEFLATE = 1

    !> @brief SZIP compression (when available)
    integer, parameter :: ESMFIOW_NC4_COMPRESSION_SZIP = 2

    !> @brief LZF compression (when available)
    integer, parameter :: ESMFIOW_NC4_COMPRESSION_LZF = 3
    !> @}

    !> @name Filter Options
    !> @{

    !> @brief Shuffle filter disabled
    integer, parameter :: ESMFIOW_NC4_SHUFFLE_OFF = 0

    !> @brief Shuffle filter enabled
    integer, parameter :: ESMFIOW_NC4_SHUFFLE_ON = 1

    !> @brief Fletcher32 checksum disabled
    integer, parameter :: ESMFIOW_NC4_FLETCHER32_OFF = 0

    !> @brief Fletcher32 checksum enabled
    integer, parameter :: ESMFIOW_NC4_FLETCHER32_ON = 1
    !> @}

    !> @name ESMF Integration Constants
    !> @{

    !> @brief Flag indicating ESMF integration is enabled
    logical, parameter :: ESMFIOW_ESMF_ENABLED = .true.

    !> @brief Flag to use ESMF logging system
    logical, parameter :: ESMFIOW_USE_ESMF_LOGGING = .true.

    !> @brief Default I/O strategy (system dependent)
    integer, parameter :: ESMFIOW_ESMF_IO_STRATEGY_DEFAULT = 0

    !> @brief NetCDF Classic format I/O strategy
    integer, parameter :: ESMFIOW_ESMF_IO_STRATEGY_NETCDF_CLASSIC = 1

    !> @brief NetCDF4/HDF5 format I/O strategy
    integer, parameter :: ESMFIOW_ESMF_IO_STRATEGY_NETCDF4 = 2

    !> @brief Parallel NetCDF (PNetCDF) I/O strategy
    integer, parameter :: ESMFIOW_ESMF_IO_STRATEGY_PNETCDF = 3

    !> @brief Sequential access pattern (optimize for sequential reads/writes)
    integer, parameter :: ESMFIOW_ESMF_ACCESS_SEQUENTIAL = 0

    !> @brief Random access pattern (optimize for random reads/writes)
    integer, parameter :: ESMFIOW_ESMF_ACCESS_RANDOM = 1
    !> @}

end module esmfiow_constants
!> @} End of esmfiow_constants group

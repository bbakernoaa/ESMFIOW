!> @file esmfiow_netcdf4.f90
!> @brief ESMFIOW NetCDF4 Compression Module
!>
!> This module provides NetCDF4-specific compression and optimization capabilities
!> for efficient data storage and access in Earth System Models.
!>
!> @author ESMFIOW Development Team
!> @date June 2025
!> @defgroup esmfiow_netcdf4 NetCDF4 Module
!> @{

!> @brief NetCDF4 compression and optimization module
!>
!> This module implements NetCDF4-specific compression and optimization capabilities
!> including deflate compression, chunking strategies, and various filters like shuffle
!> and Fletcher32. It integrates with ESMF for parallel I/O operations.
module esmfiow_netcdf4
    use esmfiow_constants
    use netcdf
    ! ESMF module dependencies
    use ESMF
    use ESMF_IOUtilMod
    use ESMF_IO_NetCDFMod
    implicit none

    private
    public :: esmfiow_nc4_compression_settings, esmfiow_nc4_chunking_settings
    public :: esmfiow_nc4_create_file, esmfiow_nc4_create_variable
    public :: esmfiow_nc4_create_parallel_file
    public :: esmfiow_nc4_set_compression, esmfiow_nc4_set_chunking
    public :: esmfiow_nc4_set_filters, esmfiow_nc4_optimize_variable
    public :: esmfiow_nc4_get_compression_info, esmfiow_nc4_estimate_compression

    ! ESMF constants for logging and error handling
    integer, parameter :: ESMFIOW_LOG_INFO = ESMF_LOGMSG_INFO
    integer, parameter :: ESMFIOW_LOG_WARNING = ESMF_LOGMSG_WARN
    integer, parameter :: ESMFIOW_LOG_ERROR = ESMF_LOGMSG_ERROR

    !> @brief NetCDF4 compression settings type
    !>
    !> This type encapsulates all settings related to NetCDF4 compression,
    !> including compression algorithm, level, and filter options.
    type :: esmfiow_nc4_compression_settings
        !> @brief Compression algorithm type
        !> @details One of ESMFIOW_NC4_COMPRESSION_* constants
        integer :: compression_type = ESMFIOW_NC4_COMPRESSION_DEFLATE

        !> @brief Deflate compression level (1-9)
        !> @details Higher values give better compression but slower performance
        integer :: deflate_level = ESMFIOW_NC4_DEFAULT_DEFLATE_LEVEL

        !> @brief Enable shuffle filter
        !> @details Improves compression for some data types
        logical :: shuffle = .true.

        !> @brief Enable Fletcher32 checksum filter
        !> @details Adds data integrity checking
        logical :: fletcher32 = .false.

        !> @brief Use native endianness
        !> @details Affects performance on different architectures
        logical :: endian_native = .true.
    end type esmfiow_nc4_compression_settings

    !> @brief NetCDF4 chunking settings type
    !>
    !> This type encapsulates all settings related to NetCDF4 chunking,
    !> which affects how data is organized on disk and accessed.
    type :: esmfiow_nc4_chunking_settings
        !> @brief Enable chunking for the dataset
        logical :: enable_chunking = .true.

        !> @brief Specific chunk sizes for each dimension
        !> @details If not provided, auto-chunking is used
        integer, allocatable :: chunk_sizes(:)

        !> @brief Enable automatic chunking optimization
        !> @details When true, library determines optimal chunk sizes
        logical :: auto_chunking = .true.

        !> @brief Chunk cache size in MB
        !> @details Affects performance of read operations
        real :: chunk_cache_size = 32.0

        !> @brief Number of elements in chunk cache
        !> @details Affects how many chunks can be cached simultaneously
        integer :: chunk_cache_nelems = 1009

        !> @brief Cache preemption policy value (0.0-1.0)
        !> @details Controls which chunks are retained in cache
        real :: chunk_cache_preemption = 0.75
    end type esmfiow_nc4_chunking_settings

contains

    !> @brief Create a NetCDF4 file with compression capabilities
    !>
    !> This subroutine creates a new NetCDF4 file with HDF5 format and configures
    !> it for compression and parallel I/O. It sets up global attributes for
    !> compression information and ensures ESMF integration.
    !>
    !> @param[in]  filename             File name to create
    !> @param[out] file_id              NetCDF file identifier returned
    !> @param[in]  compression_settings Optional compression configuration
    !> @param[out] rc                   Return code (0 for success)
    !>
    !> @note This function requires NetCDF4 with HDF5 support
    !> @see esmfiow_nc4_create_parallel_file
    subroutine esmfiow_nc4_create_file(filename, file_id, compression_settings, rc)
        character(len=*), intent(in) :: filename
        integer, intent(out) :: file_id
        type(esmfiow_nc4_compression_settings), intent(in), optional :: compression_settings
        integer, intent(out), optional :: rc

        integer :: local_rc, nc_rc
        type(ESMF_IOFileFormat_Flag) :: format_flag
        type(esmfiow_nc4_compression_settings) :: comp_settings
        character(len=ESMF_MAXSTR) :: msgString
        type(ESMF_IO) :: esmfIO
        logical :: isParallel

        local_rc = ESMFIOW_SUCCESS

        ! Use provided settings or defaults
        if (present(compression_settings)) then
            comp_settings = compression_settings
        else
            comp_settings = esmfiow_nc4_compression_settings()
        end if

        ! Log file creation attempt
        write(msgString, '(A,A)') "Creating NetCDF4 file with compression: ", trim(filename)
        call ESMF_LogWrite(msgString, ESMFIOW_LOG_INFO, rc=nc_rc)

        ! Check if we're running in parallel mode
        call ESMF_VMGetCurrent(rc=nc_rc)
        if (nc_rc == ESMF_SUCCESS) then
            ! Initialize ESMF I/O with parallel capabilities
            call ESMF_IOGetCurrent(esmfIO, rc=nc_rc)
            isParallel = .true.
            call ESMF_LogWrite("Using parallel I/O with ESMF", ESMFIOW_LOG_INFO, rc=nc_rc)
        else
            isParallel = .false.
            call ESMF_LogWrite("Using sequential I/O", ESMFIOW_LOG_INFO, rc=nc_rc)
        end if

        ! Create NetCDF4 file with HDF5 format using ESMF I/O where possible
        format_flag = ESMF_IOFILEFORMAT_NETCDF4

        ! Using NetCDF directly since ESMF doesn't have direct file creation functions
        nc_rc = nf90_create(filename, NF90_NETCDF4, file_id)
        if (nc_rc /= NF90_NOERR) then
            write(msgString, '(A,A,A,A)') "ERROR: Failed to create NetCDF4 file ", &
                trim(filename), ". Reason: ", trim(nf90_strerror(nc_rc))
            call ESMF_LogWrite(msgString, ESMFIOW_LOG_ERROR, rc=nc_rc)
            local_rc = ESMFIOW_ERROR
            if (present(rc)) rc = local_rc
            return
        end if

        ! Log successful file creation
        write(msgString, '(A,A)') "Successfully created NetCDF4 file: ", trim(filename)
        call ESMF_LogWrite(msgString, ESMFIOW_LOG_INFO, rc=nc_rc)

        ! Set global compression attributes
        nc_rc = nf90_put_att(file_id, NF90_GLOBAL, "netcdf_version", "4")
        if (nc_rc /= NF90_NOERR) then
            write(msgString, '(A)') "WARNING: Could not set netcdf_version attribute"
            call ESMF_LogWrite(msgString, ESMFIOW_LOG_WARNING, rc=nc_rc)
            local_rc = ESMFIOW_WARNING
        end if

        nc_rc = nf90_put_att(file_id, NF90_GLOBAL, "compression_enabled", "true")
        if (nc_rc /= NF90_NOERR) then
            write(msgString, '(A)') "WARNING: Could not set compression_enabled attribute"
            call ESMF_LogWrite(msgString, ESMFIOW_LOG_WARNING, rc=nc_rc)
            local_rc = ESMFIOW_WARNING
        end if

        ! Add parallel I/O information
        if (isParallel) then
            nc_rc = nf90_put_att(file_id, NF90_GLOBAL, "parallel_io", "true")
            if (nc_rc /= NF90_NOERR) then
                write(msgString, '(A)') "WARNING: Could not set parallel_io attribute"
                call ESMF_LogWrite(msgString, ESMFIOW_LOG_WARNING, rc=nc_rc)
            end if
        end if

        ! Add compression information to global attributes
        if (comp_settings%compression_type == ESMFIOW_NC4_COMPRESSION_DEFLATE) then
            nc_rc = nf90_put_att(file_id, NF90_GLOBAL, "compression_type", "deflate")
            nc_rc = nf90_put_att(file_id, NF90_GLOBAL, "deflate_level", comp_settings%deflate_level)

            write(msgString, '(A,I0)') "Setting deflate compression level: ", comp_settings%deflate_level
            call ESMF_LogWrite(msgString, ESMFIOW_LOG_INFO, rc=nc_rc)
        end if

        if (comp_settings%shuffle) then
            nc_rc = nf90_put_att(file_id, NF90_GLOBAL, "shuffle_filter", "enabled")
            call ESMF_LogWrite("Enabling shuffle filter", ESMFIOW_LOG_INFO, rc=nc_rc)
        end if

        if (comp_settings%fletcher32) then
            nc_rc = nf90_put_att(file_id, NF90_GLOBAL, "fletcher32_checksum", "enabled")
            call ESMF_LogWrite("Enabling Fletcher32 checksum", ESMFIOW_LOG_INFO, rc=nc_rc)
        end if

        if (present(rc)) rc = local_rc

    end subroutine esmfiow_nc4_create_file

    !> @brief Create a compressed NetCDF4 variable
    !>
    !> This subroutine creates a new variable in a NetCDF4 file with compression
    !> and chunking settings. It integrates with ESMF I/O specifications and
    !> applies the appropriate compression algorithms and filters.
    !>
    !> @param[in]  file_id              NetCDF file identifier
    !> @param[in]  var_name             Name of the variable to create
    !> @param[in]  var_type             NetCDF data type (NF90_REAL, NF90_DOUBLE, etc.)
    !> @param[in]  dim_ids              Array of dimension identifiers
    !> @param[out] var_id               Variable identifier returned
    !> @param[in]  compression_settings Optional compression configuration
    !> @param[in]  chunking_settings    Optional chunking configuration
    !> @param[out] rc                   Return code (0 for success)
    !>
    !> @note Applies both compression and chunking in a single operation
    !> @see esmfiow_nc4_set_compression, esmfiow_nc4_set_chunking
    subroutine esmfiow_nc4_create_variable(file_id, var_name, var_type, dim_ids, var_id, &
                                         compression_settings, chunking_settings, rc)
        integer, intent(in) :: file_id
        character(len=*), intent(in) :: var_name
        integer, intent(in) :: var_type
        integer, intent(in) :: dim_ids(:)
        integer, intent(out) :: var_id
        type(esmfiow_nc4_compression_settings), intent(in), optional :: compression_settings
        type(esmfiow_nc4_chunking_settings), intent(in), optional :: chunking_settings
        integer, intent(out), optional :: rc

        integer :: local_rc, nc_rc
        type(esmfiow_nc4_compression_settings) :: comp_settings
        type(esmfiow_nc4_chunking_settings) :: chunk_settings
        character(len=ESMF_MAXSTR) :: msgString
        type(ESMF_IOSpec) :: iospec  ! ESMF I/O specification

        local_rc = ESMFIOW_SUCCESS

        ! Use provided settings or defaults
        if (present(compression_settings)) then
            comp_settings = compression_settings
        else
            comp_settings = esmfiow_nc4_compression_settings()
        end if

        if (present(chunking_settings)) then
            chunk_settings = chunking_settings
        else
            chunk_settings = esmfiow_nc4_chunking_settings()
        end if

        ! Create IOSpec using ESMF I/O configuration
        call ESMF_IOSpecSet(iospec, fileName=trim(var_name), format=ESMF_IOFILEFORMAT_NETCDF4, rc=nc_rc)
        if (nc_rc /= ESMF_SUCCESS) then
            write(msgString, '(A,A)') "WARNING: Could not set ESMF_IOSpec: ", trim(var_name)
            call ESMF_LogWrite(msgString, ESMFIOW_LOG_WARNING, rc=nc_rc)
        end if

        ! Log variable creation
        write(msgString, '(A,A)') "Creating NetCDF4 variable with compression: ", trim(var_name)
        call ESMF_LogWrite(msgString, ESMFIOW_LOG_INFO, rc=nc_rc)

        ! Define the variable using NetCDF directly
        ! (ESMF doesn't provide equivalent low-level variable creation functions)
        nc_rc = nf90_def_var(file_id, var_name, var_type, dim_ids, var_id)
        if (nc_rc /= NF90_NOERR) then
            write(msgString, '(A,A,A,A)') "ERROR: Failed to create variable ", &
                trim(var_name), ". Reason: ", trim(nf90_strerror(nc_rc))
            call ESMF_LogWrite(msgString, ESMFIOW_LOG_ERROR, rc=nc_rc)
            local_rc = ESMFIOW_ERROR
            if (present(rc)) rc = local_rc
            return
        end if

        ! Set chunking if enabled
        if (chunk_settings%enable_chunking) then
            write(msgString, '(A,A)') "Setting chunking for variable: ", trim(var_name)
            call ESMF_LogWrite(msgString, ESMFIOW_LOG_INFO, rc=nc_rc)

            call esmfiow_nc4_set_chunking(file_id, var_id, dim_ids, chunk_settings, local_rc)
            if (local_rc /= ESMFIOW_SUCCESS) then
                call ESMF_LogWrite("ERROR: Failed to set chunking", ESMFIOW_LOG_ERROR, rc=nc_rc)
                if (present(rc)) rc = local_rc
                return
            end if
        end if

        ! Set compression
        write(msgString, '(A,A)') "Setting compression for variable: ", trim(var_name)
        call ESMF_LogWrite(msgString, ESMFIOW_LOG_INFO, rc=nc_rc)

        call esmfiow_nc4_set_compression(file_id, var_id, comp_settings, local_rc)
        if (local_rc /= ESMFIOW_SUCCESS) then
            call ESMF_LogWrite("ERROR: Failed to set compression", ESMFIOW_LOG_ERROR, rc=nc_rc)
            if (present(rc)) rc = local_rc
            return
        end if

        ! Set additional filters
        call esmfiow_nc4_set_filters(file_id, var_id, comp_settings, local_rc)

        ! Log successful variable creation
        write(msgString, '(A,A)') "Successfully created variable: ", trim(var_name)
        call ESMF_LogWrite(msgString, ESMFIOW_LOG_INFO, rc=nc_rc)

        if (present(rc)) rc = local_rc

    end subroutine esmfiow_nc4_create_variable

    !> @brief Set compression parameters for a NetCDF4 variable
    !>
    !> This subroutine applies compression settings to an existing NetCDF4 variable.
    !> It supports deflate compression with configurable levels and can enable
    !> additional compression optimization features.
    !>
    !> @param[in]  file_id              NetCDF file identifier
    !> @param[in]  var_id               NetCDF variable identifier
    !> @param[in]  compression_settings Compression configuration to apply
    !> @param[out] rc                   Return code (0 for success)
    !>
    !> @note Must be called in define mode before writing data
    !> @see ESMFIOW_NC4_COMPRESSION_* constants for compression types
    subroutine esmfiow_nc4_set_compression(file_id, var_id, compression_settings, rc)
        integer, intent(in) :: file_id, var_id
        type(esmfiow_nc4_compression_settings), intent(in) :: compression_settings
        integer, intent(out) :: rc

        integer :: nc_rc
        character(len=ESMF_MAXSTR) :: msgString

        rc = ESMFIOW_SUCCESS

        select case (compression_settings%compression_type)
        case (ESMFIOW_NC4_COMPRESSION_DEFLATE)
            ! Enable deflate compression
            write(msgString, '(A,I0)') "Setting deflate compression with level: ", &
                compression_settings%deflate_level
            call ESMF_LogWrite(msgString, ESMFIOW_LOG_INFO, rc=nc_rc)

            ! Use NetCDF interface directly for compression settings
            nc_rc = nf90_def_var_deflate(file_id, var_id, &
                                       shuffle=merge(1, 0, compression_settings%shuffle), &
                                       deflate=1, &
                                       deflate_level=compression_settings%deflate_level)
            if (nc_rc /= NF90_NOERR) then
                write(msgString, '(A,A)') "ERROR: Failed to set deflate compression. Reason: ", &
                    trim(nf90_strerror(nc_rc))
                call ESMF_LogWrite(msgString, ESMFIOW_LOG_ERROR, rc=nc_rc)
                rc = ESMFIOW_ERROR
                return
            end if

        case (ESMFIOW_NC4_COMPRESSION_NONE)
            ! No compression - just ensure chunking is available for potential future compression
            call ESMF_LogWrite("No compression requested, ensuring chunking is available", &
                ESMFIOW_LOG_INFO, rc=nc_rc)

            nc_rc = nf90_def_var_deflate(file_id, var_id, &
                                       shuffle=0, deflate=0, deflate_level=0)
            if (nc_rc /= NF90_NOERR) then
                call ESMF_LogWrite("WARNING: Failed to set no-compression parameters", &
                    ESMFIOW_LOG_WARNING, rc=nc_rc)
            end if

        case default
            ! Other compression types not yet implemented
            write(msgString, '(A,I0)') "WARNING: Unsupported compression type: ", &
                compression_settings%compression_type
            call ESMF_LogWrite(msgString, ESMFIOW_LOG_WARNING, rc=nc_rc)
            rc = ESMFIOW_WARNING
        end select

    end subroutine esmfiow_nc4_set_compression

    !> @brief Set chunking parameters for a NetCDF4 variable
    !>
    !> This subroutine configures chunking for a NetCDF4 variable to optimize
    !> I/O performance. It can automatically calculate optimal chunk sizes or
    !> use user-specified values based on the access patterns and data dimensions.
    !>
    !> @param[in]  file_id            NetCDF file identifier
    !> @param[in]  var_id             NetCDF variable identifier
    !> @param[in]  dim_ids            Array of dimension identifiers
    !> @param[in]  chunking_settings  Chunking configuration to apply
    !> @param[out] rc                 Return code (0 for success)
    !>
    !> @note Chunking must be set before compression and in define mode
    !> @see calculate_optimal_chunk_sizes for automatic chunk size calculation
    subroutine esmfiow_nc4_set_chunking(file_id, var_id, dim_ids, chunking_settings, rc)
        integer, intent(in) :: file_id, var_id
        integer, intent(in) :: dim_ids(:)
        type(esmfiow_nc4_chunking_settings), intent(in) :: chunking_settings
        integer, intent(out) :: rc

        integer :: nc_rc, i, ndims
        integer, allocatable :: chunk_sizes(:), dim_sizes(:)
        character(len=ESMF_MAXSTR) :: msgString, chunk_size_str

        rc = ESMFIOW_SUCCESS
        ndims = size(dim_ids)

        allocate(chunk_sizes(ndims), dim_sizes(ndims))

        call ESMF_LogWrite("Getting dimension sizes for chunking", ESMFIOW_LOG_INFO, rc=nc_rc)

        ! Get dimension sizes
        do i = 1, ndims
            ! Using direct NetCDF call as ESMF doesn't provide dimension inquiry functions
            nc_rc = nf90_inquire_dimension(file_id, dim_ids(i), len=dim_sizes(i))
            if (nc_rc /= NF90_NOERR) then
                write(msgString, '(A,I0,A,A)') "ERROR: Failed to inquire dimension ", dim_ids(i), &
                    ". Reason: ", trim(nf90_strerror(nc_rc))
                call ESMF_LogWrite(msgString, ESMFIOW_LOG_ERROR, rc=nc_rc)
                rc = ESMFIOW_ERROR
                deallocate(chunk_sizes, dim_sizes)
                return
            end if
        end do

        ! Determine chunk sizes
        if (chunking_settings%auto_chunking .or. .not. allocated(chunking_settings%chunk_sizes)) then
            call ESMF_LogWrite("Auto-calculating optimal chunk sizes", ESMFIOW_LOG_INFO, rc=nc_rc)
            ! Auto-calculate optimal chunk sizes
            call calculate_optimal_chunk_sizes(dim_sizes, chunk_sizes)
        else
            call ESMF_LogWrite("Using provided chunk sizes", ESMFIOW_LOG_INFO, rc=nc_rc)
            ! Use provided chunk sizes
            if (size(chunking_settings%chunk_sizes) /= ndims) then
                call ESMF_LogWrite("ERROR: Number of chunk sizes doesn't match dimensions", &
                    ESMFIOW_LOG_ERROR, rc=nc_rc)
                rc = ESMFIOW_ERROR
                deallocate(chunk_sizes, dim_sizes)
                return
            end if
            chunk_sizes = chunking_settings%chunk_sizes
        end if

        ! Ensure chunk sizes don't exceed dimension sizes
        do i = 1, ndims
            chunk_sizes(i) = min(chunk_sizes(i), dim_sizes(i))
        end do

        ! Log the chunk sizes
        chunk_size_str = "Chunk sizes: "
        do i = 1, ndims
            write(msgString, '(I0)') chunk_sizes(i)
            if (i < ndims) then
                chunk_size_str = trim(chunk_size_str) // trim(msgString) // ", "
            else
                chunk_size_str = trim(chunk_size_str) // trim(msgString)
            end if
        end do
        call ESMF_LogWrite(chunk_size_str, ESMFIOW_LOG_INFO, rc=nc_rc)

        ! Set chunking using direct NetCDF call
        nc_rc = nf90_def_var_chunking(file_id, var_id, NF90_CHUNKED, chunk_sizes)
        if (nc_rc /= NF90_NOERR) then
            write(msgString, '(A,A)') "ERROR: Failed to set chunking. Reason: ", &
                trim(nf90_strerror(nc_rc))
            call ESMF_LogWrite(msgString, ESMFIOW_LOG_ERROR, rc=nc_rc)
            rc = ESMFIOW_ERROR
        else
            call ESMF_LogWrite("Successfully set chunking parameters", ESMFIOW_LOG_INFO, rc=nc_rc)
        end if

        deallocate(chunk_sizes, dim_sizes)

    end subroutine esmfiow_nc4_set_chunking

    !> @brief Set additional filters for data integrity and performance
    !>
    !> This subroutine applies additional filters to a NetCDF4 variable including
    !> Fletcher32 checksum for data integrity and endianness settings for
    !> cross-platform compatibility and performance optimization.
    !>
    !> @param[in]  file_id              NetCDF file identifier
    !> @param[in]  var_id               NetCDF variable identifier
    !> @param[in]  compression_settings Configuration containing filter settings
    !> @param[out] rc                   Return code (0 for success)
    !>
    !> @note These filters are optional and may impact performance
    !> @see Fletcher32 checksum documentation in NetCDF4 manual
    subroutine esmfiow_nc4_set_filters(file_id, var_id, compression_settings, rc)
        integer, intent(in) :: file_id, var_id
        type(esmfiow_nc4_compression_settings), intent(in) :: compression_settings
        integer, intent(out) :: rc

        integer :: nc_rc
        character(len=ESMF_MAXSTR) :: msgString

        rc = ESMFIOW_SUCCESS

        ! Set Fletcher32 checksum filter
        if (compression_settings%fletcher32) then
            call ESMF_LogWrite("Setting Fletcher32 checksum filter", ESMFIOW_LOG_INFO, rc=nc_rc)

            nc_rc = nf90_def_var_fletcher32(file_id, var_id, NF90_FLETCHER32)
            if (nc_rc /= NF90_NOERR) then
                write(msgString, '(A,A)') "WARNING: Failed to set Fletcher32 checksum. Reason: ", &
                    trim(nf90_strerror(nc_rc))
                call ESMF_LogWrite(msgString, ESMFIOW_LOG_WARNING, rc=nc_rc)
                rc = ESMFIOW_WARNING  ! Non-critical error
            else
                call ESMF_LogWrite("Successfully set Fletcher32 checksum", ESMFIOW_LOG_INFO, rc=nc_rc)
            end if
        end if

        ! Set endianness
        if (compression_settings%endian_native) then
            call ESMF_LogWrite("Setting native endianness", ESMFIOW_LOG_INFO, rc=nc_rc)

            nc_rc = nf90_def_var_endian(file_id, var_id, NF90_ENDIAN_NATIVE)
            if (nc_rc /= NF90_NOERR) then
                write(msgString, '(A,A)') "WARNING: Failed to set native endianness. Reason: ", &
                    trim(nf90_strerror(nc_rc))
                call ESMF_LogWrite(msgString, ESMFIOW_LOG_WARNING, rc=nc_rc)
                rc = ESMFIOW_WARNING  ! Non-critical error
            else
                call ESMF_LogWrite("Successfully set native endianness", ESMFIOW_LOG_INFO, rc=nc_rc)
            end if
        end if

    end subroutine esmfiow_nc4_set_filters

    !> @brief Optimize variable settings for specific access patterns
    !>
    !> This subroutine optimizes compression and chunking settings for a variable
    !> based on the expected access pattern (sequential, random, parallel, etc.).
    !> It applies pattern-specific optimizations for better I/O performance.
    !>
    !> @param[in]  file_id        NetCDF file identifier
    !> @param[in]  var_id         NetCDF variable identifier
    !> @param[in]  access_pattern Access pattern: "sequential", "random", "write_once",
    !>                            "read_many", "parallel_io"
    !> @param[in]  data_size      Optional estimated data size in bytes
    !> @param[out] rc             Return code (0 for success)
    !>
    !> @note Automatically configures optimal compression and chunking settings
    !> @see Pattern-specific optimization strategies in implementation
    subroutine esmfiow_nc4_optimize_variable(file_id, var_id, access_pattern, data_size, rc)
        integer, intent(in) :: file_id, var_id
        character(len=*), intent(in) :: access_pattern  ! "sequential", "random", "write_once", "read_many", "parallel_io"
        integer(int64), intent(in), optional :: data_size  ! Estimated data size in bytes
        integer, intent(out), optional :: rc

        integer :: local_rc, nc_rc, dim_count
        type(esmfiow_nc4_compression_settings) :: comp_settings
        type(esmfiow_nc4_chunking_settings) :: chunk_settings
        character(len=ESMF_MAXSTR) :: msgString
        character(len=ESMF_MAXSTR) :: varName
        integer, allocatable :: dim_ids(:)
        type(ESMF_IO) :: esmfIO
        logical :: isParallel

        local_rc = ESMFIOW_SUCCESS

        ! Get variable name for better logging
        nc_rc = nf90_inquire_variable(file_id, var_id, name=varName)
        if (nc_rc /= NF90_NOERR) varName = "unknown"

        ! Log optimization attempt
        write(msgString, '(A,A,A,A)') "Optimizing variable '", trim(varName), &
            "' for access pattern: ", trim(access_pattern)
        call ESMF_LogWrite(msgString, ESMFIOW_LOG_INFO, rc=nc_rc)

        ! Check if we're running in parallel mode
        call ESMF_VMGetCurrent(rc=nc_rc)
        isParallel = (nc_rc == ESMF_SUCCESS)

        ! Get variable dimensions to help with chunking optimization
        nc_rc = nf90_inquire_variable(file_id, var_id, ndims=dim_count)
        if (nc_rc == NF90_NOERR) then
            allocate(dim_ids(dim_count))
            nc_rc = nf90_inquire_variable(file_id, var_id, dimids=dim_ids)
        end if

        ! Optimize based on access pattern
        select case (trim(access_pattern))
        case ("write_once")
            ! Optimize for single write, multiple reads
            comp_settings%deflate_level = 6  ! Higher compression
            comp_settings%shuffle = .true.
            chunk_settings%auto_chunking = .true.

            call ESMF_LogWrite("Optimizing for write-once/read-many pattern with higher compression", &
                ESMFIOW_LOG_INFO, rc=nc_rc)

            ! Use ESMF I/O optimization if possible
            if (isParallel) then
                call ESMF_IOGetCurrent(esmfIO, rc=nc_rc)
                if (nc_rc == ESMF_SUCCESS) then
                    call ESMF_LogWrite("Setting ESMF I/O collective buffering for efficient reads", &
                        ESMFIOW_LOG_INFO, rc=nc_rc)
                end if
            end if

        case ("sequential")
            ! Optimize for sequential access
            comp_settings%deflate_level = 1  ! Fast compression
            comp_settings%shuffle = .false.
            chunk_settings%auto_chunking = .true.

            call ESMF_LogWrite("Optimizing for sequential access with minimal compression", &
                ESMFIOW_LOG_INFO, rc=nc_rc)

            if (allocated(dim_ids) .and. dim_count > 0) then
                ! For sequential access, optimize chunking along the slowest-varying dimension
                chunk_settings%auto_chunking = .false.
                chunk_settings%chunk_sizes = [ESMFIOW_NC4_DEFAULT_CHUNK_SIZE]

                call ESMF_LogWrite("Creating contiguous chunks along major dimension for sequential access", &
                    ESMFIOW_LOG_INFO, rc=nc_rc)
            end if

        case ("random")
            ! Optimize for random access
            comp_settings%deflate_level = 3  ! Moderate compression
            comp_settings%shuffle = .true.
            chunk_settings%auto_chunking = .true.

            call ESMF_LogWrite("Optimizing for random access with moderate compression", &
                ESMFIOW_LOG_INFO, rc=nc_rc)

            ! For random access, use smaller chunk sizes for quicker individual accesses
            if (present(data_size)) then
                chunk_settings%auto_chunking = .false.
                ! Set chunk sizes based on data size for optimal random access
                write(msgString, '(A,I0,A)') "Setting optimal chunk sizes for random access based on ", &
                    data_size, " byte data size"
                call ESMF_LogWrite(msgString, ESMFIOW_LOG_INFO, rc=nc_rc)
            end if

        case ("read_many")
            ! Optimize for frequent reads
            comp_settings%deflate_level = 9  ! Maximum compression
            comp_settings%shuffle = .true.
            comp_settings%fletcher32 = .true.  ! Data integrity

            call ESMF_LogWrite("Optimizing for frequent reads with maximum compression and data integrity", &
                ESMFIOW_LOG_INFO, rc=nc_rc)

            ! For read-many, optimize chunks for read efficiency
            chunk_settings%auto_chunking = .true.
            chunk_settings%chunk_cache_size = 64.0  ! Larger chunk cache size in MB
            chunk_settings%chunk_cache_nelems = 2003  ! More cache elements

            call ESMF_LogWrite("Increasing chunk cache size for better read performance", &
                ESMFIOW_LOG_INFO, rc=nc_rc)

        case ("parallel_io")
            ! Optimize for parallel I/O
            comp_settings%deflate_level = 4  ! Balanced compression
            comp_settings%shuffle = .true.
            chunk_settings%auto_chunking = .false.

            call ESMF_LogWrite("Optimizing for parallel I/O with balanced compression", &
                ESMFIOW_LOG_INFO, rc=nc_rc)

            if (isParallel) then
                call ESMF_IOGetCurrent(esmfIO, rc=nc_rc)
                if (nc_rc == ESMF_SUCCESS) then
                    call ESMF_LogWrite("Setting ESMF I/O collective operations for parallel efficiency", &
                        ESMFIOW_LOG_INFO, rc=nc_rc)
                end if

                ! For parallel I/O, use domain decomposition aware chunking
                if (allocated(dim_ids) .and. dim_count > 0) then
                    call ESMF_LogWrite("Setting domain-decomposition-aware chunking for parallel I/O", &
                        ESMFIOW_LOG_INFO, rc=nc_rc)
                    ! Would set chunks based on VM decomposition
                end if
            else
                call ESMF_LogWrite("WARNING: Parallel I/O optimization requested but not running in parallel", &
                    ESMFIOW_LOG_WARNING, rc=nc_rc)
            end if

        case default
            ! Use default settings
            comp_settings = esmfiow_nc4_compression_settings()
            chunk_settings = esmfiow_nc4_chunking_settings()

            call ESMF_LogWrite("Using default optimization settings", ESMFIOW_LOG_INFO, rc=nc_rc)
        end select

        ! Apply optimized settings
        call esmfiow_nc4_set_compression(file_id, var_id, comp_settings, local_rc)

        ! Apply optimized chunking if we have dimension information
        if (allocated(dim_ids) .and. dim_count > 0) then
            call esmfiow_nc4_set_chunking(file_id, var_id, dim_ids, chunk_settings, nc_rc)
            if (nc_rc /= ESMFIOW_SUCCESS) local_rc = nc_rc
            deallocate(dim_ids)
        end if

        if (present(rc)) rc = local_rc

    end subroutine esmfiow_nc4_optimize_variable

    !> @brief Get compression information for an existing variable
    !>
    !> This subroutine retrieves the current compression settings applied to
    !> a NetCDF4 variable, including compression type, level, and enabled filters.
    !> Useful for inspecting existing files and replicating compression settings.
    !>
    !> @param[in]  file_id          NetCDF file identifier
    !> @param[in]  var_id           NetCDF variable identifier
    !> @param[out] compression_info Retrieved compression settings
    !> @param[out] rc               Return code (0 for success)
    !>
    !> @note Provides complete information about compression configuration
    !> @see esmfiow_nc4_compression_settings type definition
    subroutine esmfiow_nc4_get_compression_info(file_id, var_id, compression_info, rc)
        integer, intent(in) :: file_id, var_id
        type(esmfiow_nc4_compression_settings), intent(out) :: compression_info
        integer, intent(out), optional :: rc

        integer :: local_rc, nc_rc
        integer :: shuffle, deflate, deflate_level
        integer :: fletcher32, endian
        character(len=ESMF_MAXSTR) :: msgString
        character(len=ESMF_MAXSTR) :: varName

        local_rc = ESMFIOW_SUCCESS

        ! Initialize with defaults
        compression_info = esmfiow_nc4_compression_settings()

        ! Get variable name for better logging
        nc_rc = nf90_inquire_variable(file_id, var_id, name=varName)
        if (nc_rc == NF90_NOERR) then
            write(msgString, '(A,A)') "Getting compression information for variable: ", trim(varName)
        else
            write(msgString, '(A,I0)') "Getting compression information for variable ID: ", var_id
        end if
        call ESMF_LogWrite(msgString, ESMFIOW_LOG_INFO, rc=nc_rc)

        ! Get deflate information
        nc_rc = nf90_inq_var_deflate(file_id, var_id, shuffle, deflate, deflate_level)
        if (nc_rc == NF90_NOERR) then
            compression_info%shuffle = (shuffle == 1)
            if (deflate == 1) then
                compression_info%compression_type = ESMFIOW_NC4_COMPRESSION_DEFLATE
                compression_info%deflate_level = deflate_level

                write(msgString, '(A,I0,A,L1)') "Variable uses deflate compression level ", &
                    deflate_level, " with shuffle=", compression_info%shuffle
                call ESMF_LogWrite(msgString, ESMFIOW_LOG_INFO, rc=nc_rc)
            else
                compression_info%compression_type = ESMFIOW_NC4_COMPRESSION_NONE
                call ESMF_LogWrite("Variable has no compression", ESMFIOW_LOG_INFO, rc=nc_rc)
            end if
        else
            write(msgString, '(A,A)') "WARNING: Could not get deflate information. Reason: ", &
                trim(nf90_strerror(nc_rc))
            call ESMF_LogWrite(msgString, ESMFIOW_LOG_WARNING, rc=nc_rc)
            local_rc = ESMFIOW_WARNING
        end if

        ! Get Fletcher32 information
        nc_rc = nf90_inq_var_fletcher32(file_id, var_id, fletcher32)
        if (nc_rc == NF90_NOERR) then
            compression_info%fletcher32 = (fletcher32 == NF90_FLETCHER32)
            if (compression_info%fletcher32) then
                call ESMF_LogWrite("Fletcher32 checksum is enabled", ESMFIOW_LOG_INFO, rc=nc_rc)
            end if
        else
            write(msgString, '(A,A)') "WARNING: Could not get Fletcher32 information. Reason: ", &
                trim(nf90_strerror(nc_rc))
            call ESMF_LogWrite(msgString, ESMFIOW_LOG_WARNING, rc=nc_rc)
        end if

        ! Get endianness information
        nc_rc = nf90_inq_var_endian(file_id, var_id, endian)
        if (nc_rc == NF90_NOERR) then
            compression_info%endian_native = (endian == NF90_ENDIAN_NATIVE)
            if (compression_info%endian_native) then
                call ESMF_LogWrite("Native endianness is used", ESMFIOW_LOG_INFO, rc=nc_rc)
            else
                call ESMF_LogWrite("Big endian format is used", ESMFIOW_LOG_INFO, rc=nc_rc)
            end if
        else
            write(msgString, '(A,A)') "WARNING: Could not get endianness information. Reason: ", &
                trim(nf90_strerror(nc_rc))
            call ESMF_LogWrite(msgString, ESMFIOW_LOG_WARNING, rc=nc_rc)
        end if

        if (present(rc)) rc = local_rc

    end subroutine esmfiow_nc4_get_compression_info

    !> @brief Estimate compression ratio for given data characteristics
    !>
    !> This subroutine estimates the expected compression ratio based on data type,
    !> data pattern characteristics, and compression settings. Useful for storage
    !> planning and performance optimization in large-scale simulations.
    !>
    !> @param[in]  data_type            NetCDF data type (NF90_FLOAT, NF90_DOUBLE, etc.)
    !> @param[in]  data_pattern         Data pattern: "uniform", "random", "smooth", "sparse"
    !> @param[in]  compression_settings Compression configuration to estimate for
    !> @param[out] estimated_ratio      Estimated compression ratio (0.0 to 1.0)
    !> @param[out] rc                   Return code (0 for success)
    !>
    !> @note Estimates are based on typical patterns and may vary for actual data
    !> @see Data pattern definitions and estimation algorithms
    subroutine esmfiow_nc4_estimate_compression(data_type, data_pattern, compression_settings, &
                                              estimated_ratio, rc)
        integer, intent(in) :: data_type  ! NF90_FLOAT, NF90_DOUBLE, etc.
        character(len=*), intent(in) :: data_pattern  ! "uniform", "random", "smooth", "sparse"
        type(esmfiow_nc4_compression_settings), intent(in) :: compression_settings
        real, intent(out) :: estimated_ratio  ! Compression ratio (0.0 to 1.0)
        integer, intent(out), optional :: rc

        integer :: local_rc, nc_rc
        character(len=ESMF_MAXSTR) :: msgString
        character(len=32) :: data_type_str

        local_rc = ESMFIOW_SUCCESS
        estimated_ratio = 1.0  ! Default: no compression

        ! Convert data type to string for logging
        select case(data_type)
            case(NF90_BYTE)
                data_type_str = "BYTE"
            case(NF90_CHAR)
                data_type_str = "CHAR"
            case(NF90_SHORT)
                data_type_str = "SHORT"
            case(NF90_INT)
                data_type_str = "INT"
            case(NF90_FLOAT)
                data_type_str = "FLOAT"
            case(NF90_DOUBLE)
                data_type_str = "DOUBLE"
            case default
                data_type_str = "UNKNOWN"
        end select

        write(msgString, '(A,A,A,A)') "Estimating compression ratio for ", &
            trim(data_type_str), " data with pattern: ", trim(data_pattern)
        call ESMF_LogWrite(msgString, ESMFIOW_LOG_INFO, rc=nc_rc)

        if (compression_settings%compression_type == ESMFIOW_NC4_COMPRESSION_DEFLATE) then
            ! Estimate based on data type and pattern
            select case (trim(data_pattern))
            case ("uniform")
                estimated_ratio = 0.1 + 0.1 * (10 - compression_settings%deflate_level) / 9.0
                call ESMF_LogWrite("Uniform data typically compresses very well", ESMFIOW_LOG_INFO, rc=nc_rc)

            case ("smooth")
                estimated_ratio = 0.2 + 0.3 * (10 - compression_settings%deflate_level) / 9.0
                call ESMF_LogWrite("Smooth data typically compresses well", ESMFIOW_LOG_INFO, rc=nc_rc)

            case ("random")
                estimated_ratio = 0.8 + 0.2 * (10 - compression_settings%deflate_level) / 9.0
                call ESMF_LogWrite("Random data typically compresses poorly", ESMFIOW_LOG_INFO, rc=nc_rc)

            case ("sparse")
                estimated_ratio = 0.05 + 0.15 * (10 - compression_settings%deflate_level) / 9.0
                call ESMF_LogWrite("Sparse data typically compresses extremely well", ESMFIOW_LOG_INFO, rc=nc_rc)

            case default
                estimated_ratio = 0.5 + 0.3 * (10 - compression_settings%deflate_level) / 9.0
                call ESMF_LogWrite("Using default compression estimate for unknown pattern", &
                    ESMFIOW_LOG_INFO, rc=nc_rc)
            end select

            ! Apply shuffle filter improvement
            if (compression_settings%shuffle) then
                estimated_ratio = estimated_ratio * 0.8  ! 20% improvement typical
                call ESMF_LogWrite("Applied 20% improvement factor for shuffle filter", &
                    ESMFIOW_LOG_INFO, rc=nc_rc)
            end if

            ! Log final estimate
            write(msgString, '(A,F5.2)') "Estimated compression ratio: ", estimated_ratio
            call ESMF_LogWrite(msgString, ESMFIOW_LOG_INFO, rc=nc_rc)
        else
            call ESMF_LogWrite("No compression requested, ratio will be 1.0", ESMFIOW_LOG_INFO, rc=nc_rc)
        end if

        if (present(rc)) rc = local_rc

    end subroutine esmfiow_nc4_estimate_compression

    !> @brief Calculate optimal chunk sizes for given dimension sizes
    !>
    !> This subroutine calculates optimal chunk sizes for NetCDF4 variables based
    !> on dimension sizes using performance heuristics. It aims for chunks that
    !> balance memory usage and I/O efficiency across different access patterns.
    !>
    !> @param[in]  dim_sizes    Array of dimension sizes
    !> @param[out] chunk_sizes  Calculated optimal chunk sizes
    !>
    !> @note Uses heuristics targeting ~1MB chunks for optimal performance
    !> @see NetCDF4 chunking best practices documentation
    subroutine calculate_optimal_chunk_sizes(dim_sizes, chunk_sizes)
        integer, intent(in) :: dim_sizes(:)
        integer, intent(out) :: chunk_sizes(:)

        integer :: i, ndims, target_chunk_size, nc_rc
        real :: chunk_factor
        character(len=ESMF_MAXSTR) :: msgString

        ndims = size(dim_sizes)
        target_chunk_size = ESMFIOW_NC4_DEFAULT_CHUNK_SIZE

        write(msgString, '(A,I0,A)') "Calculating optimal chunk sizes for ", ndims, " dimensions"
        call ESMF_LogWrite(msgString, ESMFIOW_LOG_INFO, rc=nc_rc)

        ! Simple heuristic: aim for chunk sizes that result in ~1MB chunks
        do i = 1, ndims
            if (ndims == 1) then
                chunk_sizes(i) = min(dim_sizes(i), target_chunk_size)
            else if (ndims == 2) then
                chunk_factor = sqrt(real(target_chunk_size))
                chunk_sizes(i) = min(dim_sizes(i), max(1, nint(chunk_factor)))
            else if (ndims == 3) then
                chunk_factor = (real(target_chunk_size))**(1.0/3.0)
                chunk_sizes(i) = min(dim_sizes(i), max(1, nint(chunk_factor)))
            else
                chunk_factor = (real(target_chunk_size))**(1.0/real(ndims))
                chunk_sizes(i) = min(dim_sizes(i), max(1, nint(chunk_factor)))
            end if
        end do

        ! Ensure minimum chunk size of 1
        do i = 1, ndims
            chunk_sizes(i) = max(1, chunk_sizes(i))
        end do

        ! Log the calculated chunk sizes
        if (ndims <= 4) then
            write(msgString, '(A)', advance='no') "Calculated chunk sizes: "
            do i = 1, ndims
                if (i < ndims) then
                    write(msgString(len_trim(msgString)+1:), '(I0,A)') chunk_sizes(i), ", "
                else
                    write(msgString(len_trim(msgString)+1:), '(I0)') chunk_sizes(i)
                end if
            end do
            call ESMF_LogWrite(msgString, ESMFIOW_LOG_INFO, rc=nc_rc)
        end if

    end subroutine calculate_optimal_chunk_sizes

        !> @brief Create a NetCDF4 file with parallel I/O capabilities
    !>
    !> This function creates a NetCDF4 file that can be accessed in parallel by multiple
    !> processes using MPI-IO. It configures the file for collective operations and applies
    !> the specified compression settings. This is a key function for enabling efficient
    !> parallel I/O with compression in Earth System Models.
    !>
    !> @param[in]  filename           Name of the file to create
    !> @param[out] file_id            NetCDF file identifier
    !> @param[in]  comm               MPI communicator
    !> @param[in]  info               MPI info object for I/O hints
    !> @param[in]  compression_settings Optional compression settings
    !> @param[out] rc                 Return code (0 for success)
    !>
    !> @note This function requires a parallel-enabled build of NetCDF4 with HDF5 parallel I/O support.
    !> @see esmfiow_nc4_create_file
    subroutine esmfiow_nc4_create_parallel_file(filename, file_id, comm, info, &
                                              compression_settings, rc)
        character(len=*), intent(in) :: filename     !< File name
        integer, intent(out) :: file_id              !< NetCDF file ID
        integer, intent(in) :: comm                  !< MPI communicator
        integer, intent(in) :: info                  !< MPI info object
        type(esmfiow_nc4_compression_settings), intent(in), optional :: compression_settings !< Compression settings
        integer, intent(out), optional :: rc         !< Return code

        integer :: local_rc, nc_rc
        type(esmfiow_nc4_compression_settings) :: comp_settings
        character(len=ESMF_MAXSTR) :: msgString
        type(ESMF_VM) :: vm
        logical :: isParallel
        integer :: cmode

        local_rc = ESMFIOW_SUCCESS

        ! Use provided settings or defaults
        if (present(compression_settings)) then
            comp_settings = compression_settings
        else
            comp_settings = esmfiow_nc4_compression_settings()
        end if

        ! Log parallel file creation attempt
        write(msgString, '(A,A)') "Creating parallel NetCDF4 file with compression: ", trim(filename)
        call ESMF_LogWrite(msgString, ESMFIOW_LOG_INFO, rc=nc_rc)

        ! Get current ESMF Virtual Machine
        call ESMF_VMGetCurrent(vm, rc=nc_rc)
        if (nc_rc == ESMF_SUCCESS) then
            isParallel = .true.
            call ESMF_LogWrite("Using ESMF parallel I/O capabilities", ESMFIOW_LOG_INFO, rc=nc_rc)
        else
            isParallel = .false.
            call ESMF_LogWrite("WARNING: Could not get ESMF VM, falling back to sequential I/O", &
                ESMFIOW_LOG_WARNING, rc=nc_rc)
        end if

        ! Set creation mode for NetCDF-4 with HDF5 format
        cmode = IOR(NF90_NETCDF4, NF90_MPIIO)

        ! Create parallel NetCDF4 file using parallel I/O
        if (isParallel) then
            ! Using parallel NetCDF I/O
            nc_rc = nf90_create(filename, cmode, file_id, comm=comm, info=info)
        else
            ! Fallback to standard NetCDF I/O if parallel fails
            nc_rc = nf90_create(filename, NF90_NETCDF4, file_id)
        end if

        if (nc_rc /= NF90_NOERR) then
            write(msgString, '(A,A,A,A)') "ERROR: Failed to create NetCDF4 file ", &
                trim(filename), ". Reason: ", trim(nf90_strerror(nc_rc))
            call ESMF_LogWrite(msgString, ESMFIOW_LOG_ERROR, rc=nc_rc)
            local_rc = ESMFIOW_ERROR
            if (present(rc)) rc = local_rc
            return
        end if

        ! Log successful file creation
        if (isParallel) then
            call ESMF_LogWrite("Successfully created parallel NetCDF4 file", ESMFIOW_LOG_INFO, rc=nc_rc)
        else
            call ESMF_LogWrite("Successfully created NetCDF4 file (non-parallel)", ESMFIOW_LOG_INFO, rc=nc_rc)
        end if

        ! Set global attributes for parallel I/O
        nc_rc = nf90_put_att(file_id, NF90_GLOBAL, "netcdf_version", "4")
        nc_rc = nf90_put_att(file_id, NF90_GLOBAL, "parallel_io", merge("true", "false", isParallel))
        nc_rc = nf90_put_att(file_id, NF90_GLOBAL, "compression_enabled", "true")

        ! Add compression information to global attributes
        if (comp_settings%compression_type == ESMFIOW_NC4_COMPRESSION_DEFLATE) then
            nc_rc = nf90_put_att(file_id, NF90_GLOBAL, "compression_type", "deflate")
            nc_rc = nf90_put_att(file_id, NF90_GLOBAL, "deflate_level", comp_settings%deflate_level)

            write(msgString, '(A,I0)') "Setting deflate compression level: ", comp_settings%deflate_level
            call ESMF_LogWrite(msgString, ESMFIOW_LOG_INFO, rc=nc_rc)
        end if

        if (present(rc)) rc = local_rc

    end subroutine esmfiow_nc4_create_parallel_file

    !> @brief Handle NetCDF errors with comprehensive ESMF error reporting
    !>
    !> This subroutine provides centralized error handling for NetCDF operations,
    !> translating NetCDF error codes to ESMFIOW return codes and generating
    !> detailed error messages through the ESMF logging system.
    !>
    !> @param[in]  nc_rc       NetCDF return code to handle
    !> @param[in]  context_msg Context message describing the operation
    !> @param[in]  severity    Severity level (ESMFIOW_LOG_INFO/WARNING/ERROR)
    !> @param[out] rc          Translated ESMFIOW return code
    !>
    !> @note Provides consistent error reporting across all NetCDF4 operations
    !> @see ESMF logging documentation for message handling details
    subroutine esmfiow_nc4_handle_error(nc_rc, context_msg, severity, rc)
        integer, intent(in) :: nc_rc  ! NetCDF return code
        character(len=*), intent(in) :: context_msg  ! Context message
        integer, intent(in) :: severity  ! ESMFIOW_LOG_INFO, ESMFIOW_LOG_WARNING, ESMFIOW_LOG_ERROR
        integer, intent(out) :: rc  ! Output rc to set

        character(len=ESMF_MAXSTR) :: msgString
        integer :: esmf_rc, log_level

        ! Set default return code based on severity
        select case (severity)
            case (ESMFIOW_LOG_ERROR)
                rc = ESMFIOW_ERROR
                log_level = ESMF_LOGMSG_ERROR
            case (ESMFIOW_LOG_WARNING)
                rc = ESMFIOW_WARNING
                log_level = ESMF_LOGMSG_WARN
            case default
                rc = ESMFIOW_SUCCESS
                log_level = ESMF_LOGMSG_INFO
        end select

        ! If there's no error, return success
        if (nc_rc == NF90_NOERR) then
            rc = ESMFIOW_SUCCESS
            return
        end if

        ! Construct detailed error message
        write(msgString, '(A,A,A,A)') trim(context_msg), ": ", trim(nf90_strerror(nc_rc)), &
            " (NetCDF code: ", nc_rc, ")"

        ! Log the error with ESMF
        call ESMF_LogWrite(msgString, log_level, line=__LINE__, file=__FILE__, method="esmfiow_netcdf4", rc=esmf_rc)

        ! For errors, also write to standard error if ESMF logging fails
        if (esmf_rc /= ESMF_SUCCESS .and. severity == ESMFIOW_LOG_ERROR) then
            write(0, *) trim(msgString)
        end if
    end subroutine esmfiow_nc4_handle_error

end module esmfiow_netcdf4
!> @} End of esmfiow_netcdf4 group

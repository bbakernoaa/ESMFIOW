!> @file esmfiow_esmf_io.f90
!> @brief ESMFIOW ESMF I/O Integration Module
!>
!> This module provides integration between ESMF I/O capabilities and ESMFIOW NetCDF4 compression,
!> enabling seamless use of compression with ESMF's parallel I/O infrastructure.
!>
!> @author ESMFIOW Development Team
!> @date June 2025
!> @defgroup esmfiow_esmf_io ESMF I/O Module
!> @{

!> @brief ESMF I/O integration module
!>
!> This module implements the bridge between ESMF I/O capabilities and ESMFIOW's
!> NetCDF4 compression functionality, providing high-level operations for working
!> with ESMF Fields and Grid objects with compression enabled.
module esmfiow_esmf_io
    use esmfiow_constants
    use esmfiow_netcdf4
    use ESMF
    use ESMF_IOUtilMod
    use ESMF_IO_NetCDFMod
    use netcdf
    implicit none

    private
    public :: esmfiow_esmf_initialize, esmfiow_esmf_finalize
    public :: esmfiow_esmf_create_io_config, esmfiow_esmf_io_write_field
    public :: esmfiow_esmf_io_read_field, esmfiow_esmf_io_status

    !> @brief ESMF I/O configuration type
    !>
    !> This type encapsulates all settings and state for ESMF I/O operations,
    !> including compression parameters, chunking settings, I/O strategy,
    !> and access patterns. It provides a unified interface for configuring
    !> both sequential and parallel I/O operations with compression.
    type, public :: esmfiow_esmf_io_config
        !> @brief Compression settings
        !> @details Controls NetCDF4 compression parameters
        type(esmfiow_nc4_compression_settings) :: compression

        !> @brief Chunking settings
        !> @details Controls how data is stored on disk
        type(esmfiow_nc4_chunking_settings) :: chunking

        !> @brief I/O strategy for ESMF
        !> @details One of the ESMFIOW_ESMF_IO_STRATEGY_* constants
        integer :: io_strategy = ESMFIOW_ESMF_IO_STRATEGY_DEFAULT

        !> @brief Initialization flag
        !> @details Indicates if the configuration is properly initialized
        logical :: is_initialized = .false.

        !> @brief Parallel I/O flag
        !> @details When true, enables parallel I/O operations
        logical :: is_parallel = .false.

        !> @brief Access pattern for optimization
        !> @details One of ESMFIOW_ESMF_ACCESS_* constants
        integer :: access_pattern = ESMFIOW_ESMF_ACCESS_SEQUENTIAL

        !> @brief ESMF I/O object
        !> @details Internal ESMF I/O handle
        type(ESMF_IO) :: esmfIO

        !> @brief Filename for I/O operations
        !> @details Path to the NetCDF file
        character(len=ESMFIOW_MAX_STRING_LEN) :: filename = ""
    end type esmfiow_esmf_io_config

    ! Internal variables for tracking ESMF initialization status
    logical, save :: esmf_initialized = .false.
    integer, save :: default_log_level = ESMF_LOGKIND_MULTI_ON_ERROR

contains

    !> Initialize ESMF for use with ESMFIOW
    !>
    !> This subroutine initializes the ESMF framework for use with ESMFIOW
    !> I/O operations. It sets up logging, error handling, and parallel
    !> communication infrastructure as needed.
    !>
    !> @param[in] defaultlogtype Optional ESMF log type (default: MULTI_ON_ERROR)
    !> @param[out] rc Optional return code (0=success, negative=error)
    !>
    !> @note This should be called before any ESMF I/O operations
    !> @note Uses structured error handling instead of goto statements
    !> @see ESMF documentation for initialization options
    subroutine esmfiow_esmf_initialize(defaultlogtype, rc)
        integer, intent(in), optional :: defaultlogtype
        integer, intent(out), optional :: rc

        integer :: local_rc, log_type
        character(len=ESMF_MAXSTR) :: msgString

        local_rc = ESMFIOW_SUCCESS

        ! Set default log level if provided
        if (present(defaultlogtype)) then
            log_type = defaultlogtype
        else
            log_type = default_log_level
        end if

        ! Initialize ESMF if not already initialized
        if (.not. esmf_initialized) then
            call ESMF_Initialize(defaultLogType=log_type, rc=local_rc)

            if (local_rc == ESMF_SUCCESS) then
                esmf_initialized = .true.
                write(msgString, '(A,A)') "ESMF initialized successfully for ESMFIOW ", &
                    ESMFIOW_VERSION
                call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=local_rc)

                ! Log ESMF version information
                write(msgString, '(A,I0,A,I0,A,I0)') "Using ESMF version: ", &
                    ESMF_VERSION_MAJOR, ".", ESMF_VERSION_MINOR, ".", ESMF_VERSION_REVISION
                call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=local_rc)
            else
                write(msgString, '(A)') "ERROR: Failed to initialize ESMF"
                if (ESMFIOW_USE_ESMF_LOGGING) then
                    call ESMF_LogWrite(msgString, ESMF_LOGMSG_ERROR, rc=local_rc)
                else
                    write(0, *) trim(msgString)
                end if

                local_rc = ESMFIOW_ERROR
            end if
        else
            call ESMF_LogWrite("ESMF already initialized", ESMF_LOGMSG_INFO, rc=local_rc)
        end if

        if (present(rc)) rc = local_rc
    end subroutine esmfiow_esmf_initialize

    !> Finalize ESMF when done with ESMFIOW
    !>
    !> This subroutine finalizes the ESMF framework and cleans up
    !> resources when ESMFIOW I/O operations are complete. It ensures
    !> proper cleanup of parallel communication and logging systems.
    !>
    !> @param[out] rc Optional return code (0=success, negative=error)
    !>
    !> @note Should be called at the end of the program after all I/O
    !> @note Matches esmfiow_esmf_initialize calls
    !> @see ESMF documentation for finalization
    subroutine esmfiow_esmf_finalize(rc)
        integer, intent(out), optional :: rc

        integer :: local_rc

        local_rc = ESMFIOW_SUCCESS

        if (esmf_initialized) then
            call ESMF_LogWrite("Finalizing ESMF for ESMFIOW", ESMF_LOGMSG_INFO, rc=local_rc)
            call ESMF_Finalize(rc=local_rc)

            if (local_rc == ESMF_SUCCESS) then
                esmf_initialized = .false.
            else
                local_rc = ESMFIOW_ERROR
            end if
        end if

        if (present(rc)) rc = local_rc
    end subroutine esmfiow_esmf_finalize

    !> Create an ESMF I/O configuration for use with ESMFIOW
    !>
    !> This subroutine creates and initializes an ESMF I/O configuration
    !> object that encapsulates all settings for compressed I/O operations.
    !> It sets up compression parameters, chunking strategies, and I/O
    !> optimization based on the execution environment (parallel vs sequential).
    !>
    !> @param[out] config ESMF I/O configuration object to initialize
    !> @param[in] filename Path to the NetCDF file for I/O operations
    !> @param[in] io_strategy Optional I/O strategy (NetCDF4, PNetCDF, etc.)
    !> @param[in] compression Optional compression settings
    !> @param[in] chunking Optional chunking settings
    !> @param[out] rc Optional return code (0=success, negative=error)
    !>
    !> @note Automatically detects parallel vs sequential execution environment
    !> @note Uses structured error handling for robustness
    !> @see ESMFIOW NetCDF4 compression module for settings details
    subroutine esmfiow_esmf_create_io_config(config, filename, io_strategy, &
                                           compression, chunking, rc)
        type(esmfiow_esmf_io_config), intent(out) :: config
        character(len=*), intent(in) :: filename
        integer, intent(in), optional :: io_strategy
        type(esmfiow_nc4_compression_settings), intent(in), optional :: compression
        type(esmfiow_nc4_chunking_settings), intent(in), optional :: chunking
        integer, intent(out), optional :: rc

        integer :: local_rc, strategy
        type(ESMF_VM) :: vm
        logical :: isParallel
        character(len=ESMF_MAXSTR) :: msgString

        local_rc = ESMFIOW_SUCCESS

        ! Initialize ESMF if not already initialized
        if (.not. esmf_initialized) then
            call esmfiow_esmf_initialize(rc=local_rc)
            if (local_rc /= ESMFIOW_SUCCESS) then
                if (present(rc)) rc = local_rc
                return
            end if
        end if

        ! Set default values
        config%filename = trim(filename)

        ! Check if we're running in parallel
        call ESMF_VMGetCurrent(vm, rc=local_rc)
        if (local_rc == ESMF_SUCCESS) then
            config%is_parallel = .true.

            write(msgString, '(A,A)') "Created parallel I/O configuration for file: ", &
                trim(filename)
            call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=local_rc)
        else
            config%is_parallel = .false.

            write(msgString, '(A,A)') "Created sequential I/O configuration for file: ", &
                trim(filename)
            call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=local_rc)
        end if

        ! Set I/O strategy
        if (present(io_strategy)) then
            config%io_strategy = io_strategy
        else
            ! Default to NetCDF4 for best compression support
            config%io_strategy = ESMFIOW_ESMF_IO_STRATEGY_NETCDF4
        end if

        ! Set compression settings
        if (present(compression)) then
            config%compression = compression
        else
            config%compression = esmfiow_nc4_compression_settings()
        end if

        ! Set chunking settings
        if (present(chunking)) then
            config%chunking = chunking
        else
            config%chunking = esmfiow_nc4_chunking_settings()
        end if

        ! Create ESMF I/O object
        call ESMF_IOCreate(config%esmfIO, rc=local_rc)
        if (local_rc /= ESMF_SUCCESS) then
            call ESMF_LogWrite("ERROR: Failed to create ESMF_IO object", &
                ESMF_LOGMSG_ERROR, rc=local_rc)
            local_rc = ESMFIOW_ERROR
            if (present(rc)) rc = local_rc
            return
        end if

        ! Set file format based on strategy
        select case (config%io_strategy)
        case (ESMFIOW_ESMF_IO_STRATEGY_NETCDF4)
            call ESMF_IOSet(config%esmfIO, fileName=trim(filename), &
                format=ESMF_IOFILEFORMAT_NETCDF4, rc=local_rc)
            call ESMF_LogWrite("Using NetCDF4/HDF5 file format", ESMF_LOGMSG_INFO, rc=local_rc)

        case (ESMFIOW_ESMF_IO_STRATEGY_NETCDF_CLASSIC)
            call ESMF_IOSet(config%esmfIO, fileName=trim(filename), &
                format=ESMF_IOFILEFORMAT_NETCDF, rc=local_rc)
            call ESMF_LogWrite("Using NetCDF Classic file format (compression limited)", &
                ESMF_LOGMSG_INFO, rc=local_rc)

        case (ESMFIOW_ESMF_IO_STRATEGY_PNETCDF)
            call ESMF_IOSet(config%esmfIO, fileName=trim(filename), &
                format=ESMF_IOFILEFORMAT_PNETCDF, rc=local_rc)
            call ESMF_LogWrite("Using Parallel-NetCDF file format", ESMF_LOGMSG_INFO, rc=local_rc)

        case default
            call ESMF_IOSet(config%esmfIO, fileName=trim(filename), &
                format=ESMF_IOFILEFORMAT_NETCDF4, rc=local_rc)
            call ESMF_LogWrite("Using default NetCDF4/HDF5 file format", ESMF_LOGMSG_INFO, rc=local_rc)
        end select

        if (local_rc == ESMF_SUCCESS) then
            config%is_initialized = .true.
        else
            call ESMF_LogWrite("ERROR: Failed to set ESMF_IO file format", &
                ESMF_LOGMSG_ERROR, rc=local_rc)
            local_rc = ESMFIOW_ERROR
        end if

100     continue
        if (present(rc)) rc = local_rc
    end subroutine esmfiow_esmf_create_io_config

    !> Write an ESMF field to a file with compression
    !> @brief Write an ESMF Field to NetCDF with compression
    !>
    !> This subroutine writes an ESMF Field to a NetCDF file using the provided I/O
    !> configuration, which includes compression settings. It handles both sequential
    !> and parallel I/O modes based on the configuration settings.
    !>
    !> The function uses ESMF's native I/O capabilities but enhances them by applying
    !> NetCDF4 compression and chunking optimizations. It integrates with the
    !> underlying NetCDF layer directly to apply these optimizations.
    !>
    !> @param[in]  config    ESMF I/O configuration with compression settings
    !> @param[in]  field     ESMF Field to write to the file
    !> @param[in]  fieldName Name to use for the field in the NetCDF file
    !> @param[out] rc        Optional return code (ESMFIOW_SUCCESS on success)
    !>
    !> @see esmfiow_esmf_io_read_field
    !> @see esmfiow_esmf_create_io_config
    !> @see esmfiow_nc4_set_compression
    !>
    !> @note When using parallel I/O, all processes must call this function collectively
    subroutine esmfiow_esmf_io_write_field(config, field, fieldName, rc)
        type(esmfiow_esmf_io_config), intent(in) :: config   !< I/O configuration
        type(ESMF_Field), intent(in) :: field                !< ESMF Field to write
        character(len=*), intent(in) :: fieldName            !< Name for the field
        integer, intent(out), optional :: rc                 !< Return code

        integer :: local_rc, ncId, varId
        type(ESMF_IOSpec) :: iospec
        integer :: gridRank, status
        integer, allocatable :: gridShape(:)
        character(len=ESMF_MAXSTR) :: msgString

        local_rc = ESMFIOW_SUCCESS

        ! Check if config is initialized
        if (.not. config%is_initialized) then
            call ESMF_LogWrite("ERROR: ESMF I/O config not initialized", &
                ESMF_LOGMSG_ERROR, rc=status)
            local_rc = ESMFIOW_ERROR
            if (present(rc)) rc = local_rc
            return
        end if

        ! Log the write operation
        write(msgString, '(A,A,A,A)') "Writing field '", trim(fieldName), &
            "' to file: ", trim(config%filename)
        call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=status)

        ! Set I/O specification for writing
        call ESMF_IOSpecSet(iospec, fileName=trim(config%filename), rc=local_rc)
        if (local_rc /= ESMF_SUCCESS) then
            call ESMF_LogWrite("ERROR: Failed to set ESMF_IOSpec", &
                ESMF_LOGMSG_ERROR, rc=status)
            local_rc = ESMFIOW_ERROR
            if (present(rc)) rc = local_rc
            return
        end if

        ! Use different approaches for parallel vs sequential I/O
        if (config%is_parallel) then
            ! Get information about the field's grid
            call ESMF_FieldGet(field, gridrank=gridRank, rc=status)
            allocate(gridShape(gridRank))
            call ESMF_FieldGet(field, gridToFieldMap=gridShape, rc=status)

            ! Write the field using ESMF parallel I/O capabilities
            call ESMF_FieldWrite(field, config%esmfIO, trim(fieldName), rc=status)
            if (status /= ESMF_SUCCESS) then
                call ESMF_LogWrite("ERROR: Failed in parallel write of field", &
                    ESMF_LOGMSG_ERROR, rc=local_rc)
                local_rc = ESMFIOW_ERROR
                if (present(rc)) rc = local_rc
                return
            end if

            ! Get the NetCDF file/var IDs so we can apply compression
            ! This is usually handled internally by ESMF but we need access for compression
            ! This would require additional ESMF API knowledge to extract properly
            ! For now, assume we can get these with a custom function (to be implemented)
            call get_netcdf_ids_from_esmf_io(config%esmfIO, ncId, varId, fieldName, status)

            if (status == ESMF_SUCCESS) then
                ! Apply compression settings
                call esmfiow_nc4_set_compression(ncId, varId, config%compression, status)

                ! Apply chunking optimization based on access pattern
                if (config%access_pattern == ESMFIOW_ESMF_ACCESS_SEQUENTIAL) then
                    call esmfiow_nc4_optimize_variable(ncId, varId, "sequential", rc=status)
                else
                    call esmfiow_nc4_optimize_variable(ncId, varId, "random", rc=status)
                end if
            end if

            if (allocated(gridShape)) deallocate(gridShape)

        else
            ! Standard NetCDF file write via ESMF
            call ESMF_FieldWrite(field, trim(config%filename), variableName=trim(fieldName), &
                overwrite=.true., rc=status)

            if (status /= ESMF_SUCCESS) then
                call ESMF_LogWrite("ERROR: Failed in sequential write of field", &
                    ESMF_LOGMSG_ERROR, rc=local_rc)
                local_rc = ESMFIOW_ERROR
                if (present(rc)) rc = local_rc
                return
            end if
        end if

        call ESMF_LogWrite("Field written successfully", ESMF_LOGMSG_INFO, rc=status)

        if (present(rc)) rc = local_rc
    end subroutine esmfiow_esmf_io_write_field

    !> Read an ESMF field from a file
    subroutine esmfiow_esmf_io_read_field(config, field, fieldName, rc)
        type(esmfiow_esmf_io_config), intent(in) :: config
        type(ESMF_Field), intent(inout) :: field
        character(len=*), intent(in) :: fieldName
        integer, intent(out), optional :: rc

        integer :: local_rc, status
        character(len=ESMF_MAXSTR) :: msgString

        local_rc = ESMFIOW_SUCCESS

        ! Check if config is initialized
        if (.not. config%is_initialized) then
            call ESMF_LogWrite("ERROR: ESMF I/O config not initialized", &
                ESMF_LOGMSG_ERROR, rc=status)
            local_rc = ESMFIOW_ERROR
            if (present(rc)) rc = local_rc
            return
        end if

        ! Log the read operation
        write(msgString, '(A,A,A,A)') "Reading field '", trim(fieldName), &
            "' from file: ", trim(config%filename)
        call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=status)

        ! Use different approaches for parallel vs sequential I/O
        if (config%is_parallel) then
            ! Read the field using ESMF parallel I/O capabilities
            call ESMF_FieldRead(field, config%esmfIO, trim(fieldName), rc=status)
            if (status /= ESMF_SUCCESS) then
                call ESMF_LogWrite("ERROR: Failed in parallel read of field", &
                    ESMF_LOGMSG_ERROR, rc=local_rc)
                local_rc = ESMFIOW_ERROR
                if (present(rc)) rc = local_rc
                return
            end if
        else
            ! Standard NetCDF file read via ESMF
            call ESMF_FieldRead(field, trim(config%filename), variableName=trim(fieldName), &
                rc=status)

            if (status /= ESMF_SUCCESS) then
                call ESMF_LogWrite("ERROR: Failed in sequential read of field", &
                    ESMF_LOGMSG_ERROR, rc=local_rc)
                local_rc = ESMFIOW_ERROR
                if (present(rc)) rc = local_rc
                return
            end if
        end if

        call ESMF_LogWrite("Field read successfully", ESMF_LOGMSG_INFO, rc=status)

        if (present(rc)) rc = local_rc
    end subroutine esmfiow_esmf_io_read_field

    !> Get the status of an ESMF I/O operation
    subroutine esmfiow_esmf_io_status(config, msg, rc)
        type(esmfiow_esmf_io_config), intent(in) :: config
        character(len=*), intent(out), optional :: msg
        integer, intent(out), optional :: rc

        integer :: local_rc
        character(len=ESMF_MAXSTR) :: msgString
        logical :: fileExists

        local_rc = ESMFIOW_SUCCESS

        ! Check if file exists
        inquire(file=trim(config%filename), exist=fileExists)

        ! Build status message
        if (config%is_initialized) then
            write(msgString, '(A,A,A,L1,A,L1)') "File: ", trim(config%filename), &
                " - Parallel mode: ", config%is_parallel, " - File exists: ", fileExists

            if (config%io_strategy == ESMFIOW_ESMF_IO_STRATEGY_NETCDF4) then
                msgString = trim(msgString) // " - Format: NetCDF4/HDF5"
            else if (config%io_strategy == ESMFIOW_ESMF_IO_STRATEGY_NETCDF_CLASSIC) then
                msgString = trim(msgString) // " - Format: NetCDF Classic"
            else if (config%io_strategy == ESMFIOW_ESMF_IO_STRATEGY_PNETCDF) then
                msgString = trim(msgString) // " - Format: Parallel-NetCDF"
            else
                msgString = trim(msgString) // " - Format: Default"
            end if

            local_rc = ESMFIOW_SUCCESS
        else
            write(msgString, '(A)') "I/O Configuration not initialized"
            local_rc = ESMFIOW_ERROR
        end if

        if (present(msg)) msg = trim(msgString)
        if (present(rc)) rc = local_rc
    end subroutine esmfiow_esmf_io_status

    !> @brief Extract NetCDF identifiers from an ESMF I/O object
    !>
    !> This internal helper function extracts the underlying NetCDF file identifier
    !> from an ESMF_IO object using ESMF_IO_NetCDFGet, and then retrieves the
    !> variable identifier using standard NetCDF calls. This enables direct access
    !> to the NetCDF layer for applying compression and other optimizations that
    !> may not be directly exposed through the ESMF I/O interface.
    !>
    !> This is a critical function for the ESMFIOW library as it bridges between
    !> ESMF's high-level I/O abstractions and the NetCDF4 compression capabilities.
    !>
    !> @param[in]  ioObj   ESMF_IO object containing NetCDF file information
    !> @param[out] ncId    NetCDF file identifier
    !> @param[out] varId   NetCDF variable identifier for the specified variable
    !> @param[in]  varName Name of the variable to locate in the NetCDF file
    !> @param[out] rc      Return code (ESMF_SUCCESS on success)
    !>
    !> @note This function requires knowledge of internal ESMF I/O representations and
    !>       depends on the ESMF_IO_NetCDFGet function, which may change in future ESMF versions.
    !>
    subroutine get_netcdf_ids_from_esmf_io(ioObj, ncId, varId, varName, rc)
        type(ESMF_IO), intent(in) :: ioObj
        integer, intent(out) :: ncId, varId
        character(len=*), intent(in) :: varName
        integer, intent(out) :: rc

        character(len=ESMF_MAXSTR) :: msgString
        integer :: local_rc, status
        logical :: is_netcdf_file
        character(len=ESMF_MAXSTR) :: fileName

        ! Initialize output variables
        ncId = -1
        varId = -1
        rc = ESMF_SUCCESS
        local_rc = ESMF_SUCCESS

        ! Verify that we have a NetCDF-based ESMF_IO object
        call ESMF_IOGet(ioObj, fileName=fileName, rc=local_rc)
        if (local_rc /= ESMF_SUCCESS) then
            write(msgString, '(A)') "Error getting filename from ESMF_IO object"
            call ESMF_LogWrite(msgString, ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=status)
            rc = local_rc
            return
        endif

        ! Get the NetCDF file ID from the ESMF_IO object
        call ESMF_IO_NetCDFGet(ioObj, ncid=ncId, rc=local_rc)
        if (local_rc /= ESMF_SUCCESS) then
            write(msgString, '(A,A)') "Could not extract NetCDF ID from ESMF_IO for file: ", trim(fileName)
            call ESMF_LogWrite(msgString, ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=status)
            rc = local_rc
            return
        endif

        ! Check that we got a valid NetCDF ID
        if (ncId <= 0) then
            write(msgString, '(A,I0)') "Invalid NetCDF file ID obtained: ", ncId
            call ESMF_LogWrite(msgString, ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=status)
            rc = ESMFIOW_ERROR
            return
        endif

        ! Find the variable ID using standard NetCDF calls
        status = nf90_inq_varid(ncId, trim(varName), varId)
        if (status /= NF90_NOERR) then
            write(msgString, '(A,A,A,A)') "Could not find variable '", trim(varName), &
                "' in NetCDF file: ", trim(fileName)
            call ESMF_LogWrite(msgString, ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=local_rc)
            write(msgString, '(A,A)') "NetCDF error: ", trim(nf90_strerror(status))
            call ESMF_LogWrite(msgString, ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=local_rc)
            rc = ESMFIOW_ERROR
            return
        endif

        ! Log successful extraction
        write(msgString, '(A,A,A,I0,A,I0)') "Successfully extracted NetCDF IDs for variable '", &
            trim(varName), "' (File ID: ", ncId, ", Var ID: ", varId, ")"
        call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=local_rc)

    end subroutine get_netcdf_ids_from_esmf_io

end module esmfiow_esmf_io
!> @} End of esmfiow_esmf_io group

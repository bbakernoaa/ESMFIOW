!> @file esmfiow_io.f90
!> @brief High-level I/O operations for ESMF fields with CF-compliant NetCDF
!> @author ESMFIOW Development Team
!> @date 2024
!>
!> This module provides high-level I/O operations for ESMF fields, including
!> reading and writing CF-compliant NetCDF files. It acts as the main interface
!> for users who need to perform I/O operations with ESMF fields.
!>
!> The module handles:
!> - Creating and opening CF-compliant NetCDF files
!> - Writing ESMF fields to NetCDF with proper metadata
!> - Reading NetCDF data into ESMF fields
!> - Managing file handles and error states
!>
!> @note All operations use CF conventions for metadata and attributes
!> @see CF Conventions: http://cfconventions.org/

!> @brief High-level I/O operations for ESMF fields with CF-compliant NetCDF
!>
!> This module provides the primary interface for I/O operations between
!> ESMF fields and CF-compliant NetCDF files. It abstracts the complexity
!> of NetCDF operations and ESMF field manipulation.
module esmfiow_io
    use esmfiow_constants
    use esmfiow_cf_utils
    use ESMF
    use ESMF_IOUtilMod
    use ESMF_IO_NetCDFMod
    use netcdf
    implicit none

    private
    public :: esmfiow_file_create, esmfiow_file_open, esmfiow_file_close
    public :: esmfiow_write_field, esmfiow_read_field
    public :: esmfiow_file_handle

    ! File handle type
    type :: esmfiow_file_handle
        integer :: file_id = -1
        character(len=ESMFIOW_MAX_STRING_LEN) :: filename = ""
        logical :: is_open = .false.
        logical :: is_cf_compliant = .false.
        type(ESMF_IO) :: esmf_io  ! ESMF I/O object for parallel operations
        logical :: use_esmf_io = .false.  ! Flag to use ESMF I/O
    end type esmfiow_file_handle

contains

    !> Create a new CF-compliant netCDF file
    !>
    !> This subroutine creates a new NetCDF file with CF-compliant global
    !> attributes. The file is immediately ready for writing field data
    !> and coordinate information.
    !>
    !> @param[in] filename Path and name of the NetCDF file to create
    !> @param[out] file_handle File handle for subsequent operations
    !> @param[in] title Optional title attribute for the file
    !> @param[in] institution Optional institution attribute
    !> @param[out] rc Optional return code (0=success, negative=error)
    !>
    !> @note File handle must be closed with esmfiow_file_close()
    !> @see CF Convention section on global attributes
    subroutine esmfiow_file_create(filename, file_handle, title, institution, rc)
        character(len=*), intent(in) :: filename
        type(esmfiow_file_handle), intent(out) :: file_handle
        character(len=*), intent(in), optional :: title, institution
        integer, intent(out), optional :: rc

        integer :: local_rc, esmf_rc
        type(ESMF_VM) :: vm
        logical :: isPet0

        local_rc = ESMFIOW_SUCCESS

        ! Initialize file handle
        file_handle%filename = filename
        file_handle%is_open = .false.
        file_handle%is_cf_compliant = .true.
        file_handle%use_esmf_io = .true.

        ! Get VM to check if this is the root processor
        call ESMF_VMGetCurrent(vm, rc=esmf_rc)
        if (esmf_rc /= ESMF_SUCCESS) then
            local_rc = ESMFIOW_ERROR
            if (present(rc)) rc = local_rc
            return
        end if

        ! Use ESMF I/O for parallel-safe file creation
        file_handle%esmf_io = ESMF_IOCreate(filename, &
                                          convention=ESMF_FILEFORMAT_NETCDF, &
                                          rc=esmf_rc)
        if (esmf_rc /= ESMF_SUCCESS) then
            local_rc = ESMFIOW_ERROR
            call ESMF_LogWrite("Failed to create ESMF IO object", &
                              ESMF_LOGMSG_ERROR, rc=esmf_rc)
            if (present(rc)) rc = local_rc
            return
        end if

        ! Get the underlying NetCDF file ID from ESMF
        call get_netcdf_file_id_from_esmf_io(file_handle%esmf_io, &
                                            file_handle%file_id, local_rc)
        if (local_rc /= ESMFIOW_SUCCESS) then
            if (present(rc)) rc = local_rc
            return
        end if

        ! Add CF global attributes (only on root processor for parallel runs)
        call ESMF_VMGet(vm, localPet=esmf_rc)
        isPet0 = (esmf_rc == 0)

        if (isPet0) then
            call cf_add_global_attributes(file_handle%file_id, &
                                        title=title, institution=institution, rc=local_rc)
        end if

        ! Synchronize across all processors
        call ESMF_VMBarrier(vm, rc=esmf_rc)

        if (local_rc == ESMFIOW_SUCCESS) then
            file_handle%is_open = .true.
        endif

        if (present(rc)) rc = local_rc

    end subroutine esmfiow_file_create

    !> Open an existing netCDF file
    !>
    !> This subroutine opens an existing NetCDF file for reading or writing.
    !> It checks for CF compliance and initializes the file handle for
    !> subsequent operations.
    !>
    !> @param[in] filename Path and name of the NetCDF file to open
    !> @param[out] file_handle File handle for subsequent operations
    !> @param[in] readonly Optional flag for read-only access (default: false)
    !> @param[out] rc Optional return code (0=success, negative=error)
    !>
    !> @note File handle must be closed with esmfiow_file_close()
    !> @see esmfiow_file_create for creating new files
    subroutine esmfiow_file_open(filename, file_handle, readonly, rc)
        character(len=*), intent(in) :: filename
        type(esmfiow_file_handle), intent(out) :: file_handle
        logical, intent(in), optional :: readonly
        integer, intent(out), optional :: rc

        integer :: local_rc, esmf_rc
        logical :: read_only
        type(ESMF_FileFormat_Flag) :: file_format

        local_rc = ESMFIOW_SUCCESS
        read_only = .false.
        if (present(readonly)) read_only = readonly

        ! Initialize file handle
        file_handle%filename = filename
        file_handle%is_open = .false.
        file_handle%is_cf_compliant = .false.  ! Will be checked
        file_handle%use_esmf_io = .true.

        ! Open file using ESMF I/O for parallel safety
        if (read_only) then
            file_handle%esmf_io = ESMF_IOCreate(filename, &
                                              convention=ESMF_FILEFORMAT_NETCDF, &
                                              rc=esmf_rc)
        else
            file_handle%esmf_io = ESMF_IOCreate(filename, &
                                              convention=ESMF_FILEFORMAT_NETCDF, &
                                              rc=esmf_rc)
        end if

        if (esmf_rc /= ESMF_SUCCESS) then
            local_rc = ESMFIOW_ERROR
            call ESMF_LogWrite("Failed to open file with ESMF IO", &
                              ESMF_LOGMSG_ERROR, rc=esmf_rc)
            if (present(rc)) rc = local_rc
            return
        end if

        ! Get the underlying NetCDF file ID
        call get_netcdf_file_id_from_esmf_io(file_handle%esmf_io, &
                                            file_handle%file_id, local_rc)
        if (local_rc /= ESMFIOW_SUCCESS) then
            if (present(rc)) rc = local_rc
            return
        end if

        ! Check if file is CF compliant (basic check for Conventions attribute)
        call check_cf_compliance(file_handle, local_rc)

        if (local_rc == ESMFIOW_SUCCESS) then
            file_handle%is_open = .true.
        endif

        if (present(rc)) rc = local_rc

    end subroutine esmfiow_file_open

    !> Close a netCDF file
    !>
    !> This subroutine properly closes a NetCDF file and cleans up
    !> the associated file handle. All data is flushed to disk.
    !>
    !> @param[inout] file_handle File handle to close (will be marked as closed)
    !> @param[out] rc Optional return code (0=success, negative=error)
    !>
    !> @note Should always be called to properly close files
    subroutine esmfiow_file_close(file_handle, rc)
        type(esmfiow_file_handle), intent(inout) :: file_handle
        integer, intent(out), optional :: rc

        integer :: local_rc, esmf_rc

        local_rc = ESMFIOW_SUCCESS

        if (file_handle%is_open) then
            ! Close ESMF I/O object
            if (file_handle%use_esmf_io) then
                call ESMF_IODestroy(file_handle%esmf_io, rc=esmf_rc)
                if (esmf_rc /= ESMF_SUCCESS) then
                    local_rc = ESMFIOW_ERROR
                    call ESMF_LogWrite("Failed to destroy ESMF IO object", &
                                      ESMF_LOGMSG_ERROR, rc=esmf_rc)
                end if
            end if
            file_handle%is_open = .false.
            file_handle%file_id = -1
        endif

        if (present(rc)) rc = local_rc

    end subroutine esmfiow_file_close

    !> Write an ESMF field to file
    !>
    !> This subroutine extracts data from an ESMF field and writes it to
    !> a NetCDF file with CF-compliant attributes and metadata.
    !>
    !> @param[in] file_handle File handle for the target NetCDF file
    !> @param[in] field ESMF field containing the data to write
    !> @param[in] variable_name Name for the variable in the NetCDF file
    !> @param[out] rc Optional return code (0=success, negative=error)
    !>
    !> @note File must be open before calling this subroutine
    !> @note Uses structured error handling instead of goto statements
    subroutine esmfiow_write_field(file_handle, field, variable_name, rc)
        type(esmfiow_file_handle), intent(in) :: file_handle
        type(ESMF_Field), intent(in) :: field
        character(len=*), intent(in) :: variable_name
        integer, intent(out), optional :: rc

        integer :: local_rc, esmf_rc

        local_rc = ESMFIOW_SUCCESS

        if (.not. file_handle%is_open) then
            local_rc = ESMFIOW_ERROR
            call ESMF_LogWrite("File is not open for writing", &
                              ESMF_LOGMSG_ERROR, rc=esmf_rc)
            if (present(rc)) rc = local_rc
            return
        endif

        ! Use ESMF I/O to write field data directly
        call ESMF_IOWrite(file_handle%esmf_io, field, variable_name, rc=esmf_rc)
        if (esmf_rc /= ESMF_SUCCESS) then
            local_rc = ESMFIOW_ERROR
            call ESMF_LogWrite("Failed to write field to file", &
                              ESMF_LOGMSG_ERROR, rc=esmf_rc)
        else
            call ESMF_LogWrite("Successfully wrote field: " // trim(variable_name), &
                              ESMF_LOGMSG_INFO, rc=esmf_rc)
        end if

        if (present(rc)) rc = local_rc

    end subroutine esmfiow_write_field

    !> Read a field from file into ESMF field
    !>
    !> This subroutine reads data from a NetCDF variable and populates
    !> an ESMF field with the data and metadata. It handles coordinate
    !> mapping and CF convention interpretation.
    !>
    !> @param[in] file_handle File handle for the source NetCDF file
    !> @param[in] variable_name Name of the variable to read from the NetCDF file
    !> @param[inout] field ESMF field to populate with the data
    !> @param[out] rc Optional return code (0=success, negative=error)
    !>
    !> @note File must be open before calling this subroutine
    !> @note Uses structured error handling for robustness
    subroutine esmfiow_read_field(file_handle, variable_name, field, rc)
        type(esmfiow_file_handle), intent(in) :: file_handle
        character(len=*), intent(in) :: variable_name
        type(ESMF_Field), intent(inout) :: field
        integer, intent(out), optional :: rc

        integer :: local_rc, esmf_rc

        local_rc = ESMFIOW_SUCCESS

        if (.not. file_handle%is_open) then
            local_rc = ESMFIOW_ERROR
            call ESMF_LogWrite("File is not open for reading", &
                              ESMF_LOGMSG_ERROR, rc=esmf_rc)
            if (present(rc)) rc = local_rc
            return
        endif

        ! Use ESMF I/O to read field data directly
        call ESMF_IORead(file_handle%esmf_io, field, variable_name, rc=esmf_rc)
        if (esmf_rc /= ESMF_SUCCESS) then
            local_rc = ESMFIOW_ERROR
            call ESMF_LogWrite("Failed to read field from file", &
                              ESMF_LOGMSG_ERROR, rc=esmf_rc)
        else
            call ESMF_LogWrite("Successfully read field: " // trim(variable_name), &
                              ESMF_LOGMSG_INFO, rc=esmf_rc)
        end if

        if (present(rc)) rc = local_rc

    end subroutine esmfiow_read_field

    !> Check if a NetCDF file follows CF conventions
    !>
    !> This subroutine checks for basic CF compliance by looking for
    !> the required Conventions global attribute.
    !>
    !> @param[inout] file_handle File handle to check
    !> @param[out] rc Return code (0=success, negative=error)
    subroutine check_cf_compliance(file_handle, rc)
        use netcdf
        use ESMF
        type(esmfiow_file_handle), intent(inout) :: file_handle
        integer, intent(out) :: rc

        integer :: nc_rc, att_len
        character(len=256) :: conventions_str

        rc = ESMFIOW_SUCCESS

        ! Check for Conventions attribute
        nc_rc = nf90_inquire_attribute(file_handle%file_id, NF90_GLOBAL, &
                                      "Conventions", len=att_len)
        if (nc_rc /= NF90_NOERR) then
            file_handle%is_cf_compliant = .false.
            call ESMF_LogWrite("WARNING: No Conventions attribute found", &
                              ESMF_LOGMSG_WARN, rc=nc_rc)
            return
        end if

        ! Read the Conventions attribute
        nc_rc = nf90_get_att(file_handle%file_id, NF90_GLOBAL, &
                            "Conventions", conventions_str)
        if (nc_rc /= NF90_NOERR) then
            file_handle%is_cf_compliant = .false.
            rc = ESMFIOW_ERROR
            return
        end if

        ! Check if it contains CF
        if (index(conventions_str, "CF") > 0) then
            file_handle%is_cf_compliant = .true.
            call ESMF_LogWrite("File follows CF conventions", &
                              ESMF_LOGMSG_INFO, rc=nc_rc)
        else
            file_handle%is_cf_compliant = .false.
            call ESMF_LogWrite("WARNING: File may not follow CF conventions", &
                              ESMF_LOGMSG_WARN, rc=nc_rc)
        end if

    end subroutine check_cf_compliance

    !> @brief Extract NetCDF file ID from ESMF I/O object
    !>
    !> This helper subroutine extracts the underlying NetCDF file identifier
    !> from an ESMF_IO object. This enables direct access to the NetCDF layer
    !> for applying compression and other optimizations.
    !>
    !> @param[in]  esmf_io  ESMF_IO object containing NetCDF file information
    !> @param[out] file_id  NetCDF file identifier
    !> @param[out] rc       Return code (ESMFIOW_SUCCESS on success)
    subroutine get_netcdf_file_id_from_esmf_io(esmf_io, file_id, rc)
        type(ESMF_IO), intent(in) :: esmf_io
        integer, intent(out) :: file_id
        integer, intent(out) :: rc

        integer :: esmf_rc, local_rc
        character(len=ESMF_MAXSTR) :: filename, msgString
        logical :: is_netcdf_file

        rc = ESMFIOW_SUCCESS
        file_id = -1

        ! Get filename from ESMF_IO object to verify it's NetCDF
        call ESMF_IOGet(esmf_io, fileName=filename, rc=local_rc)
        if (local_rc /= ESMF_SUCCESS) then
            call ESMF_LogWrite("ERROR: Could not get filename from ESMF_IO object", &
                              ESMF_LOGMSG_ERROR, rc=esmf_rc)
            rc = ESMFIOW_ERROR
            return
        end if

        ! Check if this is likely a NetCDF file
        is_netcdf_file = (index(filename, '.nc') > 0 .or. index(filename, '.nc4') > 0 .or. &
                         index(filename, '.netcdf') > 0)

        if (.not. is_netcdf_file) then
            write(msgString, '(A,A)') "WARNING: File may not be NetCDF format: ", trim(filename)
            call ESMF_LogWrite(msgString, ESMF_LOGMSG_WARNING, rc=esmf_rc)
        end if

        ! Extract NetCDF file ID using ESMF's NetCDF interface
        call ESMF_IO_NetCDFGet(esmf_io, ncid=file_id, rc=local_rc)
        if (local_rc /= ESMF_SUCCESS) then
            write(msgString, '(A,A)') "ERROR: Could not extract NetCDF ID from ESMF_IO for file: ", &
                trim(filename)
            call ESMF_LogWrite(msgString, ESMF_LOGMSG_ERROR, rc=esmf_rc)
            rc = ESMFIOW_ERROR
            return
        end if

        ! Verify we got a valid NetCDF file ID
        if (file_id <= 0) then
            write(msgString, '(A,I0,A,A)') "ERROR: Invalid NetCDF file ID (", file_id, &
                ") obtained for file: ", trim(filename)
            call ESMF_LogWrite(msgString, ESMF_LOGMSG_ERROR, rc=esmf_rc)
            rc = ESMFIOW_ERROR
            return
        end if

        ! Log successful extraction
        write(msgString, '(A,A,A,I0)') "Successfully extracted NetCDF file ID for '", &
            trim(filename), "' (ID: ", file_id, ")"
        call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=esmf_rc)

    end subroutine get_netcdf_file_id_from_esmf_io

end module esmfiow_io

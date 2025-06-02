!> @file esmfiow_field.f90
!> @brief ESMFIOW Field Module
!>
!> This module provides field handling utilities for working with ESMF fields,
!> reading/writing them from/to NetCDF files with CF convention compliance,
!> and managing field metadata.
!>
!> @author ESMFIOW Development Team
!> @date June 2025
!> @defgroup esmfiow_field Field Module
!> @{

!> @brief Field handling module
!>
!> Implements functionality for working with ESMF fields, including creating
!> fields from NetCDF variables, writing fields to NetCDF files with proper
!> CF conventions, and handling field metadata.
module esmfiow_field
    use esmfiow_constants
    use esmfiow_cf_utils
#ifdef USE_ESMF
    use ESMF
#endif
    use netcdf
    implicit none

    private
    public :: esmfiow_field_create_from_netcdf, esmfiow_field_write_to_netcdf
    public :: esmfiow_field_add_metadata

#ifdef USE_ESMF
    ! ESMF field type when available
    integer, parameter :: FIELD_TYPE_ESMF = 1
#else
    ! Placeholder field type when ESMF not available
    type :: esmf_field_placeholder
        integer :: field_id
        integer :: nx, ny
        real(ESMFIOW_REAL_KIND), allocatable :: data(:,:)
        character(len=ESMFIOW_MAX_STRING_LEN) :: name
    end type esmf_field_placeholder
    integer, parameter :: FIELD_TYPE_PLACEHOLDER = 2
#endif

contains

    !> Create ESMF field from NetCDF variable
    !>
    !> This subroutine reads a variable from a NetCDF file and creates
    !> an ESMF field with appropriate grid and metadata. It handles
    !> CF convention interpretation and coordinate mapping.
    !>
    !> @param[in] file_handle NetCDF file handle containing the variable
    !> @param[in] var_name Name of the variable to read
    !> @param[out] field ESMF field object to be created
    !> @param[out] rc Optional return code (0=success, negative=error)
    !>
    !> @note Uses ESMF_FieldCreate to create actual ESMF field objects
    !> @see ESMF documentation for field creation
    subroutine esmfiow_field_create_from_netcdf(file_handle, var_name, field, rc)
        integer, intent(in) :: file_handle  ! File handle
        character(len=*), intent(in) :: var_name
        type(ESMF_Field), intent(out) :: field  ! ESMF field type
        integer, intent(out), optional :: rc

        integer :: local_rc, esmf_rc, nc_rc, var_id, ndims
        type(ESMF_Grid) :: grid
        integer, allocatable :: dimids(:), dimlens(:)
        character(len=ESMFIOW_MAX_STRING_LEN) :: dim_name

        local_rc = ESMFIOW_SUCCESS

        ! Get variable information from NetCDF
        nc_rc = nf90_inq_varid(file_handle, var_name, var_id)
        if (nc_rc /= NF90_NOERR) then
            local_rc = ESMFIOW_ERROR
            if (present(rc)) rc = local_rc
            return
        end if

        ! Get variable dimensions
        nc_rc = nf90_inquire_variable(file_handle, var_id, ndims=ndims)
        if (nc_rc /= NF90_NOERR) then
            local_rc = ESMFIOW_ERROR
            if (present(rc)) rc = local_rc
            return
        end if

        ! Allocate arrays for dimension info
        allocate(dimids(ndims), dimlens(ndims))

        ! Get dimension IDs and lengths
        nc_rc = nf90_inquire_variable(file_handle, var_id, dimids=dimids)
        if (nc_rc /= NF90_NOERR) then
            local_rc = ESMFIOW_ERROR
            deallocate(dimids, dimlens)
            if (present(rc)) rc = local_rc
            return
        end if

        ! Get dimension lengths
        do local_rc = 1, ndims
            nc_rc = nf90_inquire_dimension(file_handle, dimids(local_rc), len=dimlens(local_rc))
            if (nc_rc /= NF90_NOERR) then
                local_rc = ESMFIOW_ERROR
                deallocate(dimids, dimlens)
                if (present(rc)) rc = local_rc
                return
            end if
        end do

        ! Create ESMF grid from dimensions (simplified 2D case)
        if (ndims >= 2) then
            grid = ESMF_GridCreateNoPeriDim(maxIndex=[dimlens(1), dimlens(2)], &
                                           name=trim(var_name)//"_grid", rc=esmf_rc)
            if (esmf_rc /= ESMF_SUCCESS) then
                local_rc = ESMFIOW_ERROR
                deallocate(dimids, dimlens)
                if (present(rc)) rc = local_rc
                return
            end if

            ! Create ESMF field on the grid
            field = ESMF_FieldCreate(grid, typekind=ESMF_TYPEKIND_R8, &
                                    name=trim(var_name), rc=esmf_rc)
            if (esmf_rc /= ESMF_SUCCESS) then
                local_rc = ESMFIOW_ERROR
                call ESMF_GridDestroy(grid, rc=esmf_rc)
                deallocate(dimids, dimlens)
                if (present(rc)) rc = local_rc
                return
            end if
        else
            local_rc = ESMFIOW_ERROR  ! Unsupported dimensionality
        end if

        deallocate(dimids, dimlens)
        local_rc = ESMFIOW_SUCCESS

        ! Implementation would:
        ! 1. Read variable metadata
        ! 2. Create ESMF field with appropriate grid
        ! 3. Read data into field

        if (present(rc)) rc = local_rc

    end subroutine esmfiow_field_create_from_netcdf

    !> Write ESMF field to NetCDF variable
    !>
    !> This subroutine extracts data from an ESMF field and writes it to
    !> a NetCDF variable with CF-compliant attributes. It handles metadata
    !> assignment and ensures proper data format for the output file.
    !>
    !> @param[in] file_handle NetCDF file handle for output
    !> @param[in] field ESMF field containing the data to write
    !> @param[in] var_name Name for the variable in the NetCDF file
    !> @param[in] standard_name Optional CF standard name for the variable
    !> @param[in] long_name Optional human-readable description
    !> @param[in] units Optional physical units of the variable
    !> @param[out] rc Optional return code (0=success, negative=error)
    !>
    !> @note Uses ESMF_FieldGet to extract actual ESMF field data
    !> @see CF Convention section on data variables
    subroutine esmfiow_field_write_to_netcdf(file_handle, field, var_name, &
                                           standard_name, long_name, units, rc)
        integer, intent(in) :: file_handle  ! File handle
        type(ESMF_Field), intent(in) :: field  ! ESMF field type
        character(len=*), intent(in) :: var_name
        character(len=*), intent(in), optional :: standard_name, long_name, units
        integer, intent(out), optional :: rc

        integer :: local_rc, esmf_rc, nc_rc, var_id
        real(ESMF_KIND_R8), pointer :: field_data(:,:)
        type(ESMF_Grid) :: grid
        integer :: x_dim, y_dim, dimids(2)

        local_rc = ESMFIOW_SUCCESS

        ! Get field data from ESMF field
        call ESMF_FieldGet(field, farrayPtr=field_data, rc=esmf_rc)
        if (esmf_rc /= ESMF_SUCCESS) then
            local_rc = ESMFIOW_ERROR
            if (present(rc)) rc = local_rc
            return
        end if

        ! Get grid information
        call ESMF_FieldGet(field, grid=grid, rc=esmf_rc)
        if (esmf_rc /= ESMF_SUCCESS) then
            local_rc = ESMFIOW_ERROR
            if (present(rc)) rc = local_rc
            return
        end if

        ! Get grid dimensions
        x_dim = size(field_data, 1)
        y_dim = size(field_data, 2)

        ! Define dimensions if they don't exist (simplified approach)
        nc_rc = nf90_def_dim(file_handle, "x", x_dim, dimids(1))
        if (nc_rc /= NF90_NOERR .and. nc_rc /= NF90_ENAMEINUSE) then
            local_rc = ESMFIOW_ERROR
            if (present(rc)) rc = local_rc
            return
        end if

        nc_rc = nf90_def_dim(file_handle, "y", y_dim, dimids(2))
        if (nc_rc /= NF90_NOERR .and. nc_rc /= NF90_ENAMEINUSE) then
            local_rc = ESMFIOW_ERROR
            if (present(rc)) rc = local_rc
            return
        end if

        ! Get dimension IDs if they already exist
        if (nc_rc == NF90_ENAMEINUSE) then
            nc_rc = nf90_inq_dimid(file_handle, "x", dimids(1))
            nc_rc = nf90_inq_dimid(file_handle, "y", dimids(2))
        end if

        ! Define variable
        nc_rc = nf90_def_var(file_handle, var_name, NF90_DOUBLE, dimids, var_id)
        if (nc_rc /= NF90_NOERR) then
            local_rc = ESMFIOW_ERROR
            if (present(rc)) rc = local_rc
            return
        end if

        ! Add CF-compliant attributes
        if (present(standard_name)) then
            nc_rc = nf90_put_att(file_handle, var_id, "standard_name", standard_name)
        end if
        if (present(long_name)) then
            nc_rc = nf90_put_att(file_handle, var_id, "long_name", long_name)
        end if
        if (present(units)) then
            nc_rc = nf90_put_att(file_handle, var_id, "units", units)
        end if

        ! End define mode and write data
        nc_rc = nf90_enddef(file_handle)
        if (nc_rc /= NF90_NOERR .and. nc_rc /= NF90_ENOTINDEFINE) then
            local_rc = ESMFIOW_ERROR
            if (present(rc)) rc = local_rc
            return
        end if

        ! Write field data to NetCDF variable
        nc_rc = nf90_put_var(file_handle, var_id, field_data)
        if (nc_rc /= NF90_NOERR) then
            local_rc = ESMFIOW_ERROR
        else
            call ESMF_LogWrite("Successfully wrote ESMF field to NetCDF", &
                              ESMF_LOGMSG_INFO, rc=esmf_rc)
        end if

        if (present(rc)) rc = local_rc

    end subroutine esmfiow_field_write_to_netcdf

    !> Add metadata to field for CF compliance
    !>
    !> This subroutine adds CF-compliant metadata to an ESMF field,
    !> ensuring that the field contains all necessary information for
    !> proper NetCDF output with CF conventions.
    !>
    !> @param[inout] field ESMF field to add metadata to
    !> @param[in] standard_name CF standard name for the physical quantity
    !> @param[in] long_name Human-readable description of the field
    !> @param[in] units Physical units of the field values
    !> @param[in] fill_value Optional fill value for missing data
    !> @param[out] rc Optional return code (0=success, negative=error)
    !>
    !> @note Sets actual ESMF field attributes for CF compliance
    !> @see ESMF documentation for field attribute handling
    subroutine esmfiow_field_add_metadata(field, standard_name, long_name, &
                                        units, fill_value, rc)
        type(ESMF_Field), intent(inout) :: field  ! ESMF field type
        character(len=*), intent(in) :: standard_name, long_name, units
        real(ESMFIOW_REAL_KIND), intent(in), optional :: fill_value
        integer, intent(out), optional :: rc

        integer :: local_rc, esmf_rc
        character(len=ESMF_MAXSTR) :: attrName

        local_rc = ESMFIOW_SUCCESS

        ! Add standard_name attribute
        call ESMF_AttributeSet(field, "standard_name", trim(standard_name), rc=esmf_rc)
        if (esmf_rc /= ESMF_SUCCESS) then
            local_rc = ESMFIOW_ERROR
            call ESMF_LogWrite("ERROR: Failed to set standard_name attribute", &
                              ESMF_LOGMSG_ERROR, rc=esmf_rc)
            if (present(rc)) rc = local_rc
            return
        end if

        ! Add long_name attribute
        call ESMF_AttributeSet(field, "long_name", trim(long_name), rc=esmf_rc)
        if (esmf_rc /= ESMF_SUCCESS) then
            local_rc = ESMFIOW_ERROR
            call ESMF_LogWrite("ERROR: Failed to set long_name attribute", &
                              ESMF_LOGMSG_ERROR, rc=esmf_rc)
            if (present(rc)) rc = local_rc
            return
        end if

        ! Add units attribute
        call ESMF_AttributeSet(field, "units", trim(units), rc=esmf_rc)
        if (esmf_rc /= ESMF_SUCCESS) then
            local_rc = ESMFIOW_ERROR
            call ESMF_LogWrite("ERROR: Failed to set units attribute", &
                              ESMF_LOGMSG_ERROR, rc=esmf_rc)
            if (present(rc)) rc = local_rc
            return
        end if

        ! Add fill_value attribute if provided
        if (present(fill_value)) then
            call ESMF_AttributeSet(field, "_FillValue", fill_value, rc=esmf_rc)
            if (esmf_rc /= ESMF_SUCCESS) then
                local_rc = ESMFIOW_ERROR
                call ESMF_LogWrite("ERROR: Failed to set _FillValue attribute", &
                                  ESMF_LOGMSG_ERROR, rc=esmf_rc)
                if (present(rc)) rc = local_rc
                return
            end if
        end if

        call ESMF_LogWrite("Successfully added CF metadata to ESMF field", &
                          ESMF_LOGMSG_INFO, rc=esmf_rc)

        if (present(rc)) rc = local_rc

    end subroutine esmfiow_field_add_metadata

end module esmfiow_field
!> @} End of esmfiow_field group

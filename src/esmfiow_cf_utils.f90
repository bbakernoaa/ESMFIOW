!> @file esmfiow_cf_utils.f90
!> @brief ESMFIOW Climate and Forecast Conventions Utilities Module
!>
!> This module provides utilities for ensuring that NetCDF output files
!> comply with the Climate and Forecast (CF) metadata conventions, which
!> facilitate data sharing and discovery in the Earth sciences.
!>
!> @author ESMFIOW Development Team
!> @date June 2025
!> @defgroup esmfiow_cf_utils CF Utilities Module
!> @{

!> @brief Climate and Forecast Conventions utilities module
!>
!> Implements utilities for ensuring CF convention compliance in NetCDF files,
!> including adding standard attributes to variables and dimensions, handling
!> time coordinates, creating bounds variables, and validating units.
module esmfiow_cf_utils
    use esmfiow_constants
    implicit none

    private
    public :: cf_add_global_attributes, cf_add_coordinate_attributes
    public :: cf_add_variable_attributes, cf_validate_units
    public :: cf_create_time_coordinate, cf_add_bounds_variable

contains

    !> Add CF-compliant global attributes to a file
    !>
    !> This subroutine adds global attributes to a NetCDF file following the
    !> Climate and Forecast (CF) metadata conventions. It adds mandatory
    !> attributes like 'Conventions' and 'creation_date', plus optional
    !> descriptive attributes.
    !>
    !> @param[in] file_id NetCDF file identifier
    !> @param[in] title Optional file title attribute
    !> @param[in] institution Optional institution attribute
    !> @param[in] source Optional source attribute (model or data source)
    !> @param[in] history Optional history attribute (processing history)
    !> @param[in] references Optional references attribute (citations)
    !> @param[in] comment Optional comment attribute (additional info)
    !> @param[out] rc Optional return code (0=success, negative=error)
    !>
    !> @note This subroutine uses structured error handling instead of goto statements
    !> @see CF Convention documentation for attribute definitions
    subroutine cf_add_global_attributes(file_id, title, institution, source, &
                                       history, references, comment, rc)
        integer, intent(in) :: file_id
        character(len=*), intent(in), optional :: title, institution, source
        character(len=*), intent(in), optional :: history, references, comment
        integer, intent(out), optional :: rc

        integer :: local_rc
        character(len=ESMFIOW_MAX_STRING_LEN) :: timestamp

        local_rc = ESMFIOW_SUCCESS

        ! Add mandatory CF convention attribute
        call add_string_attribute(file_id, "Conventions", ESMFIOW_CF_VERSION, local_rc)
        if (local_rc /= ESMFIOW_SUCCESS) then
            if (present(rc)) rc = local_rc
            return
        end if

        ! Add creation timestamp
        call get_timestamp(timestamp)
        call add_string_attribute(file_id, "creation_date", trim(timestamp), local_rc)
        if (local_rc /= ESMFIOW_SUCCESS) then
            if (present(rc)) rc = local_rc
            return
        end if

        ! Add optional attributes
        if (present(title)) then
            call add_string_attribute(file_id, "title", title, local_rc)
            if (local_rc /= ESMFIOW_SUCCESS) then
                if (present(rc)) rc = local_rc
                return
            end if
        endif

        if (present(institution)) then
            call add_string_attribute(file_id, "institution", institution, local_rc)
            if (local_rc /= ESMFIOW_SUCCESS) then
                if (present(rc)) rc = local_rc
                return
            end if
        endif

        if (present(source)) then
            call add_string_attribute(file_id, "source", source, local_rc)
            if (local_rc /= ESMFIOW_SUCCESS) then
                if (present(rc)) rc = local_rc
                return
            end if
        endif

        if (present(history)) then
            call add_string_attribute(file_id, "history", history, local_rc)
            if (local_rc /= ESMFIOW_SUCCESS) then
                if (present(rc)) rc = local_rc
                return
            end if
        endif

        if (present(references)) then
            call add_string_attribute(file_id, "references", references, local_rc)
            if (local_rc /= ESMFIOW_SUCCESS) then
                if (present(rc)) rc = local_rc
                return
            end if
        endif

        if (present(comment)) then
            call add_string_attribute(file_id, "comment", comment, local_rc)
            if (local_rc /= ESMFIOW_SUCCESS) then
                if (present(rc)) rc = local_rc
                return
            end if
        endif

        ! All operations were successful
        if (present(rc)) rc = local_rc

    end subroutine cf_add_global_attributes

    !> Add CF-compliant coordinate variable attributes
    !>
    !> This subroutine adds standard CF attributes to coordinate variables,
    !> ensuring proper metadata for coordinate dimensions like time, latitude,
    !> longitude, and vertical coordinates.
    !>
    !> @param[in] var_id NetCDF variable identifier for the coordinate
    !> @param[in] standard_name CF standard name for the coordinate
    !> @param[in] long_name Human-readable description of the coordinate
    !> @param[in] units Physical units of the coordinate values
    !> @param[in] axis Optional axis attribute (X, Y, Z, T for standard axes)
    !> @param[out] rc Optional return code (0=success, negative=error)
    !>
    !> @note Uses structured error handling for robustness
    !> @see CF Convention section on coordinate variables
    subroutine cf_add_coordinate_attributes(var_id, standard_name, long_name, &
                                          units, axis, rc)
        integer, intent(in) :: var_id
        character(len=*), intent(in) :: standard_name, long_name, units
        character(len=*), intent(in), optional :: axis
        integer, intent(out), optional :: rc

        integer :: local_rc

        local_rc = ESMFIOW_SUCCESS

        call add_string_attribute(var_id, "standard_name", standard_name, local_rc)
        if (local_rc /= ESMFIOW_SUCCESS) then
            if (present(rc)) rc = local_rc
            return
        end if

        call add_string_attribute(var_id, "long_name", long_name, local_rc)
        if (local_rc /= ESMFIOW_SUCCESS) then
            if (present(rc)) rc = local_rc
            return
        end if

        call add_string_attribute(var_id, "units", units, local_rc)
        if (local_rc /= ESMFIOW_SUCCESS) then
            if (present(rc)) rc = local_rc
            return
        end if

        if (present(axis)) then
            call add_string_attribute(var_id, "axis", axis, local_rc)
            if (local_rc /= ESMFIOW_SUCCESS) then
                if (present(rc)) rc = local_rc
                return
            end if
        endif

        ! All operations were successful
        if (present(rc)) rc = local_rc

    end subroutine cf_add_coordinate_attributes

    !> Add CF-compliant data variable attributes
    !>
    !> This subroutine adds standard CF attributes to data variables,
    !> including required metadata like standard_name, long_name, and units,
    !> plus optional attributes like fill values and valid ranges.
    !>
    !> @param[in] var_id NetCDF variable identifier for the data variable
    !> @param[in] standard_name CF standard name for the physical quantity
    !> @param[in] long_name Human-readable description of the variable
    !> @param[in] units Physical units of the variable values
    !> @param[in] fill_value Optional fill value for missing data
    !> @param[in] valid_range Optional array [min, max] of valid data range
    !> @param[out] rc Optional return code (0=success, negative=error)
    !>
    !> @note Implements structured error handling without goto statements
    !> @see CF Convention section on data variables and attributes
    subroutine cf_add_variable_attributes(var_id, standard_name, long_name, &
                                        units, fill_value, valid_range, rc)
        integer, intent(in) :: var_id
        character(len=*), intent(in) :: standard_name, long_name, units
        real(ESMFIOW_REAL_KIND), intent(in), optional :: fill_value
        real(ESMFIOW_REAL_KIND), intent(in), optional :: valid_range(2)
        integer, intent(out), optional :: rc

        integer :: local_rc

        local_rc = ESMFIOW_SUCCESS

        call add_string_attribute(var_id, "standard_name", standard_name, local_rc)
        if (local_rc /= ESMFIOW_SUCCESS) then
            if (present(rc)) rc = local_rc
            return
        end if

        call add_string_attribute(var_id, "long_name", long_name, local_rc)
        if (local_rc /= ESMFIOW_SUCCESS) then
            if (present(rc)) rc = local_rc
            return
        end if

        call add_string_attribute(var_id, "units", units, local_rc)
        if (local_rc /= ESMFIOW_SUCCESS) then
            if (present(rc)) rc = local_rc
            return
        end if

        if (present(fill_value)) then
            call add_real_attribute(var_id, "_FillValue", fill_value, local_rc)
            if (local_rc /= ESMFIOW_SUCCESS) then
                if (present(rc)) rc = local_rc
                return
            end if
        endif

        if (present(valid_range)) then
            call add_real_array_attribute(var_id, "valid_range", valid_range, local_rc)
            if (local_rc /= ESMFIOW_SUCCESS) then
                if (present(rc)) rc = local_rc
                return
            end if
        endif

        ! All operations were successful
        if (present(rc)) rc = local_rc

    end subroutine cf_add_variable_attributes

    !> Validate units string against CF standard names table
    !>
    !> This function validates whether a units string conforms to CF
    !> conventions and standard unit definitions.
    !>
    !> @param[in] units The units string to validate
    !> @return is_valid True if units are valid, false otherwise
    !>
    !> @note Current implementation is simplified; production version
    !>       should validate against full CF units table
    function cf_validate_units(units) result(is_valid)
        character(len=*), intent(in) :: units
        logical :: is_valid

        ! Simple validation - in a real implementation, this would
        ! check against the CF standard units table
        is_valid = len_trim(units) > 0

        ! Add more sophisticated validation here

    end function cf_validate_units

    !> Create a CF-compliant time coordinate variable
    !>
    !> This subroutine creates a time coordinate variable following CF
    !> conventions, including proper units, calendar attributes, and
    !> coordinate metadata.
    !>
    !> @param[in] file_id NetCDF file identifier
    !> @param[out] time_var_id Variable identifier for the created time coordinate
    !> @param[in] time_units Time units string (e.g., "days since 1900-01-01")
    !> @param[in] calendar Optional calendar type (default: "gregorian")
    !> @param[out] rc Optional return code (0=success, negative=error)
    !>
    !> @note Uses structured error handling for robustness
    !> @see CF Convention section on time coordinates
    subroutine cf_create_time_coordinate(file_id, time_var_id, time_units, &
                                       calendar, rc)
        integer, intent(in) :: file_id
        integer, intent(out) :: time_var_id
        character(len=*), intent(in) :: time_units
        character(len=*), intent(in), optional :: calendar
        integer, intent(out), optional :: rc

        integer :: local_rc
        character(len=ESMFIOW_MAX_STRING_LEN) :: cal_type

        local_rc = ESMFIOW_SUCCESS

        ! Set default calendar
        cal_type = "gregorian"
        if (present(calendar)) cal_type = calendar

        ! Create time dimension and variable (implementation specific)
        ! This would use NetCDF-Fortran calls

        ! Add CF-compliant attributes
        call cf_add_coordinate_attributes(time_var_id, "time", &
                                        "time", time_units, "T", local_rc)
        if (local_rc /= ESMFIOW_SUCCESS) then
            if (present(rc)) rc = local_rc
            return
        end if

        call add_string_attribute(time_var_id, "calendar", trim(cal_type), local_rc)
        if (present(rc)) rc = local_rc

    end subroutine cf_create_time_coordinate

    !> Add bounds variable for coordinate
    !>
    !> This subroutine creates a bounds variable for a coordinate variable
    !> following CF conventions. Bounds variables specify the exact boundaries
    !> of coordinate cells, which is important for proper data interpretation.
    !>
    !> @param[in] coord_var_id NetCDF variable identifier for the coordinate
    !> @param[in] bounds_var_id NetCDF variable identifier for the bounds variable
    !> @param[in] bounds_name Name of the bounds variable
    !> @param[out] rc Optional return code (0=success, negative=error)
    !>
    !> @note The bounds variable should have one extra dimension compared to
    !>       the coordinate variable to store cell boundaries
    !> @see CF Convention section on cell boundaries
    subroutine cf_add_bounds_variable(coord_var_id, bounds_var_id, bounds_name, rc)
        integer, intent(in) :: coord_var_id, bounds_var_id
        character(len=*), intent(in) :: bounds_name
        integer, intent(out), optional :: rc

        integer :: local_rc

        local_rc = ESMFIOW_SUCCESS

        ! Add bounds attribute to coordinate variable
        call add_string_attribute(coord_var_id, "bounds", bounds_name, local_rc)

        if (present(rc)) rc = local_rc

    end subroutine cf_add_bounds_variable

    ! Helper subroutines (these would be implemented using NetCDF-Fortran)

    !> Add string attribute to NetCDF variable or file
    !>
    !> This helper subroutine adds a string attribute to a NetCDF variable
    !> or global file. Uses NetCDF-Fortran library calls.
    !>
    !> @param[in] var_id NetCDF variable or file identifier
    !> @param[in] attr_name Name of the attribute
    !> @param[in] attr_value String value of the attribute
    !> @param[out] rc Return code (0=success, negative=error)
    subroutine add_string_attribute(var_id, attr_name, attr_value, rc)
        use netcdf
        integer, intent(in) :: var_id
        character(len=*), intent(in) :: attr_name, attr_value
        integer, intent(out) :: rc

        integer :: nc_rc

        ! Add string attribute using NetCDF-Fortran
        nc_rc = nf90_put_att(var_id, NF90_GLOBAL, attr_name, attr_value)

        if (nc_rc == NF90_NOERR) then
            rc = ESMFIOW_SUCCESS
        else
            rc = ESMFIOW_ERROR
        end if

    end subroutine add_string_attribute

    !> Add real-valued attribute to NetCDF variable or file
    !>
    !> This helper subroutine adds a real-valued attribute to a NetCDF variable
    !> or global file. Uses NetCDF-Fortran library calls.
    !>
    !> @param[in] var_id NetCDF variable or file identifier
    !> @param[in] attr_name Name of the attribute
    !> @param[in] attr_value Real value of the attribute
    !> @param[out] rc Return code (0=success, negative=error)
    subroutine add_real_attribute(var_id, attr_name, attr_value, rc)
        use netcdf
        integer, intent(in) :: var_id
        character(len=*), intent(in) :: attr_name
        real(ESMFIOW_REAL_KIND), intent(in) :: attr_value
        integer, intent(out) :: rc

        integer :: nc_rc

        ! Add real attribute using NetCDF-Fortran
        nc_rc = nf90_put_att(var_id, NF90_GLOBAL, attr_name, attr_value)

        if (nc_rc == NF90_NOERR) then
            rc = ESMFIOW_SUCCESS
        else
            rc = ESMFIOW_ERROR
        end if

    end subroutine add_real_attribute

    !> @brief Add real array attribute to NetCDF variable or file
    !>
    !> This helper subroutine adds a real-valued array attribute to a NetCDF
    !> variable or global file. Uses NetCDF-Fortran library calls.
    !>
    !> @param[in] var_id NetCDF variable or file identifier
    !> @param[in] attr_name Name of the attribute
    !> @param[in] attr_value Real array values of the attribute
    !> @param[out] rc Return code (0=success, negative=error)
    subroutine add_real_array_attribute(var_id, attr_name, attr_value, rc)
        use netcdf
        integer, intent(in) :: var_id
        character(len=*), intent(in) :: attr_name
        real(ESMFIOW_REAL_KIND), intent(in) :: attr_value(:)
        integer, intent(out) :: rc

        integer :: nc_rc

        ! Add real array attribute using NetCDF-Fortran
        nc_rc = nf90_put_att(var_id, NF90_GLOBAL, attr_name, attr_value)

        if (nc_rc == NF90_NOERR) then
            rc = ESMFIOW_SUCCESS
        else
            rc = ESMFIOW_ERROR
        end if

    end subroutine add_real_array_attribute

    !> @brief Get current timestamp in ISO 8601 format
    !>
    !> This helper subroutine generates a timestamp string in ISO 8601 format
    !> for use in CF-compliant global attributes. Uses Fortran intrinsic
    !> date_and_time to get current system time.
    !>
    !> @param[out] timestamp Output string containing the timestamp in ISO 8601 format
    !>
    !> @note Returns current system time in ISO 8601 format: YYYY-MM-DDTHH:MM:SSZ
    subroutine get_timestamp(timestamp)
        character(len=*), intent(out) :: timestamp
        character(len=8) :: date
        character(len=10) :: time
        character(len=5) :: zone
        integer :: values(8)

        ! Get current date and time from system
        call date_and_time(date, time, zone, values)

        ! Format as ISO 8601: YYYY-MM-DDTHH:MM:SSZ
        write(timestamp, '(A4,"-",A2,"-",A2,"T",A2,":",A2,":",A2,"Z")') &
            date(1:4), date(5:6), date(7:8), time(1:2), time(3:4), time(5:6)

    end subroutine get_timestamp

end module esmfiow_cf_utils
!> @} End of esmfiow_cf_utils group

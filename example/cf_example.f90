program cf_example
    use esmfiow
    implicit none

    integer :: rc, file_id, var_id

    print *, "CF Convention Example"
    print *, "===================="

    ! Initialize ESMFIOW
    call esmfiow_initialize(rc)

    ! This example demonstrates CF convention utilities
    file_id = 1  ! Placeholder for actual NetCDF file ID
    var_id = 1   ! Placeholder for actual NetCDF variable ID

    ! Add CF-compliant global attributes
    call cf_add_global_attributes(file_id, &
                                title="Temperature Data", &
                                institution="Research Center", &
                                source="Model v1.0", &
                                rc=rc)

    if (rc == ESMFIOW_SUCCESS) then
        print *, "Added CF global attributes"
    end if

    ! Add CF-compliant variable attributes for temperature
    call cf_add_variable_attributes(var_id, &
                                  "air_temperature", &
                                  "Air Temperature", &
                                  "K", &
                                  rc=rc)

    if (rc == ESMFIOW_SUCCESS) then
        print *, "Added CF variable attributes"
    end if

    ! Validate units
    if (cf_validate_units("K")) then
        print *, "Temperature units are valid"
    end if

    call esmfiow_finalize(rc)
    print *, "CF convention example completed"

end program cf_example

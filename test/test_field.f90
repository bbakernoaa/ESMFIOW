program test_field
    use esmfiow_field
    use esmfiow_constants
    implicit none

    integer :: rc, file_handle, field
    logical :: test_passed

    test_passed = .true.

    print *, "Field Module Tests"
    print *, "=================="

    ! Test 1: Field creation from NetCDF
    print *, "Test 1: Field creation from NetCDF"
    file_handle = 1  ! Placeholder

    call esmfiow_field_create_from_netcdf(file_handle, "temperature", field, rc)
    if (rc /= ESMFIOW_SUCCESS) then
        print *, "FAILED: Could not create field from NetCDF"
        test_passed = .false.
    else
        print *, "PASSED: Field created from NetCDF"
    end if

    ! Test 2: Field write to NetCDF
    print *, "Test 2: Field write to NetCDF"
    call esmfiow_field_write_to_netcdf(file_handle, field, "temperature", &
                                     standard_name="air_temperature", &
                                     long_name="Air Temperature", &
                                     units="K", rc=rc)
    if (rc /= ESMFIOW_SUCCESS) then
        print *, "FAILED: Could not write field to NetCDF"
        test_passed = .false.
    else
        print *, "PASSED: Field written to NetCDF"
    end if

    ! Test 3: Field metadata
    print *, "Test 3: Field metadata"
    call esmfiow_field_add_metadata(field, "air_temperature", &
                                   "Air Temperature", "K", rc=rc)
    if (rc /= ESMFIOW_SUCCESS) then
        print *, "FAILED: Could not add field metadata"
        test_passed = .false.
    else
        print *, "PASSED: Field metadata added"
    end if

    ! Final result
    if (test_passed) then
        print *, "All field tests PASSED"
        stop 0
    else
        print *, "Some field tests FAILED"
        stop 1
    end if

end program test_field

program integration_test
    use esmfiow
    implicit none

    integer :: rc
    type(esmfiow_file_handle) :: file_handle
    logical :: test_passed
    character(len=256) :: version_string

    test_passed = .true.

    print *, "ESMFIOW Integration Test"
    print *, "========================"

    ! Initialize library
    call esmfiow_initialize(rc)
    if (rc /= ESMFIOW_SUCCESS) then
        print *, "FAILED: Library initialization"
        stop 1
    end if

    ! Get version
    call esmfiow_get_version(version_string, rc)
    if (rc == ESMFIOW_SUCCESS) then
        print *, "Running integration test with ", trim(version_string)
    end if

    ! Test complete workflow
    print *, "Test: Complete file I/O workflow"

    ! 1. Create file
    call esmfiow_file_create("integration_test.nc", file_handle, &
                           title="Integration Test File", &
                           institution="Test Institution", &
                           rc=rc)

    if (rc /= ESMFIOW_SUCCESS) then
        print *, "FAILED: File creation"
        test_passed = .false.
    else

    print *, "  - File created successfully"

    ! 2. In a real implementation, we would:
    !    - Create ESMF grids
    !    - Create ESMF fields
    !    - Write field data with CF compliance
    !    - Read field data back

    print *, "  - Grid and field operations (placeholder)"

    ! 3. Close file
    call esmfiow_file_close(file_handle, rc)
    if (rc /= ESMFIOW_SUCCESS) then
        print *, "FAILED: File closing"
        test_passed = .false.
    else
        print *, "  - File closed successfully"
    end if
    end if

    ! Finalize library
    call esmfiow_finalize(rc)

    ! Final result
    if (test_passed) then
        print *, "Integration test PASSED"
        stop 0
    else
        print *, "Integration test FAILED"
        stop 1
    end if

end program integration_test

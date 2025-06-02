program test_io
    use esmfiow_io
    use esmfiow_constants
    implicit none

    type(esmfiow_file_handle) :: file_handle
    integer :: rc
    logical :: test_passed

    test_passed = .true.

    print *, "I/O Module Tests"
    print *, "================"

    ! Test 1: File handle initialization
    print *, "Test 1: File creation"
    call esmfiow_file_create("test_output.nc", file_handle, &
                           title="Test File", rc=rc)

    if (rc /= ESMFIOW_SUCCESS) then
        print *, "FAILED: Could not create file"
        test_passed = .false.
    else
        print *, "PASSED: File created successfully"

        ! Test 2: File closing
        print *, "Test 2: File closing"
        call esmfiow_file_close(file_handle, rc)
        if (rc /= ESMFIOW_SUCCESS) then
            print *, "FAILED: Could not close file"
            test_passed = .false.
        else
            print *, "PASSED: File closed successfully"
        end if
    end if

    ! Final result
    if (test_passed) then
        print *, "All I/O tests PASSED"
        stop 0
    else
        print *, "Some I/O tests FAILED"
        stop 1
    end if

end program test_io

program test_esmfiow
    use esmfiow
    implicit none

    integer :: rc
    character(len=256) :: version_string
    logical :: test_passed

    test_passed = .true.

    print *, "ESMFIOW Library Tests"
    print *, "===================="

    ! Test 1: Library initialization
    print *, "Test 1: Library initialization"
    call esmfiow_initialize(rc)
    if (rc /= ESMFIOW_SUCCESS) then
        print *, "FAILED: Could not initialize library"
        test_passed = .false.
    else
        print *, "PASSED: Library initialized successfully"
    end if

    ! Test 2: Get version
    print *, "Test 2: Get version information"
    call esmfiow_get_version(version_string, rc)
    if (rc /= ESMFIOW_SUCCESS) then
        print *, "FAILED: Could not get version"
        test_passed = .false.
    else
        print *, "PASSED: Version = ", trim(version_string)
    end if

    ! Test 3: Library finalization
    print *, "Test 3: Library finalization"
    call esmfiow_finalize(rc)
    if (rc /= ESMFIOW_SUCCESS) then
        print *, "FAILED: Could not finalize library"
        test_passed = .false.
    else
        print *, "PASSED: Library finalized successfully"
    end if

    ! Final result
    if (test_passed) then
        print *, "All tests PASSED"
        stop 0
    else
        print *, "Some tests FAILED"
        stop 1
    end if

end program test_esmfiow

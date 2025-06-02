program test_cf_utils
    use esmfiow_cf_utils
    use esmfiow_constants
    implicit none

    logical :: test_passed
    logical :: is_valid

    test_passed = .true.

    print *, "CF Utilities Tests"
    print *, "=================="

    ! Test 1: Units validation
    print *, "Test 1: Units validation"
    is_valid = cf_validate_units("K")
    if (.not. is_valid) then
        print *, "FAILED: Temperature units should be valid"
        test_passed = .false.
    else
        print *, "PASSED: Temperature units are valid"
    end if

    is_valid = cf_validate_units("")
    if (is_valid) then
        print *, "FAILED: Empty units should be invalid"
        test_passed = .false.
    else
        print *, "PASSED: Empty units are invalid"
    end if

    ! Test 2: CF version constant
    print *, "Test 2: CF version constant"
    if (len_trim(ESMFIOW_CF_VERSION) == 0) then
        print *, "FAILED: CF version should not be empty"
        test_passed = .false.
    else
        print *, "PASSED: CF version = ", ESMFIOW_CF_VERSION
    end if

    ! Final result
    if (test_passed) then
        print *, "All CF utils tests PASSED"
        stop 0
    else
        print *, "Some CF utils tests FAILED"
        stop 1
    end if

end program test_cf_utils

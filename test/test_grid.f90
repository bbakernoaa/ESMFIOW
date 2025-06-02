program test_grid
    use esmfiow_grid
    use esmfiow_constants
    implicit none

    integer :: rc, file_handle, grid
    logical :: test_passed

    test_passed = .true.

    print *, "Grid Module Tests"
    print *, "================="

    ! Test 1: Grid to NetCDF
    print *, "Test 1: Grid to NetCDF"
    file_handle = 1  ! Placeholder
    grid = 1         ! Placeholder

    call esmfiow_grid_to_netcdf(file_handle, grid, rc)
    if (rc /= ESMFIOW_SUCCESS) then
        print *, "FAILED: Could not write grid to NetCDF"
        test_passed = .false.
    else
        print *, "PASSED: Grid written to NetCDF"
    end if

    ! Test 2: Grid from NetCDF
    print *, "Test 2: Grid from NetCDF"
    call esmfiow_grid_from_netcdf(file_handle, grid, rc)
    if (rc /= ESMFIOW_SUCCESS) then
        print *, "FAILED: Could not read grid from NetCDF"
        test_passed = .false.
    else
        print *, "PASSED: Grid read from NetCDF"
    end if

    ! Final result
    if (test_passed) then
        print *, "All grid tests PASSED"
        stop 0
    else
        print *, "Some grid tests FAILED"
        stop 1
    end if

end program test_grid

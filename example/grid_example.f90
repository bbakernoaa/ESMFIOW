program grid_example
    use esmfiow
    implicit none

    integer :: rc
    type(esmfiow_file_handle) :: file_handle
    integer :: grid  ! Placeholder for ESMF_Grid

    ! Initialize ESMFIOW library
    call esmfiow_initialize(rc)
    if (rc /= ESMFIOW_SUCCESS) then
        print *, "Failed to initialize ESMFIOW"
        stop 1
    end if

    print *, "ESMFIOW Grid I/O Example"
    print *, "========================"

    ! Create a file for grid output
    call esmfiow_file_create("grid_example_output.nc", file_handle, &
                           title="ESMF Grid Example", &
                           institution="Grid Processing Center", &
                           rc=rc)

    if (rc == ESMFIOW_SUCCESS) then
        print *, "Successfully created file for grid output"

        ! In a real implementation, this would:
        ! 1. Create an ESMF grid (regular lat-lon, curvilinear, etc.)
        ! 2. Set up coordinate arrays
        ! 3. Write grid coordinates to netCDF with CF conventions

        print *, "Processing grid operations..."
        print *, "  - Creating ESMF grid structure"
        print *, "  - Setting up coordinate arrays"
        print *, "  - Writing CF-compliant coordinate variables"

        ! Simulate grid operations
        grid = 1  ! Placeholder
        call esmfiow_grid_to_netcdf(file_handle%file_id, grid, rc)
        if (rc == ESMFIOW_SUCCESS) then
            print *, "  - Grid written to netCDF successfully"
        end if

        call esmfiow_add_grid_coordinates(file_handle%file_id, grid, rc)
        if (rc == ESMFIOW_SUCCESS) then
            print *, "  - Grid coordinates added with CF metadata"
        end if

        ! Close the file
        call esmfiow_file_close(file_handle, rc)
        if (rc == ESMFIOW_SUCCESS) then
            print *, "Grid file closed successfully"
        end if
    else
        print *, "Failed to create grid file"
    end if

    ! Finalize ESMFIOW library
    call esmfiow_finalize(rc)
    print *, "Grid example completed"

end program grid_example

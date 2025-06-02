program example
    use esmfiow
    implicit none

    integer :: rc
    type(esmfiow_file_handle) :: file_handle

    ! Initialize ESMFIOW library
    call esmfiow_initialize(rc)
    if (rc /= ESMFIOW_SUCCESS) then
        print *, "Failed to initialize ESMFIOW"
        stop 1
    end if

    print *, "ESMFIOW Basic Example"
    print *, "====================="

    ! Create a new CF-compliant file
    call esmfiow_file_create("example_output.nc", file_handle, &
                           title="ESMFIOW Example Output", &
                           institution="Example Institution", &
                           rc=rc)

    if (rc == ESMFIOW_SUCCESS) then
        print *, "Successfully created file: example_output.nc"

        ! Close the file
        call esmfiow_file_close(file_handle, rc)
        if (rc == ESMFIOW_SUCCESS) then
            print *, "File closed successfully"
        end if
    else
        print *, "Failed to create file"
    end if

    ! Finalize ESMFIOW library
    call esmfiow_finalize(rc)
    print *, "ESMFIOW example completed"

end program example

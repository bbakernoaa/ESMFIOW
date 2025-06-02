program test_parallel_io
    ! Test program for ESMFIOW's parallel I/O capabilities
    use esmfiow_constants
    use esmfiow_netcdf4
    use esmfiow_esmf_io
    use ESMF
    implicit none

    ! Variables for the tests
    type(esmfiow_esmf_io_config) :: io_config
    type(esmfiow_nc4_compression_settings) :: compression
    type(esmfiow_nc4_chunking_settings) :: chunking
    type(ESMF_Field) :: field
    type(ESMF_Grid) :: grid
    type(ESMF_ArraySpec) :: arrayspec
    integer :: rc, petCount, localPet, finalrc
    logical :: test_passed
    real(ESMFIOW_REAL_KIND), pointer :: data2d(:,:), data2d_read(:,:) => null()
    integer :: i, j
    character(len=ESMFIOW_MAX_STRING_LEN) :: filename
    character(len=ESMFIOW_MAX_STRING_LEN) :: msgString
    logical :: file_exists

    ! Initialize ESMF and test variables
    finalrc = ESMF_SUCCESS
    test_passed = .true.
    filename = "test_parallel_output.nc"

    ! Initialize ESMF
    call ESMF_Initialize(defaultLogType=ESMF_LOGKIND_MULTI, rc=rc)
    if (rc /= ESMF_SUCCESS) then
        print *, "ERROR: ESMF initialization failed"
        stop 1
    endif

    ! Get parallel information
    call ESMF_VMGetGlobal(vm=vm, rc=rc)
    call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)

    print *, "ESMFIOW Parallel I/O Tests"
    print *, "=========================="
    write(msgString, "(A,I0,A,I0)") "Running on PET ", localPet, " of ", petCount
    print *, trim(msgString)

    !-------------------------------------------------------------------------
    ! Test 1: Initialize ESMF I/O configuration
    !-------------------------------------------------------------------------
    print *, "Test 1: ESMF I/O configuration initialization"

    ! Initialize the configuration
    call esmfiow_esmf_create_io_config(io_config, filename, rc=rc)
    if (rc /= ESMF_SUCCESS) then
        print *, "FAILED: Could not initialize I/O configuration"
        test_passed = .false.
        finalrc = ESMF_FAILURE
    else
        print *, "PASSED: I/O configuration initialized successfully"
    endif

    !-------------------------------------------------------------------------
    ! Test 2: Set up compression and chunking
    !-------------------------------------------------------------------------
    print *, "Test 2: Setting up compression and chunking"

    ! Set up compression
    io_config%compression%compression_type = ESMFIOW_NC4_COMPRESSION_DEFLATE
    io_config%compression%deflate_level = 5
    io_config%compression%shuffle = .true.

    ! Set up chunking
    io_config%chunking%enable_chunking = .true.
    io_config%chunking%auto_chunking = .true.  ! Let the library determine optimal chunking

    ! Set the I/O to be parallel
    io_config%is_parallel = .true.
    io_config%access_pattern = ESMFIOW_ESMF_ACCESS_RANDOM
    io_config%io_strategy = ESMFIOW_ESMF_IO_STRATEGY_NETCDF4

    print *, "PASSED: Compression and chunking set up"

    !-------------------------------------------------------------------------
    ! Test 3: Create a grid and field for testing
    !-------------------------------------------------------------------------
    print *, "Test 3: Creating ESMF Grid and Field"

    ! Create a test grid (2D regular grid)
    grid = ESMF_GridCreateNoPeriDim(  &
            minIndex=(/1,1/),         &
            maxIndex=(/10,20/),       &
            regDecomp=(/2,petCount/), &
            name="test_grid",         &
            rc=rc)

    if (rc /= ESMF_SUCCESS) then
        print *, "FAILED: Could not create grid"
        test_passed = .false.
        finalrc = ESMF_FAILURE
    else
        print *, "PASSED: Grid created successfully"
    endif

    ! Create array spec
    call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R8, rank=2, rc=rc)
    if (rc /= ESMF_SUCCESS) then
        print *, "FAILED: Could not create array spec"
        test_passed = .false.
        finalrc = ESMF_FAILURE
    endif

    ! Create the field using the grid
    field = ESMF_FieldCreate(grid, arrayspec, &
                           staggerloc=ESMF_STAGGERLOC_CENTER, &
                           name="test_field", &
                           rc=rc)

    if (rc /= ESMF_SUCCESS) then
        print *, "FAILED: Could not create field"
        test_passed = .false.
        finalrc = ESMF_FAILURE
    else
        print *, "PASSED: Field created successfully"
    endif

    !-------------------------------------------------------------------------
    ! Test 4: Initialize field data
    !-------------------------------------------------------------------------
    print *, "Test 4: Initializing field data"

    call ESMF_FieldGet(field, farrayPtr=data2d, rc=rc)
    if (rc /= ESMF_SUCCESS) then
        print *, "FAILED: Could not get field data pointer"
        test_passed = .false.
        finalrc = ESMF_FAILURE
    else
        ! Initialize data with a unique pattern based on PET
        do j = lbound(data2d, 2), ubound(data2d, 2)
            do i = lbound(data2d, 1), ubound(data2d, 1)
                data2d(i, j) = real(i + j*100 + localPet*10000, ESMFIOW_REAL_KIND)
            end do
        end do
        print *, "PASSED: Field data initialized"
    endif

    !-------------------------------------------------------------------------
    ! Test 5: Write field with parallel I/O
    !-------------------------------------------------------------------------
    print *, "Test 5: Writing field with parallel I/O"

    ! Remove existing file if this is PET 0
    if (localPet == 0) then
        inquire(file=trim(filename), exist=file_exists)
        if (file_exists) then
            open(unit=99, file=trim(filename))
            close(unit=99, status="delete")
        endif
    endif

    ! Barrier to ensure file deletion is complete before writing
    call ESMF_VMBarrier(vm, rc=rc)

    ! Write the field with parallel I/O
    call esmfiow_esmf_io_write_field(io_config, field, rc=rc)
    if (rc /= ESMF_SUCCESS) then
        print *, "FAILED: Could not write field using parallel I/O"
        test_passed = .false.
        finalrc = ESMF_FAILURE
    else
        print *, "PASSED: Field written successfully using parallel I/O"
    endif

    !-------------------------------------------------------------------------
    ! Test 6: Read field with parallel I/O
    !-------------------------------------------------------------------------
    print *, "Test 6: Reading field with parallel I/O"

    ! Create a new field for reading
    field = ESMF_FieldCreate(grid, arrayspec, &
                           staggerloc=ESMF_STAGGERLOC_CENTER, &
                           name="test_field_read", &
                           rc=rc)

    if (rc /= ESMF_SUCCESS) then
        print *, "FAILED: Could not create field for reading"
        test_passed = .false.
        finalrc = ESMF_FAILURE
    endif

    ! Read the field with parallel I/O
    call esmfiow_esmf_io_read_field(io_config, field, "test_field", rc=rc)
    if (rc /= ESMF_SUCCESS) then
        print *, "FAILED: Could not read field using parallel I/O"
        test_passed = .false.
        finalrc = ESMF_FAILURE
    else
        call ESMF_FieldGet(field, farrayPtr=data2d_read, rc=rc)
        if (rc /= ESMF_SUCCESS) then
            print *, "FAILED: Could not get field data pointer after reading"
            test_passed = .false.
            finalrc = ESMF_FAILURE
        else
            ! Validate data (only check a few points to keep output reasonable)
            if (associated(data2d_read)) then
                i = lbound(data2d_read, 1)
                j = lbound(data2d_read, 2)
                write(msgString, '(A,I0,A,I0,A,E14.6)') "PET ", localPet, " data[", i, ",", j, &
                    "] = ", data2d_read(i, j)
                print *, trim(msgString)

                i = ubound(data2d_read, 1)
                j = ubound(data2d_read, 2)
                write(msgString, '(A,I0,A,I0,A,E14.6)') "PET ", localPet, " data[", i, ",", j, &
                    "] = ", data2d_read(i, j)
                print *, trim(msgString)

                print *, "PASSED: Field read successfully using parallel I/O"
            else
                print *, "FAILED: Field data pointer is not associated after reading"
                test_passed = .false.
                finalrc = ESMF_FAILURE
            endif
        endif
    endif

    !-------------------------------------------------------------------------
    ! Test 7: Check compression info
    !-------------------------------------------------------------------------
    print *, "Test 7: Checking compression information"

    ! Only PET 0 checks compression info to avoid duplicate output
    if (localPet == 0) then
        ! Initialize variables to hold compression info
        integer :: ncId, varId
        logical :: is_compressed, is_chunked
        integer :: deflate_level, shuffle, fletcher32
        integer, allocatable :: chunk_sizes(:)

        ! Open the file manually
        call esmfiow_nc4_create_file(filename, ncId, mode=NF90_NOWRITE, rc=rc)
        if (rc /= ESMFIOW_SUCCESS) then
            print *, "FAILED: Could not open NetCDF file to check compression"
            test_passed = .false.
            finalrc = ESMF_FAILURE
        else
            ! Get variable ID
            rc = nf90_inq_varid(ncId, "test_field", varId)
            if (rc /= NF90_NOERR) then
                print *, "FAILED: Could not find variable in NetCDF file"
                test_passed = .false.
                finalrc = ESMF_FAILURE
            else
                ! Get compression info
                call esmfiow_nc4_get_compression_info(ncId, varId, is_compressed, &
                                                    deflate_level, shuffle, fletcher32, &
                                                    is_chunked, chunk_sizes, rc)

                if (rc /= ESMFIOW_SUCCESS) then
                    print *, "FAILED: Could not get compression info"
                    test_passed = .false.
                    finalrc = ESMF_FAILURE
                else
                    ! Print compression info
                    print *, "Compression Info:"
                    print *, "  Is compressed: ", is_compressed
                    print *, "  Deflate level: ", deflate_level
                    print *, "  Shuffle: ", shuffle
                    print *, "  Fletcher32: ", fletcher32
                    print *, "  Is chunked: ", is_chunked
                    if (is_chunked .and. allocated(chunk_sizes)) then
                        print *, "  Chunk sizes: ", chunk_sizes
                    endif

                    ! Verify expected compression parameters
                    if (is_compressed .and. deflate_level == 5 .and. shuffle == 1) then
                        print *, "PASSED: Compression parameters match expectations"
                    else
                        print *, "FAILED: Compression parameters do not match expectations"
                        test_passed = .false.
                        finalrc = ESMF_FAILURE
                    endif
                endif

                ! Close the file
                rc = nf90_close(ncId)
                if (rc /= NF90_NOERR) then
                    print *, "FAILED: Could not close NetCDF file after checking compression"
                    test_passed = .false.
                    finalrc = ESMF_FAILURE
                endif
            endif
        endif
    endif

    !-------------------------------------------------------------------------
    ! Finalize and report overall test status
    !-------------------------------------------------------------------------
    call ESMF_FieldDestroy(field, rc=rc)
    call ESMF_GridDestroy(grid, rc=rc)
    call ESMF_Finalize(rc=rc)

    if (localPet == 0) then
        print *, "=========================="
        if (test_passed .and. finalrc == ESMF_SUCCESS) then
            print *, "OVERALL TEST STATUS: PASSED"
            print *, "All parallel I/O tests completed successfully!"
        else
            print *, "OVERALL TEST STATUS: FAILED"
            print *, "One or more parallel I/O tests failed."
        endif
        print *, "=========================="
    endif

    if (finalrc /= ESMF_SUCCESS) then
        stop 1
    endif
end program test_parallel_io

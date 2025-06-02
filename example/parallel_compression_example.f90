!> @file parallel_compression_example.f90
!> @brief ESMFIOW parallel compression benchmark example
!>
!> This example demonstrates how to use the ESMF I/O integration with NetCDF4 compression
!> for both sequential and parallel I/O operations, showing performance comparisons.
!>
!> The example:
!> 1. Creates a large dataset with wave patterns that compress well
!> 2. Writes the data using sequential I/O with compression
!> 3. Writes the same data using parallel I/O with compression
!> 4. Reads the data back using both methods
!> 5. Compares performance and file sizes
!>
!> @note This example requires a parallel build of ESMF and NetCDF with HDF5 parallel I/O support

!> @brief Benchmark program for comparing sequential vs parallel I/O with compression
program esmfiow_parallel_example
    use esmfiow_constants
    use esmfiow_netcdf4
    use esmfiow_esmf_io
    use ESMF
    implicit none

    ! ESMF variables
    type(ESMF_VM) :: vm
    type(ESMF_Grid) :: grid
    type(ESMF_Field) :: field_seq, field_par
    type(ESMF_ArraySpec) :: arrayspec

    ! ESMFIOW configuration variables
    type(esmfiow_esmf_io_config) :: config_seq, config_par
    type(esmfiow_nc4_compression_settings) :: compression

    ! General variables
    integer :: rc, petCount, localPet
    character(len=ESMFIOW_MAX_STRING_LEN) :: filename_seq, filename_par
    real(ESMFIOW_REAL_KIND), pointer :: data2d(:,:) => null()
    real(ESMFIOW_REAL_KIND), pointer :: data2d_read(:,:) => null()
    integer :: grid_size_x, grid_size_y
    logical :: file_exists
    real :: start_time, end_time, write_time_seq, write_time_par
    real :: read_time_seq, read_time_par
    character(len=ESMFIOW_MAX_STRING_LEN) :: msgString
    integer :: i, j, ierr

    ! File sizes
    integer(kind=int64) :: file_size_seq, file_size_par

    ! Initialize ESMF
    call ESMF_Initialize(defaultLogType=ESMF_LOGKIND_MULTI, rc=rc)
    if (rc /= ESMF_SUCCESS) then
        print *, "ERROR: ESMF initialization failed"
        stop 1
    endif

    ! Get VM information
    call ESMF_VMGetGlobal(vm=vm, rc=rc)
    call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)

    ! Print header
    if (localPet == 0) then
        print *, "----------------------------------------"
        print *, "ESMFIOW Parallel Compression Benchmark"
        print *, "----------------------------------------"
        write(msgString, "(A,I0)") "Running on ", petCount, " processors"
        print *, trim(msgString)
        print *, "----------------------------------------"
    endif

    ! Set filenames for sequential and parallel output
    filename_seq = "esmfiow_seq_benchmark.nc"
    filename_par = "esmfiow_par_benchmark.nc"

    ! Define grid sizes - make them large enough to show compression benefits
    grid_size_x = 500
    grid_size_y = 500

    ! Create a common grid for both sequential and parallel I/O
    if (localPet == 0) then
        print *, "Creating grid of size ", grid_size_x, "x", grid_size_y
    endif

    grid = ESMF_GridCreateNoPeriDim(  &
            minIndex=(/1,1/),         &
            maxIndex=(/grid_size_x,grid_size_y/), &
            regDecomp=(/1,petCount/), &
            name="benchmark_grid",    &
            rc=rc)

    if (rc /= ESMF_SUCCESS) then
        print *, "ERROR: Failed to create grid"
        call ESMF_Finalize(rc=rc)
        stop 1
    endif

    ! Create array specification
    call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R8, rank=2, rc=rc)

    ! Create fields for sequential and parallel I/O
    field_seq = ESMF_FieldCreate(grid, arrayspec, &
                               staggerloc=ESMF_STAGGERLOC_CENTER, &
                               name="field_data", &
                               rc=rc)

    field_par = ESMF_FieldCreate(grid, arrayspec, &
                               staggerloc=ESMF_STAGGERLOC_CENTER, &
                               name="field_data", &
                               rc=rc)

    if (rc /= ESMF_SUCCESS) then
        print *, "ERROR: Failed to create fields"
        call ESMF_Finalize(rc=rc)
        stop 1
    endif

    ! Initialize field with representative data
    ! We'll use a wave pattern that should compress well
    call ESMF_FieldGet(field_seq, farrayPtr=data2d, rc=rc)
    if (rc == ESMF_SUCCESS) then
        do j = lbound(data2d, 2), ubound(data2d, 2)
            do i = lbound(data2d, 1), ubound(data2d, 1)
                ! Generate a wave pattern that should compress well
                data2d(i, j) = sin(real(i)/50.0) * cos(real(j)/50.0)
            end do
        end do
    endif

    ! Copy the same data to the parallel field
    call ESMF_FieldGet(field_par, farrayPtr=data2d, rc=rc)
    if (rc == ESMF_SUCCESS) then
        do j = lbound(data2d, 2), ubound(data2d, 2)
            do i = lbound(data2d, 1), ubound(data2d, 1)
                ! Generate a wave pattern that should compress well
                data2d(i, j) = sin(real(i)/50.0) * cos(real(j)/50.0)
            end do
        end do
    endif

    ! Setup compression settings - we'll use the same for both
    compression%compression_type = ESMFIOW_NC4_COMPRESSION_DEFLATE
    compression%deflate_level = 5
    compression%shuffle = .true.

    ! -----------------------------------------------------------------------
    ! SEQUENTIAL I/O BENCHMARK
    ! -----------------------------------------------------------------------

    ! Initialize sequential I/O config
    call esmfiow_esmf_create_io_config(config_seq, filename_seq, rc=rc)
    config_seq%compression = compression
    config_seq%is_parallel = .false.
    config_seq%access_pattern = ESMFIOW_ESMF_ACCESS_SEQUENTIAL

    ! Delete existing file if it exists (only on PET 0)
    if (localPet == 0) then
        inquire(file=trim(filename_seq), exist=file_exists)
        if (file_exists) then
            open(unit=99, file=trim(filename_seq))
            close(unit=99, status="delete")
        endif
    endif

    ! Synchronize before timing
    call ESMF_VMBarrier(vm, rc=rc)

    ! Time sequential write
    if (localPet == 0) then
        print *, "Starting sequential write benchmark..."
        call cpu_time(start_time)
        call esmfiow_esmf_io_write_field(config_seq, field_seq, rc=rc)
        call cpu_time(end_time)
        write_time_seq = end_time - start_time

        if (rc == ESMF_SUCCESS) then
            print *, "Sequential write completed in ", write_time_seq, " seconds"
        else
            print *, "ERROR: Sequential write failed"
        endif
    endif

    ! -----------------------------------------------------------------------
    ! PARALLEL I/O BENCHMARK
    ! -----------------------------------------------------------------------

    ! Initialize parallel I/O config
    call esmfiow_esmf_create_io_config(config_par, filename_par, rc=rc)
    config_par%compression = compression
    config_par%is_parallel = .true.
    config_par%access_pattern = ESMFIOW_ESMF_ACCESS_RANDOM
    config_par%io_strategy = ESMFIOW_ESMF_IO_STRATEGY_NETCDF4

    ! Delete existing file if it exists (only on PET 0)
    if (localPet == 0) then
        inquire(file=trim(filename_par), exist=file_exists)
        if (file_exists) then
            open(unit=99, file=trim(filename_par))
            close(unit=99, status="delete")
        endif
    endif

    ! Synchronize before timing
    call ESMF_VMBarrier(vm, rc=rc)

    ! Start timer on all PETs but only report from PET 0
    call cpu_time(start_time)
    call esmfiow_esmf_io_write_field(config_par, field_par, rc=rc)
    call cpu_time(end_time)
    write_time_par = end_time - start_time

    ! Gather timing info from all PETs - use max time
    call ESMF_VMAllReduce(vm, sendData=(/write_time_par/), recvData=(/write_time_par/), &
                       count=1, reduceflag=ESMF_REDUCE_MAX, rc=rc)

    if (localPet == 0) then
        if (rc == ESMF_SUCCESS) then
            print *, "Parallel write completed in ", write_time_par, " seconds"
        else
            print *, "ERROR: Parallel write failed"
        endif
    endif

    ! -----------------------------------------------------------------------
    ! READ BENCHMARK
    ! -----------------------------------------------------------------------

    ! Create new fields for reading
    field_seq = ESMF_FieldCreate(grid, arrayspec, &
                               staggerloc=ESMF_STAGGERLOC_CENTER, &
                               name="field_read_seq", &
                               rc=rc)

    field_par = ESMF_FieldCreate(grid, arrayspec, &
                               staggerloc=ESMF_STAGGERLOC_CENTER, &
                               name="field_read_par", &
                               rc=rc)

    ! Sequential read (from PET 0 only)
    if (localPet == 0) then
        print *, "Starting sequential read benchmark..."
        call cpu_time(start_time)
        call esmfiow_esmf_io_read_field(config_seq, field_seq, "field_data", rc=rc)
        call cpu_time(end_time)
        read_time_seq = end_time - start_time

        if (rc == ESMF_SUCCESS) then
            print *, "Sequential read completed in ", read_time_seq, " seconds"
        else
            print *, "ERROR: Sequential read failed"
        endif
    endif

    ! Synchronize before parallel read
    call ESMF_VMBarrier(vm, rc=rc)

    ! Parallel read
    call cpu_time(start_time)
    call esmfiow_esmf_io_read_field(config_par, field_par, "field_data", rc=rc)
    call cpu_time(end_time)
    read_time_par = end_time - start_time

    ! Gather timing info from all PETs
    call ESMF_VMAllReduce(vm, sendData=(/read_time_par/), recvData=(/read_time_par/), &
                       count=1, reduceflag=ESMF_REDUCE_MAX, rc=rc)

    if (localPet == 0) then
        if (rc == ESMF_SUCCESS) then
            print *, "Parallel read completed in ", read_time_par, " seconds"
        else
            print *, "ERROR: Parallel read failed"
        endif
    endif

    ! -----------------------------------------------------------------------
    ! FILE SIZE COMPARISON AND SUMMARY
    ! -----------------------------------------------------------------------

    ! Get file sizes
    if (localPet == 0) then
        ! Get sequential file size
        inquire(file=trim(filename_seq), size=file_size_seq)

        ! Get parallel file size
        inquire(file=trim(filename_par), size=file_size_par)

        ! Print compression summary
        print *, "----------------------------------------"
        print *, "Benchmark Summary:"
        print *, "----------------------------------------"
        print *, "Grid size: ", grid_size_x, "x", grid_size_y
        print *, "Number of processors: ", petCount
        print *, "Compression level: ", compression%deflate_level
        print *, "Shuffle filter: ", compression%shuffle
        print *, "----------------------------------------"
        print *, "Sequential file size: ", real(file_size_seq)/(1024.0*1024.0), " MB"
        print *, "Parallel file size: ", real(file_size_par)/(1024.0*1024.0), " MB"
        print *, "----------------------------------------"
        print *, "Write time (sequential): ", write_time_seq, " seconds"
        print *, "Write time (parallel): ", write_time_par, " seconds"
        print *, "Speedup (write): ", write_time_seq/write_time_par, "x"
        print *, "----------------------------------------"
        print *, "Read time (sequential): ", read_time_seq, " seconds"
        print *, "Read time (parallel): ", read_time_par, " seconds"
        print *, "Speedup (read): ", read_time_seq/read_time_par, "x"
        print *, "----------------------------------------"
    endif

    ! Cleanup and finalize
    call ESMF_FieldDestroy(field_seq, rc=rc)
    call ESMF_FieldDestroy(field_par, rc=rc)
    call ESMF_GridDestroy(grid, rc=rc)
    call ESMF_Finalize(rc=rc)

end program esmfiow_parallel_example

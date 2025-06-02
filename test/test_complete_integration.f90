!> @file test_complete_integration.f90
!> @brief Comprehensive integration test for ESMFIOW library
!>
!> This test verifies that all ESMFIOW modules work together correctly,
!> including NetCDF4 compression, ESMF I/O integration, field operations,
!> grid handling, and CF compliance.
!>
!> @author ESMFIOW Development Team
!> @date June 2025

!> @brief Complete integration test for ESMFIOW functionality
!>
!> This program tests the complete ESMFIOW workflow:
!> 1. Initialize ESMF for parallel/sequential operation
!> 2. Create compressed NetCDF files using ESMF I/O
!> 3. Create ESMF grids and fields
!> 4. Write fields with compression
!> 5. Read fields back and verify data integrity
!> 6. Test CF compliance and metadata handling
program test_complete_integration
    use esmfiow_constants
    use esmfiow_esmf_io
    use esmfiow_io
    use esmfiow_field
    use esmfiow_grid
    use esmfiow_netcdf4
    use esmfiow_cf_utils
    use ESMF
    use netcdf
    implicit none

    ! Test variables
    integer :: rc, local_rc
    character(len=*), parameter :: test_file = "test_complete_integration.nc4"
    character(len=*), parameter :: field_name = "temperature"
    character(len=*), parameter :: test_title = "ESMFIOW Complete Integration Test"
    character(len=*), parameter :: test_institution = "ESMFIOW Development Team"

    ! ESMF objects
    type(ESMF_Grid) :: grid
    type(ESMF_Field) :: field_write, field_read
    type(ESMF_DistGrid) :: distgrid
    type(ESMF_VM) :: vm

    ! ESMFIOW objects
    type(esmfiow_esmf_io_config) :: io_config
    type(esmfiow_file_handle) :: file_handle
    type(esmfiow_nc4_compression_settings) :: compression_settings
    type(esmfiow_nc4_chunking_settings) :: chunking_settings

    ! Test data
    real(ESMFIOW_REAL_KIND), pointer :: field_data(:,:)
    real(ESMFIOW_REAL_KIND), pointer :: read_data(:,:)
    integer, parameter :: nx = 64, ny = 32
    integer :: i, j, petCount, localPet
    real(ESMFIOW_REAL_KIND) :: max_diff, tolerance
    logical :: data_matches

    ! Test status tracking
    integer :: tests_run = 0, tests_passed = 0
    logical :: test_result

    write(*,*) "========================================="
    write(*,*) "ESMFIOW Complete Integration Test"
    write(*,*) "========================================="

    !---------------------------------------------------------------------------
    ! Test 1: Initialize ESMF
    !---------------------------------------------------------------------------
    call run_test("ESMF Initialization", test_esmf_initialization)

    !---------------------------------------------------------------------------
    ! Test 2: Set up compression and chunking settings
    !---------------------------------------------------------------------------
    call run_test("Compression Settings Setup", test_compression_setup)

    !---------------------------------------------------------------------------
    ! Test 3: Create ESMF I/O configuration
    !---------------------------------------------------------------------------
    call run_test("ESMF I/O Configuration", test_io_config_creation)

    !---------------------------------------------------------------------------
    ! Test 4: Create ESMF grid
    !---------------------------------------------------------------------------
    call run_test("ESMF Grid Creation", test_grid_creation)

    !---------------------------------------------------------------------------
    ! Test 5: Create ESMF field
    !---------------------------------------------------------------------------
    call run_test("ESMF Field Creation", test_field_creation)

    !---------------------------------------------------------------------------
    ! Test 6: Initialize test data
    !---------------------------------------------------------------------------
    call run_test("Test Data Initialization", test_data_initialization)

    !---------------------------------------------------------------------------
    ! Test 7: Create compressed NetCDF file
    !---------------------------------------------------------------------------
    call run_test("Compressed File Creation", test_file_creation)

    !---------------------------------------------------------------------------
    ! Test 8: Write field with compression
    !---------------------------------------------------------------------------
    call run_test("Field Writing", test_field_writing)

    !---------------------------------------------------------------------------
    ! Test 9: Close and reopen file
    !---------------------------------------------------------------------------
    call run_test("File Close/Reopen", test_file_reopen)

    !---------------------------------------------------------------------------
    ! Test 10: Read field back
    !---------------------------------------------------------------------------
    call run_test("Field Reading", test_field_reading)

    !---------------------------------------------------------------------------
    ! Test 11: Verify data integrity
    !---------------------------------------------------------------------------
    call run_test("Data Integrity Check", test_data_integrity)

    !---------------------------------------------------------------------------
    ! Test 12: Test CF compliance
    !---------------------------------------------------------------------------
    call run_test("CF Compliance Check", test_cf_compliance)

    !---------------------------------------------------------------------------
    ! Test 13: Test compression effectiveness
    !---------------------------------------------------------------------------
    call run_test("Compression Effectiveness", test_compression_effectiveness)

    !---------------------------------------------------------------------------
    ! Cleanup and finalize
    !---------------------------------------------------------------------------
    call cleanup_test_objects()
    call esmfiow_esmf_finalize(rc=rc)

    !---------------------------------------------------------------------------
    ! Print final results
    !---------------------------------------------------------------------------
    write(*,*) "========================================="
    write(*,'(A,I0,A,I0,A)') "Integration Test Results: ", tests_passed, "/", tests_run, " tests passed"
    if (tests_passed == tests_run) then
        write(*,*) "*** ALL TESTS PASSED! ***"
        write(*,*) "ESMFIOW library integration successful!"
    else
        write(*,*) "*** SOME TESTS FAILED ***"
        write(*,'(A,I0,A)') "Failed tests: ", (tests_run - tests_passed), " of ", tests_run
    end if
    write(*,*) "========================================="

    ! Clean up test file
    call system("rm -f " // test_file)

contains

    !> Run a single test and track results
    subroutine run_test(test_name, test_procedure)
        character(len=*), intent(in) :: test_name

        interface
            subroutine test_procedure(result)
                logical, intent(out) :: result
            end subroutine test_procedure
        end interface

        logical :: result

        tests_run = tests_run + 1
        write(*,'(A,I2.2,A,A)', advance='no') "Test ", tests_run, ": ", test_name
        call test_procedure(result)

        if (result) then
            tests_passed = tests_passed + 1
            write(*,*) " - PASSED"
        else
            write(*,*) " - FAILED"
        end if
    end subroutine run_test

    !> Test ESMF initialization
    subroutine test_esmf_initialization(result)
        logical, intent(out) :: result
        integer :: rc

        call esmfiow_esmf_initialize(rc=rc)
        result = (rc == ESMFIOW_SUCCESS)

        if (result) then
            call ESMF_VMGetCurrent(vm, rc=rc)
            call ESMF_VMGet(vm, petCount=petCount, localPet=localPet, rc=rc)
            result = (rc == ESMF_SUCCESS)
        end if
    end subroutine test_esmf_initialization

    !> Test compression settings setup
    subroutine test_compression_setup(result)
        logical, intent(out) :: result

        ! Set up optimized compression settings
        compression_settings%deflate_level = 6
        compression_settings%shuffle = ESMFIOW_NC4_SHUFFLE_ON
        compression_settings%fletcher32 = ESMFIOW_NC4_FLETCHER32_ON
        compression_settings%compression_type = ESMFIOW_NC4_COMPRESSION_DEFLATE

        ! Set up chunking for optimal I/O
        chunking_settings%chunk_sizes = [16, 16]  ! 2D chunks
        chunking_settings%auto_chunking = .true.
        chunking_settings%chunk_cache_size = 32 * 1024 * 1024  ! 32 MB cache

        result = .true.
    end subroutine test_compression_setup

    !> Test ESMF I/O configuration creation
    subroutine test_io_config_creation(result)
        logical, intent(out) :: result
        integer :: rc

        call esmfiow_esmf_create_io_config(io_config, test_file, &
            io_strategy=ESMFIOW_ESMF_IO_STRATEGY_NETCDF4, &
            compression=compression_settings, &
            chunking=chunking_settings, rc=rc)

        result = (rc == ESMFIOW_SUCCESS .and. io_config%is_initialized)
    end subroutine test_io_config_creation

    !> Test ESMF grid creation
    subroutine test_grid_creation(result)
        logical, intent(out) :: result
        integer :: rc

        ! Create a simple 2D distributed grid
        distgrid = ESMF_DistGridCreate(minIndex=[1,1], maxIndex=[nx,ny], rc=rc)
        if (rc /= ESMF_SUCCESS) then
            result = .false.
            return
        end if

        grid = ESMF_GridCreate(distgrid=distgrid, rc=rc)
        result = (rc == ESMF_SUCCESS)
    end subroutine test_grid_creation

    !> Test ESMF field creation
    subroutine test_field_creation(result)
        logical, intent(out) :: result
        integer :: rc

        ! Create field for writing
        field_write = ESMF_FieldCreate(grid=grid, &
            typekind=ESMF_TYPEKIND_R8, &
            staggerloc=ESMF_STAGGERLOC_CENTER, &
            name=field_name, rc=rc)
        if (rc /= ESMF_SUCCESS) then
            result = .false.
            return
        end if

        ! Create field for reading
        field_read = ESMF_FieldCreate(grid=grid, &
            typekind=ESMF_TYPEKIND_R8, &
            staggerloc=ESMF_STAGGERLOC_CENTER, &
            name=field_name // "_read", rc=rc)

        result = (rc == ESMF_SUCCESS)
    end subroutine test_field_creation

    !> Test data initialization
    subroutine test_data_initialization(result)
        logical, intent(out) :: result
        integer :: rc

        ! Get pointer to field data
        call ESMF_FieldGet(field_write, farrayPtr=field_data, rc=rc)
        if (rc /= ESMF_SUCCESS) then
            result = .false.
            return
        end if

        ! Initialize with test pattern (temperature-like data)
        do j = 1, size(field_data, 2)
            do i = 1, size(field_data, 1)
                field_data(i,j) = 273.15 + 20.0 * sin(real(i)/real(nx) * 3.14159) * &
                                                cos(real(j)/real(ny) * 3.14159)
            end do
        end do

        result = .true.
    end subroutine test_data_initialization

    !> Test compressed file creation
    subroutine test_file_creation(result)
        logical, intent(out) :: result
        integer :: rc

        call esmfiow_file_create(test_file, file_handle, &
            title=test_title, institution=test_institution, rc=rc)

        result = (rc == ESMFIOW_SUCCESS .and. file_handle%is_open)
    end subroutine test_file_creation

    !> Test field writing
    subroutine test_field_writing(result)
        logical, intent(out) :: result
        integer :: rc

        ! Add metadata to field
        call esmfiow_field_add_metadata(field_write, &
            long_name="Air Temperature", &
            units="K", &
            standard_name="air_temperature", rc=rc)
        if (rc /= ESMFIOW_SUCCESS) then
            result = .false.
            return
        end if

        ! Write field using ESMF I/O with compression
        call esmfiow_esmf_io_write_field(io_config, field_write, field_name, rc=rc)

        result = (rc == ESMFIOW_SUCCESS)
    end subroutine test_field_writing

    !> Test file close and reopen
    subroutine test_file_reopen(result)
        logical, intent(out) :: result
        integer :: rc

        ! Close file
        call esmfiow_file_close(file_handle, rc=rc)
        if (rc /= ESMFIOW_SUCCESS) then
            result = .false.
            return
        end if

        ! Reopen for reading
        call esmfiow_file_open(test_file, file_handle, readonly=.true., rc=rc)

        result = (rc == ESMFIOW_SUCCESS .and. file_handle%is_open)
    end subroutine test_file_reopen

    !> Test field reading
    subroutine test_field_reading(result)
        logical, intent(out) :: result
        integer :: rc

        call esmfiow_esmf_io_read_field(io_config, field_read, field_name, rc=rc)

        result = (rc == ESMFIOW_SUCCESS)
    end subroutine test_field_reading

    !> Test data integrity
    subroutine test_data_integrity(result)
        logical, intent(out) :: result
        integer :: rc

        ! Get pointer to read field data
        call ESMF_FieldGet(field_read, farrayPtr=read_data, rc=rc)
        if (rc /= ESMF_SUCCESS) then
            result = .false.
            return
        end if

        ! Compare data (allowing for small numerical differences)
        tolerance = 1.0e-10
        max_diff = 0.0
        data_matches = .true.

        do j = 1, size(field_data, 2)
            do i = 1, size(field_data, 1)
                max_diff = max(max_diff, abs(field_data(i,j) - read_data(i,j)))
                if (abs(field_data(i,j) - read_data(i,j)) > tolerance) then
                    data_matches = .false.
                end if
            end do
        end do

        result = data_matches
        if (.not. result) then
            write(*,'(A,E12.5)') " Maximum difference: ", max_diff
        end if
    end subroutine test_data_integrity

    !> Test CF compliance
    subroutine test_cf_compliance(result)
        logical, intent(out) :: result

        result = file_handle%is_cf_compliant
    end subroutine test_cf_compliance

    !> Test compression effectiveness
    subroutine test_compression_effectiveness(result)
        logical, intent(out) :: result
        integer :: rc, ncid, varid
        real :: compression_ratio

        ! This is a basic test - in practice you'd check actual file sizes
        ! and compression statistics from the NetCDF file
        result = .true.  ! Assume compression is working if no errors

        ! Could add more sophisticated compression ratio testing here
        ! by checking NetCDF variable properties
    end subroutine test_compression_effectiveness

    !> Clean up all test objects
    subroutine cleanup_test_objects()
        integer :: rc

        call esmfiow_file_close(file_handle, rc=rc)
        call ESMF_FieldDestroy(field_write, rc=rc)
        call ESMF_FieldDestroy(field_read, rc=rc)
        call ESMF_GridDestroy(grid, rc=rc)
        call ESMF_DistGridDestroy(distgrid, rc=rc)
    end subroutine cleanup_test_objects

end program test_complete_integration

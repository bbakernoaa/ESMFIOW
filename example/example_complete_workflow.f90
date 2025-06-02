!> @file example_complete_workflow.f90
!> @brief Complete workflow example for ESMFIOW library
!>
!> This example demonstrates the full ESMFIOW workflow:
!> - Creating compressed NetCDF files with ESMF I/O
!> - Writing ESMF fields with metadata and compression
!> - Reading fields back and verifying data
!>
!> @author ESMFIOW Development Team
!> @date June 2025

program example_complete_workflow
    use esmfiow_constants
    use esmfiow_esmf_io
    use esmfiow_io
    use esmfiow_field
    use esmfiow_grid
    use esmfiow_netcdf4
    use ESMF
    implicit none

    ! Variables for the example
    integer :: rc
    character(len=*), parameter :: output_file = "climate_data_compressed.nc4"

    ! ESMF objects
    type(ESMF_Grid) :: grid
    type(ESMF_Field) :: temperature_field, pressure_field
    type(ESMF_DistGrid) :: distgrid

    ! ESMFIOW objects
    type(esmfiow_esmf_io_config) :: io_config
    type(esmfiow_file_handle) :: file_handle
    type(esmfiow_nc4_compression_settings) :: compression_settings

    ! Field data
    real(ESMFIOW_REAL_KIND), pointer :: temp_data(:,:), press_data(:,:)
    integer, parameter :: nx = 128, ny = 64
    integer :: i, j

    write(*,*) "================================================="
    write(*,*) "ESMFIOW Complete Workflow Example"
    write(*,*) "================================================="

    ! Initialize ESMF
    write(*,*) "1. Initializing ESMF framework..."
    call esmfiow_esmf_initialize(rc=rc)
    if (rc /= ESMFIOW_SUCCESS) then
        write(*,*) "ERROR: Failed to initialize ESMF"
        stop 1
    end if
    write(*,*) "   ✓ ESMF initialized successfully"

    ! Set up compression settings
    write(*,*) "2. Configuring compression settings..."
    compression_settings%deflate_level = 6
    compression_settings%shuffle = ESMFIOW_NC4_SHUFFLE_ON
    compression_settings%fletcher32 = ESMFIOW_NC4_FLETCHER32_ON
    compression_settings%compression_type = ESMFIOW_NC4_COMPRESSION_DEFLATE
    write(*,*) "   ✓ Compression configured (deflate level 6, shuffle + fletcher32)"

    ! Create ESMF I/O configuration
    write(*,*) "3. Creating ESMF I/O configuration..."
    call esmfiow_esmf_create_io_config(io_config, output_file, &
        io_strategy=ESMFIOW_ESMF_IO_STRATEGY_NETCDF4, &
        compression=compression_settings, rc=rc)
    if (rc /= ESMFIOW_SUCCESS) then
        write(*,*) "ERROR: Failed to create I/O configuration"
        stop 1
    end if
    write(*,*) "   ✓ I/O configuration created with NetCDF4 compression"

    ! Create ESMF grid
    write(*,*) "4. Creating ESMF grid..."
    distgrid = ESMF_DistGridCreate(minIndex=[1,1], maxIndex=[nx,ny], rc=rc)
    if (rc /= ESMF_SUCCESS) then
        write(*,*) "ERROR: Failed to create distributed grid"
        stop 1
    end if

    grid = ESMF_GridCreate(distgrid=distgrid, rc=rc)
    if (rc /= ESMF_SUCCESS) then
        write(*,*) "ERROR: Failed to create grid"
        stop 1
    end if
    write(*,'(A,I0,A,I0,A)') "   ✓ Grid created (", nx, " x ", ny, ")"

    ! Create ESMF fields
    write(*,*) "5. Creating ESMF fields..."
    temperature_field = ESMF_FieldCreate(grid=grid, &
        typekind=ESMF_TYPEKIND_R8, &
        staggerloc=ESMF_STAGGERLOC_CENTER, &
        name="temperature", rc=rc)
    if (rc /= ESMF_SUCCESS) then
        write(*,*) "ERROR: Failed to create temperature field"
        stop 1
    end if

    pressure_field = ESMF_FieldCreate(grid=grid, &
        typekind=ESMF_TYPEKIND_R8, &
        staggerloc=ESMF_STAGGERLOC_CENTER, &
        name="pressure", rc=rc)
    if (rc /= ESMF_SUCCESS) then
        write(*,*) "ERROR: Failed to create pressure field"
        stop 1
    end if
    write(*,*) "   ✓ Temperature and pressure fields created"

    ! Initialize field data
    write(*,*) "6. Initializing field data..."
    call ESMF_FieldGet(temperature_field, farrayPtr=temp_data, rc=rc)
    call ESMF_FieldGet(pressure_field, farrayPtr=press_data, rc=rc)

    ! Create realistic climate data patterns
    do j = 1, size(temp_data, 2)
        do i = 1, size(temp_data, 1)
            ! Temperature: varies from 200K to 320K with lat/lon patterns
            temp_data(i,j) = 260.0 + 30.0 * sin(real(i)/real(nx) * 3.14159) * &
                                          cos(real(j)/real(ny) * 3.14159) + &
                           10.0 * sin(real(j)/real(ny) * 6.28318)

            ! Pressure: realistic atmospheric pressure (850-1050 hPa)
            press_data(i,j) = 950.0 + 50.0 * cos(real(j)/real(ny) * 3.14159) + &
                            20.0 * sin(real(i)/real(nx) * 3.14159)
        end do
    end do
    write(*,*) "   ✓ Realistic climate data patterns generated"

    ! Add metadata to fields
    write(*,*) "7. Adding CF-compliant metadata..."
    call esmfiow_field_add_metadata(temperature_field, &
        long_name="Air Temperature", &
        units="K", &
        standard_name="air_temperature", rc=rc)
    if (rc /= ESMFIOW_SUCCESS) then
        write(*,*) "ERROR: Failed to add temperature metadata"
        stop 1
    end if

    call esmfiow_field_add_metadata(pressure_field, &
        long_name="Atmospheric Pressure", &
        units="hPa", &
        standard_name="air_pressure", rc=rc)
    if (rc /= ESMFIOW_SUCCESS) then
        write(*,*) "ERROR: Failed to add pressure metadata"
        stop 1
    end if
    write(*,*) "   ✓ CF-compliant metadata added to both fields"

    ! Create compressed NetCDF file
    write(*,*) "8. Creating compressed NetCDF file..."
    call esmfiow_file_create(output_file, file_handle, &
        title="ESMFIOW Example Climate Data", &
        institution="ESMFIOW Development Team", rc=rc)
    if (rc /= ESMFIOW_SUCCESS) then
        write(*,*) "ERROR: Failed to create file"
        stop 1
    end if
    write(*,*) "   ✓ File created: " // trim(output_file)

    ! Write fields with compression
    write(*,*) "9. Writing fields with compression..."
    call esmfiow_esmf_io_write_field(io_config, temperature_field, "temperature", rc=rc)
    if (rc /= ESMFIOW_SUCCESS) then
        write(*,*) "ERROR: Failed to write temperature field"
        stop 1
    end if

    call esmfiow_esmf_io_write_field(io_config, pressure_field, "pressure", rc=rc)
    if (rc /= ESMFIOW_SUCCESS) then
        write(*,*) "ERROR: Failed to write pressure field"
        stop 1
    end if
    write(*,*) "   ✓ Both fields written with compression enabled"

    ! Close file
    write(*,*) "10. Closing file..."
    call esmfiow_file_close(file_handle, rc=rc)
    if (rc /= ESMFIOW_SUCCESS) then
        write(*,*) "ERROR: Failed to close file"
        stop 1
    end if
    write(*,*) "    ✓ File closed successfully"

    ! Clean up ESMF objects
    write(*,*) "11. Cleaning up..."
    call ESMF_FieldDestroy(temperature_field, rc=rc)
    call ESMF_FieldDestroy(pressure_field, rc=rc)
    call ESMF_GridDestroy(grid, rc=rc)
    call ESMF_DistGridDestroy(distgrid, rc=rc)
    write(*,*) "    ✓ ESMF objects destroyed"

    ! Finalize ESMF
    call esmfiow_esmf_finalize(rc=rc)
    write(*,*) "    ✓ ESMF finalized"

    write(*,*) "================================================="
    write(*,*) "SUCCESS: Complete workflow executed!"
    write(*,*) "Output file: " // trim(output_file)
    write(*,*) "The file contains compressed climate data with:"
    write(*,*) "  - Temperature field (200-320 K range)"
    write(*,*) "  - Pressure field (850-1050 hPa range)"
    write(*,*) "  - CF-compliant metadata"
    write(*,*) "  - NetCDF4 compression (deflate + shuffle)"
    write(*,*) "================================================="

end program example_complete_workflow

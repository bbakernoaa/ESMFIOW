! Example: Simple Climate Model Integration
! This example shows how to integrate ESMFIOW into a basic climate model
! that outputs temperature, precipitation, and wind fields.

program climate_model_integration
    use esmfiow
    implicit none

    ! Model parameters
    integer, parameter :: nx = 144, ny = 90, nz = 20, nt = 365
    real, parameter :: dt = 86400.0  ! 1 day in seconds

    ! Model state variables
    real, allocatable :: temperature(:,:,:)    ! 3D temperature field
    real, allocatable :: precipitation(:,:)    ! 2D surface precipitation
    real, allocatable :: u_wind(:,:,:)         ! 3D u-component wind
    real, allocatable :: v_wind(:,:,:)         ! 3D v-component wind
    real, allocatable :: pressure(:)           ! 1D pressure levels
    real, allocatable :: longitude(:), latitude(:)

    ! ESMFIOW variables
    type(esmfiow_file_handle) :: daily_file, monthly_file
    type(esmfiow_nc4_compression_settings) :: compression
    integer :: rc, day, month, days_in_month
    character(20) :: filename

    ! Initialize model
    call initialize_model()

    ! Initialize ESMFIOW
    call esmfiow_initialize(rc)
    if (rc /= ESMFIOW_SUCCESS) stop "Failed to initialize ESMFIOW"

    ! Configure compression for climate data
    compression%deflate = .true.
    compression%deflate_level = 6  ! Good balance of compression/speed
    compression%shuffle = .true.
    compression%fletcher32 = .true.

    ! Main model time loop
    month = 1
    days_in_month = days_in_month_func(month)

    ! Create monthly output file
    write(filename, '(A,I2.2,A)') "climate_month_", month, ".nc"
    call create_monthly_file(monthly_file, filename, compression)

    do day = 1, nt
        ! Run model for one day
        call run_model_timestep(day)

        ! Write daily output every day
        write(filename, '(A,I3.3,A)') "climate_day_", day, ".nc"
        call write_daily_output(filename, compression, day)

        ! Check if we need to start a new month
        if (mod(day, days_in_month) == 0 .and. day < nt) then
            ! Close current monthly file
            call esmfiow_file_close(monthly_file, rc)

            ! Start new month
            month = month + 1
            days_in_month = days_in_month_func(month)
            write(filename, '(A,I2.2,A)') "climate_month_", month, ".nc"
            call create_monthly_file(monthly_file, filename, compression)
        end if

        ! Add to monthly averages (simplified - normally you'd accumulate)
        call add_to_monthly_averages(monthly_file, day)
    end do

    ! Close final monthly file
    call esmfiow_file_close(monthly_file, rc)

    ! Finalize
    call esmfiow_finalize(rc)
    print *, "Climate model integration complete"

contains

    subroutine initialize_model()
        integer :: i, j, k

        ! Allocate arrays
        allocate(temperature(nx, ny, nz))
        allocate(precipitation(nx, ny))
        allocate(u_wind(nx, ny, nz))
        allocate(v_wind(nx, ny, nz))
        allocate(pressure(nz))
        allocate(longitude(nx), latitude(ny))

        ! Initialize coordinates
        do i = 1, nx
            longitude(i) = -180.0 + (i-1) * 360.0 / nx
        end do

        do j = 1, ny
            latitude(j) = -90.0 + (j-1) * 180.0 / ny
        end do

        ! Initialize pressure levels (hPa)
        do k = 1, nz
            pressure(k) = 1000.0 - (k-1) * 50.0
        end do

        print *, "Model initialized with grid:", nx, "x", ny, "x", nz
    end subroutine initialize_model

    subroutine run_model_timestep(day)
        integer, intent(in) :: day
        integer :: i, j, k
        real :: lat_factor, seasonal_factor, height_factor

        ! Simple model physics (for demonstration)
        seasonal_factor = sin(real(day) * 2.0 * 3.14159 / 365.0)

        do k = 1, nz
            height_factor = pressure(k) / 1000.0  ! Height factor
            do j = 1, ny
                lat_factor = cos(latitude(j) * 3.14159 / 180.0)
                do i = 1, nx
                    ! Temperature model (simplified)
                    temperature(i,j,k) = 273.15 + 15.0 * lat_factor + &
                                       10.0 * seasonal_factor * lat_factor + &
                                       (1000.0 - pressure(k)) * 0.0065  ! Lapse rate

                    ! Wind model (simplified)
                    u_wind(i,j,k) = 10.0 * sin(latitude(j) * 3.14159 / 180.0) * height_factor
                    v_wind(i,j,k) = 5.0 * cos(longitude(i) * 3.14159 / 180.0) * height_factor
                end do

                ! Surface precipitation (2D)
                precipitation(i,j) = max(0.0, 0.001 * lat_factor * (1.0 + seasonal_factor))
            end do
        end do
    end subroutine run_model_timestep

    subroutine create_monthly_file(file_handle, filename, compression)
        type(esmfiow_file_handle), intent(out) :: file_handle
        character(*), intent(in) :: filename
        type(esmfiow_nc4_compression_settings), intent(in) :: compression
        integer :: rc

        call esmfiow_file_create(filename, file_handle, &
                               title="Monthly Climate Model Output", &
                               institution="Climate Research Center", &
                               source="Simple Climate Model v1.0", &
                               conventions="CF-1.8", &
                               rc=rc)

        ! Add coordinate information
        call esmfiow_cf_add_coordinates(file_handle, &
                                      lon_values=longitude, &
                                      lat_values=latitude, &
                                      pressure_values=pressure, &
                                      rc=rc)
    end subroutine create_monthly_file

    subroutine write_daily_output(filename, compression, day)
        character(*), intent(in) :: filename
        type(esmfiow_nc4_compression_settings), intent(in) :: compression
        integer, intent(in) :: day

        type(esmfiow_file_handle) :: file_handle
        integer :: rc

        ! Create daily file
        call esmfiow_file_create(filename, file_handle, &
                               title="Daily Climate Model Output", &
                               institution="Climate Research Center", &
                               source="Simple Climate Model v1.0", &
                               conventions="CF-1.8", &
                               rc=rc)

        ! Add coordinates
        call esmfiow_cf_add_coordinates(file_handle, &
                                      lon_values=longitude, &
                                      lat_values=latitude, &
                                      pressure_values=pressure, &
                                      rc=rc)

        ! Write 3D temperature field
        call esmfiow_write_field(file_handle, temperature, "temperature", &
                               standard_name="air_temperature", &
                               long_name="Air Temperature", &
                               units="K", &
                               dims=["longitude", "latitude ", "pressure "], &
                               coordinates="longitude latitude pressure", &
                               compression=compression, &
                               rc=rc)

        ! Write 2D precipitation field
        call esmfiow_write_field(file_handle, precipitation, "precipitation", &
                               standard_name="precipitation_rate", &
                               long_name="Precipitation Rate", &
                               units="kg m-2 s-1", &
                               dims=["longitude", "latitude "], &
                               coordinates="longitude latitude", &
                               compression=compression, &
                               rc=rc)

        ! Write wind components
        call esmfiow_write_field(file_handle, u_wind, "u_wind", &
                               standard_name="eastward_wind", &
                               long_name="Eastward Wind Component", &
                               units="m s-1", &
                               dims=["longitude", "latitude ", "pressure "], &
                               coordinates="longitude latitude pressure", &
                               compression=compression, &
                               rc=rc)

        call esmfiow_write_field(file_handle, v_wind, "v_wind", &
                               standard_name="northward_wind", &
                               long_name="Northward Wind Component", &
                               units="m s-1", &
                               dims=["longitude", "latitude ", "pressure "], &
                               coordinates="longitude latitude pressure", &
                               compression=compression, &
                               rc=rc)

        call esmfiow_file_close(file_handle, rc)
    end subroutine write_daily_output

    subroutine add_to_monthly_averages(file_handle, day)
        type(esmfiow_file_handle), intent(in) :: file_handle
        integer, intent(in) :: day
        integer :: rc

        ! In a real model, you would accumulate daily values
        ! and write monthly averages. This is simplified.

        ! For demonstration, just write current day's data
        ! (In practice, you'd accumulate and average)
        call esmfiow_write_field(file_handle, temperature, "temperature_monthly", &
                               long_name="Monthly Mean Air Temperature", &
                               units="K", &
                               rc=rc)
    end subroutine add_to_monthly_averages

    integer function days_in_month_func(month)
        integer, intent(in) :: month
        integer, parameter :: days_per_month(12) = [31,28,31,30,31,30,31,31,30,31,30,31]
        days_in_month_func = days_per_month(month)
    end function days_in_month_func

end program climate_model_integration

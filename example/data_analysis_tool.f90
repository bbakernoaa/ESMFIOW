! Example: Data Analysis Tool using ESMFIOW
! This example demonstrates how to use ESMFIOW for reading and analyzing
! climate model output data with different compression and chunking strategies.

program data_analysis_tool
    use esmfiow
    implicit none

    ! Analysis parameters
    character(256) :: input_file, output_file
    character(64) :: variable_name
    integer :: analysis_type

    ! Data arrays
    real, allocatable :: input_data(:,:,:)
    real, allocatable :: result_data(:,:)
    real, allocatable :: longitude(:), latitude(:), time(:)

    ! ESMFIOW handles
    type(esmfiow_file_handle) :: input_handle, output_handle
    type(esmfiow_nc4_compression_settings) :: compression
    integer :: rc, nx, ny, nt, i, j, t

    ! Get command line arguments
    call get_command_line_args()

    ! Initialize ESMFIOW
    call esmfiow_initialize(rc)
    if (rc /= ESMFIOW_SUCCESS) stop "Failed to initialize ESMFIOW"

    ! Open input file for reading
    call esmfiow_file_open(input_file, input_handle, "r", rc=rc)
    if (rc /= ESMFIOW_SUCCESS) then
        print *, "ERROR: Cannot open input file: ", trim(input_file)
        stop 1
    end if

    ! Read coordinate information
    call read_coordinates()

    ! Read the variable data
    call read_variable_data()

    ! Perform analysis
    select case (analysis_type)
    case (1)
        call compute_time_mean()
    case (2)
        call compute_seasonal_cycle()
    case (3)
        call compute_spatial_statistics()
    case (4)
        call compute_trends()
    case default
        print *, "Unknown analysis type:", analysis_type
        stop 1
    end select

    ! Write results
    call write_analysis_results()

    ! Cleanup
    call esmfiow_file_close(input_handle, rc)
    call esmfiow_file_close(output_handle, rc)
    call esmfiow_finalize(rc)

    print *, "Analysis complete. Results written to: ", trim(output_file)

contains

    subroutine get_command_line_args()
        integer :: nargs, i
        character(256) :: arg

        nargs = command_argument_count()
        if (nargs < 4) then
            print *, "Usage: data_analysis_tool <input_file> <variable> <analysis_type> <output_file>"
            print *, "Analysis types:"
            print *, "  1 - Time mean"
            print *, "  2 - Seasonal cycle"
            print *, "  3 - Spatial statistics"
            print *, "  4 - Trends"
            stop 1
        end if

        call get_command_argument(1, input_file)
        call get_command_argument(2, variable_name)
        call get_command_argument(3, arg)
        read(arg, *) analysis_type
        call get_command_argument(4, output_file)

        print *, "Input file:", trim(input_file)
        print *, "Variable:", trim(variable_name)
        print *, "Analysis type:", analysis_type
        print *, "Output file:", trim(output_file)
    end subroutine get_command_line_args

    subroutine read_coordinates()
        integer :: rc

        ! Get coordinate dimensions first
        call esmfiow_get_dimension_size(input_handle, "longitude", nx, rc)
        call esmfiow_get_dimension_size(input_handle, "latitude", ny, rc)
        call esmfiow_get_dimension_size(input_handle, "time", nt, rc)

        print *, "Grid dimensions:", nx, "x", ny, "x", nt

        ! Allocate coordinate arrays
        allocate(longitude(nx), latitude(ny), time(nt))

        ! Read coordinate values
        call esmfiow_read_field(input_handle, "longitude", longitude, rc=rc)
        call esmfiow_read_field(input_handle, "latitude", latitude, rc=rc)
        call esmfiow_read_field(input_handle, "time", time, rc=rc)

        print *, "Longitude range:", minval(longitude), "to", maxval(longitude)
        print *, "Latitude range:", minval(latitude), "to", maxval(latitude)
        print *, "Time range:", minval(time), "to", maxval(time)
    end subroutine read_coordinates

    subroutine read_variable_data()
        integer :: rc

        ! Allocate data array
        allocate(input_data(nx, ny, nt))

        ! Read variable data with optimized chunking
        print *, "Reading variable data:", trim(variable_name)
        call esmfiow_read_field(input_handle, trim(variable_name), input_data, rc=rc)
        if (rc /= ESMFIOW_SUCCESS) then
            print *, "ERROR: Cannot read variable:", trim(variable_name)
            stop 1
        end if

        print *, "Data range:", minval(input_data), "to", maxval(input_data)
        print *, "Data shape:", shape(input_data)
    end subroutine read_variable_data

    subroutine compute_time_mean()
        print *, "Computing time mean..."

        ! Allocate result array (2D: lon x lat)
        allocate(result_data(nx, ny))

        ! Compute mean over time dimension
        do j = 1, ny
            do i = 1, nx
                result_data(i, j) = sum(input_data(i, j, :)) / real(nt)
            end do
        end do

        print *, "Time mean computed. Range:", minval(result_data), "to", maxval(result_data)
    end subroutine compute_time_mean

    subroutine compute_seasonal_cycle()
        integer :: month, t_month, count_month
        real :: monthly_sum

        print *, "Computing seasonal cycle..."

        ! Allocate result array (12 months x ny latitudes - simplified)
        allocate(result_data(12, ny))
        result_data = 0.0

        ! Compute monthly means (simplified - assumes monthly data)
        do month = 1, 12
            do j = 1, ny
                monthly_sum = 0.0
                count_month = 0

                do t = month, nt, 12  ! Every 12th timestep starting from month
                    if (t <= nt) then
                        monthly_sum = monthly_sum + sum(input_data(:, j, t)) / real(nx)
                        count_month = count_month + 1
                    end if
                end do

                if (count_month > 0) then
                    result_data(month, j) = monthly_sum / real(count_month)
                end if
            end do
        end do

        print *, "Seasonal cycle computed."
    end subroutine compute_seasonal_cycle

    subroutine compute_spatial_statistics()
        real :: global_mean, global_std, lat_weight
        integer :: valid_points

        print *, "Computing spatial statistics..."

        ! Allocate result array (4 statistics x nt times)
        allocate(result_data(4, nt))

        do t = 1, nt
            ! Global mean (area-weighted)
            global_mean = 0.0
            global_std = 0.0
            valid_points = 0

            do j = 1, ny
                lat_weight = cos(latitude(j) * 3.14159 / 180.0)
                do i = 1, nx
                    if (.not. isnan(input_data(i, j, t))) then
                        global_mean = global_mean + input_data(i, j, t) * lat_weight
                        valid_points = valid_points + 1
                    end if
                end do
            end do

            if (valid_points > 0) then
                global_mean = global_mean / real(valid_points)
            end if

            ! Global standard deviation
            do j = 1, ny
                lat_weight = cos(latitude(j) * 3.14159 / 180.0)
                do i = 1, nx
                    if (.not. isnan(input_data(i, j, t))) then
                        global_std = global_std + (input_data(i, j, t) - global_mean)**2 * lat_weight
                    end if
                end do
            end do

            if (valid_points > 1) then
                global_std = sqrt(global_std / real(valid_points - 1))
            end if

            ! Store statistics
            result_data(1, t) = global_mean
            result_data(2, t) = global_std
            result_data(3, t) = minval(input_data(:, :, t))
            result_data(4, t) = maxval(input_data(:, :, t))
        end do

        print *, "Spatial statistics computed."
    end subroutine compute_spatial_statistics

    subroutine compute_trends()
        real :: slope, intercept, x_mean, y_mean, num, den
        integer :: n

        print *, "Computing trends..."

        ! Allocate result array (2 coefficients x ny latitudes x nx longitudes)
        ! Simplified to trends at each grid point
        allocate(result_data(nx, ny))

        n = nt

        do j = 1, ny
            do i = 1, nx
                ! Compute linear trend using least squares
                x_mean = sum(time) / real(n)
                y_mean = sum(input_data(i, j, :)) / real(n)

                num = 0.0
                den = 0.0
                do t = 1, nt
                    num = num + (time(t) - x_mean) * (input_data(i, j, t) - y_mean)
                    den = den + (time(t) - x_mean)**2
                end do

                if (abs(den) > 1e-10) then
                    slope = num / den
                else
                    slope = 0.0
                end if

                result_data(i, j) = slope  ! Store trend (units per time)
            end do
        end do

        print *, "Trends computed. Range:", minval(result_data), "to", maxval(result_data)
    end subroutine compute_trends

    subroutine write_analysis_results()
        integer :: rc
        character(128) :: result_description, result_units

        ! Configure compression for output
        compression%deflate = .true.
        compression%deflate_level = 6
        compression%shuffle = .true.
        compression%fletcher32 = .true.

        ! Create output file
        call esmfiow_file_create(output_file, output_handle, &
                               title="Climate Data Analysis Results", &
                               institution="Data Analysis Center", &
                               source="ESMFIOW Data Analysis Tool", &
                               conventions="CF-1.8", &
                               rc=rc)

        ! Write appropriate coordinates based on analysis type
        select case (analysis_type)
        case (1)  ! Time mean
            call esmfiow_cf_add_coordinates(output_handle, &
                                          lon_values=longitude, &
                                          lat_values=latitude, &
                                          rc=rc)
            result_description = "Time Mean of " // trim(variable_name)
            result_units = "same as input"

        case (2)  ! Seasonal cycle
            ! Create month coordinate
            real :: months(12) = [(real(i), i=1,12)]
            call esmfiow_cf_add_coordinates(output_handle, &
                                          month_values=months, &
                                          lat_values=latitude, &
                                          rc=rc)
            result_description = "Seasonal Cycle of " // trim(variable_name)
            result_units = "same as input"

        case (3)  ! Spatial statistics
            call esmfiow_cf_add_coordinates(output_handle, &
                                          time_values=time, &
                                          time_units="days since 2000-01-01", &
                                          rc=rc)
            result_description = "Spatial Statistics of " // trim(variable_name)
            result_units = "same as input"

        case (4)  ! Trends
            call esmfiow_cf_add_coordinates(output_handle, &
                                          lon_values=longitude, &
                                          lat_values=latitude, &
                                          rc=rc)
            result_description = "Linear Trend of " // trim(variable_name)
            result_units = "input_units per time_unit"
        end select

        ! Write result data
        call esmfiow_write_field(output_handle, result_data, "analysis_result", &
                               long_name=trim(result_description), &
                               units=trim(result_units), &
                               compression=compression, &
                               rc=rc)

        ! Add analysis metadata
        call esmfiow_add_global_attribute(output_handle, "analysis_type", analysis_type, rc=rc)
        call esmfiow_add_global_attribute(output_handle, "input_file", trim(input_file), rc=rc)
        call esmfiow_add_global_attribute(output_handle, "input_variable", trim(variable_name), rc=rc)

        print *, "Results written to output file."
    end subroutine write_analysis_results

    logical function isnan(x)
        real, intent(in) :: x
        isnan = (x /= x)  ! NaN is not equal to itself
    end function isnan

end program data_analysis_tool

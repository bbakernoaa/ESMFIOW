!> @file esmfiow_grid.f90
!> @brief ESMFIOW Grid Module
!>
!> This module provides grid handling utilities for working with ESMF grids
!> and converting them to/from NetCDF files with CF convention compliance.
!>
!> @author ESMFIOW Development Team
!> @date June 2025
!> @defgroup esmfiow_grid Grid Module
!> @{

!> @brief Grid handling module
!>
!> Implements functionality for working with ESMF grids, including writing
!> and reading grid definitions to/from NetCDF with CF conventions, and
!> handling grid coordinates in various formats.
module esmfiow_grid
    use esmfiow_constants
    use esmfiow_cf_utils
#ifdef USE_ESMF
    use ESMF
#endif
    use netcdf
    implicit none

    private
    public :: esmfiow_grid_to_netcdf, esmfiow_grid_from_netcdf
    public :: esmfiow_add_grid_coordinates

#ifndef USE_ESMF
    ! Placeholder grid type when ESMF not available
    type :: esmf_grid_placeholder
        integer :: grid_id
        integer :: nx, ny
        real(ESMFIOW_REAL_KIND), allocatable :: lon(:,:), lat(:,:)
        character(len=ESMFIOW_MAX_STRING_LEN) :: name
    end type esmf_grid_placeholder
#endif

contains

    !> Write ESMF grid to NetCDF file with CF conventions
    !>
    !> This subroutine extracts coordinate information from an ESMF grid
    !> and writes it to a NetCDF file following CF conventions. It creates
    !> coordinate dimensions, coordinate variables, and adds appropriate
    !> CF-compliant attributes.
    !>
    !> @param[in] file_handle NetCDF file handle for output
    !> @param[in] grid ESMF grid object containing coordinate information
    !> @param[out] rc Optional return code (0=success, negative=error)
    !>
    !> @note Uses ESMF_GridGetCoord to extract actual ESMF grid coordinates
    !> @see ESMF documentation for grid coordinate extraction
    subroutine esmfiow_grid_to_netcdf(file_handle, grid, rc)
        integer, intent(in) :: file_handle  ! File handle
        type(ESMF_Grid), intent(in) :: grid  ! ESMF grid type
        integer, intent(out), optional :: rc

        integer :: local_rc, esmf_rc, nc_rc
        integer :: x_dim_id, y_dim_id, x_var_id, y_var_id
        integer :: dimCount, i1, i2, j1, j2
        real(ESMF_KIND_R8), pointer :: lon_ptr(:,:), lat_ptr(:,:)
        integer, allocatable :: maxIndex(:)

        local_rc = ESMFIOW_SUCCESS

        ! Get grid information
        call ESMF_GridGet(grid, dimCount=dimCount, rc=esmf_rc)
        if (esmf_rc /= ESMF_SUCCESS) then
            local_rc = ESMFIOW_ERROR
            call ESMF_LogWrite("ERROR: Failed to get grid information", &
                              ESMF_LOGMSG_ERROR, rc=esmf_rc)
            if (present(rc)) rc = local_rc
            return
        end if

        ! Allocate maxIndex array
        allocate(maxIndex(dimCount))
        call ESMF_GridGet(grid, maxIndex=maxIndex, rc=esmf_rc)
        if (esmf_rc /= ESMF_SUCCESS) then
            local_rc = ESMFIOW_ERROR
            deallocate(maxIndex)
            if (present(rc)) rc = local_rc
            return
        end if

        ! Create dimensions in NetCDF file
        nc_rc = nf90_def_dim(file_handle, "x", maxIndex(1), x_dim_id)
        if (nc_rc /= NF90_NOERR .and. nc_rc /= NF90_ENAMEINUSE) then
            local_rc = ESMFIOW_ERROR
            deallocate(maxIndex)
            if (present(rc)) rc = local_rc
            return
        end if

        nc_rc = nf90_def_dim(file_handle, "y", maxIndex(2), y_dim_id)
        if (nc_rc /= NF90_NOERR .and. nc_rc /= NF90_ENAMEINUSE) then
            local_rc = ESMFIOW_ERROR
            deallocate(maxIndex)
            if (present(rc)) rc = local_rc
            return
        end if

        ! Create coordinate variables
        nc_rc = nf90_def_var(file_handle, "lon", NF90_DOUBLE, [x_dim_id, y_dim_id], x_var_id)
        if (nc_rc /= NF90_NOERR) then
            local_rc = ESMFIOW_ERROR
            deallocate(maxIndex)
            if (present(rc)) rc = local_rc
            return
        end if

        nc_rc = nf90_def_var(file_handle, "lat", NF90_DOUBLE, [x_dim_id, y_dim_id], y_var_id)
        if (nc_rc /= NF90_NOERR) then
            local_rc = ESMFIOW_ERROR
            deallocate(maxIndex)
            if (present(rc)) rc = local_rc
            return
        end if

        ! Add CF-compliant attributes for coordinates
        call cf_add_coordinate_attributes(x_var_id, "lon", "longitude", &
                                        "degrees_east", "X", esmf_rc)
        call cf_add_coordinate_attributes(y_var_id, "lat", "latitude", &
                                        "degrees_north", "Y", esmf_rc)

        ! End define mode
        nc_rc = nf90_enddef(file_handle)
        if (nc_rc /= NF90_NOERR .and. nc_rc /= NF90_ENOTINDEFINE) then
            local_rc = ESMFIOW_ERROR
            deallocate(maxIndex)
            if (present(rc)) rc = local_rc
            return
        end if

        ! Get coordinate data from ESMF grid
        call ESMF_GridGetCoord(grid, coordDim=1, farrayPtr=lon_ptr, rc=esmf_rc)
        if (esmf_rc /= ESMF_SUCCESS) then
            local_rc = ESMFIOW_ERROR
            call ESMF_LogWrite("ERROR: Failed to get longitude coordinates", &
                              ESMF_LOGMSG_ERROR, rc=esmf_rc)
            deallocate(maxIndex)
            if (present(rc)) rc = local_rc
            return
        end if

        call ESMF_GridGetCoord(grid, coordDim=2, farrayPtr=lat_ptr, rc=esmf_rc)
        if (esmf_rc /= ESMF_SUCCESS) then
            local_rc = ESMFIOW_ERROR
            call ESMF_LogWrite("ERROR: Failed to get latitude coordinates", &
                              ESMF_LOGMSG_ERROR, rc=esmf_rc)
            deallocate(maxIndex)
            if (present(rc)) rc = local_rc
            return
        end if

        ! Write coordinate data to NetCDF variables
        nc_rc = nf90_put_var(file_handle, x_var_id, lon_ptr)
        if (nc_rc /= NF90_NOERR) then
            local_rc = ESMFIOW_ERROR
            deallocate(maxIndex)
            if (present(rc)) rc = local_rc
            return
        end if

        nc_rc = nf90_put_var(file_handle, y_var_id, lat_ptr)
        if (nc_rc /= NF90_NOERR) then
            local_rc = ESMFIOW_ERROR
        end if

        deallocate(maxIndex)
        call ESMF_LogWrite("Successfully wrote ESMF grid to NetCDF", &
                          ESMF_LOGMSG_INFO, rc=esmf_rc)
        ! 1. Extract grid coordinates from ESMF
        ! 2. Create coordinate dimensions
        ! 3. Create coordinate variables
        ! 4. Add CF-compliant attributes

        if (present(rc)) rc = local_rc

    end subroutine esmfiow_grid_to_netcdf

    !> Read grid from NetCDF file and create ESMF grid
    !>
    !> This subroutine reads coordinate information from a CF-compliant
    !> NetCDF file and creates an ESMF grid object. It handles various
    !> coordinate systems and grid topologies supported by CF conventions.
    !>
    !> @param[in] file_handle NetCDF file handle for input
    !> @param[out] grid ESMF grid object to be created
    !> @param[out] rc Optional return code (0=success, negative=error)
    !>
    !> @note Creates actual ESMF grid objects from NetCDF coordinate data
    !> @see CF Convention section on coordinate systems
    subroutine esmfiow_grid_from_netcdf(file_handle, grid, rc)
        integer, intent(in) :: file_handle  ! File handle
        type(ESMF_Grid), intent(out) :: grid  ! ESMF grid type
        integer, intent(out), optional :: rc

        integer :: local_rc, esmf_rc, nc_rc
        integer :: lon_varid, lat_varid, x_dimid, y_dimid
        integer :: nx, ny
        real(ESMF_KIND_R8), allocatable :: lon_data(:,:), lat_data(:,:)
        real(ESMF_KIND_R8), pointer :: lon_ptr(:,:), lat_ptr(:,:)
        character(len=ESMFIOW_MAX_STRING_LEN) :: coord_name

        local_rc = ESMFIOW_SUCCESS

        ! Try to find longitude coordinate variable
        nc_rc = nf90_inq_varid(file_handle, "lon", lon_varid)
        if (nc_rc /= NF90_NOERR) then
            nc_rc = nf90_inq_varid(file_handle, "longitude", lon_varid)
            if (nc_rc /= NF90_NOERR) then
                local_rc = ESMFIOW_ERROR
                call ESMF_LogWrite("ERROR: Cannot find longitude coordinate variable", &
                                  ESMF_LOGMSG_ERROR, rc=esmf_rc)
                if (present(rc)) rc = local_rc
                return
            end if
        end if

        ! Try to find latitude coordinate variable
        nc_rc = nf90_inq_varid(file_handle, "lat", lat_varid)
        if (nc_rc /= NF90_NOERR) then
            nc_rc = nf90_inq_varid(file_handle, "latitude", lat_varid)
            if (nc_rc /= NF90_NOERR) then
                local_rc = ESMFIOW_ERROR
                call ESMF_LogWrite("ERROR: Cannot find latitude coordinate variable", &
                                  ESMF_LOGMSG_ERROR, rc=esmf_rc)
                if (present(rc)) rc = local_rc
                return
            end if
        end if

        ! Get dimensions from coordinate variables
        nc_rc = nf90_inq_dimid(file_handle, "x", x_dimid)
        if (nc_rc /= NF90_NOERR) then
            local_rc = ESMFIOW_ERROR
            if (present(rc)) rc = local_rc
            return
        end if

        nc_rc = nf90_inq_dimid(file_handle, "y", y_dimid)
        if (nc_rc /= NF90_NOERR) then
            local_rc = ESMFIOW_ERROR
            if (present(rc)) rc = local_rc
            return
        end if

        nc_rc = nf90_inquire_dimension(file_handle, x_dimid, len=nx)
        if (nc_rc /= NF90_NOERR) then
            local_rc = ESMFIOW_ERROR
            if (present(rc)) rc = local_rc
            return
        end if

        nc_rc = nf90_inquire_dimension(file_handle, y_dimid, len=ny)
        if (nc_rc /= NF90_NOERR) then
            local_rc = ESMFIOW_ERROR
            if (present(rc)) rc = local_rc
            return
        end if

        ! Create ESMF grid with coordinate support
        grid = ESMF_GridCreateNoPeriDim(maxIndex=[nx, ny], &
                                       coordSys=ESMF_COORDSYS_SPH_DEG, &
                                       name="netcdf_grid", rc=esmf_rc)
        if (esmf_rc /= ESMF_SUCCESS) then
            local_rc = ESMFIOW_ERROR
            call ESMF_LogWrite("ERROR: Failed to create ESMF grid", &
                              ESMF_LOGMSG_ERROR, rc=esmf_rc)
            if (present(rc)) rc = local_rc
            return
        end if

        ! Add coordinates to the grid
        call ESMF_GridAddCoord(grid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=esmf_rc)
        if (esmf_rc /= ESMF_SUCCESS) then
            local_rc = ESMFIOW_ERROR
            call ESMF_GridDestroy(grid, rc=esmf_rc)
            if (present(rc)) rc = local_rc
            return
        end if

        ! Allocate arrays for coordinate data
        allocate(lon_data(nx, ny), lat_data(nx, ny))

        ! Read coordinate data from NetCDF
        nc_rc = nf90_get_var(file_handle, lon_varid, lon_data)
        if (nc_rc /= NF90_NOERR) then
            local_rc = ESMFIOW_ERROR
            deallocate(lon_data, lat_data)
            call ESMF_GridDestroy(grid, rc=esmf_rc)
            if (present(rc)) rc = local_rc
            return
        end if

        nc_rc = nf90_get_var(file_handle, lat_varid, lat_data)
        if (nc_rc /= NF90_NOERR) then
            local_rc = ESMFIOW_ERROR
            deallocate(lon_data, lat_data)
            call ESMF_GridDestroy(grid, rc=esmf_rc)
            if (present(rc)) rc = local_rc
            return
        end if

        ! Get coordinate arrays from ESMF grid and populate them
        call ESMF_GridGetCoord(grid, coordDim=1, farrayPtr=lon_ptr, rc=esmf_rc)
        if (esmf_rc /= ESMF_SUCCESS) then
            local_rc = ESMFIOW_ERROR
            deallocate(lon_data, lat_data)
            call ESMF_GridDestroy(grid, rc=esmf_rc)
            if (present(rc)) rc = local_rc
            return
        end if

        call ESMF_GridGetCoord(grid, coordDim=2, farrayPtr=lat_ptr, rc=esmf_rc)
        if (esmf_rc /= ESMF_SUCCESS) then
            local_rc = ESMFIOW_ERROR
            deallocate(lon_data, lat_data)
            call ESMF_GridDestroy(grid, rc=esmf_rc)
            if (present(rc)) rc = local_rc
            return
        end if

        ! Copy data to ESMF grid coordinate arrays
        lon_ptr = lon_data
        lat_ptr = lat_data

        deallocate(lon_data, lat_data)
        call ESMF_LogWrite("Successfully created ESMF grid from NetCDF", &
                          ESMF_LOGMSG_INFO, rc=esmf_rc)

        if (present(rc)) rc = local_rc

    end subroutine esmfiow_grid_from_netcdf

    !> Add coordinate variables to NetCDF file
    !>
    !> This subroutine adds coordinate variables (longitude, latitude, levels, etc.)
    !> to a NetCDF file based on an ESMF grid. It ensures proper CF-compliant
    !> coordinate metadata and attributes.
    !>
    !> @param[in] file_handle NetCDF file handle for output
    !> @param[in] grid ESMF grid object containing coordinate information
    !> @param[out] rc Optional return code (0=success, negative=error)
    !>
    !> @note This complements grid output by adding coordinate metadata
    !> @see CF Convention section on coordinate variables
    subroutine esmfiow_add_grid_coordinates(file_handle, grid, rc)
        integer, intent(in) :: file_handle  ! File handle
        type(ESMF_Grid), intent(in) :: grid  ! ESMF grid type
        integer, intent(out), optional :: rc

        integer :: local_rc, esmf_rc, nc_rc
        integer :: x_dimid, y_dimid, x_varid, y_varid
        integer :: dimCount
        integer, allocatable :: maxIndex(:)
        real(ESMF_KIND_R8), pointer :: lon_ptr(:,:), lat_ptr(:,:)
        real(ESMF_KIND_R8), allocatable :: x_coord(:), y_coord(:)
        integer :: i, j

        local_rc = ESMFIOW_SUCCESS

        ! Get grid information
        call ESMF_GridGet(grid, dimCount=dimCount, rc=esmf_rc)
        if (esmf_rc /= ESMF_SUCCESS) then
            local_rc = ESMFIOW_ERROR
            call ESMF_LogWrite("ERROR: Failed to get grid information", &
                              ESMF_LOGMSG_ERROR, rc=esmf_rc)
            if (present(rc)) rc = local_rc
            return
        end if

        ! Allocate maxIndex array
        allocate(maxIndex(dimCount))
        call ESMF_GridGet(grid, maxIndex=maxIndex, rc=esmf_rc)
        if (esmf_rc /= ESMF_SUCCESS) then
            local_rc = ESMFIOW_ERROR
            deallocate(maxIndex)
            if (present(rc)) rc = local_rc
            return
        end if

        ! Ensure we're in define mode
        nc_rc = nf90_redef(file_handle)
        ! Don't error if already in define mode

        ! Create or verify coordinate dimensions exist
        nc_rc = nf90_inq_dimid(file_handle, "x", x_dimid)
        if (nc_rc /= NF90_NOERR) then
            nc_rc = nf90_def_dim(file_handle, "x", maxIndex(1), x_dimid)
            if (nc_rc /= NF90_NOERR) then
                local_rc = ESMFIOW_ERROR
                deallocate(maxIndex)
                if (present(rc)) rc = local_rc
                return
            end if
        end if

        nc_rc = nf90_inq_dimid(file_handle, "y", y_dimid)
        if (nc_rc /= NF90_NOERR) then
            nc_rc = nf90_def_dim(file_handle, "y", maxIndex(2), y_dimid)
            if (nc_rc /= NF90_NOERR) then
                local_rc = ESMFIOW_ERROR
                deallocate(maxIndex)
                if (present(rc)) rc = local_rc
                return
            end if
        end if

        ! Create 1D coordinate variables for x and y
        nc_rc = nf90_def_var(file_handle, "x", NF90_DOUBLE, [x_dimid], x_varid)
        if (nc_rc /= NF90_NOERR .and. nc_rc /= NF90_ENAMEINUSE) then
            local_rc = ESMFIOW_ERROR
            deallocate(maxIndex)
            if (present(rc)) rc = local_rc
            return
        end if

        nc_rc = nf90_def_var(file_handle, "y", NF90_DOUBLE, [y_dimid], y_varid)
        if (nc_rc /= NF90_NOERR .and. nc_rc /= NF90_ENAMEINUSE) then
            local_rc = ESMFIOW_ERROR
            deallocate(maxIndex)
            if (present(rc)) rc = local_rc
            return
        end if

        ! Add CF-compliant attributes for 1D coordinate variables
        if (nc_rc /= NF90_ENAMEINUSE) then
            nc_rc = nf90_put_att(file_handle, x_varid, "standard_name", "projection_x_coordinate")
            nc_rc = nf90_put_att(file_handle, x_varid, "long_name", "x coordinate")
            nc_rc = nf90_put_att(file_handle, x_varid, "axis", "X")

            nc_rc = nf90_put_att(file_handle, y_varid, "standard_name", "projection_y_coordinate")
            nc_rc = nf90_put_att(file_handle, y_varid, "long_name", "y coordinate")
            nc_rc = nf90_put_att(file_handle, y_varid, "axis", "Y")
        end if

        ! End define mode
        nc_rc = nf90_enddef(file_handle)
        if (nc_rc /= NF90_NOERR .and. nc_rc /= NF90_ENOTINDEFINE) then
            local_rc = ESMFIOW_ERROR
            deallocate(maxIndex)
            if (present(rc)) rc = local_rc
            return
        end if

        ! Get coordinate arrays from ESMF grid if coordinates exist
        call ESMF_GridGetCoord(grid, coordDim=1, farrayPtr=lon_ptr, rc=esmf_rc)
        if (esmf_rc == ESMF_SUCCESS) then
            call ESMF_GridGetCoord(grid, coordDim=2, farrayPtr=lat_ptr, rc=esmf_rc)
            if (esmf_rc == ESMF_SUCCESS) then
                ! Extract 1D coordinate arrays from 2D data
                allocate(x_coord(maxIndex(1)), y_coord(maxIndex(2)))

                ! For structured grids, extract coordinate values
                do i = 1, maxIndex(1)
                    x_coord(i) = lon_ptr(i, 1)  ! First row
                end do

                do j = 1, maxIndex(2)
                    y_coord(j) = lat_ptr(1, j)  ! First column
                end do

                ! Write coordinate data
                nc_rc = nf90_put_var(file_handle, x_varid, x_coord)
                if (nc_rc /= NF90_NOERR) then
                    local_rc = ESMFIOW_ERROR
                end if

                nc_rc = nf90_put_var(file_handle, y_varid, y_coord)
                if (nc_rc /= NF90_NOERR) then
                    local_rc = ESMFIOW_ERROR
                end if

                deallocate(x_coord, y_coord)
            end if
        else
            ! Grid doesn't have coordinates yet - create default index-based coordinates
            allocate(x_coord(maxIndex(1)), y_coord(maxIndex(2)))

            do i = 1, maxIndex(1)
                x_coord(i) = real(i-1, ESMF_KIND_R8)
            end do

            do j = 1, maxIndex(2)
                y_coord(j) = real(j-1, ESMF_KIND_R8)
            end do

            nc_rc = nf90_put_var(file_handle, x_varid, x_coord)
            nc_rc = nf90_put_var(file_handle, y_varid, y_coord)

            deallocate(x_coord, y_coord)
        end if

        deallocate(maxIndex)
        call ESMF_LogWrite("Successfully added grid coordinates to NetCDF", &
                          ESMF_LOGMSG_INFO, rc=esmf_rc)

        if (present(rc)) rc = local_rc

    end subroutine esmfiow_add_grid_coordinates

end module esmfiow_grid
!> @} End of esmfiow_grid group

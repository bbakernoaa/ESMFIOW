!> @file esmfiow.f90
!> @brief ESMF I/O Wrapper Library Main Module
!>
!> This is the main module of the ESMFIOW library which provides a high-level interface
!> for ESMF I/O operations with NetCDF4 compression and CF convention support. It serves
!> as an integration point for all the library components and provides a simplified API
!> for client applications.
!>
!> @author ESMFIOW Development Team
!> @date June 2025
!> @version 1.0.0
!> @defgroup esmfiow Main Module
!> @{

module esmfiow
    use esmfiow_constants
    use esmfiow_cf_utils
    use esmfiow_io
    use esmfiow_grid
    use esmfiow_field
    implicit none

    private

    ! Re-export constants
    public :: ESMFIOW_SUCCESS, ESMFIOW_ERROR, ESMFIOW_WARNING
    public :: ESMFIOW_MAX_STRING_LEN, ESMFIOW_MAX_NAME_LEN
    public :: ESMFIOW_REAL_KIND, ESMFIOW_INT_KIND

    ! Re-export CF utilities
    public :: cf_add_global_attributes, cf_add_coordinate_attributes
    public :: cf_add_variable_attributes, cf_validate_units
    public :: cf_create_time_coordinate, cf_add_bounds_variable

    ! Re-export I/O operations
    public :: esmfiow_file_create, esmfiow_file_open, esmfiow_file_close
    public :: esmfiow_write_field, esmfiow_read_field
    public :: esmfiow_file_handle

    ! Re-export grid operations
    public :: esmfiow_grid_to_netcdf, esmfiow_grid_from_netcdf
    public :: esmfiow_add_grid_coordinates

    ! Re-export field operations
    public :: esmfiow_field_create_from_netcdf, esmfiow_field_write_to_netcdf
    public :: esmfiow_field_add_metadata

    ! Library-level operations
    public :: esmfiow_initialize, esmfiow_finalize, esmfiow_get_version


contains

    !> Initialize the ESMFIOW library
    !>
    !> This subroutine initializes ESMF and sets up the environment
    !> for I/O operations with CF convention support.
    !>
    !> @param[out] rc Return code (0 = success)
    subroutine esmfiow_initialize(rc)
        integer, intent(out), optional :: rc

        integer :: local_rc

        local_rc = ESMFIOW_SUCCESS

        ! Initialize ESMF if not already done
        ! Call ESMF_Initialize() here in real implementation

        if (present(rc)) rc = local_rc

    end subroutine esmfiow_initialize

    !> Finalize the ESMFIOW library
    !>
    !> This subroutine finalizes ESMF and cleans up resources.
    !>
    !> @param[out] rc Return code (0 = success)
    subroutine esmfiow_finalize(rc)
        integer, intent(out), optional :: rc

        integer :: local_rc

        local_rc = ESMFIOW_SUCCESS

        ! Finalize ESMF if appropriate
        ! Call ESMF_Finalize() here in real implementation

        if (present(rc)) rc = local_rc

    end subroutine esmfiow_finalize

    !> Get library version information
    !>
    !> @param[out] version_string Version string
    !> @param[out] rc Return code (0 = success)
    subroutine esmfiow_get_version(version_string, rc)
        character(len=*), intent(out) :: version_string
        integer, intent(out), optional :: rc

        integer :: local_rc

        local_rc = ESMFIOW_SUCCESS
        version_string = "ESMFIOW v1.0.0"

        if (present(rc)) rc = local_rc

    end subroutine esmfiow_get_version

end module esmfiow
!> @} End of esmfiow group

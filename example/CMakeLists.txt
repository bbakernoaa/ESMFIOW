# Example programs for ESMFIOW library

# Basic I/O example
add_executable(esmfiow_example example.f90)
target_link_libraries(esmfiow_example PRIVATE esmfiow)

# CF convention example
add_executable(cf_example cf_example.f90)
target_link_libraries(cf_example PRIVATE esmfiow)

# Grid I/O example
add_executable(grid_example grid_example.f90)
target_link_libraries(grid_example PRIVATE esmfiow)

# Parallel compression benchmark example
add_executable(parallel_compression_example parallel_compression_example.f90)
target_link_libraries(parallel_compression_example PRIVATE esmfiow)

# Install examples
install(TARGETS esmfiow_example cf_example grid_example parallel_compression_example
    RUNTIME DESTINATION bin/examples
)

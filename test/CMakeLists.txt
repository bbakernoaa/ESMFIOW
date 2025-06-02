# Tests for ESMFIOW library

# Test helper function
function(add_esmfiow_test test_name)
    add_executable(${test_name} ${test_name}.f90)
    target_link_libraries(${test_name} PRIVATE esmfiow)
    add_test(NAME ${test_name} COMMAND ${test_name})
    set_tests_properties(${test_name} PROPERTIES
        TIMEOUT 30
        LABELS "unit"
    )
endfunction()

# Unit tests
add_esmfiow_test(test_esmfiow)
add_esmfiow_test(test_cf_utils)
add_esmfiow_test(test_io)
add_esmfiow_test(test_grid)
add_esmfiow_test(test_field)

# Parallel I/O test (using MPI)
add_executable(test_parallel_io test_parallel_io.f90)
target_link_libraries(test_parallel_io PRIVATE esmfiow)
add_test(NAME test_parallel_io
         COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} 4 ${MPIEXEC_PREFLAGS} $<TARGET_FILE:test_parallel_io> ${MPIEXEC_POSTFLAGS})
set_tests_properties(test_parallel_io PROPERTIES
    TIMEOUT 60
    LABELS "parallel;mpi;integration"
    PROCESSORS 4
)

# Integration tests
add_executable(integration_test integration_test.f90)
target_link_libraries(integration_test PRIVATE esmfiow)
add_test(NAME integration_test COMMAND integration_test)
set_tests_properties(integration_test PROPERTIES
    TIMEOUT 60
    LABELS "integration"
)

# Install test executables (optional)
install(TARGETS test_esmfiow test_cf_utils test_io test_grid test_field integration_test
    RUNTIME DESTINATION bin/tests
    OPTIONAL
)

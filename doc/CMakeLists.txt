# Documentation configuration

# Find Doxygen package
find_package(Doxygen REQUIRED)

# Check if Doxygen found
if(NOT DOXYGEN_FOUND)
    message(FATAL_ERROR "Doxygen is required to build the documentation")
endif()

# Configure Doxygen
set(DOXYGEN_INPUT_DIR ${CMAKE_SOURCE_DIR})
set(DOXYGEN_OUTPUT_DIR ${CMAKE_CURRENT_BINARY_DIR}/doxygen)
set(DOXYGEN_INDEX_FILE ${DOXYGEN_OUTPUT_DIR}/html/index.html)
set(DOXYFILE_IN ${CMAKE_CURRENT_SOURCE_DIR}/Doxyfile)
set(DOXYFILE_OUT ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile)

# Set Doxyfile variables
configure_file(${DOXYFILE_IN} ${DOXYFILE_OUT} @ONLY)

# Make sure the Doxygen Awesome theme files are properly set up
file(GLOB DOXYGEN_AWESOME_FILES
    ${CMAKE_CURRENT_SOURCE_DIR}/doxygen-awesome-theme/*.css
    ${CMAKE_CURRENT_SOURCE_DIR}/doxygen-awesome-theme/*.js
)

# Create a directory for Doxygen output
file(MAKE_DIRECTORY ${DOXYGEN_OUTPUT_DIR})

# Copy Doxygen Awesome theme files to output directory
file(MAKE_DIRECTORY ${DOXYGEN_OUTPUT_DIR}/doxygen-awesome-theme)
foreach(FILE ${DOXYGEN_AWESOME_FILES})
    get_filename_component(FILENAME ${FILE} NAME)
    configure_file(
        ${FILE}
        ${DOXYGEN_OUTPUT_DIR}/doxygen-awesome-theme/${FILENAME}
        COPYONLY
    )
endforeach()

# Add target to build documentation
add_custom_command(
    OUTPUT ${DOXYGEN_INDEX_FILE}
    COMMAND ${DOXYGEN_EXECUTABLE} ${DOXYFILE_OUT}
    MAIN_DEPENDENCY ${DOXYFILE_OUT} ${DOXYFILE_IN}
    COMMENT "Generating documentation with Doxygen"
    VERBATIM
)

# Add the custom target for generating documentation
add_custom_target(docs ALL DEPENDS ${DOXYGEN_INDEX_FILE})

# Install documentation
install(
    DIRECTORY ${DOXYGEN_OUTPUT_DIR}/html
    DESTINATION share/doc/esmfiow
    OPTIONAL
    COMPONENT documentation
)

# Add a message to inform the user about documentation location
add_custom_command(
    TARGET docs
    POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E echo "Doxygen documentation built at ${DOXYGEN_INDEX_FILE}"
    VERBATIM
)

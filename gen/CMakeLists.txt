set(MODULE gen)

set(GEN_HEADERS
    libmscore/types-gen
    libmscore/tremolo-gen
    libmscore/property-gen
    libmscore/style-gen
    )

foreach(genfile ${GEN_HEADERS})
    add_custom_command(
        OUTPUT ${PROJECT_BINARY_DIR}/${genfile}.h
        COMMAND
        COMMAND guile -q -l ${PROJECT_SOURCE_DIR}/gen/gen.scm --
                            ${PROJECT_SOURCE_DIR}/${genfile}.h.scm
                            ${PROJECT_BINARY_DIR}/${genfile}.h
        DEPENDS ${PROJECT_SOURCE_DIR}/${genfile}.h.scm
        COMMENT "Generating ${genfile} from ${genfile}.h.scm"
        )
    list(APPEND generated_headers ${PROJECT_BINARY_DIR}/${genfile}.h)
endforeach()

add_custom_target(generated_sources DEPENDS ${generated_headers})

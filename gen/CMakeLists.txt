set(MODULE gen)

add_custom_command(
    OUTPUT ${PROJECT_BINARY_DIR}/libmscore/types-gen.h
    COMMAND guile -q -l ${PROJECT_SOURCE_DIR}/gen/gen.scm --
            ${PROJECT_SOURCE_DIR}/libmscore/types-gen.h.scm
            ${PROJECT_BINARY_DIR}/libmscore/types-gen.h
    DEPENDS ${PROJECT_SOURCE_DIR}/libmscore/types-gen.h.scm
    COMMENT "Generating libmscore/types.h from libmscore/types-gen.h.scm"
)
add_custom_command(
    OUTPUT ${PROJECT_BINARY_DIR}/libmscore/tremolo-gen.h
    COMMAND guile -q -l ${PROJECT_SOURCE_DIR}/gen/gen.scm --
            ${PROJECT_SOURCE_DIR}/libmscore/tremolo-gen.h.scm
            ${PROJECT_BINARY_DIR}/libmscore/tremolo-gen.h
    DEPENDS ${PROJECT_SOURCE_DIR}/libmscore/tremolo-gen.h.scm
    COMMENT "Generating libmscore/tremolo-gen.h from libmscore/tremolo-gen.h.scm"
)
add_custom_command(
    OUTPUT ${PROJECT_BINARY_DIR}/libmscore/property-gen.h
    COMMAND guile -q -l ${PROJECT_SOURCE_DIR}/gen/gen.scm --
            ${PROJECT_SOURCE_DIR}/libmscore/property-gen.h.scm
            ${PROJECT_BINARY_DIR}/libmscore/property-gen.h
    DEPENDS ${PROJECT_SOURCE_DIR}/libmscore/property-gen.h.scm
    COMMENT "Generating libmscore/property-gen.h from libmscore/property-gen.h.scm"
)

add_custom_target(generated_sources
  DEPENDS ${PROJECT_BINARY_DIR}/libmscore/types-gen.h
          ${PROJECT_BINARY_DIR}/libmscore/tremolo-gen.h
          ${PROJECT_BINARY_DIR}/libmscore/property-gen.h)

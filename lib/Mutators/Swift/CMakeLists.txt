set (SOURCES
  SwiftLogicalAndToOr.cpp)

add_library(mull-swift-mutators ${SOURCES})
target_link_libraries(mull-swift-mutators irm)
target_include_directories(mull-swift-mutators PRIVATE ${MULL_INCLUDE_DIRS})
target_include_directories(mull-swift-mutators SYSTEM PRIVATE ${THIRD_PARTY_INCLUDE_DIRS})

# TODO: replace with target_compile_options and target_link_options
set_target_properties(mull-swift-mutators PROPERTIES
  COMPILE_FLAGS ${MULL_CXX_FLAGS}
  LINK_FLAGS ${MULL_LINK_FLAGS}
  )

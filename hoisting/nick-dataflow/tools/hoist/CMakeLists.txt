set(SOURCES
  handler.cpp
  hoist.cpp
  taintAnalysis.cpp
)

add_executable(hoist_condition_list
  handler.cpp
  hoist.cpp
  taintAnalysis.cpp
)

llvm_map_components_to_libnames(REQ_LLVM_LIBRARIES ${LLVM_TARGETS_TO_BUILD}
        asmparser core linker bitreader irreader ipo scalaropts
        analysis support
)

target_link_libraries( hoist_condition_list LLVMSvf LLVMCudd ${REQ_LLVM_LIBRARIES})

# Platform dependencies.
if( WIN32 )
  find_library(SHLWAPI_LIBRARY shlwapi)
  target_link_libraries(hoist_condition_list
    ${SHLWAPI_LIBRARY}
  )
else()
  find_package(Threads REQUIRED)
  find_package(Curses REQUIRED)
  target_link_libraries(hoist_condition_list
    ${CMAKE_THREAD_LIBS_INIT}
    ${CMAKE_DL_LIBS}
    ${CURSES_LIBRARIES}
  )
endif()


set_target_properties(hoist_condition_list
                      PROPERTIES
                      LINKER_LANGUAGE CXX
                      PREFIX ""
install(TARGETS hoist
  RUNTIME DESTINATION bin)
)

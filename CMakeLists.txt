cmake_minimum_required(VERSION 2.8.8)
project(vplan-qml)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${CMAKE_SOURCE_DIR}/cmake)
include(EnableCXX11)

set(CMAKE_AUTOMOC ON)
set(CMAKE_INCLUDE_CURRENT_DIR ON)
find_package(Qt5 COMPONENTS Quick Qml REQUIRED)

execute_process(COMMAND ghc --print-libdir OUTPUT_VARIABLE GHC_LIBDIR OUTPUT_STRIP_TRAILING_WHITESPACE)

file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/gen/include)
file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/gen/objects)

set(RESOURCES 
  ${CMAKE_SOURCE_DIR}/resources.rsc
)

set(CXX_SOURCES
  ${CMAKE_SOURCE_DIR}/cpp/main.cpp
  ${CMAKE_SOURCE_DIR}/cpp/ScheduleModel.cpp
)

set(HS_PACKAGES 
  base
  lens
  vplan
)

set(HS_FLAGS
)

set(HS_MODULES
  Schedule
)

set(HS_STUBS
)

set(INCLUDES
  ${CMAKE_BINARY_DIR}/gen/include
  cpp
  ${GHC_LIBDIR}/include
)

set(OBJECTS
)

foreach(HS_PACKAGE ${HS_PACKAGES})
  set(HS_FLAGS "-package ${HS_PACKAGE}" ${HS_FLAGS})
endforeach(HS_PACKAGE)

foreach(HS_MODULE ${HS_MODULES})
  set(OBJ_NAME ${CMAKE_BINARY_DIR}/gen/objects/${HS_MODULE}.o)
  set(STUB_NAME ${CMAKE_BINARY_DIR}/gen/include/${HS_MODULE}_stub.h)
  add_custom_command(
    OUTPUT ${OBJ_NAME} ${STUB_NAME}
    COMMAND ghc -c ${HS_FLAGS} ${CMAKE_SOURCE_DIR}/hs/${HS_MODULE}.hs -stubdir ${CMAKE_BINARY_DIR}/gen/include -o ${OBJ_NAME}
    DEPENDS ${CMAKE_SOURCE_DIR}/hs/${HS_MODULE}.hs
  )
  set(OBJECTS ${OBJECTS} ${OBJ_NAME})
  set(HS_STUBS ${HS_STUBS} ${STUB_NAME})
endforeach(HS_MODULE)

qt5_add_resources(RESOURCE_SOURCES ${RESOURCES} OPTIONS)
include_directories(${INCLUDES})
add_executable(vplan-qml ${CXX_SOURCES} ${RESOURCE_SOURCES} ${HS_STUBS})
set(CMAKE_CXX
target_link_libraries(vplan-qml ${OBJECTS})
qt5_use_modules(vplan-qml Quick Qml)
cmake_minimum_required(VERSION 3.0.0)
project(Serk_Lang C CXX)
file(GLOB_RECURSE SOURCE_FILES CONFIGURE_DEPENDS
	${CMAKE_SOURCE_DIR}/src/*.cpp
	${CMAKE_SOURCE_DIR}/src/*.c

)
file(GLOB_RECURSE HEADER_FILES CONFIGURE_DEPENDS
	${CMAKE_SOURCE_DIR}/src/*.h
	${CMAKE_SOURCE_DIR}/src/*.hpp
)
set(LIBS LLVMX86Disassembler LLVMX86AsmParser LLVMX86CodeGen LLVMCFGuard LLVMGlobalISel LLVMX86Desc LLVMX86Info LLVMMCDisassembler LLVMSelectionDAG LLVMAsmPrinter LLVMDebugInfoMSF LLVMDebugInfoDWARF LLVMCodeGen LLVMOrcJIT LLVMPasses LLVMObjCARCOpts LLVMCoroutines LLVMipo LLVMInstrumentation LLVMVectorize LLVMScalarOpts LLVMLinker LLVMIRReader LLVMAsmParser LLVMInstCombine LLVMFrontendOpenMP LLVMBitWriter LLVMAggressiveInstCombine LLVMTransformUtils LLVMJITLink LLVMExecutionEngine LLVMTarget LLVMAnalysis LLVMProfileData LLVMRuntimeDyld LLVMOrcTargetProcess LLVMOrcShared LLVMObject LLVMTextAPI LLVMMCParser LLVMBitReader LLVMMC LLVMDebugInfoCodeView LLVMCore LLVMRemarks LLVMBitstreamReader LLVMBinaryFormat LLVMSupport LLVMDemangle)
add_executable(${PROJECT_NAME} ${HEADER_FILES} ${SOURCE_FILES})

set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} /MT")
set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "${CMAKE_CXX_FLAGS_RELWITHDEBINFO} /MT")
#set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} /MTd")


target_include_directories(${PROJECT_NAME} 
	PUBLIC ${CMAKE_SOURCE_DIR}/include
	PUBLIC ${CMAKE_SOURCE_DIR}/src
)
target_compile_features(${PROJECT_NAME}
    PRIVATE
        cxx_std_17
    )
# if(CMAKE_BUILD_TYPE MATCHES "Release")
# target_link_directories(${PROJECT_NAME} PRIVATE ${CMAKE_SOURCE_DIR}/lib_release)
# endif()
# if(CMAKE_BUILD_TYPE MATCHES "Debug")
target_link_directories(${PROJECT_NAME} PRIVATE ${CMAKE_SOURCE_DIR}/lib_debug)
# endif()
target_link_libraries(${PROJECT_NAME} LINK_PUBLIC ${LIBS})
set_target_properties(${PROJECT_NAME} PROPERTIES LINKER_LANGUAGE CXX)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

target_link_options(${PROJECT_NAME} PUBLIC "/DEBUG:FASTLINK")
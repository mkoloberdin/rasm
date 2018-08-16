cmake_minimum_required(VERSION 3.7)
project(rasm)

set(SOURCE_FILES
        rasm.c
        rasm.h
        exomizer.h
        lz4.h
        minilib.h
        zx7.h
)

add_executable(rasm ${SOURCE_FILES})

target_link_libraries(rasm m)

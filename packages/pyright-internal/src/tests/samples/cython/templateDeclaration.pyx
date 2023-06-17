# Templated Declarations
# This file should parse without errors

cdef:
    # Template return type
    set[int] f0(int) except +
    # Multiple Template args
    set[int, int] f1() except +
    # Nested Template
    set[set[int, int], int] f2(int) except +
    set[int, set[int, int]] f3(int) except +

    # Template arguments
    set[int, int] f4(set[int, int] p0) except +
    # Nested template arguments
    set[int, int] f5(set[int, int] p0, set[int, set[int, int]] p1) except +

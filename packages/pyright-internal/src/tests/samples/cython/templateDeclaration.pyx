# Templated Declarations
# This file should parse without errors

# Template return type
cdef set[int] f0(int) except +
# Multiple Template args
cdef set[int, int] f1() except +
# Nested Template
cdef set[set[int, int], int] f2(int) except +
cdef set[int, set[int, int]] f3(int) except +

# Template arguments
cdef set[int, int] f4(set[int, int] p0) except +
# Nested template arguments
cdef set[int, int] f5(set[int, int] p0, set[int, set[int, int]] p1) except +

# Chained Declarations
# This file should parse without errors

cdef v0 = 1, v1 = 1
cdef int v2, v3, v4
cdef int v5, v6 = 1
cdef int v7 = 1, v8 = 1

cdef int *v9, **v10
cdef int *v11 = 1, ***v12
cdef int **v13, ***v14 = 1
cdef int **v15 = 1, ***v16 = 1


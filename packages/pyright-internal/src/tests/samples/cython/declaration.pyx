# Declarations
# This file should parse without errors

cdef int v0 = 0
cdef int* v1
cdef int *v2, v3
cdef long v4
cdef double complex v5
cdef float complex v6
cdef const long int v7
cdef unsigned int v8
cdef signed int v9
cdef signed long int v10
cdef signed long long int v11

cdef int[:] m0
cdef int[::] m1
cdef int[::1] m2

cdef int a0[2]

cdef int (*c0)(int, int)

cdef (int*, int, (int**, int)) t0

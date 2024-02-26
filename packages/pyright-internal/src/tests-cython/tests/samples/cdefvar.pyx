cdef int var0 = 0
cdef const int** var1

DEF SIZE = 1
# Arrays
cdef int** avar0[1]
cdef int avar1[SIZE]
cdef int** avar2[1][2]

cdef int[0] avar3
cdef int[SIZE][0] avar4

# Buffer
cdef object[int] bvar0
cdef object[int, ndims=1] bvar1

# Views
cdef int[:] vvar0
cdef int[::1] vvar1
cdef int[::SIZE] vvar2
cdef int[:, :] vvar3
cdef int[:, ::1] vvar4
cdef int[::1, :] vvar5

# Chained
cdef int** cvar0, cvar1 = 0
cdef const int** cvar2, **cvar3 = NULL
cdef int cvar4 = 0, **cvar5
cdef int[:] cvar6, cvar7
cdef int[:] cvar8, **cvar9 = []

# C Tuple
cdef (double, double) tvar0
cdef (double, double******) tvar1
cdef (double, double******, (double*, double**)) tvar2
cdef (double, double*) tvar3, tvar4

# Callback
cdef int** (***cbvar0)(int)

# AddressOf
cdef int addrvar0 = 0
cdef int* addrvar1 = &addrvar0

# Cast
cdef int* castvar0 = NULL
cdef const int* castvar1 = <const int*>castvar0

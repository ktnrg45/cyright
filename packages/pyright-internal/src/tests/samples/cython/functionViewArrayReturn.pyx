# Function with view and array return types
# This file should parse without errors

# Callbacks Valid view types
# Array
cdef int[]**(*c0)(void*, void**)
# Sized array
cdef int[1](*c1)(void* p, void**) nogil
# View
cdef int[::1](*c2)(void*, void** p) nogil

# Function Valid View Types
# Array only if pointer
cdef int[1]* f0(void* p0, void** p1):
    pass
# View
cdef int[::1] f1(void* p0[1], void** p1) nogil:
    pass

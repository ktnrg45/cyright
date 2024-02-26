cdef func0(p0, const unsigned int **p1, unsigned int p2 = 1):
    return 1

cdef signed int func1(p0, unsigned int p1, unsigned int p2 = 1) nogil:
    return 1

cdef signed int** func2(p0, unsigned int p1, unsigned int p2 = 1):
    return NULL

cpdef signed int func3(p0, unsigned int p1, unsigned int p2 = 1) with gil:
    return 1

cdef int* func4(int p0):
    return &p0

cdef int* func5(short* p0):
    return <int*> p0
# Functions
# This file should parse without errors

def f0():
    pass

def f1(int p):
    pass

def f2(int[::1] p0, int[:] p1 not None):
    pass

cpdef f3(int p):
    pass

cpdef int[::1] f4(p):
    pass

cdef f5():
    pass

cdef int f6(p) nogil:
    pass

cdef int f7(int p):
    pass

cdef inline int f8(int p):
    pass

cdef signed long int *f9(int p):
    pass

cdef (int*, int, (int**, int)) f10((int*, int, (int**, int)) p):
    pass

cdef f11(int[::1] p0, int[:, :, :, :] p1, int p2[]) with gil:
    pass

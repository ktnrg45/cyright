# Suite
cdef nogil:
    cdef int **svar0
    int svar1
    int svar2 = 1
    (double, double***) svar3

# Extern
cdef extern from * namespace "" nogil:
    """int evar00;"""
    int** evar0
    cdef int*** evar1


cdef:
    enum enum0: num = 0, num1
    enum enum1:
        num2,
        num3


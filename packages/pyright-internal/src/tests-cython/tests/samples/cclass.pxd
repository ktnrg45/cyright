
cdef class Base():
    cdef int* bvar

    @staticmethod
    cdef static_func()

    cdef int func(self)
    cdef inline ifunc(self):
        return 1

cdef class Cls(Base):
    cdef func2(self)
    cdef func3(self)
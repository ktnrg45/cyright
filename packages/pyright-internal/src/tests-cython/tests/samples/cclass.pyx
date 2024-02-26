
# distutils: language = c++
cdef class Base():
    @staticmethod
    cdef static_func():...

    def __init__(self):
        self.bvar
        self.ifunc()

    cdef int func(self):
        return 0

cdef class Cls(Base):
    cdef func2(self): ...
    cdef int func(self):

        return 1



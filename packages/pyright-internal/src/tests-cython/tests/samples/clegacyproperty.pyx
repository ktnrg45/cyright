
cdef class Cls:
    def __init__(self):
        self.var = 1
    property prop:
        def __get__(self): return self.var
        def __set__(self, value: int): self.var = value

    @property
    def prop2(self):
        return self.var
    @prop2.setter
    def prop2(self, value: int):
        self.var = value

c = Cls()
c.prop
c.prop = 1
c.prop2 = 1


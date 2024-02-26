# distutils: language = c++

DEF NUM = 1

cdef enum enum0:
    var0,
    var1 = NUM + 1,
    var2, var3

cdef enum enum1:
    var4 = 0,
    var5,

cdef enum enum2: var6, var7=1, var8

cpdef enum enum3:
    var14,
    var15

cpdef enum class enum4:
    var16,
    var17=1,

cpdef enum class enum5(signed int): var18

cdef enum:
    var9, var10,
    var11=1

cdef:
    enum:
        var12,
        var13

cdef enum3 evar0
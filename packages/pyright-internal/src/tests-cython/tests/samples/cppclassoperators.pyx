from cython.operator import dereference, postdecrement, postincrement, predecrement, preincrement, comma

cdef extern from *:
    cdef cppclass Cpp[T]:
        Cpp()
        T** operator,(int)
        T& operator*()
        T* operator++(int)
        T** operator++()
        T* operator--(int)
        T** operator--()

        int* operator+(int)
        int* operator-(int)
        double* operator*(float)

cdef Cpp[int] obj = Cpp[int]()
deref = dereference(obj)
com = comma(obj, 1)
posti = postincrement(obj)
prei = preincrement(obj)
postd = postdecrement(obj)
prei = predecrement(obj)

add = obj + 1
sub = obj - 1
mult = obj * 1.0
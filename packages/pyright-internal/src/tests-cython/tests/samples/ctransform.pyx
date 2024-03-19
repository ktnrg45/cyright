"""Auto Transform basic C types to Python types."""


# Getting attributes of basic c types should transform to Python types automatically

cdef char c_val0
py_val0 = c_val0.bit_count()

cdef short c_val1
py_val1 = c_val1.bit_count()

cdef long c_val2
py_val2 = c_val2.bit_count()

cdef double c_val3
py_val3 = c_val3.hex()

cdef char* c_val4 = b''
py_val4 = c_val4.decode()

cdef struct c_struct:
    double f0
cdef c_struct c_val5
py_val5 = c_val5.get('')
c_structf0 = c_val5.f0

cdef char[1] c_val6
py_val6 = c_val6.count(0)

cdef double[1][1] c_val7
py_val7 = c_val7[0]
print(py_val7)
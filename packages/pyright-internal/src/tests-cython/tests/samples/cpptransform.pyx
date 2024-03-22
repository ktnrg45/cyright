# distutils: language = c++
"""Sample that tests implicit type conversions from certain Cpp classes to Python types."""
from libcpp.vector cimport vector
from libcpp.string cimport string
from libcpp.pair cimport pair
from libcpp.map cimport map
from libcpp.unordered_map cimport unordered_map
from libcpp.set cimport set
from libcpp.list cimport list

cdef vector[int] v0
cdef string v1
cdef pair[int, char*] v2 = (1, b'')
cdef map[char*, int] v3 = {b'': 8}
cdef unordered_map[char*, int] v4 = {b'': 8}
cdef set[unsigned int] v5 = {1}
cdef list[int] v6 = [1]

p0 = v0
p1 = v1
p2 = v2
p3 = v3
p4 = v4
p5 = v5
p6 = v6

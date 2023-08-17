# Extern declarations
# This file should parse without errors

cdef extern int spam_counter

cdef extern signed long int spam_counter

cdef extern void order_spam(int tons)

cdef extern signed long int order_ham(int tons)

cdef extern void order_more_spam(int tons) nogil

cdef extern signed long int order_more_ham(int tons) nogil

cdef extern from "spam.h":
    pass

cdef extern from *:
    pass

cdef extern from "spam.h" namespace "food":
    pass

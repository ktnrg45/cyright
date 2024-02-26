ctypedef double type0
ctypedef double** type1
ctypedef const int** type2
ctypedef readonly const int type3
ctypedef public const int type4
ctypedef unsigned type5

# long types
ctypedef long ltype0
ctypedef long long ltype1
ctypedef long long int ltype2
ctypedef long short ltype3
ctypedef long int ltype4
ctypedef long double ltype5
ctypedef long complex ltype6
ctypedef const long long ltype7
ctypedef const long long* ltype8
ctypedef const unsigned long** ltype9
ctypedef signed long long int ltype10

# Compound types
ctypedef double complex ctype0
ctypedef float complex ctype1
ctypedef long double complex ctype2
ctypedef short int ctype3

# Soft keywords
ctypedef const long int const
ctypedef unsigned long int unsigned
ctypedef const int nogil
ctypedef volatile int volatile;

# Misc
ctypedef (double**, double**, (double*, const double**)) mtype0
ctypedef short* (*mtype1)(const int**)

cdef struct struct0:
    int f0
    signed int **f1
    int[4] f2

cdef packed struct struct1:
    int f0
    signed int **f1
    int[4] f2

cdef union union0:
    int f0
    signed int **f1
    int[4] f2

cdef fused fused0:
    int
    signed int
    int[4]

cdef:
    cdef struct struct00:
        int f0
        signed int **f1
        int[4] f2
    packed struct struct01:
        int f0
        signed int **f1
        int[4] f2
        int (*f3)(int**)
        int f4, f5
    cdef union union00:
        int f0
        signed int **f1
        int[4] f2

    cdef fused fused00:
        int
        signed int
        int[4]

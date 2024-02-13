"""Special Cython typings."""
import dataclasses
import typing as _typing
from .operator cimport address

__version__: str
compiled: bool

class _CythonType: ...
class typedef(_CythonType):
    def __init__(self, type: type, name: str | None = None) -> None: ...

class _FusedType(_CythonType): ...

def fused_type(*args) -> typedef | _FusedType: ...

# Types
basestring = str
unicode = str
NULL = NULL # noqa: type: ignore

void = void
bint = bint
char = char
ctypedef signed char schar
ctypedef unsigned char uchar
short = short
ctypedef signed short sshort
ctypedef unsigned short ushort
int = int
ctypedef signed int sint
ctypedef unsigned int uint
typedef(long, "long")
ctypedef signed long slong
ctypedef unsigned long ulong
ctypedef long long longlong
ctypedef signed long long slonglong
ctypedef unsigned long long ulonglong
float = float
double = double
ctypedef long double longdouble
ctypedef float complex floatcomplex
ctypedef double complex doublecomplex
ctypedef long double complex longdoublecomplex
size_t = size_t
Py_ssize_t = Py_ssize_t
Py_hash_t = Py_hash_t
Py_UCS4 = Py_UCS4
Py_tss_t = Py_tss_t

# Shorthand Pointer Types
# Can be prefixed with 'p_'
# Unlimited 'p' prefixes can be used but they are synthetic
# i.e. 'ppvoid' is equivalent to 'void**' and 'pp_void'
ctypedef void* pvoid
ctypedef bint* pbint
ctypedef char* pchar
ctypedef signed char* pschar
ctypedef unsigned char* puchar
ctypedef short* pshort
ctypedef unsigned short* pushort
ctypedef int* pint
ctypedef unsigned int* puint
ctypedef long* plong
ctypedef unsigned long* pulong
ctypedef long long* plonglong
ctypedef unsigned long long* pulonglong
ctypedef float* pfloat
ctypedef double* pdouble
ctypedef long double* plongdouble
ctypedef float complex* pfloatcomplex
ctypedef double complex* pdoublecomplex
ctypedef long double complex* plongdoublecomplex
ctypedef size_t* psize_t
ctypedef Py_ssize_t* pPy_ssize_t
ctypedef Py_hash_t* pPy_hash_t
ctypedef Py_UCS4* pPy_UCS4
ctypedef Py_tss_t* pPy_tss_t

ctypedef fused integral:
    void
    bint
    char
    schar
    uchar
    short
    sshort
    ushort
    int
    sint
    uint
    long
    slong
    ulong
    slonglong
    ulonglong

ctypedef fused floating:
    float
    double
    longdouble
    floatcomplex
    doublecomplex
    longdoublecomplex

ctypedef fused numeric:
    integral
    floating

# Directives

class _DirectiveDecoratorAndManager:
    """Directive decorator and context manager."""
    def __call__(self, x: _typing.Callable | type): ...
    def __enter__(self) -> None: ...
    def __exit__(self, exc_type, exc_value, traceback) -> None: ...

class _DirectiveDecoratorAndManagerBool(_DirectiveDecoratorAndManager):
    def __init__(self, __value: bool) ...

def final(x: _typing.Callable | type):
    """Directive decorator."""

def internal(x: _typing.Callable | type):
    """Directive decorator."""

def type_version_tag(x: _typing.Callable | type):
    """Directive decorator."""

def no_gc_clear(x: _typing.Callable | type):
    """Directive decorator."""

def no_gc(x: _typing.Callable | type):
    """Directive decorator."""

def total_ordering(x: _typing.Callable | type):
    """Directive decorator."""

def locals(f: _typing.Callable, **arg_types):
    """Directive decorator."""

def inline(f: _typing.Callable, *args, **kwds):
    """Directive decorator."""

def compile(x):
    """Directive decorator."""

def cclass(cls: type):
    """Directive decorator."""

def ccall(f: _typing.Callable):
    """Directive decorator."""

def cfunc(f: _typing.Callable):
    """Directive decorator."""

class _nogil(_DirectiveDecoratorAndManager):
    def __init__(self __value: bool = None): ...
    def __enter__(self, __value: bool = None) -> None: ...

nogil =_nogil()
gil = _nogil()

returns = wraparound = boundscheck = initializedcheck = nonecheck = \
    embedsignature = cdivision = cdivision_warnings = \
    always_allows_keywords = profile = linetrace = infer_types = \
    unraisable_tracebacks = freelist = \
        _DirectiveDecoratorAndManagerBool

class exceptval(_DirectiveDecoratorAndManager):
    def __init__(self, __value = None, check = True): ...

class overflowcheck(_DirectiveDecoratorAndManagerBool):
    class fold(_DirectiveDecoratorAndManagerBool): ...

class _Optimization:
    class use_switch(_DirectiveDecoratorAndManagerBool): ...
    class unpack_method_calls(_DirectiveDecoratorAndManagerBool): ...
optimize = _Optimization()

def cdiv(a: _typing.SupportsInt, b: _typing.SupportsInt) -> _typing.SupportsInt: ...
def cmod(a: _typing.SupportsInt, b: _typing.SupportsInt) -> _typing.SupportsInt: ...
def sizeof(__arg) -> int: ...
def typeof(__arg) -> str: ...

_T = _typing.TypeVar("_T")
def cast(t: _T, __object: _typing.Any, typecheck=False) -> _T: ...
def declare(t: Any | None = None, value=..., **kwds): ...


from _typeshed import Incomplete
from typing import Any, Literal
import dataclasses

__version__: str
basestring = str

class _ArrayType:
    is_array: bool
    subtypes: list[str]
    dtype: Any
    ndim: int
    is_c_contig: bool
    is_f_contig: bool
    inner_contig: bool
    broadcasting: Any
    def __init__(self, dtype: Any, ndim: int, is_c_contig: bool = False, is_f_contig: bool = False, inner_contig: bool = False, broadcasting: Any | None = None) -> None: ...

def index_type(base_type, item): ...

compiled: bool
def _empty_decorator(x): ...
def locals(**arg_types): ...
def test_assert_path_exists(*paths): ...
def test_fail_if_path_exists(*paths): ...

class _EmptyDecoratorAndManager:
    def __call__(self, x): ...
    def __enter__(self) -> None: ...
    def __exit__(self, exc_type, exc_value, traceback) -> None: ...

class _Optimization: ...

cclass = _EmptyDecoratorAndManager()
ccall = _EmptyDecoratorAndManager()
cfunc = _EmptyDecoratorAndManager()

returns = lambda _: _EmptyDecoratorAndManager()
wraparound = lambda _: _EmptyDecoratorAndManager()
boundscheck = lambda _: _EmptyDecoratorAndManager()
initializedcheck = lambda _: _EmptyDecoratorAndManager()
nonecheck = lambda _: _EmptyDecoratorAndManager()
embedsignature = lambda _: _EmptyDecoratorAndManager()
cdivision = lambda _: _EmptyDecoratorAndManager()
cdivision_warnings = lambda _: _EmptyDecoratorAndManager()
always_allows_keywords = lambda _: _EmptyDecoratorAndManager()
profile = lambda _: _EmptyDecoratorAndManager()
linetrace = lambda _: _EmptyDecoratorAndManager()
infer_types = lambda _: _EmptyDecoratorAndManager()
unraisable_tracebacks = lambda _: _EmptyDecoratorAndManager()
freelist = lambda _: _EmptyDecoratorAndManager()

exceptval = lambda _=None, check=True: _EmptyDecoratorAndManager()

overflowcheck = lambda _: _EmptyDecoratorAndManager()
optimize = _Optimization()

overflowcheck.fold = lambda arg: _EmptyDecoratorAndManager()
optimize.use_switch = lambda arg: _EmptyDecoratorAndManager()
optimize.unpack_method_calls = lambda arg: _EmptyDecoratorAndManager()

final = _empty_decorator
internal = _empty_decorator
type_version_tag = _empty_decorator
no_gc_clear = _empty_decorator
no_gc = _empty_decorator
total_ordering = _empty_decorator

binding = lambda _: _empty_decorator

def inline(f, *args, **kwds): ...
def compile(f): ...
def cdiv(a, b): ...
def cmod(a, b): ...
def cast(t, *args, **kwargs): ...
def sizeof(arg): ...
def typeof(arg): ...
def address(arg): ...
def declare(t: Any | None = None, value=..., **kwds): ...

class _nogil:
    def __call__(self, x): ...
    def __enter__(self) -> None: ...
    def __exit__(self, exc_class, exc, tb): ...

nogil =_nogil()
gil = _nogil()

class CythonMetaType(type):
    def __getitem__(type, ix): ...

CythonTypeObject = CythonMetaType('CythonTypeObject', (object,), {})

class CythonType(CythonTypeObject): ...

class PointerType(CythonType):
    def __init__(self, value: PointerType | list | Literal[0] | None = None) -> None: ...
    def __getitem__(self, ix): ...
    def __setitem__(self, ix, value) -> None: ...
    def __eq__(self, value): ...

class ArrayType(PointerType): ...

class StructType(CythonType):
    def __init__(self, *posargs, **data) -> None: ...
    def __setattr__(self, key, value) -> None: ...

class UnionType(CythonType):
    def __init__(self, cast_from=..., **data) -> None: ...
    __dict__: dict
    def __setattr__(self, key, value) -> None: ...

def pointer(basetype): ...
def array(basetype, n): ...
def struct(**members): ...
def union(**members): ...

class typedef(CythonType):
    name: str
    def __init__(self, type, name: str | None = None) -> None: ...
    def __call__(self, *arg): ...
    __getitem__ = index_type

class _FusedType(CythonType):
    __getitem__ = index_type

def fused_type(*args) -> typedef | _FusedType: ...

py_int: typedef
py_long: typedef
py_float: typedef
py_complex: typedef

int_types: list[str]
float_types: list[str]
complex_types: list[str]
other_types: list[str]

def to_repr(x: str) -> str: ...

gs: dict[str, Any]
bint: typedef
void: typedef
Py_tss_t: typedef
NULL: None
integral: _FusedType
floating: _FusedType
numeric: _FusedType
type_ordering: list[typedef]

class CythonDotParallel:
    def parallel(self, num_threads: int | None = None): ...
    def prange(self, start: int = 0, stop: int | None = None, step: int = 1, nogil: bool = False, schedule: Literal['static', 'dynamic', 'guided', 'runtime'] | None = None, chunksize: int | None = None, num_threads: int | None = None): ...
    def threadid(self): ...

class CythonDotImportedFromElsewhere:
    __path__: list[str]
    __file__: str
    __name__: str
    __package__: str
    def __init__(self, module) -> None: ...
    def __getattr__(self, attr): ...

class CythonCImports:
    __path__: list[str]
    __file__: str
    __name__: str
    __package__: str
    def __init__(self, module) -> None: ...
    def __getattr__(self, item): ...


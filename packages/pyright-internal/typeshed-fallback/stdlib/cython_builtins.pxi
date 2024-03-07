"""Cython Built In Types. Only used in Cython files. All types are not instantiable with calls."""

from typing import TypeVar, Any, SupportsIndex, Iterator, overload

_T = TypeVar("_T")
_N = TypeVar("_N", bound=None)
_B = TypeVar("_B", bound=bool | int)
_I = TypeVar("_I", bound=bool | int | float)

unicode = str
basestring = str

class void(_I):
    """Cython Type. Represents void."""

class bint(_B):
    """Cython Type. Represents a boolean integer."""

class char(_B):
    """Cython Type. Represents a char integer."""

class short(_B):
    """Cython Type. Represents a short integer."""

class size_t(_B):
    """Cython Type. Represents size_t."""

class ssize_t(_B):
    """Cython Type. Represents ssize_t."""

class long(_I):
    """Cython Type. Represents a long integer."""

class complex(_I):
    """Cython Type. Represents a complex integer."""

class double(_I):
    """Cython Type. Represents a double."""

class ptrdiff_t(_I):
    """Cython Type. Represents the signed integer type of the result of subtracting two pointers."""

class Py_UNICODE(_T):
    """Cython Type. Represents a Py_UNICODE. Equivalent to wchar_t."""

class Py_UCS4(_T):
    """Cython Type. Represents a Py_UCS4. Single unicode character."""

class Py_hash_t(_T):
    """Cython Type. Represents a Py_hash_t. Hash of object. Size of Py_ssize_t."""

class Py_ssize_t(_T):
    """Cython Type. Represents a Py_ssize_t."""

class Py_tss_t(_T):
    """Cython Type. Represents a Py_tss_t. Thread state."""

def sizeof(__obj: Any) -> int:
    """Cython Function. Return size of object or variable."""

ctypedef struct Py_buffer:
    void* buf
    object obj # Py_object
    Py_ssize_t len
    Py_ssize_t itemsize
    bint readonly
    int ndim
    char* format
    Py_ssize_t* shape
    Py_ssize_t* strides
    Py_ssize_t* suboffsets
    Py_ssize_t[2] smalltable
    void* internal

ctypedef struct Py_complex:
    double real
    double imag

class __MemoryView__[ItemType, Base]():
    """Cython MemoryView. Wraps an object that supports the buffer protocol."""
    def __delitem__(self, __key: SupportsIndex): ...
    def __setitem__(self, __key: SupportsIndex, __value: ItemType): ...
    @overload
    def __getitem__(self, __key: slice) -> __MemoryView__[ItemType, Base]: ...
    def __getitem__(self, __key: SupportsIndex) -> ItemType: ...
    def __len__(self) -> int: ...
    def __contains__(self, __value: Any) -> bool:
        """Not actually defined."""
    def __iter__ (self) -> Iterator[ItemType]:
        """Not actually defined."""
    def count(self, __value: Any) -> int: ...
    def index(self, __value: ItemType, start: SupportsIndex = ..., stop: SupportsIndex = ...) -> int: ...
    def copy(self) -> __MemoryView__[ItemType, Base]:
        """Return a copy of view with C layout."""
    def copy_fortran(self) -> __MemoryView__[ItemType, Base]:
        """Return a copy of view with Fortran layout."""
    def is_c_contig(self) -> bool:
        """Return True if view layout is C contiguous."""
    def is_f_contig(self) -> bool:
        """Return True if view layout is Fortran contiguous."""
    @property
    def base(self) -> Base:
        """Return the base buffer object."""
    @property
    def itemsize(self) -> int:
        """Return the size of an item contained in buffer in bytes."""
    @property
    def nbytes(self) -> int:
        """Return the size of view in bytes."""
    @property
    def ndim(self) -> int:
        """Return the number of dimensions."""
    @property
    def shape(self) -> tuple[int, ...]:
        """Return the shape."""
    @property
    def strides(self) -> tuple[int, ...]:
        """Return the strides."""
    @property
    def suboffsets(self) -> tuple[int, ...]:
        """Return the suboffsets."""
    @property
    def T(self) -> __MemoryView__[ItemType, Base]:
        """Return transposed view."""

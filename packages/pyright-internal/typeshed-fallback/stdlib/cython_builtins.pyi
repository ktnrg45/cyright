"""Cython Built In Types. Only used in Cython files."""

from typing import TypeVar, Any, Callable

_T = TypeVar("_T")
_N = TypeVar("_N", bound=None)
_B = TypeVar("_B", bound=bool|int)
_I = TypeVar("_I", bound=bool|int|float)

__CYTHON_CALLABLE__ = Callable

class NULL(_N):
    """Cython Type. Represents null."""
    ...


class void(_N):
    """Cython Type. Represents void."""
    ...


class bint(_B):
    """Cython Type. Represents a boolean integer."""
    ...


class char(_B):
    """Cython Type. Represents a char integer."""
    ...


class short(_B):
    """Cython Type. Represents a short integer."""
    ...


class size_t(_B):
    """Cython Type. Represents size_t."""
    ...


class ssize_t(_B):
    """Cython Type. Represents ssize_t."""
    ...


class long(_I):
    """Cython Type. Represents a long integer."""
    ...


class complex(_I):
    """Cython Type. Represents a complex integer."""
    ...


class double(_I):
    """Cython Type. Represents a double."""
    ...


class struct(_T):
    """Cython Type. Represents a struct."""
    ...


class enum(_T):
    """Cython Type. Represents a enum."""
    ...


class union(_T):
    """Cython Type. Represents a union."""
    ...


class Py_UNICODE(_T):
    """Cython Type. Represents a Py_UNICODE. Equivalent to wchar_t."""
    ...


class Py_UCS4(_T):
    """Cython Type. Represents a Py_UCS4. Single unicode character."""
    ...


class Py_hash_t(_T):
    """Cython Type. Represents a Py_hash_t. Hash of object. Size of Py_ssize_t."""
    ...


class Py_ssize_t(_T):
    """Cython Type. Represents a Py_ssize_t."""
    ...


class Py_tss_t(_T):
    """Cython Type. Represents a Py_tss_t. Thread state."""
    ...


def sizeof(__obj: Any) -> int:
    """Cython Function. Return size of object or variable."""
    ...
"""Cython Built In Types. Only used in Cython files."""

from typing import TypeVar

_T = TypeVar("_T")
_N = TypeVar("_N", bound=None)
_B = TypeVar("_B", bound=bool|int)
_I = TypeVar("_I", bound=bool|int|float)


class NULL(_N):
    """Cython Type. Represents null."""


class void(_N):
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


class complex(_I):
    """Cython Type. Represents a complex integer."""


class double(_I):
    """Cython Type. Represents a double."""


class struct(_T):
    """Cython Type. Represents a struct."""


class enum(_T):
    """Cython Type. Represents a enum."""
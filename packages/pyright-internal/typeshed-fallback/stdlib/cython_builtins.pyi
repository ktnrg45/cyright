"""Cython Built In Types. Only used in Cython files."""

from typing import TypeVar

T = TypeVar("T")


class NULL(T):
    """Cython Type. Represents null."""


class void(T):
    """Cython Type. Represents void."""


class bint(T):
    """Cython Type. Represents a boolean integer."""


class char(T):
    """Cython Type. Represents a char integer."""


class short(T):
    """Cython Type. Represents a short integer."""


class complex(T):
    """Cython Type. Represents a complex integer."""


class double(T):
    """Cython Type. Represents a double."""


class size_t(T):
    """Cython Type. Represents size_t."""


class ssize_t(T):
    """Cython Type. Represents ssize_t."""


class struct(T):
    """Cython Type. Represents a struct."""


class enum(T):
    """Cython Type. Represents a enum."""
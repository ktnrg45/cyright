"""Special Cython typings."""
from typing import Any


def typeof(__obj: Any) -> str:
    """Cython Function. Return type of object as str."""
    ...


def address(__obj: Any) -> int:
    """Cython Function. Return address of object. Equivalent of '&obj'."""
    ...

"""Cython Operators."""
from typing import Any

def address(__arg) -> Any:
    """Cython Operator Function. Return address of object."""
    ...

def typeid(__obj: Any) -> Any:
    """Cython Operator Function. Return type id of object."""
    ...

def dereference (__obj: Any) -> Any:
    """Cython Operator Function. Perform dereference. Equivalent of '*obj'."""
    ...

def preincrement (__obj: Any) -> Any:
    """Cython Operator Function. Perform preincrement. Equivalent of '++obj'."""
    ...

def predecrement (__obj: Any) -> Any:
    """Cython Operator Function. Perform predecrement. Equivalent of '--obj'."""
    ...

def postincrement (__obj: Any) -> Any:
    """Cython Operator Function. Perform postincrement. Equivalent of 'obj++'."""
    ...

def postdecrement (__obj: Any) -> Any:
    """Cython Operator Function. Perform postdecrement. Equivalent of 'obj--'."""
    ...

def comma (__obj: Any, __other: Any) -> Any:
    """Cython Operator Function. Perform Comma Operation. Equivalent of 'obj, other'."""
    ...

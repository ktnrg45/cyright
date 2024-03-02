from typing import Literal, SupportsIndex, Any, Iterator, TypeVar, overload

class __ViewFlag: ...

generic = __ViewFlag()
strided = __ViewFlag()
indirect = __ViewFlag()
contiguous = __ViewFlag()
indirect_contiguous = __ViewFlag()

_T = TypeVar("_T")
class array():
    def __init__(
        self,
        shape: tuple[int, ...],
        itemsize: int,
        format: str,
        mode: Literal["c", "fortran"] = ...,
        allocate_buffer: bool = ...,
    ): ...
    def __delitem__(self, __key: SupportsIndex): ...
    def __setitem__(self, __key: SupportsIndex, __value: _T): ...
    @overload
    def __getitem__(self, __key: slice) -> array: ...
    def __getitem__(self, __key: SupportsIndex) -> _T: ...
    def __len__(self) -> int: ...
    def __contains__(self, __value: Any) -> bool:
        """Not actually defined."""
    def __iter__ (self) -> Iterator[_T]:
        """Not actually defined."""
    def count(self, __value: Any) -> int: ...
    def index(self, __value: _T, start: SupportsIndex = ..., stop: SupportsIndex = ...) -> int: ...
    @property
    def memview(self) -> __MemoryView__[_T, array[_T]]: ...
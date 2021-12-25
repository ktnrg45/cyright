import sys
import types
from _typeshed import SupportsAllComparisons, SupportsItems
from typing import (
    Any,
    Callable,
    Generic,
    Hashable,
    Iterable,
    NamedTuple,
    Protocol,
    Sequence,
    Sized,
    Tuple,
    Type,
    TypeVar,
    overload,
)
from typing_extensions import ParamSpec, final

if sys.version_info >= (3, 9):
    from types import GenericAlias

_AnyCallable = Callable[..., Any]

_T = TypeVar("_T")
_S = TypeVar("_S")
_P1 = ParamSpec("_P1")
_R1 = TypeVar("_R1", covariant=True)
_P2 = ParamSpec("_P2")
_R2 = TypeVar("_R2", contravariant=True)

@overload
def reduce(
    function: Callable[[_T, _S], _T], sequence: Iterable[_S], initial: _T
) -> _T: ...
@overload
def reduce(function: Callable[[_T, _T], _T], sequence: Iterable[_T]) -> _T: ...

class _CacheInfo(NamedTuple):
    hits: int
    misses: int
    maxsize: int
    currsize: int

@final
class _lru_cache_wrapper(Generic[_T]):
    __wrapped__: Callable[..., _T]
    def __call__(self, *args: Hashable, **kwargs: Hashable) -> _T: ...
    def cache_info(self) -> _CacheInfo: ...
    def cache_clear(self) -> None: ...

if sys.version_info >= (3, 8):
    @overload
    def lru_cache(
        maxsize: int | None = ..., typed: bool = ...
    ) -> Callable[[Callable[..., _T]], _lru_cache_wrapper[_T]]: ...
    @overload
    def lru_cache(
        maxsize: Callable[..., _T], typed: bool = ...
    ) -> _lru_cache_wrapper[_T]: ...

else:
    def lru_cache(
        maxsize: int | None = ..., typed: bool = ...
    ) -> Callable[[Callable[..., _T]], _lru_cache_wrapper[_T]]: ...

WRAPPER_ASSIGNMENTS: Sequence[str]
WRAPPER_UPDATES: Sequence[str]

class _Wrapped(Protocol[_P1, _R1, _P2, _R2]):
    __wrapped__: Callable[_P2, _R2]
    def __call__(self, *args: _P1.args, **kwargs: _P1.kwargs) -> _R2: ...

class _Wrapper(Protocol[_P1, _R1]):
    def __call__(self, f: Callable[_P2, _R2]) -> _Wrapped[_P1, _R1, _P2, _R2]: ...

def update_wrapper(
    wrapper: Callable[_P2, _R2],
    wrapped: Callable[_P1, _R1],
    assigned: Sequence[str] = ...,
    updated: Sequence[str] = ...,
) -> _Wrapped[_P1, _R1, _P2, _R2]: ...
def wraps(
    wrapped: Callable[_P1, _R1],
    assigned: Sequence[str] = ...,
    updated: Sequence[str] = ...,
) -> _Wrapper[_P1, _R1]: ...
def total_ordering(cls: Type[_T]) -> Type[_T]: ...
def cmp_to_key(
    mycmp: Callable[[_T, _T], int]
) -> Callable[[_T], SupportsAllComparisons]: ...

class partial(Generic[_T]):
    func: Callable[..., _T]
    args: Tuple[Any, ...]
    keywords: dict[str, Any]
    def __init__(self, func: Callable[..., _T], *args: Any, **kwargs: Any) -> None: ...
    def __call__(self, *args: Any, **kwargs: Any) -> _T: ...
    if sys.version_info >= (3, 9):
        def __class_getitem__(cls, item: Any) -> GenericAlias: ...

# With protocols, this could change into a generic protocol that defines __get__ and returns _T
_Descriptor = Any

class partialmethod(Generic[_T]):
    func: Callable[..., _T] | _Descriptor
    args: Tuple[Any, ...]
    keywords: dict[str, Any]
    @overload
    def __init__(
        self, __func: Callable[..., _T], *args: Any, **keywords: Any
    ) -> None: ...
    @overload
    def __init__(self, __func: _Descriptor, *args: Any, **keywords: Any) -> None: ...
    def __get__(self, obj: Any, cls: Type[Any]) -> Callable[..., _T]: ...
    @property
    def __isabstractmethod__(self) -> bool: ...
    if sys.version_info >= (3, 9):
        def __class_getitem__(cls, item: Any) -> GenericAlias: ...

class _SingleDispatchCallable(Generic[_T]):
    registry: types.MappingProxyType[Any, Callable[..., _T]]
    def dispatch(self, cls: Any) -> Callable[..., _T]: ...
    # @fun.register(complex)
    # def _(arg, verbose=False): ...
    @overload
    def register(
        self, cls: Type[Any], func: None = ...
    ) -> Callable[[Callable[..., _T]], Callable[..., _T]]: ...
    # @fun.register
    # def _(arg: int, verbose=False):
    @overload
    def register(
        self, cls: Callable[..., _T], func: None = ...
    ) -> Callable[..., _T]: ...
    # fun.register(int, lambda x: x)
    @overload
    def register(
        self, cls: Type[Any], func: Callable[..., _T]
    ) -> Callable[..., _T]: ...
    def _clear_cache(self) -> None: ...
    def __call__(self, *args: Any, **kwargs: Any) -> _T: ...

def singledispatch(func: Callable[..., _T]) -> _SingleDispatchCallable[_T]: ...

if sys.version_info >= (3, 8):
    class singledispatchmethod(Generic[_T]):
        dispatcher: _SingleDispatchCallable[_T]
        func: Callable[..., _T]
        def __init__(self, func: Callable[..., _T]) -> None: ...
        @overload
        def register(
            self, cls: Type[Any], method: None = ...
        ) -> Callable[[Callable[..., _T]], Callable[..., _T]]: ...
        @overload
        def register(
            self, cls: Callable[..., _T], method: None = ...
        ) -> Callable[..., _T]: ...
        @overload
        def register(
            self, cls: Type[Any], method: Callable[..., _T]
        ) -> Callable[..., _T]: ...
        def __call__(self, *args: Any, **kwargs: Any) -> _T: ...
    class cached_property(Generic[_T]):
        func: Callable[[Any], _T]
        attrname: str | None
        def __init__(self, func: Callable[[Any], _T]) -> None: ...
        @overload
        def __get__(
            self, instance: None, owner: Type[Any] | None = ...
        ) -> cached_property[_T]: ...
        @overload
        def __get__(self, instance: object, owner: Type[Any] | None = ...) -> _T: ...
        def __set_name__(self, owner: Type[Any], name: str) -> None: ...
        if sys.version_info >= (3, 9):
            def __class_getitem__(cls, item: Any) -> GenericAlias: ...

if sys.version_info >= (3, 9):
    def cache(__user_function: Callable[..., _T]) -> _lru_cache_wrapper[_T]: ...

def _make_key(
    args: Tuple[Hashable, ...],
    kwds: SupportsItems[Any, Any],
    typed: bool,
    kwd_mark: Tuple[object, ...] = ...,
    fasttypes: set[type] = ...,
    tuple: type = ...,
    type: Any = ...,
    len: Callable[[Sized], int] = ...,
) -> Hashable: ...

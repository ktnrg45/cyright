import typing as _typing

class parallel:
    def __init__(self, *, num_threads: int | None = None): ...
    def __enter__(self) -> parallel: ...
    def __exit__(self, *args, **kwargs): ...

class prange:
    def __init__(
        self,
        start: int = 0,
        stop: int | None = None,
        step: int = 1,
        nogil: bool = False,
        schedule: (
            _typing.Literal["static", "dynamic", "guided", "runtime"] | None
        ) = None,
        chunksize: int | None = None,
        num_threads: int | None = None,
    ): ...

    def __enter__(self) -> prange: ...
    def __exit__(self, *args, **kwargs): ...

def threadid() -> int: ...

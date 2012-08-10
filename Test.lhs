This test shows the power of thread objects very simply.
You see, that a single update evaluating to an error causes no harm to the object as a hole.  The previous value is retained, it's as if the exception causing update never occured at all(except for that an error is printed to stderr).

>import Control.Concurrent.ThreadObject
>main :: IO ()
>main = do
> to <- threadObject
> objectInit
>  to
>  (SyncWithoutSignal (SeedInitially (1::Int)))
>  (\a signal -> print a)
>  (handleExceptionStdErr "myNumberObject")
> update to (\n -> n+1)       -- 2
> update to (\n -> n+1)       -- 3
> update to (\n -> n+1)       -- 4
> update to (\n -> n `div` 0) -- 4
> update to (\n -> n+1)       -- 5
> freeObject to

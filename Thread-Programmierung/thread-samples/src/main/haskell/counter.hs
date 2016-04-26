import Control.Concurrent
import Control.Monad
import Data.IORef

inc z = do
    v <- readIORef z
    writeIORef z (v+1)

main = do
    z <- newIORef 0
    forM_ [1..10000]
        (\k -> forkIO (inc z))
import Control.Concurrent
import Control.Monad

main :: IO()
main = do
    l <- newEmptyMVar
    forM_ [1..4]
        ( \ k -> forkIO (hello k l)) -- Lambda-Abstraktion: Definition einer anonymen Funktion (Funktion ohne Namen)

hello i l = do
    takeMVar l
    putStrLn ("Hallo von" ++ show i)
    putStrLn l ()

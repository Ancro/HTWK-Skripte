# Haskell in Threadprogrammierung

- Bibliothek: Control.Concurrent
- forkIO p: erzeugt einen Thread, in dem das Programm p läuft
`forkIO::IO() -> IOThreadId`
- was macht eine Monade: hintereinander ausführen von Funktionen und Ergebnisse aufsammeln (auch Funktionskombination)

MVar t
- Typ einer Synchronisationsvariablen, die Werte vom Typ t enthält.
- Ähnlich wie Warteschlange mit einem Element
- v vom Typ `MVar t` kann leer sein oder einen Wert (vom Typ t) enthalten.


`putMVar::MVar t -> t -> IO()`

`putMVar v a`
- Warten bis v leer ist. Dann v mit Wert a auffüllen.

`takeMVar::MVar t -> IO t`
`takeMVar v`
- Warten bis v voll ist. Dann den Wert von v entnehmen.
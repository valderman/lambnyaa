-- | A Chan wrapper which can be "closed" when there is no more input to send.
module Network.LambNyaa.FiniteChan (
    Chan,
    newChan, writeList2Chan, closeChan, getChanContents, writeChan, readChan
  ) where
import qualified Control.Concurrent.Chan as CCC

newtype Chan a = Chan (CCC.Chan (Maybe a))

newChan :: IO (Chan a)
newChan = fmap Chan CCC.newChan

writeList2Chan :: Chan a -> [a] -> IO ()
writeList2Chan (Chan c) = CCC.writeList2Chan c . map Just

closeChan :: Chan a -> IO ()
closeChan (Chan c) = CCC.writeChan c Nothing

getChanContents :: Chan a -> IO [a]
getChanContents (Chan c) = do
    xs <- CCC.getChanContents c
    return [x | Just x <- takeWhile open xs]
  where
    open Nothing = False
    open _       = True

writeChan :: Chan a -> a -> IO ()
writeChan (Chan c) = CCC.writeChan c . Just

readChan :: Chan a -> IO (Maybe a)
readChan (Chan c) = CCC.readChan c

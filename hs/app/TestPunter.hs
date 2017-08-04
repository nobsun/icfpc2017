import Data.ByteString.Lazy.Char8 as B (pack, unpack)
import Data.Char (isDigit)
import Data.Aeson (FromJSON, ToJSON, encode, decode)
import Data.List (isPrefixOf, tails)
import Data.Text as T (pack, unpack)
import Debug.Trace (traceShow)
import Network.Socket
import Control.Exception (bracket)
import Control.Monad (forever, replicateM)
import System.IO
import System.Environment


import Online
import InternalJSON

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    [port] <- getArgs
    withSocketsDo $ do
      addrinfos <- getAddrInfo Nothing (Just "punter.inf.ed.ac.uk") (Just port)
      let addr = head addrinfos
      sock <- socket (addrFamily addr) Stream defaultProtocol
      connect sock (addrAddress addr)
      bracket
        (socketToHandle sock ReadWriteMode)
        (hClose)
        (\h -> do
          hSetBuffering h NoBuffering
          output h (HandshakePunter{me=T.pack"sampou"})
          input h
          msetup <- fmap (decode. B.pack) (input h)
          case msetup :: Maybe Setup of
            Just setup -> do
              let pid = 0 -- TODO ambigouse field label
              output h (ReadyOn{ready=pid})
              loop h pid
        )
  where
    loop h pid = do
      token <- fmap B.pack (input h)
{- dummy -}
      output h (MvPass{pass=Punter pid})
      loop h pid
{- not work
      case decode token :: Maybe PrevMoves of
         Just movest -> do
           output h (MvPass{pass=Punter pid})
           loop h pid
         Nothing ->
           case decode token :: Maybe Stop of
             Just stop -> do
               print (scores stop)
             Nothing -> do
               putStrLn $ "fail to parse JSON: "++(B.unpack token)
               hFlush stdout
-}

output :: ToJSON a => Handle -> a -> IO ()
output h x = do
  let json = B.unpack (encode x)
  putStrLn ("-> " ++ json)
  hFlush stdout
  hPutStrLn h (show (length json+1)++":"++json)

input :: Handle -> IO String
input h = do
  len <- getLength h []
  token <- replicateM len (hGetChar h)
  putStrLn ("<- " ++ token)
  hFlush stdout
  return token
  where
    getLength h cs = do
      c <- hGetChar h
      if isDigit c then getLength h (c:cs) else return (read (reverse cs))

getPunterId :: String -> Int
getPunterId = read.takeWhile isDigit.drop 8.head.dropWhile(not.isPrefixOf"punter\":").tails

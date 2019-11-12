module Main where

import Control.Arrow
import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.Reader
import qualified Control.Exception as E
import qualified Data.ByteString.UTF8 as B8
import Data.Void
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)
import System.IO
import System.Exit

import Text.Megaparsec hiding (State)
import Text.Megaparsec.Byte
import qualified Text.Megaparsec.Byte.Lexer as L

type Parser = Parsec Void B8.ByteString

server = "localhost"
port = "32771"
chan = "#test"
nick = "Baronbot"

main :: IO ()
main = withSocketsDo $ do
    addr <- resolve server port
    E.bracket (conn addr) disconnect talk
  where
    disconnect   = close
    resolve host port = do
      let hints = defaultHints { addrSocketType = Stream }
      addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
      return addr
    conn addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      connect sock $ addrAddress addr
      return sock
    talk sock = do
      rmsg <- recv sock 1024
      let msg = B8.toString rmsg
      printMsg msg
      handleMsg msg
      talk sock

printMsg :: String -> IO ()
printMsg "" = putStr ""
printMsg msg = putStrLn msg

getPrefix :: String -> String
getPrefix = takeWhile(/= ' ')

getMessage = dropWhile(/= ' ')

handleMsg msg
  | getPrefix msg == "ERROR" = error $ getMessage msg
  | otherwise = return ()


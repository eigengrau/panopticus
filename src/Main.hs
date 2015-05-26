{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}


import           Control.Concurrent            (ThreadId, threadDelay)
import           Control.Monad.Loops           (anyM, whileM_)
import qualified Data.Set                      as Set
import           GHC.Conc.Sync                 (ThreadStatus (..), threadStatus)
import           Network                       (PortID (..))
import           Network.IRC.Bot               (LogLevel (..), User (..),
                                                simpleBot', stdoutLogger)
import           Network.IRC.Bot.Part.Channels (channelsPart, initChannelsPart)
import           Network.IRC.Bot.Part.Ping     (pingPart)
import           Prelude.Unicode.SR


import           Panopticus.Part.Greet         (greetPart)


type Bot = IO ([ThreadId], IO ())


panopticus ∷ Bot
panopticus = do

  (channels, channelsPart) ← initChannelsPart channels
  let parts = [pingPart, channelsPart, greetPart]

  simpleBot'
    channelLogger
    logger
    limits
    host
    port
    nick
    commandPrefix
    user
    parts

        where
          channelLogger = Nothing
          commandPrefix = "!"
          nick          = "panopticus"
          host          = "irc.freenode.net"
          port          = PortNumber 6667
          logger        = stdoutLogger logLevel
          logLevel      = Debug
          channels      = ["#panopticus"]
          limits        = Nothing
          user          = User {
                              username   = nick,
                              hostname   = "panopticus",
                              servername = "⁇",
                              realname   = nick
                            }


main ∷ IO ()
main = do

  (tids, reconnect) ← panopticus
  whileM_ (anyM isRunning tids) (threadDelay ∥ seconds 1)
      where
        seconds = (10↑6×)

        isRunning ∷ ThreadId → IO Bool
        isRunning tid = do

          status ← threadStatus tid
          return ∥ case status of
                     ThreadFinished → False
                     ThreadDied     → False
                     otherwise      → True

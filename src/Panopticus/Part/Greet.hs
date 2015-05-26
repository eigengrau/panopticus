{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}


module Panopticus.Part.Greet (greetPart) where

import           Control.Monad.Unicode
import           Data.ByteString.Char8    (ByteString, pack, unpack)
import           Data.Maybe               (fromMaybe)
import           Network.IRC.Bot.BotMonad (BotMonad (..), maybeZero)
import           Network.IRC.Bot.Commands (PrivMsg (..), askSenderNickName,
                                           replyTo, sendCommand)
import           Network.IRC.Bot.Log      (LogLevel (Debug))
import           Network.IRC.Bot.Parsec   (botPrefix, parsecPart)
import           Prelude.Unicode
import           Prelude.Unicode.SR
import           Text.Parsec              (ParsecT, string, try)
import           Text.Printf              (printf)


greetPart ∷ BotMonad μ ⇒ μ ()
greetPart = parsecPart greetCommand


greetCommand ∷ BotMonad μ ⇒ ParsecT ByteString () μ ()
greetCommand = do

  try ∥ botPrefix ≫ string "hello"
  target ← maybeZero =≪ replyTo
  mNick ← askSenderNickName
  let
      mNick' = fromMaybe "stranger" mNick
      msg    = mNick' ⊕ ", " ⊕ greeting
  sendCommand (PrivMsg Nothing [target] msg)

 +≫ return ()

     where
       greeting = "I am bot"

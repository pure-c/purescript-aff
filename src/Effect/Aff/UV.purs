module Effect.Aff.UV
  ( runAff
  , runAff_
  , launchAff
  , launchAff_
  , module Reexport
  ) where

import Effect.Aff hiding (launchAff,launchAff_)
import Prelude

import Control.Monad.Error.Class (try)
import Control.Monad.Except (runExceptT)
import Data.Either (Either, fromRight)
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Aff hiding (launchAff,launchAff_) as Reexport
import Effect.Class (liftEffect)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import UV as UV

runAff
  :: ∀ e a
   . (Either e a -> Effect Unit)
  -> Aff e a
  -> UV.Loop
  -> Effect (Fiber e Unit)
runAff k aff = launchAff $ liftEffect <<< k =<< try aff

runAff_
  :: ∀ e a
   . (Either e a -> Effect Unit)
  -> Aff e a
  -> UV.Loop
  -> Effect Unit
runAff_ k aff l = void $ runAff k aff l

launchAff_
  :: ∀ e a
   . Aff e a
  -> UV.Loop
  -> Effect Unit
launchAff_ action loop =
  void $ launchAff action loop

launchAff
  :: ∀ e a
   . Aff e a
  -> UV.Loop
  -> Effect (Fiber e a)
launchAff action loop =
  Aff.launchAff
    (setTimeout loop)
    (\_ -> pure $ unsafeCrashWith "TODO: render and print error")
    action

setTimeout
  :: UV.Loop
  -> Int
  -> Effect Unit
  -> Effect Unit
setTimeout loop timeout cb =
  unsafePartial $ fromRight <$> do
    runExceptT $
      void $
        setTimeout_ loop timeout cb
  where
  setTimeout_ loop timeout cb = do
    timer <- UV.timerNew loop
    timer <$ UV.timerStart (UV.Timeout timeout) (UV.Repeat 0) cb timer

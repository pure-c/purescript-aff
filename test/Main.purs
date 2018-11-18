module Test.Main where

import Effect.Aff
import Prelude
import Test.Assert

import Control.Monad.Error.Class (throwError, try)
import Control.Monad.Except (runExcept, runExceptT)
import Data.Either (Either(..), fromRight, isLeft)
import Effect (Effect)
import Effect.Aff.UV as Aff.UV
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Partial.Unsafe (unsafePartial)
import UV as UV

--------------------------------------------------------------------------------
-- Test-local, globally shared UV loop
--------------------------------------------------------------------------------

-- | Shared Loop for this test
testLoop :: UV.Loop
testLoop = unsafePerformEffect $ UV.newLoop

launchTestLoopAff_ :: ∀ e a . Aff e a -> Effect Unit
launchTestLoopAff_ = void <<< launchTestLoopAff

launchTestLoopAff :: ∀ e a . Aff e a -> Effect (Fiber e a)
launchTestLoopAff = Aff.UV.launchAff <@> testLoop

runTestLoopAff
  :: ∀ e a
   . (Either e a -> Effect Unit)
  -> Aff e a
  -> Effect (Fiber e Unit)
runTestLoopAff k aff =
  launchTestLoopAff (liftEffect <<< k =<< try aff)

runTestLoopAff_
  :: ∀ e a
   . (Either e a -> Effect Unit)
  -> Aff e a
  -> Effect Unit
runTestLoopAff_ k = void <<< runTestLoopAff k

--------------------------------------------------------------------------------
-- Ref convenience functions
--------------------------------------------------------------------------------

newRef :: ∀ m a. MonadEffect m => a -> m (Ref a)
newRef = liftEffect <<< Ref.new

readRef :: ∀ m a. MonadEffect m => Ref a -> m a
readRef = liftEffect <<< Ref.read

writeRef :: ∀ m a. MonadEffect m => Ref a -> a -> m Unit
writeRef r = liftEffect <<< flip Ref.write r

modifyRef :: ∀ m a. MonadEffect m => Ref a -> (a -> a) -> m a
modifyRef r = liftEffect <<< flip Ref.modify r

--------------------------------------------------------------------------------
-- Running tests
--------------------------------------------------------------------------------

assertEff :: String -> Either _ Boolean -> Effect Unit
assertEff s = case _ of
  Left _ -> do
    Console.log ("[Error]" <> s)
    assert false
  Right r -> do
    assert' ("Assertion failure " <> s) r
    Console.log ("[OK] " <> s)

runAssert :: ∀ e. String -> Aff e Boolean -> Effect Unit
runAssert s = runTestLoopAff_ (assertEff s)

runAssertEq :: ∀ e a. Eq a => String -> a -> Aff e a -> Effect Unit
runAssertEq s a = runTestLoopAff_ (assertEff s <<< map (eq a))

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

test_pure :: Effect Unit
test_pure = runAssertEq "pure" 42 (pure 42)

test_bind ∷ Effect Unit
test_bind = runAssertEq "bind" 44 do
  n1 <- pure 42
  n2 <- pure (n1 + 1)
  n3 <- pure (n2 + 1)
  pure n3

test_try :: Effect Unit
test_try = runAssert "try" do
  n <- try (pure 42)
  case n of
    Right 42 -> pure true
    _ -> pure false

test_throw :: Effect Unit
test_throw = runAssert "try/throw" do
  n <- try (throwError "Nope.")
  pure (isLeft n)

test_liftEffect :: Effect Unit
test_liftEffect = runAssertEq "liftEffect" 42 do
  ref <- newRef 0
  liftEffect do
    writeRef ref 42
    readRef ref

main :: Effect Unit
main = do
  test_pure
  test_bind
  test_try
  test_throw
  test_liftEffect

  launchTestLoopAff_ do
    pure unit -- TODO

  unsafeUV $ UV.run UV._RunDefault testLoop

  where
  -- | Evaluate an UV action and assume it will succeed.
  -- | If the assumption is not met, abort the program.
  unsafeUV action =
    unsafePartial $
      map fromRight $
        runExceptT action

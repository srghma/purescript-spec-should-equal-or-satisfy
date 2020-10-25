module Test.ShouldEqualOrSatisfySpec where

import Data.Maybe (Maybe(..))
import Data.Predicate (Predicate(..))
import Data.Tuple (Tuple(..))
import Prelude
import Test.Spec.ShouldEqualOrSatisfy (shouldEqualOrSatisfy)

import Control.Monad.Error.Class (class MonadError, throwError, try)
import Data.Either (Either(..))
import Effect.Exception (Error)
import Effect.Exception as Exception
import Test.Spec (Spec, it)
import Test.Spec.Assertions (shouldEqual)

expectErrorWithMessage
  :: forall m t
   . MonadError Error m
  => String
  -> m t
  -> m Unit
expectErrorWithMessage message a = do
  e <- try a
  case e of
    Left e -> Exception.message e `shouldEqual` message
    Right _ -> throwError $ Exception.error "Expected error"

spec :: Spec Unit
spec = do
  it "shouldEqualOrSatisfy" do
    Just 1 `shouldEqualOrSatisfy` Just 1

    expectErrorWithMessage "Doesn't pass spec: (Just 1)"
      (Just 1 `shouldEqualOrSatisfy` Just 2)

    Just 1 `shouldEqualOrSatisfy` Just (Predicate \x -> x == 1)
    ---
    Just [1] `shouldEqualOrSatisfy` Just [1]
    ---
    Tuple 1 2 `shouldEqualOrSatisfy` Tuple 1 (Predicate \x -> x == 2)
    -- nested
    Tuple 1 (Just 1) `shouldEqualOrSatisfy` Tuple 1 (Just 1)
    Tuple 1 (Just (Just 1)) `shouldEqualOrSatisfy` Tuple 1 (Just (Just 1))
    --
    Tuple 1 (Just 1) `shouldEqualOrSatisfy` Tuple 1 (Predicate \x -> x == Just 1)
    Tuple 1 (Just 1) `shouldEqualOrSatisfy` Tuple 1 (Just (Predicate \(x :: Int) -> x == 1))
    ---
    { a: 1 } `shouldEqualOrSatisfy` { a: 1 }
    { a: 1 } `shouldEqualOrSatisfy` { a: (Predicate \x -> x == 1) }
    ---
    { } `shouldEqualOrSatisfy` { }
    { a: 1, b: 2 } `shouldEqualOrSatisfy` { a: 1, b: (Predicate \x -> x == 2) }
    --
    { a: 1, b: { c: 2 } } `shouldEqualOrSatisfy` { a: 1, b: { c: 2 } }
    { a: 1, b: { c: 2 } } `shouldEqualOrSatisfy` { a: 1, b: { c: (Predicate \x -> x == 2) } }
    --
    { a: 1, b: { c: Just 1 } } `shouldEqualOrSatisfy` { a: 1, b: { c: Just 1 } }
    { a: 1, b: { c: Just { d: 1 } } } `shouldEqualOrSatisfy` { a: 1, b: { c: Just { d: 1 } } }

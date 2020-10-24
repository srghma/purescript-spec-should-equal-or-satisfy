module Test.ShouldEqualDeepSpec where

import Data.Maybe
import Data.Predicate
import Data.Tuple
import Prelude
import Test.Spec.ShouldEqualDeep

import Effect.Console (log)
import Test.Spec (Spec, it, describe)
import Test.Spec.Assertions (shouldEqual, shouldNotEqual)

spec :: Spec Unit
spec = do
  it "ShouldEqualDeep" do
    Just 1 `shouldSatisfyOrEqualDeep` Just 1
    Just 1 `shouldSatisfyOrEqualDeep` Just (Predicate \x -> x == 1)
    ---
    Tuple 1 2 `shouldSatisfyOrEqualDeep` Tuple 1 (Predicate \x -> x == 2)
    -- nested
    Tuple 1 (Just 1) `shouldSatisfyOrEqualDeep` Tuple 1 (Just 1)
    Tuple 1 (Just (Just 1)) `shouldSatisfyOrEqualDeep` Tuple 1 (Just (Just 1))
    --
    Tuple 1 (Just 1) `shouldSatisfyOrEqualDeep` Tuple 1 (Predicate \x -> x == Just 1)
    Tuple 1 (Just 1) `shouldSatisfyOrEqualDeep` Tuple 1 (Just (Predicate \(x :: Int) -> x == 1))
    ---
    { a: 1 } `shouldSatisfyOrEqualDeep` { a: 1 }
    { a: 1 } `shouldSatisfyOrEqualDeep` { a: (Predicate \x -> x == 1) }
    ---
    { } `shouldSatisfyOrEqualDeep` { }
    { a: 1, b: 2 } `shouldSatisfyOrEqualDeep` { a: 1, b: (Predicate \x -> x == 2) }
    --
    { a: 1, b: { c: 2 } } `shouldSatisfyOrEqualDeep` { a: 1, b: { c: 2 } }
    { a: 1, b: { c: 2 } } `shouldSatisfyOrEqualDeep` { a: 1, b: { c: (Predicate \x -> x == 2) } }
    --
    { a: 1, b: { c: Just 1 } } `shouldSatisfyOrEqualDeep` { a: 1, b: { c: Just 1 } }
    { a: 1, b: { c: Just { d: 1 } } } `shouldSatisfyOrEqualDeep` { a: 1, b: { c: Just { d: 1 } } }

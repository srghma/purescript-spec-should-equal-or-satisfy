module Test.Spec.ShouldEqualDeep where

import Protolude
import Data.Generic.Rep
import Data.Predicate
import Test.Spec.Assertions

class GenericEqOrSatisfy input inputOrPredicate | inputOrPredicate -> input where
  genericShould :: input -> inputOrPredicate -> Boolean

instance genericEqNoConstructors :: GenericEqOrSatisfy NoConstructors NoConstructors where
  genericShould _ _ = true

instance genericEqNoArguments :: GenericEqOrSatisfy NoArguments NoArguments where
  genericShould _ _ = true

instance genericEqSum :: (GenericEqOrSatisfy a a', GenericEqOrSatisfy b b') => GenericEqOrSatisfy (Sum a b) (Sum a' b') where
  genericShould (Inl a1) (Inl a2) = genericShould a1 a2
  genericShould (Inr b1) (Inr b2) = genericShould b1 b2
  genericShould _ _ = false

instance genericEqProduct :: (GenericEqOrSatisfy a a', GenericEqOrSatisfy b b') => GenericEqOrSatisfy (Product a b) (Product a' b') where
  genericShould (Product a1 b1) (Product a2 b2) = genericShould a1 a2 && genericShould b1 b2

instance genericEqConstructor :: GenericEqOrSatisfy a a' => GenericEqOrSatisfy (Constructor name a) (Constructor name a') where
  genericShould (Constructor a1) (Constructor a2) = genericShould a1 a2

instance genericPredicateArgument :: GenericEqOrSatisfy (Argument a) (Argument (Predicate a)) where
  genericShould (Argument a1) (Argument (Predicate a2)) = a2 a1

else

instance genericEqArgument :: Eq a => GenericEqOrSatisfy (Argument a) (Argument a) where
  genericShould (Argument a1) (Argument a2) = a1 == a2

---------------

class EqOrSatisfy input inputOrPredicate | inputOrPredicate -> input where
  should :: input -> inputOrPredicate -> Boolean

instance eqOrSatisfyGeneric ::
  ( Generic input inputRep
  , Generic inputOrPredicate inputOrPredicateRep
  , GenericEqOrSatisfy inputRep inputOrPredicateRep
  ) =>
  EqOrSatisfy input inputOrPredicate where
  should input inputOrPredicate = genericShould (from input) (from inputOrPredicate)

shouldSatisfyOrEqualDeep
  :: forall input inputOrPredicate m
   . EqOrSatisfy input inputOrPredicate
  => MonadThrow Error m
  => Show input
  => input
  -> inputOrPredicate
  -> m Unit
shouldSatisfyOrEqualDeep input inputOrPredicate =
  when (not $ should input inputOrPredicate) $
    fail $ show input <> " ≠ spec"

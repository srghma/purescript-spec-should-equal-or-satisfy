module Test.Spec.ShouldEqualDeep where

import Protolude
import Data.Generic.Rep
import Data.Predicate
import Test.Spec.Assertions
import Type.Prelude (class IsSymbol, RLProxy(RLProxy), RProxy, SProxy(SProxy), reflectSymbol)
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record

class EqOrSatisfyImpl input inputOrPredicate | inputOrPredicate -> input where
  eqOrSatisfyImpl :: input -> inputOrPredicate -> Boolean

instance eqOrSatisfyImplPred :: EqOrSatisfyImpl a (Predicate a) where
  eqOrSatisfyImpl a (Predicate b) = b a

else

instance eqOrSatisfyImplEq :: Eq a => EqOrSatisfyImpl a a where
  eqOrSatisfyImpl a b = a == b

---------------

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

instance genericEq1 :: GenericEqOrSatisfy (Argument a) (Argument (Predicate a)) where
  genericShould (Argument a) (Argument (Predicate b)) = b a

else

instance genericEq2 :: Eq a => GenericEqOrSatisfy (Argument a) (Argument a) where
  genericShould (Argument a) (Argument b) = a == b

else

instance genericPredicateImpl :: EqOrSatisfy a b => GenericEqOrSatisfy (Argument a) (Argument b) where
  genericShould (Argument a) (Argument b) = should a b

---------------

class RecordIndexOrSatisfy
  (inputRowList :: RL.RowList Type)
  (inputOrPredicateRowList :: RL.RowList Type)
  (inputRow :: Row Type)
  (inputOrPredicateRow :: Row Type)
  | inputRowList -> inputRow
  , inputOrPredicateRowList -> inputOrPredicateRow inputRowList inputRow
  where
  recordIndexOrSatisfy
    :: RLProxy inputRowList
    -> RLProxy inputOrPredicateRowList
    -> Record inputRow
    -> Record inputOrPredicateRow
    -> Boolean

instance recordIndexOrSatisfyCons ::
  ( IsSymbol name
  , RecordIndexOrSatisfy inputRowList'Tail inputOrPredicateRowList'Tail inputRow inputOrPredicateRow
  , EqOrSatisfyImpl inputRowList'Val inputOrPredicateRowList'Val
  , Row.Cons name inputRowList'Val trash1 inputRow
  , Row.Cons name inputOrPredicateRowList'Val trash2 inputOrPredicateRow
  ) => RecordIndexOrSatisfy
  (RL.Cons name inputRowList'Val inputRowList'Tail)
  (RL.Cons name inputOrPredicateRowList'Val inputOrPredicateRowList'Tail)
  inputRow
  inputOrPredicateRow
  where
  recordIndexOrSatisfy _ _ input inputOrPredicate = eqOrSatisfyImpl a b && rest
    where
      a :: inputRowList'Val
      a = Record.get (SProxy :: SProxy name) input

      b :: inputOrPredicateRowList'Val
      b = Record.get (SProxy :: SProxy name) inputOrPredicate

      rest :: Boolean
      rest = recordIndexOrSatisfy (RLProxy :: RLProxy inputRowList'Tail) (RLProxy :: RLProxy inputOrPredicateRowList'Tail) input inputOrPredicate

instance recordIndexOrSatisfyNil :: RecordIndexOrSatisfy RL.Nil RL.Nil inputRow inputOrPredicateRow where
  recordIndexOrSatisfy _ _ _ _ = true

---------------

class EqOrSatisfy input inputOrPredicate | inputOrPredicate -> input where
  should :: input -> inputOrPredicate -> Boolean

instance eqOrSatisfy1 :: EqOrSatisfy a (Predicate a) where
  should a (Predicate b) = b a

else

instance eqOrSatisfyRecord ::
  ( RL.RowToList inputRow inputRowList
  , RL.RowToList inputOrPredicateRow inputOrPredicateRowList
  , RecordIndexOrSatisfy inputRowList inputOrPredicateRowList inputRow inputOrPredicateRow
  ) =>
  EqOrSatisfy (Record inputRow) (Record inputOrPredicateRow) where
  should = recordIndexOrSatisfy (RLProxy :: RLProxy inputRowList) (RLProxy :: RLProxy inputOrPredicateRowList)

else

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

module Test.Spec.ShouldEqualDeep where

import Protolude
import Data.Generic.Rep
import Data.Predicate
import Test.Spec.Assertions
import Type.Prelude (class IsSymbol, RLProxy(RLProxy), RProxy, SProxy(SProxy), reflectSymbol)
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record

class PassesSpecGeneric input inputOrPredicate | inputOrPredicate -> input where
  passesSpecGeneric :: input -> inputOrPredicate -> Boolean

instance passesSpecGeneric'NoConstructors :: PassesSpecGeneric NoConstructors NoConstructors where
  passesSpecGeneric _ _ = true

instance passesSpecGeneric'NoArguments :: PassesSpecGeneric NoArguments NoArguments where
  passesSpecGeneric _ _ = true

instance passesSpecGeneric'Sum :: (PassesSpecGeneric a a', PassesSpecGeneric b b') => PassesSpecGeneric (Sum a b) (Sum a' b') where
  passesSpecGeneric (Inl a1) (Inl a2) = passesSpecGeneric a1 a2
  passesSpecGeneric (Inr b1) (Inr b2) = passesSpecGeneric b1 b2
  passesSpecGeneric _ _ = false

instance passesSpecGeneric'Product :: (PassesSpecGeneric a a', PassesSpecGeneric b b') => PassesSpecGeneric (Product a b) (Product a' b') where
  passesSpecGeneric (Product a1 b1) (Product a2 b2) = passesSpecGeneric a1 a2 && passesSpecGeneric b1 b2

instance passesSpecGeneric'Constructor :: PassesSpecGeneric a a' => PassesSpecGeneric (Constructor name a) (Constructor name a') where
  passesSpecGeneric (Constructor a1) (Constructor a2) = passesSpecGeneric a1 a2

instance passesSpecGeneric'Nested :: PassesSpec a b => PassesSpecGeneric (Argument a) (Argument b) where
  passesSpecGeneric (Argument a) (Argument b) = passesSpec a b

---------------

class PassesSpecRecord
  (inputRowList :: RL.RowList Type)
  (inputOrPredicateRowList :: RL.RowList Type)
  (inputRow :: Row Type)
  (inputOrPredicateRow :: Row Type)
  | inputRowList -> inputRow
  , inputOrPredicateRowList -> inputOrPredicateRow inputRowList inputRow
  where
  passesSpecRecord
    :: RLProxy inputRowList
    -> RLProxy inputOrPredicateRowList
    -> Record inputRow
    -> Record inputOrPredicateRow
    -> Boolean

instance passesSpecRecord'Cons ::
  ( IsSymbol name
  , PassesSpecRecord inputRowList'Tail inputOrPredicateRowList'Tail inputRow inputOrPredicateRow
  , PassesSpec inputRowList'Val inputOrPredicateRowList'Val
  , Row.Cons name inputRowList'Val trash1 inputRow
  , Row.Cons name inputOrPredicateRowList'Val trash2 inputOrPredicateRow
  ) => PassesSpecRecord
  (RL.Cons name inputRowList'Val inputRowList'Tail)
  (RL.Cons name inputOrPredicateRowList'Val inputOrPredicateRowList'Tail)
  inputRow
  inputOrPredicateRow
  where
  passesSpecRecord _ _ input inputOrPredicate = passesSpec a b && rest
    where
      a :: inputRowList'Val
      a = Record.get (SProxy :: SProxy name) input

      b :: inputOrPredicateRowList'Val
      b = Record.get (SProxy :: SProxy name) inputOrPredicate

      rest :: Boolean
      rest = passesSpecRecord (RLProxy :: RLProxy inputRowList'Tail) (RLProxy :: RLProxy inputOrPredicateRowList'Tail) input inputOrPredicate

instance passesSpecRecord'Nil :: PassesSpecRecord RL.Nil RL.Nil inputRow inputOrPredicateRow where
  passesSpecRecord _ _ _ _ = true

---------------

class PassesSpec input inputOrPredicate where
  passesSpec :: input -> inputOrPredicate -> Boolean

instance passesSpec'Predicate :: PassesSpec a (Predicate a) where
  passesSpec a (Predicate b) = b a

else

instance passesSpec'Eq :: Eq a => PassesSpec a a where
  passesSpec a b = a == b

else

instance passesSpec'Record ::
  ( RL.RowToList inputRow inputRowList
  , RL.RowToList inputOrPredicateRow inputOrPredicateRowList
  , PassesSpecRecord inputRowList inputOrPredicateRowList inputRow inputOrPredicateRow
  ) =>
  PassesSpec (Record inputRow) (Record inputOrPredicateRow) where
  passesSpec = passesSpecRecord (RLProxy :: RLProxy inputRowList) (RLProxy :: RLProxy inputOrPredicateRowList)

else

instance passesSpec'Generic ::
  ( Generic input inputRep
  , Generic inputOrPredicate inputOrPredicateRep
  , PassesSpecGeneric inputRep inputOrPredicateRep
  ) =>
  PassesSpec input inputOrPredicate where
  passesSpec input inputOrPredicate = passesSpecGeneric (from input) (from inputOrPredicate)

shouldPassSpec
  :: forall input inputOrPredicate m
   . PassesSpec input inputOrPredicate
  => MonadThrow Error m
  => Show input
  => input
  -> inputOrPredicate
  -> m Unit
shouldPassSpec input inputOrPredicate =
  when (not $ passesSpec input inputOrPredicate) $
    fail $ show input <> " ≠ spec"

module Test.Spec.ShouldPassSpecification where

import Protolude
import Data.Generic.Rep
import Data.Predicate
import Test.Spec.Assertions
import Type.Prelude (class IsSymbol, RLProxy(RLProxy), RProxy, SProxy(SProxy), reflectSymbol)
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record

class PassesSpecificationGeneric input inputOrPredicate | inputOrPredicate -> input where
  passesSpecificationGeneric :: input -> inputOrPredicate -> Boolean

instance passesSpecificationGeneric'NoConstructors :: PassesSpecificationGeneric NoConstructors NoConstructors where
  passesSpecificationGeneric _ _ = true

instance passesSpecificationGeneric'NoArguments :: PassesSpecificationGeneric NoArguments NoArguments where
  passesSpecificationGeneric _ _ = true

instance passesSpecificationGeneric'Sum :: (PassesSpecificationGeneric a a', PassesSpecificationGeneric b b') => PassesSpecificationGeneric (Sum a b) (Sum a' b') where
  passesSpecificationGeneric (Inl a1) (Inl a2) = passesSpecificationGeneric a1 a2
  passesSpecificationGeneric (Inr b1) (Inr b2) = passesSpecificationGeneric b1 b2
  passesSpecificationGeneric _ _ = false

instance passesSpecificationGeneric'Product :: (PassesSpecificationGeneric a a', PassesSpecificationGeneric b b') => PassesSpecificationGeneric (Product a b) (Product a' b') where
  passesSpecificationGeneric (Product a1 b1) (Product a2 b2) = passesSpecificationGeneric a1 a2 && passesSpecificationGeneric b1 b2

instance passesSpecificationGeneric'Constructor :: PassesSpecificationGeneric a a' => PassesSpecificationGeneric (Constructor name a) (Constructor name a') where
  passesSpecificationGeneric (Constructor a1) (Constructor a2) = passesSpecificationGeneric a1 a2

instance passesSpecificationGeneric'Nested :: PassesSpecification a b => PassesSpecificationGeneric (Argument a) (Argument b) where
  passesSpecificationGeneric (Argument a) (Argument b) = passesSpecification a b

---------------

class PassesSpecificationRecord
  (inputRowList :: RL.RowList Type)
  (inputOrPredicateRowList :: RL.RowList Type)
  (inputRow :: Row Type)
  (inputOrPredicateRow :: Row Type)
  | inputRowList -> inputRow
  , inputOrPredicateRowList -> inputOrPredicateRow inputRowList inputRow
  where
  passesSpecificationRecord
    :: RLProxy inputRowList
    -> RLProxy inputOrPredicateRowList
    -> Record inputRow
    -> Record inputOrPredicateRow
    -> Boolean

instance passesSpecificationRecord'Cons ::
  ( IsSymbol name
  , PassesSpecificationRecord inputRowList'Tail inputOrPredicateRowList'Tail inputRow inputOrPredicateRow
  , PassesSpecification inputRowList'Val inputOrPredicateRowList'Val
  , Row.Cons name inputRowList'Val trash1 inputRow
  , Row.Cons name inputOrPredicateRowList'Val trash2 inputOrPredicateRow
  ) => PassesSpecificationRecord
  (RL.Cons name inputRowList'Val inputRowList'Tail)
  (RL.Cons name inputOrPredicateRowList'Val inputOrPredicateRowList'Tail)
  inputRow
  inputOrPredicateRow
  where
  passesSpecificationRecord _ _ input inputOrPredicate = passesSpecification a b && rest
    where
      a :: inputRowList'Val
      a = Record.get (SProxy :: SProxy name) input

      b :: inputOrPredicateRowList'Val
      b = Record.get (SProxy :: SProxy name) inputOrPredicate

      rest :: Boolean
      rest = passesSpecificationRecord (RLProxy :: RLProxy inputRowList'Tail) (RLProxy :: RLProxy inputOrPredicateRowList'Tail) input inputOrPredicate

instance passesSpecificationRecord'Nil :: PassesSpecificationRecord RL.Nil RL.Nil inputRow inputOrPredicateRow where
  passesSpecificationRecord _ _ _ _ = true

---------------

class PassesSpecification input inputOrPredicate where
  passesSpecification :: input -> inputOrPredicate -> Boolean

instance passesSpecification'Predicate :: PassesSpecification a (Predicate a) where
  passesSpecification a (Predicate b) = b a

else

instance passesSpecification'Eq :: Eq a => PassesSpecification a a where
  passesSpecification a b = a == b

else

instance passesSpecification'Record ::
  ( RL.RowToList inputRow inputRowList
  , RL.RowToList inputOrPredicateRow inputOrPredicateRowList
  , PassesSpecificationRecord inputRowList inputOrPredicateRowList inputRow inputOrPredicateRow
  ) =>
  PassesSpecification (Record inputRow) (Record inputOrPredicateRow) where
  passesSpecification = passesSpecificationRecord (RLProxy :: RLProxy inputRowList) (RLProxy :: RLProxy inputOrPredicateRowList)

else

instance passesSpecification'Generic ::
  ( Generic input inputRep
  , Generic inputOrPredicate inputOrPredicateRep
  , PassesSpecificationGeneric inputRep inputOrPredicateRep
  ) =>
  PassesSpecification input inputOrPredicate where
  passesSpecification input inputOrPredicate = passesSpecificationGeneric (from input) (from inputOrPredicate)

shouldPassSpecification
  :: forall input inputOrPredicate m
   . PassesSpecification input inputOrPredicate
  => MonadThrow Error m
  => Show input
  => input
  -> inputOrPredicate
  -> m Unit
shouldPassSpecification input inputOrPredicate =
  when (not $ passesSpecification input inputOrPredicate) $
    fail $ "Doesn't pass spec: " <> show input

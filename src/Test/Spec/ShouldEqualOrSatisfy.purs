module Test.Spec.ShouldEqualOrSatisfy where

import Protolude

import Data.Array as Array
import Data.Foldable (all)
import Data.Generic.Rep (Argument(..), Constructor(..), NoArguments, NoConstructors, Product(..), Sum(..), from)
import Data.Predicate (Predicate(..))
import Data.Tuple (curry)
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Test.Spec.Assertions (fail)
import Type.Prelude (class IsSymbol, RLProxy(RLProxy), SProxy(SProxy))

class EqualOrSatisfiesGeneric input inputOrPredicate | inputOrPredicate -> input where
  equalOrSatisfiesGeneric :: input -> inputOrPredicate -> Boolean

instance equalOrSatisfiesGeneric'NoConstructors :: EqualOrSatisfiesGeneric NoConstructors NoConstructors where
  equalOrSatisfiesGeneric _ _ = true

instance equalOrSatisfiesGeneric'NoArguments :: EqualOrSatisfiesGeneric NoArguments NoArguments where
  equalOrSatisfiesGeneric _ _ = true

instance equalOrSatisfiesGeneric'Sum :: (EqualOrSatisfiesGeneric a a', EqualOrSatisfiesGeneric b b') => EqualOrSatisfiesGeneric (Sum a b) (Sum a' b') where
  equalOrSatisfiesGeneric (Inl a1) (Inl a2) = equalOrSatisfiesGeneric a1 a2
  equalOrSatisfiesGeneric (Inr b1) (Inr b2) = equalOrSatisfiesGeneric b1 b2
  equalOrSatisfiesGeneric _ _ = false

instance equalOrSatisfiesGeneric'Product :: (EqualOrSatisfiesGeneric a a', EqualOrSatisfiesGeneric b b') => EqualOrSatisfiesGeneric (Product a b) (Product a' b') where
  equalOrSatisfiesGeneric (Product a1 b1) (Product a2 b2) = equalOrSatisfiesGeneric a1 a2 && equalOrSatisfiesGeneric b1 b2

instance equalOrSatisfiesGeneric'Constructor :: EqualOrSatisfiesGeneric a a' => EqualOrSatisfiesGeneric (Constructor name a) (Constructor name a') where
  equalOrSatisfiesGeneric (Constructor a1) (Constructor a2) = equalOrSatisfiesGeneric a1 a2

instance equalOrSatisfiesGeneric'Nested :: EqualOrSatisfies a b => EqualOrSatisfiesGeneric (Argument a) (Argument b) where
  equalOrSatisfiesGeneric (Argument a) (Argument b) = equalOrSatisfies a b

---------------

class EqualOrSatisfiesRecord
  (inputRowList :: RL.RowList Type)
  (inputOrPredicateRowList :: RL.RowList Type)
  (inputRow :: Row Type)
  (inputOrPredicateRow :: Row Type)
  | inputRowList -> inputRow
  , inputOrPredicateRowList -> inputOrPredicateRow inputRowList inputRow
  where
  equalOrSatisfiesRecord
    :: RLProxy inputRowList
    -> RLProxy inputOrPredicateRowList
    -> Record inputRow
    -> Record inputOrPredicateRow
    -> Boolean

instance equalOrSatisfiesRecord'Cons ::
  ( IsSymbol name
  , EqualOrSatisfiesRecord inputRowList'Tail inputOrPredicateRowList'Tail inputRow inputOrPredicateRow
  , EqualOrSatisfies inputRowList'Val inputOrPredicateRowList'Val
  , Row.Cons name inputRowList'Val trash1 inputRow
  , Row.Cons name inputOrPredicateRowList'Val trash2 inputOrPredicateRow
  ) => EqualOrSatisfiesRecord
  (RL.Cons name inputRowList'Val inputRowList'Tail)
  (RL.Cons name inputOrPredicateRowList'Val inputOrPredicateRowList'Tail)
  inputRow
  inputOrPredicateRow
  where
  equalOrSatisfiesRecord _ _ input inputOrPredicate = equalOrSatisfies a b && rest
    where
      a :: inputRowList'Val
      a = Record.get (SProxy :: SProxy name) input

      b :: inputOrPredicateRowList'Val
      b = Record.get (SProxy :: SProxy name) inputOrPredicate

      rest :: Boolean
      rest = equalOrSatisfiesRecord (RLProxy :: RLProxy inputRowList'Tail) (RLProxy :: RLProxy inputOrPredicateRowList'Tail) input inputOrPredicate

instance equalOrSatisfiesRecord'Nil :: EqualOrSatisfiesRecord RL.Nil RL.Nil inputRow inputOrPredicateRow where
  equalOrSatisfiesRecord _ _ _ _ = true

---------------

class EqualOrSatisfies input inputOrPredicate where
  equalOrSatisfies :: input -> inputOrPredicate -> Boolean

instance equalOrSatisfies'Predicate :: EqualOrSatisfies a (Predicate a) where
  equalOrSatisfies a (Predicate b) = b a

else

instance equalOrSatisfies'Eq :: Eq a => EqualOrSatisfies a a where
  equalOrSatisfies a b = a == b

else

instance equalOrSatisfies'Array :: EqualOrSatisfies a b => EqualOrSatisfies (Array a) (Array b) where
  equalOrSatisfies as bs =
    if Array.length as == Array.length bs
      then all (uncurry equalOrSatisfies) $ Array.zip as bs
      else false

else

instance equalOrSatisfies'Record ::
  ( RL.RowToList inputRow inputRowList
  , RL.RowToList inputOrPredicateRow inputOrPredicateRowList
  , EqualOrSatisfiesRecord inputRowList inputOrPredicateRowList inputRow inputOrPredicateRow
  ) =>
  EqualOrSatisfies (Record inputRow) (Record inputOrPredicateRow) where
  equalOrSatisfies = equalOrSatisfiesRecord (RLProxy :: RLProxy inputRowList) (RLProxy :: RLProxy inputOrPredicateRowList)

else

instance equalOrSatisfies'Generic ::
  ( Generic input inputRep
  , Generic inputOrPredicate inputOrPredicateRep
  , EqualOrSatisfiesGeneric inputRep inputOrPredicateRep
  ) =>
  EqualOrSatisfies input inputOrPredicate where
  equalOrSatisfies input inputOrPredicate = equalOrSatisfiesGeneric (from input) (from inputOrPredicate)

shouldEqualOrSatisfy
  :: forall input inputOrPredicate m
   . EqualOrSatisfies input inputOrPredicate
  => MonadThrow Error m
  => Show input
  => input
  -> inputOrPredicate
  -> m Unit
shouldEqualOrSatisfy input inputOrPredicate =
  when (not $ equalOrSatisfies input inputOrPredicate) $
    fail $ "Doesn't pass spec: " <> show input

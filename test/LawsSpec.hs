{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module LawsSpec where

import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Functor.Identity
import           Test.Hspec
import           Test.Hspec.Checkers
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

import           BoardProcessor           (GameApp)
import           Generators

instance Eq (GameApp String)  where
  (==) a b =
    let aResult = execWriter $ evalStateT a ""
        bResult = execWriter $ evalStateT b ""
     in aResult == bResult

instance Show (GameApp String) where
  show a = mconcat $ execWriter $ evalStateT a ""

instance Arbitrary (GameApp String) where
  arbitrary = do
    a <- arbitrary
    return $ StateT $ \s -> WriterT $ Identity (((), s), [a])

instance EqProp (GameApp String) where
  (=-=) = eq

spec :: Spec
spec = testBatch (monoid (undefined :: GameApp String))

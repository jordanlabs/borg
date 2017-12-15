{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module LawsSpec where

import           Control.Monad.State      (StateT (StateT), evalStateT)
import           Control.Monad.Writer     (WriterT (WriterT), execWriter)
import           Data.Functor.Identity    (Identity (Identity))
import           Test.Hspec               (Spec)
import           Test.Hspec.Checkers      (testBatch)
import           Test.QuickCheck          (Arbitrary, arbitrary)
import           Test.QuickCheck.Checkers (EqProp, eq, (=-=))
import           Test.QuickCheck.Classes  (monoid)

import           BoardProcessor           (GameApp)

instance Eq (GameApp String) where
  (==) a b =
    let aResult = execWriter $ evalStateT a ""
        bResult = execWriter $ evalStateT b ""
    in  aResult == bResult

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

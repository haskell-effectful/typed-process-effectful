{-# LANGUAGE RankNTypes #-}

module Utils (
    assertBool,
    assertEqual,
    assertFailure,
    assertException,
) where

import GHC.Stack
import qualified Test.Tasty.HUnit as T

import Control.Exception
import Effectful

assertBool :: (HasCallStack, IOE :> es) => String -> Bool -> Eff es ()
assertBool msg p = liftIO $ T.assertBool msg p

assertEqual
    :: (HasCallStack, Eq a, Show a, IOE :> es)
    => String
    -> a
    -> a
    -> Eff es ()
assertEqual msg expected given = liftIO $ T.assertEqual msg expected given

assertFailure :: (HasCallStack, IOE :> es) => String -> Eff es a
assertFailure msg = liftIO $ T.assertFailure msg

assertException
    :: forall e es a
     . (HasCallStack, Exception e, IOE :> es)
    => IO a
    -> (e -> Bool)
    -> Eff es ()
assertException action expectedClassifier = do
    r <- liftIO $ try action
    case r of
        Right _ ->
            assertFailure "The action did not raise an exception"
        Left e ->
            assertBool "The expected exception doesn't match what was given" (expectedClassifier e)

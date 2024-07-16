{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Effectful
import Effectful.Temporary
import System.IO.Error (isDoesNotExistError)
import qualified Utils as U

import Effectful.Process.Typed
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (Assertion, testCase)

main :: IO ()
main =
    defaultMain $
        testGroup
            "typed-process-effectful"
            [ testCase "Existing executable" testExistingExecutable
            , testCase "Non-existent executable" testNonExistentExecutable
            , testCase "Continue process execution" testContinueProcessExecution
            , testCase "Terminate process" testTerminateProcess
            , testCase "Wait for process" testWaitForProcess
            , testCase "Helper functions" testHelperFunctions
            , testCase "Exit Codes" testExitCodes
            ]

testExistingExecutable :: Assertion
testExistingExecutable = runEff $ do
    let
        pc = proc "/usr/bin/env" ["true"]
        action = runTypedProcess $ do
            p <- startProcess pc
            waitExitCode p
    result <- action
    U.assertEqual "Executable exists" ExitSuccess result

testNonExistentExecutable :: Assertion
testNonExistentExecutable = runEff $ do
    let
        pc = proc "/bin/doesnotexist" []
    let
        action = runTypedProcess $ do
            p <- startProcess pc
            waitExitCode p
    U.assertException (runEff action) isDoesNotExistError

testContinueProcessExecution :: Assertion
testContinueProcessExecution = runEff $ do
    let
        pc = shell "sleep 3"
        action = runTypedProcess $ do
            p <- startProcess pc
            getExitCode p
    result <- action
    U.assertEqual "" result Nothing

testTerminateProcess :: Assertion
testTerminateProcess = runEff $ do
    let
        action = runTemporary . runTypedProcess $ do
            withSystemTempFile "typed-process-effectful-test" $ \fp h -> do
                let
                    pc =
                        setStdout (useHandleClose h) $
                            shell "sleep 1; printf 'Output'"
                withProcessTerm pc (const $ pure ())
                liftIO $ readFile fp
    result <- action
    U.assertEqual "" "" result

testWaitForProcess :: Assertion
testWaitForProcess = runEff $ do
    let
        action = runTemporary . runTypedProcess $ do
            withSystemTempFile "typed-process-effectful-test" $ \fp h -> do
                let
                    pc =
                        setStdout (useHandleClose h) $
                            shell "sleep 1; printf 'Output'"
                withProcessWait pc (const $ pure ())
                liftIO $ readFile fp
    result <- action
    U.assertEqual "" result "Output"

testHelperFunctions :: Assertion
testHelperFunctions = runEff $ do
    let
        pc = proc "/usr/bin/env" ["true"]
        action = runTypedProcess $ do
            runProcess pc
    result <- action
    U.assertEqual "" result ExitSuccess

    let
        pc2 = proc "/usr/bin/env" ["true"]
    result2 <- runTypedProcess $ do
        runProcess_ pc2
    U.assertEqual "" result2 ()

    let
        pc3 = shell "printf 'stdout'; printf 'stderr' >&2"
    result3 <- runTypedProcess $ do
        readProcess pc3
    U.assertEqual "" result3 (ExitSuccess, "stdout", "stderr")

    let
        pc4 = shell "printf 'stdout'; printf 'stderr' >&2"
    result4 <- runTypedProcess $ do
        readProcess_ pc4
    U.assertEqual "" result4 ("stdout", "stderr")

    let
        pc5 = shell "printf 'Output'"
    result5 <- runTypedProcess $ do
        readProcessStdout pc5
    U.assertEqual "" result5 (ExitSuccess, "Output")

    let
        pc6 = shell "printf 'Output'"
    result6 <- runTypedProcess $ do
        readProcessStdout pc6
    U.assertEqual "" result6 (ExitSuccess, "Output")

    let
        pc7 = shell "printf 'Output'"
    result7 <- runTypedProcess $ do
        readProcessStdout_ pc7
    U.assertEqual "" result7 "Output"

    let
        pc8 = shell "printf 'Output' >&2"
    result8 <- runTypedProcess $ do
        readProcessStderr pc8
    U.assertEqual "" result8 (ExitSuccess, "Output")

testExitCodes :: Assertion
testExitCodes = runEff $ do
    let
        pc = proc "/usr/bin/env" ["false"]
        action = runEff . runTypedProcess $ do
            runProcess_ pc
    U.assertException action (const @_ @ExitCodeException True)

    let
        pc2 = proc "/usr/bin/env" ["false"]
        action2 = runEff . runTypedProcess $ do
            readProcess_ pc2
    U.assertException action2 (const @_ @ExitCodeException True)

    let
        pc3 = proc "/usr/bin/env" ["false"]
        action3 = runEff . runTypedProcess $ do
            readProcessStdout_ pc3
    U.assertException action3 (const @_ @ExitCodeException True)

    let
        pc4 = proc "/usr/bin/env" ["false"]
        action4 = runEff . runTypedProcess $ do
            readProcessStderr_ pc4
    U.assertException action4 (const @_ @ExitCodeException True)

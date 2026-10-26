{-
You aren't required to edit this file, but do feel free to take a look. You 
could add some tests, if you think of new ones and are able to figure out the
format that they should be specified in.

Tasty is the testing library that is used to specify tests.
The backends "tasty-hunit" and "tasty-quickcheck" specify the way that unit 
tests and property tests (respectively) are written.
-}
module Main where

-- The Tasty testing library, and some of its backend data
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck hiding (Failure, Success)
import Test.Tasty.Ingredients.Basic
import Test.Tasty.Runners
import Test.Tasty.Options
import Test.Tasty.Ingredients.ConsoleReporter

-- Modules related to interacting with the computer
import System.Exit (exitSuccess)
import System.Console.ANSI (clearScreen)
import System.IO
import System.IO.Temp
import GHC.IO.Handle
import Control.Exception

-- Modules related to parsing and working with strings
import Control.Monad ( forM_ )
import Control.Applicative (liftA2)
import Data.Proxy
import Text.Megaparsec (parse, errorBundlePretty, eof)
import Data.Char (isSpace)
import Text.Regex.TDFA
import Data.List (dropWhileEnd)
import Data.List.Split (splitOn)

-- Modules from this project
import LGPT.TUI qualified as TUI
import LGPT.Numbers
import qualified Data.Text as T
import LGPT.Helpers (prompt)
import Control.Monad.State
import qualified Data.Map as Map


main :: IO ()
main = do
  clearScreen

  [o1, o2, o3, o4, o6a, o6b, o6c, o7a, o7b, o8a, o8b] <- mapM
    getλGPTResults
    [ ["Hello"]
    , ["What day is it?"]
    , ["What day is it tomorrow?"]
    , ["How long ago was 2026-02-14?"]
    , ["What is two plus three times four?"]
    , ["What is one hundred minus fifty?"]
    , ["What is ten times ten plus ten?"]
    , ["Remember that the sky is blue.", "Tell me about the sky."]
    , ["Tell me about computers."]
    , ["What is that plus two?"]
    , ["What is two plus three?", "What is that times that?"]
    ]

  let
    tests = testGroup "Tests"
      [
        -- longformNumberTests
        -- , longformPropertyTest
        testGroup "Part Zero" [
          testCase "Exercise 1: Hello" $ do
            checkExercise o1 ["Hi there!"]
        ]
      , testGroup "Part One" [
          testCase "Exercise 2: What day is it?" $
            checkExercise o2 ["Today is [MTWFS][a-z]+\\."]
        , testCase "Exercise 3: What day is it tomorrow?" $
            checkExercise o3 ["Tomorrow is [MTWFS][a-z]+\\."]
        , testCase "Exercise 4: How long ago was 2026-02-14?" $
            checkExercise o4 ["2026-02-14 was [0-9]+ days ago\\."]
        ]
      , testGroup "Part Two" [
          testCase "Exercise 6: What is two plus three times four?" $
            checkExercise o6a ["The answer is twenty."]
        , testCase "Exercise 6: What is one hundred minus fifty?" $
            checkExercise o6b ["The answer is fifty."]
        , testCase "Exercise 6: What is ten times ten plus ten?" $
            checkExercise o6c ["The answer is one hundred and ten."]
        ]
      , testGroup "Part Three" [
          testCase "Exercise 7: Remember that the sky is blue. / Tell me about the sky." $
            checkExercise o7a
            ["Okay\\.","Sure - the sky is blue\\."]
        , testCase "Exercise 7: Tell me about computers." $
            checkExercise o7b
              ["Sorry, I don't know anything about computers\\."]
        , testCase "Exercise 8: What is that plus two?" $
            checkExercise o8a
              ["I haven't evaluated anything yet\\."]
        , testCase "Exercise 8: What is two plus three? / What is that times that?" $
            checkExercise o8b
              ["The answer is five\\.","The answer is twenty-five\\."]
        ]
      ]

  defaultMainWithIngredients [ listingTests, consoleTestReporterQuiet ] tests


-- | Get the output of running λGPT with a given list of input lines.
getλGPTResults :: [String] -> IO String
getλGPTResults input = runWithInput (evalStateT TUI.runREPL Map.empty) $ unlines input


-- | Check that λGPT gives back output matching a given regular expression, for a given input.
checkExercise :: String -> [String] -> IO ()
checkExercise output expectedRegexes = do
  forM_ expectedRegexes $ \expectedRegex -> do
    if output =~ expectedRegex
    then pure ()
    else assertFailure $
      "Expected output to match this regex:\n" ++ unlines expectedRegexes ++
      "\nFull output:\n" ++
        -- remove occurrences of the prompt from the output
        -- remove occurrences of the prompt from the output
        -- remove occurrences of the prompt from the output
        -- remove occurrences of the prompt from the output
        -- remove occurrences of the prompt from the output
        -- remove occurrences of the prompt from the output
        -- remove occurrences of the prompt from the output
        -- remove occurrences of the prompt from the output
        -- remove occurrences of the prompt from the output
        -- remove occurrences of the prompt from the output
        -- remove occurrences of the prompt from the output
        -- remove occurrences of the prompt from the output
        -- remove occurrences of the prompt from the output
        -- remove occurrences of the prompt from the output
        -- remove occurrences of the prompt from the output
        -- remove occurrences of the prompt from the output
        T.unpack (T.replace (T.pack prompt) mempty $ T.pack output)


--------------------------------------------------------------------------------
-- Longhand number parsing tests (these were for my benefit but you're)
-- welcome to read them and see how it works :)

longformNumberTests :: TestTree
longformNumberTests = testGroup "Longform number tests"
  $ map (\(n, s) -> testCaseSteps (show n) $ \step ->  do
      printLonghand n @?= s
      case parse (parseLonghand <* eof) "<spec>" s of
        Left e -> assertFailure $ "Failed to parse: " ++ errorBundlePretty e
        Right v -> v @?= n
  ) testPairs


longformPropertyTest :: TestTree
longformPropertyTest = testGroup "Longform number properties"
  [ testProperty "Parsing the printed form gives back the original" $
      withMaxSuccess 1000
        $ forAll (choose (0, 1000000)) $ \n ->
        case parse (parseLonghand <* eof) "<spec>" (printLonghand n) of
          Left e -> counterexample ("Failed to parse: " ++ errorBundlePretty e) False
          Right v -> v === n
  , testProperty "Printing the parsed form gives back the original" $
      withMaxSuccess 1000
        $ forAll (choose (0, 1000000)) $ \n ->
        case parse (parseLonghand <* eof) "<spec>" (printLonghand n) of
          Left e -> counterexample ("Failed to parse: " ++ errorBundlePretty e) False
          Right v -> counterexample ("Parsed to " ++ show v) $ printLonghand v === printLonghand n
  ]


-- | Some example pairs of numbers and their longhand forms, for testing.
testPairs :: [(Int, String)]
testPairs =
  [ (0, "zero")
  , (1, "one")
  , (10, "ten")
  , (11, "eleven")
  , (20, "twenty")
  , (21, "twenty-one")
  , (99, "ninety-nine")
  , (100, "one hundred")
  , (101, "one hundred and one")
  , (111, "one hundred and eleven")
  , (999, "nine hundred and ninety-nine")
  , (1000, "one thousand")
  , (1001, "one thousand and one")
  , (1010, "one thousand and ten")
  , (1100, "one thousand one hundred")
  , (1111, "one thousand one hundred and eleven")
  , (9999, "nine thousand nine hundred and ninety-nine")
  , (10000, "ten thousand")
  , (100000, "one hundred thousand")
  , (100001, "one hundred thousand and one")
  , (100011, "one hundred thousand and eleven")
  , (100100, "one hundred thousand one hundred")
  , (100101, "one hundred thousand one hundred and one")
  , (1000000, "one million")
  ]


--------------------------------------------------------------------------------
-- Some helpers to make test results more readable (feel free to ignore these)

-- | Run the test reporter. 
-- Do not add any information about how to rerun failed tests.
consoleTestReporterQuiet :: Ingredient
consoleTestReporterQuiet = TestReporter ctrOptions
  $ \opts tree ->
    let TestReporter _ cb = consoleTestReporterWithHook simplifyErrors
    in cb opts tree
  where
    ctrOptions :: [OptionDescription]
    ctrOptions =
      [ Option (Proxy :: Proxy Quiet)
      , Option (Proxy :: Proxy HideSuccesses)
      , Option (Proxy :: Proxy UseColor)
      , Option (Proxy :: Proxy AnsiTricks)
      ]


-- | Remove error call stacks and quickcheck replay information from 
-- test failure error messages.
simplifyErrors :: [TestName] -> Result -> IO Result
simplifyErrors tests res = pure $ case resultOutcome res of
  Success -> res
  Failure {} -> res { resultDescription = dropWhileEnd isSpace rd'' }
    where
      (rd'' : _)
        = splitOn "Use --quickcheck-replay"
        rd'
      (rd' : _)
          = splitOn "CallStack (from HasCallStack):"
          $ resultDescription res


-- | Replace standard input and output for the duration of the given action.
-- This is quite fragile, but we can avoid problems by evaluating the IO actions
-- before any of the tests.
redirect :: IO () -> String -> String -> IO ()
redirect action inputFileName outputFileName = do
  withFile inputFileName ReadMode $ \hIn ->
    withFile outputFileName WriteMode $ \hOut ->
      bracket
        (liftA2 (,) (hDuplicate stdin) (hDuplicate stdout))
        (\(old_stdin, old_stdout) ->
           (hDuplicateTo old_stdin stdin >> hDuplicateTo old_stdout stdout)
           `finally`
           (hClose old_stdin >> hClose old_stdout))
        (\_ ->
           do
             hDuplicateTo hIn stdin
             hDuplicateTo hOut stdout
             action `catch` (\(_::SomeException) -> pure ()))


-- | Run an IO operation with the given string as provided over standard input.
-- Returns all of standard output.
runWithInput :: IO () -> String -> IO String
runWithInput action input = do
  inputFileName <- writeSystemTempFile "input.txt" input
  outputFileName <- emptySystemTempFile "output.txt"
  redirect action inputFileName outputFileName
  readFile outputFileName

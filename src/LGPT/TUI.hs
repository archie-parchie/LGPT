{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Used otherwise as a pattern" #-}
{-# LANGUAGE OverloadedStrings #-}

module LGPT.TUI where

{-
This file is the main entry point to your coursework.

You can create or modify any files in src/ as much as you like. The 
code that is included here is a good starting point, but you don't need to 
keep it if you don't want to.
-}
import Configuration.Dotenv (loadFile, defaultConfig)
import Control.Lens
import Control.Monad
import Control.Monad.State
import Control.Monad.Combinators
import Control.Monad.Combinators.Expr
import Data.Aeson (Value, toJSON)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Lens (key, _Array, _Object)
import Data.Aeson.Text (encodeToLazyText)
import Data.ByteString.Internal as BSI (packChars)
import Data.Char as Char (isDigit)
import Data.Foldable as Foldable (toList)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text.Lazy as Text (unpack)
import Data.Time (parseTimeM, defaultTimeLocale, ZonedTime (zonedTimeToLocalTime), LocalTime (localDay), getZonedTime)
import Data.Time.Calendar
import LGPT.Helpers (Parser, prompt, runStart)
import LGPT.Numbers (parseLonghand, printLonghand)
import qualified Network.Wreq as Wreq
import System.Environment (getEnv)
import Text.Megaparsec
import Text.Megaparsec.Char


--------------------------------------------------------------------------------
{- | Our program. It runs a loop which:
      1. Reads a line of input
      2. Parses it into a structured Request
      3. Does something based on that request (normally printing something out).
-}
runREPL :: StateT (Map String String) IO ()
runREPL = forever $ do
  lift $ putStr prompt
  req <- lift getLine
  x <- get -- Getting the current contents of the state to pass to the readRequest function.
  respondTo (readRequest x req)


--------------------------------------------------------------------------------
-- Parsing and responding to requests:


-- | Our request type, the result of parsing a string.
data Request = Unknown | Hello | Weekday String | Date Day | Arithmetic Int | NoResultError | Remember String String | Recall String  | Forget String | Tram String String
  deriving (Eq, Ord, Show)


{- | Read a request. 

    This runs the parse function from Megaparsec, and
    converts any failed parses into an Unknown request.
-}
readRequest :: Map String String -> String -> Request
readRequest map str = case parse (parseRequest map) "<stdin>" str of -- The fetched contents of the state are passed on to the parser.
  Left  err -> Unknown -- If all parsers fail, readRequest uses the Unknown constructor to indicate that the string entered by the user has not been recognised.
  Right req -> req -- If any parser succeeds, it creates a Request using the appropriate constructor.


-- | Currently, the only thing λGPT understands is "Hello"...
parseRequest :: Map String String -> Parser Request
parseRequest map = choice -- parseRequest tries each of the parsers defined below in turn, backtracking if one does not succeed, then trying the next 
  [ try (string "Hello" >> pure Hello)                                                                                           -- on the same input.
  , try parseDates
  , try parseArithmetic
  , try parseNoResult -- This parser is tried after parseArithmetic to ensure that it only parses queries of the form "What is ... that ..." if 
  , try parseRemember                                                   -- parseArithmetic fails, i.e. if no expression has been evaluated yet.
  , try parseRecall
  , try parseForget
  , try parseTram
  ]
  where
    parseDates :: Parser Request -- Parses date-related queries as given in Part One of the specification.
    parseDates = choice [ string "What day is it?" >> pure (Weekday "today") -- Depending on the day asked for, a different string is passed to the Weekday
                        , string "What day is it tomorrow?" >> pure (Weekday "tomorrow") -- constructor, so that the different queries can be responded to
                        , do                                                      -- with the appropriate output based on pattern-matching with the string.
                          string "How long ago was "
                          x <- takeWhileP (Just "date") (/= '?') -- Parsing the string until a question mark is reached.
                          let date = parseTimeM True defaultTimeLocale "%Y-%m-%d" x -- Checking if the parsed string is of the right form - if yes, then
                          case date of                                                                                     -- this is converted to a Day.
                            Just res -> pure (Date res) -- If a date is parsed succesfully, this is passed into the Date constructor.
                            Nothing -> fail "Invalid date" -- Else, the parser fails and another is tried.
                        ]
    parseArithmetic :: Parser Request -- Parses arithmetic expressions, as given in Part Two and Exercise 8 of the specificiation.
    parseArithmetic = do
                      string "What is "
                      x <- choice [expr] -- Parsing and evaluating the arithmetic expression.
                      char '?'
                      pure (Arithmetic x) -- If a number is evaluated, this is passed into the Arithmetic constructor.
                      where
                        expr = makeExprParser term table -- Creating an expression parser using the makeExprParser function.
                        term = choice [parseLonghand, parseThat map] -- Parsing individual terms, in this case longhand numbers, using the given parser, or the word "that"
                        table = [[InfixL ((+) <$ string " plus ") -- Creating an operator table, with all operators given the same fixity and implementing
                                , InfixL ((*) <$ string " times ")                                                                  -- left-associativity.
                                , InfixL ((-) <$ string " minus ")
                                , InfixL (div <$ string " div ")
                                , InfixL (mod <$ string " mod ")
                                , Prefix (negate <$ string "negative ")]]
                        parseThat map = do -- Parses the word "that" and finds the result of the last evaluated expression to be used by makeExpreParser
                                        string "that"
                                        let x = Map.lookup "EVAL" map -- Using the current contents of the state that have been passed to the parser to find
                                        case x of                                                            -- the result of the last evaluated expression.
                                          Just n -> pure (read n) -- Returning the result as an integer if it exists.
                                          Nothing -> fail "No evaluation yet" -- If no expression has been evaluated yet, the parser fails - parseNoResult will run and deal with this case.
    parseNoResult :: Parser Request -- Parses queries of the form "What is ... that ..." when no expression has been evaluted yet, as given in Exercise 8 of the specification.
    parseNoResult = do
                    string "What is "
                    x <- manyTill anySingle (string "that") -- Parsing all characters until the string "that" is parsed.
                    let x = Map.lookup "EVAL" map -- Using the current contents of the state that have been passed to the parser to find the result of the last
                    case x of                                                                                                          -- evaluated expression.
                      Nothing -> pure NoResultError -- If no expression has been evaluated yet, a NoResultError is created, which later leads to the relevant message being printed.
                      Just n -> fail "Not a valid arithmetic expression" -- If "that" has been parsed, parseArithmetic has failed, and an expression has already been evaluated, then the request must not be a valid one, so the parser fails.
    parseRemember :: Parser Request -- Parses requests to store information in the chatbot's memory, as given in Exercise 7 of the specification.
    parseRemember = do
                    string "Remember that "
                    x <- manyTill anySingle (string " is ") -- Parsing all characters until the string " is " is parsed. This is the name of the object, which will become the key of the key-value pair when evaluated.
                    y <- takeWhile1P (Just "thing") (/= '.') -- Parsing the piece of information to remember about the object - this will become the value of the key-value pair when evaluated.
                    pure (Remember x y) -- The key and value are passed into the Remember constructor to be later added to the state.
    parseRecall :: Parser Request -- Parses requests to recall information about a certain item, as given in Exercise 7 of the specification.
    parseRecall = do
                  string "Tell me about "
                  x <- takeWhile1P (Just "name") (/= '.') -- Parses the name of the object, about which information should be recalled.
                  pure (Recall x) -- Passing this name to the Recall constructor to later check if the chatbot knows anything about this object.
    parseForget :: Parser Request -- Parses requests to forget information in the chatbot's memory, either about a specific item or all currently
    parseForget = do                                                                                                   -- remembered information.
                  string "Forget "
                  choice [string "everything", string "about "]
                  x <- takeWhileP (Just "name") (/= '.') -- Parses either just a full stop, indicating that everything should be forgotten, or the name of an item, about which information should be forgotten.
                  pure (Forget x) -- Either an empty string or an item name is passed to the Forget contructor. The appropriate information is forgotten when responding to the request depending on which of these is passed.
    parseTram :: Parser Request -- Parses requests to find out when the next tram from a given station to a specified final stop will arrive.
    parseTram = do
                string "When is the next tram from "
                station <- manyTill anySingle (string " towards ") -- Parses the string until the string " towards " is encountered - this value is treated as the station from which departures are being checked.
                terminal <- takeWhile1P (Just "terminal") (/= '?') -- Parses the terminal stop of the tram.
                pure (Tram station terminal) -- Passing the station and terminal to the Tram constructor to be used when filtering GEt request data.

-- | Respond to a request. This is where the behaviours of λGPT will go, but 
-- for now it just responds to "Hello".
respondTo :: Request -> StateT (Map String String) IO () -- This function pattern-matches based on the constructor of the Request data type to determine how the chatbot should respond to parsed queries. In some cases, further pattern-matching occurs on the data itself.
respondTo Unknown         = lift $ putStrLn "I don't understand that." -- If the Unknown constructor has been used, all parsers failed, so an error message is shown to the user.
respondTo Hello           = lift $ putStrLn "Hi there!" -- 'lift' is used across the function to access the IO monad and use its context within the transformer stack.
respondTo (Weekday x)     = do -- Finds the current UTC time, converts this to the current local time, then converts this back to a Day, which is used by the dayOfWeek function to then find the current day of the week.
                            weekday <- lift $ (dayOfWeek . localDay) . zonedTimeToLocalTime <$> getZonedTime
                            case x of -- Pattern matching on the string inside the constructor to determine what response should be given.
                              "today" -> lift $ putStrLn ("Today is " ++ show weekday ++ ".")
                              "tomorrow" -> lift $ putStrLn ("Tomorrow is " ++ show (succ weekday) ++ ".") -- Finds and displays the subsequent day of the week.
respondTo (Date x)        = do -- Finds the current UTC time, converts this to the current day in the local timezone.
                            today <- lift $ localDay . zonedTimeToLocalTime <$> getZonedTime
                            lift $ putStrLn (show x ++ " was " ++ show (diffDays today x) ++ " days ago.") -- Finds the difference between the parsed day and today.
respondTo (Arithmetic n)  = do
                            modify (Map.insert "EVAL" (show n)) -- Getting the old contents of the state, inserting the result of the calculation to this, then transforming the contents of the state to this updated Map.
                            lift $ putStrLn ("The answer is " ++ printLonghand n ++ ".") -- Converts the integer to a longhand number, then prints the output.
respondTo NoResultError   = lift $ putStrLn "I haven't evaluated anything yet." -- The NoResultError constructor triggers a unique error response indicating that the arithmetic expression failed due to referencing non-existant previous calculations, rather than the generic response indicating parser failure.
respondTo (Remember x y)  = do -- Takes the key and value pair representing the name and thing to be remembered by the chatbot, stores these, then responds to the user to let them know that their request has been successful.
                            modify (Map.insert x y) -- Getting the old contents of the state, inserting the new key-value pair, then transforming the contents of the state to this updated Map.
                            lift $ putStrLn "Okay."
respondTo (Recall x)      = do -- Checks if the chatbot currently knows anything about the item requested, and responds accordingly.
                            s <- get -- Getting the current value of the state from inside the monad.
                            let result = Map.lookup x s -- Checking if the name of the item provided by the user is the key of any key-value pairs - if so, 
                            case result of                                                                                            -- finding the value.
                              Just thing -> lift $ putStrLn ("Sure - " ++ x ++ " is " ++ thing ++ ".") -- If the value was found, the chatbot responds to the user with this information about the object.
                              Nothing -> lift $ putStrLn ("Sorry, I don't know anything about " ++ x ++ ".") -- If the key was not found in the Map, then no information about the item has been stored.
respondTo (Forget x)      = case x of -- Deletes the information requested if it is in the state and informs the user that this has been successful, otherwise informs the user that no information relating to the item specified is currently in memory, so cannot be forgotten.
                          ""         -> do -- If no specific item should be forgotten, every item should be forgotten.
                                        put Map.empty -- Replacing the Map inside the monad with an empty one.
                                        lift $ putStrLn "Okay, forgetting everything."
                          otherwise  -> do -- Forgetting information about a specific item, if information about that item exists in the state.
                                        s <- get -- Getting the current value of the state from inside the monad.
                                        let result = Map.lookup x s -- Checking if the item is part of a key-value pair stored in the Map.
                                        case result of
                                          Just thing -> do -- If yes, then this pair is deleted from the Map.
                                            lift $ putStrLn ("Sure - forgetting that " ++ x ++ " is " ++ thing ++ ".")
                                            put (Map.delete x s) -- Replacing the old state with an updated Map that excludes the information about the item that has been requested to be forgotten.
                                          Nothing -> lift $ putStrLn ("Sorry, I don't know anything about " ++ x ++ ".") -- If no such key-value pair exists, then the chatbot does not know anything about the item, so cannot forget it.
respondTo (Tram station terminal) = do
                          lift $ loadFile defaultConfig -- Loading the .env file.
                          apiKey <- lift $ getEnv "API_KEY" -- Fetching the API key from the .env file.
                          let opts = Wreq.defaults & Wreq.header ("Ocp-Apim-Subscription-Key") .~ [BSI.packChars apiKey] -- Setting the header values for the GET request.
                          apiCall <- lift $ Wreq.getWith opts "https://api.tfgm.com/odata/Metrolinks" -- Making the request. This is an IO() action, so needs to be lifted to a StateT (Map String String) IO () action.
                          let body = Foldable.toList (apiCall ^. Wreq.responseBody . key "value" . _Array) -- Accessing the body of the response, then extracting the Passenger Information Display (PID) data stored under the key "value", then converting this data from a Data.Aeson.Array to a list for usage in the rest of the function.
                          let values = map (^. _Object) body -- For each element of the list, the data is extracted from the Data.Aeson.Object it is stored in, giving a list where each element is a Data.Aeson.KeyMap representing one PID. The data can now be easily used in the rest of the function.
                          let pids = filter (\x -> fromMaybe "" (KeyMap.lookup "StationLocation" x) == (toJSON station)) values -- Filtering to only keep PIDs which show information about the starting station entered by the user.
                          let dest0 = filter (\x -> fromMaybe "" (KeyMap.lookup "Dest0" x) == (toJSON terminal)) pids -- Filtering to only keep PIDs if they show that the next tram arriving at the station is equal to the terminal station entered by the user.
                          if length dest0 /= 0 -- Checking if the next tram arriving at the station (in any direction) is equal to the terminal station entered by the user. If yes, then dest0 will hold the information of at least one PID (and possibly more, as platforms often have multiple PIDs showing the same information).
                            then do
                              let time = filter Char.isDigit (Text.unpack (encodeToLazyText (fromMaybe "" (KeyMap.lookup "Wait0" (dest0 !! 0))))) -- Finding the time (in minutes) until the next tram displayed on a relevant PID arrives at the station, then converting this to a usable string.
                              case time of
                                "1" -> lift $ putStrLn ("The next tram from " ++ station ++ " towards " ++ terminal ++ " is in " ++ (time) ++ " minute.")
                                otherwise -> lift $ putStrLn ("The next tram from " ++ station ++ " towards " ++ terminal ++ " is in " ++ (time) ++ " minutes.") -- The !! operator is used here, which is partial, but the if condition already ensures that the list has a value at index 0, so this will never throw an errpr.
                          else do -- If the next tram arriving at the station in any direction is not heading towards the terminal entered by the user, then the second tram to arrive is checked and the code above is repeated.
                            let dest1 = filter (\x -> fromMaybe "" (KeyMap.lookup "Dest1" x) == (toJSON terminal)) pids
                            if length dest1 /= 0
                              then do
                                let time = filter Char.isDigit (Text.unpack (encodeToLazyText (fromMaybe "" (KeyMap.lookup "Wait1" (dest1 !! 0)))))
                                case time of
                                  "1" -> lift $ putStrLn ("The next tram from " ++ station ++ " towards " ++ terminal ++ " is in " ++ (time) ++ " minute.")
                                  otherwise -> lift $ putStrLn ("The next tram from " ++ station ++ " towards " ++ terminal ++ " is in " ++ (time) ++ " minutes.")
                            else do -- If the second tram arriving at the station in any direction is not heading towards the terminal entered by the user, then the third tram to arrive is checked and the code above is repeated.
                              let dest2 = filter (\x -> fromMaybe "" (KeyMap.lookup "Dest2" x) == (toJSON terminal)) pids
                              if length dest2 /= 0
                                then do
                                  let time = filter Char.isDigit (Text.unpack (encodeToLazyText (fromMaybe "" (KeyMap.lookup "Wait2" (dest2 !! 0)))))
                                  case time of
                                    "1" -> lift $ putStrLn ("The next tram from " ++ station ++ " towards " ++ terminal ++ " is in " ++ (time) ++ " minute.")
                                    otherwise -> lift $ putStrLn ("The next tram from " ++ station ++ " towards " ++ terminal ++ " is in " ++ (time) ++ " minutes.")
                              else do -- If the third tram arriving at the station in any direction is not heading towards the terminal entered by the user, then the fourth tram to arrive is checked and the code above is repeated.
                                let dest3 = filter (\x -> fromMaybe "" (KeyMap.lookup "Dest3" x) == (toJSON terminal)) pids
                                if length dest3 /= 0
                                  then do
                                    let time = filter Char.isDigit (Text.unpack (encodeToLazyText (fromMaybe "" (KeyMap.lookup "Wait3" (dest3 !! 0)))))
                                    case time of
                                      "1" -> lift $ putStrLn ("The next tram from " ++ station ++ " towards " ++ terminal ++ " is in " ++ (time) ++ " minute.")
                                      otherwise -> lift $ putStrLn ("The next tram from " ++ station ++ " towards " ++ terminal ++ " is in " ++ (time) ++ " minutes.")
                                else lift $ putStrLn ("Sorry, there are currently no trams from " ++ station ++ " towards " ++ terminal ++ ".")
                                -- If none of the next four trams to arrive at the station in any direction are towards the terminal specified, then an appropriate error response is displayed.
                                -- The same message is displayed no matter if the stops entered by the user are invalid, if the terminal stop is invalid as it is not on the same line, or if the stations are valid but there are no trams currently due to a delay/shutdown of the network.
                                -- As no checks are being made against a list of all valid tram stops, for example, there is currently no way to differentiate between these types of errors.
-- This function contains sections of repeated code, however I believe that there is no better way to execute the required functionality. The API gives the
-- live information displayed at every PID on the network. These PIDs contain information about the next (up to) four trams to arrive at the platform that 
-- they are stationed at, using the "Dest0", "Dest1", "Dest2", and "Dest3" keys. When a user enters a station and terminal stop, the code first filters to 
-- only find the PIDs at the inputted station. The code then must check if any of these PIDs have the inputted terminal stop as the value as the "Dest0" 
-- value - if yes, then the waiting time corresponding to this under "Wait0" must be found. If not, then the second destination displayed on each PID at the  
-- inputted stop is checked to see if it matches the inputted terminal stop using the "Dest1" key. If not, this is repeated with "Dest2", and then "Dest3" -
-- only if all of these lookups fail, is the failure message displayed. As each of these lookups need to be done in turn, each using different lookup keys, I 
-- cannot think of a simple way to streamline this code and reduce repeats under the constraints of the required output type of the function.
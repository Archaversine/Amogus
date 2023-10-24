{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Monad.State

import Data.Allen
import Data.Bits
import Data.Functor (($>))
import Data.List (foldl', intercalate)
import Data.Maybe (fromJust)
import Data.Tuple (swap)
import Data.Void (Void)

import System.Environment (getArgs)
import System.IO.Unsafe (unsafePerformIO)

import Text.Megaparsec
import Text.Megaparsec.Char

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data AmogusState = AmogusState { intervalIDs :: Map String IntervalID
                               , stackOffset :: !StackOffset
                               , queries     :: ![QueryResult]
                               }

type StackOffset = Int

type Parser = Parsec Void String
type Amogus = StateT AmogusState Allen

data Event = LocalEvent  !String !StackOffset
           | GlobalEvent !String !StackOffset

data NamedEvent = NamedLocalEvent  !String !String !StackOffset 
                | NamedGlobalEvent !String !StackOffset

instance Show NamedEvent where 
    show (NamedGlobalEvent name modifier) = '#' : name <> show modifier
    show (NamedLocalEvent pName aName modifier) = pName <> "." <> aName <> show modifier

data Command = MakeList ![NamedEvent]
             | SetConstraint !NamedEvent ![Relation] !NamedEvent
             | Query !NamedEvent !NamedEvent
             | IncreaseStackOffset

instance Show Command where 
    show (MakeList events) = "list " <> unwords (map show events)
    show (SetConstraint e1 rel e2) = "set " <> show e1 <> " " <> show rel <> " " <> show e2
    show (Query e1 e2) = "query " <> show e1 <> ", " <> show e2
    show IncreaseStackOffset = "---"

data QueryResult = QueryResult String String [Relation]

instance Show QueryResult where 
    show (QueryResult e1 e2 rels) = heading <> intercalate "\n" rels'
        where heading = "QUERY (" <> e1 <> ", " <> e2 <> ")\n"
              rels' = map (\r -> "  - " <> show r) rels

toNamedEvent :: String -> Event -> NamedEvent 
toNamedEvent name (LocalEvent localName modifier) = NamedLocalEvent name localName modifier
toNamedEvent _ (GlobalEvent globalName modifier) = NamedGlobalEvent globalName modifier

globalEvent :: Parser Event 
globalEvent = GlobalEvent <$> (char '#' *> identifier) <*> numericModifier

localEvent :: Parser Event 
localEvent = LocalEvent <$> identifier <*> numericModifier

namedLocalEvent :: Parser NamedEvent 
namedLocalEvent = NamedLocalEvent <$> identifier <*> (char '.' *> identifier) <*> numericModifier

event :: Parser Event 
event = globalEvent <|> localEvent

namedEvent :: Parser NamedEvent 
namedEvent = namedLocalEvent <|> (toNamedEvent "" <$> globalEvent)

numericModifier :: Parser StackOffset
numericModifier = do 
    chars <- many (oneOf "+-")
    return $ foldl' (\acc c -> if c == '+' then acc + 1 else acc - 1) 0 chars

identifier :: Parser String 
identifier = do 
    first <- letterChar <|> char '_'
    rest  <- many (alphaNumChar <|> char '_')

    return (first : rest)

increaseStackOffsetP :: Parser Command
increaseStackOffsetP = string "---" $> IncreaseStackOffset

intervalSymbol :: Parser [Relation]
intervalSymbol = choice [ string "->"  $> [Precedes]
                        , string "->|" $> [Meets]
                        , string "=_"  $> [Overlaps]
                        , string "-=|" $> [FinishedBy]
                        , string "-=-" $> [Contains]
                        , string "=>_" $> [Starts]
                        , string "="   $> [Equals]
                        , string "=>-" $> [StartedBy]
                        , string "_=_" $> [During]
                        , string "_=|" $> [Finishes]
                        , string "_=-" $> [OverlappedBy]
                        , string "|->" $> [MetBy]
                        , string "<-"  $> [PrecededBy]
                        , string "/="  $> [Precedes, Meets, MetBy, PrecededBy]
                        , string "~"   $> [Overlaps, FinishedBy, Contains, Starts, Equals, StartedBy, During, Finishes, OverlappedBy]
                        , string "~>"  $> [Precedes, Meets, Overlaps]
                        , string "<~"  $> [MetBy, PrecededBy, OverlappedBy]
                        ]

makeList :: Parser Command 
makeList = do 
    name   <- string "list" *> hspace1 *> identifier
    events <- map (toNamedEvent name) <$> some (hspace1 *> event)

    return $ MakeList events

setConstraint :: Parser Command
setConstraint = do 
    event1 <- string "set" *> space1 *> namedEvent
    rel    <- space1 *> intervalSymbol
    event2 <- space1 *> namedEvent

    return $ SetConstraint event1 rel event2

query :: Parser Command 
query = do 
    event1 <- string "query" *> space1 *> namedEvent
    event2 <- char ',' *> space1 *> namedEvent

    return $ Query event1 event2

parseCommand :: Parser Command 
parseCommand = makeList <|> setConstraint <|> query <|> increaseStackOffsetP

parseCommands :: Parser [Command]
parseCommands = space *> parseCommand `sepBy1` space

----------------------------------------------------------

-- | Ensure that the string representation of an interval ID exists
-- Adds the new ID if it does not exist
-- Returns the ID
ensureExist :: String -> Amogus IntervalID
ensureExist name = do 
    ids <- gets intervalIDs

    case Map.lookup name ids of 
        Nothing -> do 
            newID <- lift interval
            modify $ \s -> s { intervalIDs = Map.insert name newID ids }
            return newID
        Just x -> return x

ensureConsistent :: Amogus (Maybe [IntervalID])
ensureConsistent = do 
    intervals <- lift get

    let inconsistent = [i | i <- map snd $ Map.toList intervals, 0 `elem` map snd (Map.toList $ intervalRelations i)]

    if null inconsistent then  
        return Nothing  
    else
        return $ Just $ map intervalID inconsistent

-- | Mangles the event into a unique string
mangleEvent :: StackOffset -> NamedEvent -> String
mangleEvent offset (NamedLocalEvent pName aName modifier) = pName <> "." <> aName <> show (offset + modifier)
mangleEvent offset (NamedGlobalEvent name modifier) = '#' : name <> show (offset + modifier)

createList :: StackOffset -> [NamedEvent] -> Amogus ()
createList offset events = do 
    let mangled = map (mangleEvent offset) events

    -- First element (typesafe version of head)
    forM_ (take 1 mangled) $ \name -> do 
        ensureExist name

    -- Remaining elements
    forM_ (zip (tail mangled) mangled) $ \(current, prev) -> do 
        currId <- ensureExist current
        ids    <- gets intervalIDs

        let prevId = fromJust $ Map.lookup prev ids

        lift $ assumeSet prevId [Precedes, Meets] currId

createConstraint :: StackOffset -> NamedEvent -> [Relation] -> NamedEvent -> Amogus ()
createConstraint offset e1 rel e2 = do 
    m1       <- ensureExist (mangleEvent offset e1)
    m2       <- ensureExist (mangleEvent offset e2)
    existing <- lift $ getConstraints m1 m2

    let relBits = relationUnion $ map toBits rel

    lift $ assumeBits m1 (relBits .&. existing) m2

getQuery :: StackOffset -> NamedEvent -> NamedEvent -> Amogus ()
getQuery offset e1 e2 = do 
    let m1 = mangleEvent offset e1 
        m2 = mangleEvent offset e2

    i1 <- ensureExist m1
    i2 <- ensureExist m2

    results <- lift $ fromBits <$> getConstraints i1 i2
    modify $ \s -> s { queries = queries s <> [QueryResult m1 m2 results] }

increaseStackOffset :: Amogus ()
increaseStackOffset = modify $ \s -> s { stackOffset = stackOffset s + 1 }

applyCommand :: Command -> Amogus ()
applyCommand command = do 
    offset <- gets stackOffset

    case command of 
        MakeList events         -> createList offset events
        SetConstraint e1 rel e2 -> createConstraint offset e1 rel e2
        Query e1 e2             -> getQuery offset e1 e2
        IncreaseStackOffset     -> increaseStackOffset

    inconsistent <- ensureConsistent 

    case inconsistent of 
        Nothing -> pure ()
        Just xs -> do 
            ids <- gets intervalIDs

            let idMap = Map.fromList $ map swap $ Map.toList ids
                badIntervals = map (idMap Map.!) xs

            unsafePerformIO $ do 
                putStrLn "[*] --== INCONSISTENCY ==--\n"
                mapM_ putStrLn badIntervals
                putStrLn "\n[*] --== INCONSISTENCY ==--"

                errorWithoutStackTrace $ "On Command: " <> show command

applyCommands :: [Command] -> Amogus () 
applyCommands = mapM_ applyCommand

runCommands :: [Command] -> IO ()
runCommands commands = do
    let initialState = AmogusState { intervalIDs = Map.empty
                                   , stackOffset = 0
                                   , queries     = []
                                   }

    let result = evalAllen $ execStateT (applyCommands commands) initialState
    mapM_ print (queries result)

main :: IO ()
main = getArgs >>= \case
        [filename] -> do 
            contents <- init <$> readFile filename 

            case parse parseCommands filename contents of 
                Left err -> errorWithoutStackTrace $ errorBundlePretty err
                Right x  -> runCommands x
        _ -> errorWithoutStackTrace "Usage: amogus <filename>"
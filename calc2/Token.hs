
module Token (Token(..)
             ,tokenize
             ,dropSpaceToks
             ,trimSpaceToks
             ,tokenException
             )
       where

import Compose

import Except (Excepted, Exception(..), throw)

import Data.List (elemIndices)
import Symbols (allowedSymbols)


data Token
  = TnWord Int String         -- a word token
  | TnNumber Int String       -- a number token
  | TnSymbol Int Char         -- a symbol token
  | TnCommand Int String      -- a command token
  | TnSpace Int               -- a space token


tokenize :: String -> Excepted [Token]
tokenize str
  = do let ls1 = splitLines str              -- [String]
       let ls2 = addLineNumbers ls1          -- [(Int, String)]
       let ls3 = map removeComment ls2       -- [(Int, String)]
       ls4 <- mapM toTokens ls3              -- [[Token]]
       let ts1 = concat ls4                  -- [Token]
       let ts2 = consolidateSpaces ts1       -- [Token]
       ts3 <- resolveCommands ts2            -- [Token]
       return ts3


splitLines :: String -> [String]
splitLines
  = lines .> map (++ " ")      -- trade newline character for a space


addLineNumbers :: [String] -> [(Int, String)]
addLineNumbers
  = zip [1..]


removeComment :: (Int, String) -> (Int, String)
removeComment (ln@(_, str))
  = removeComment' (elemIndices '%' str) ln

removeComment' :: [Int] -> (Int, String) -> (Int, String)
removeComment' [] ln
  = ln
removeComment' (k : ks) (n, str)
  | wasUnescaped proposed  = (n, proposed)
  | otherwise              = removeComment' ks (n, str)
  where proposed = take k str


wasUnescaped :: String -> Bool
wasUnescaped
  = reverse .> takeWhile (== '\\') .> length .> even


toTokens :: (Int, String) -> Excepted [Token]
toTokens (_, [])
  = return []
toTokens (n, c : cs)
  | isLetter c     = do let (before, after) = span isLetter (c : cs)
                        tokens <- toTokens (n, after)
                        return (TnWord n before : tokens)
  | isDigit c      = do let (before, after) = span isDigit (c : cs)
                        tokens <- toTokens (n, after)
                        return (TnNumber n before : tokens)
  | isSymbol c     = do tokens <- toTokens (n, cs)
                        return (TnSymbol n c : tokens)
  | isSpace c      = do tokens <- toTokens (n, cs)
                        return (TnSpace n : tokens)
  | otherwise      = throw (ExceptLineMessage n
                            ("Unknown character: " ++ [c] ++ "."))


isLetter :: Char -> Bool
isLetter c
  = c `elem` ['a'..'z'] ++ ['A'..'Z']

isDigit :: Char -> Bool
isDigit c
  = c `elem` ['0'..'9']

isSymbol :: Char -> Bool
isSymbol c
  = c `elem` allowedSymbols

isSpace :: Char -> Bool
isSpace c
  = c `elem` [' ', '\t']


consolidateSpaces :: [Token] -> [Token]
consolidateSpaces []
  = []
consolidateSpaces (TnSpace _ : TnSpace n : rest)
  = consolidateSpaces (TnSpace n : rest)
consolidateSpaces (t : ts)
  = t : consolidateSpaces ts


resolveCommands :: [Token] -> Excepted [Token]
resolveCommands []
  = return []
resolveCommands (TnSymbol n '\\' : rest)
  = resolveCommands' n rest
resolveCommands (t : ts)
  = do tokens <- resolveCommands ts
       return (t : tokens)

resolveCommands' :: Int -> [Token] -> Excepted [Token]
resolveCommands' _ (TnWord n str : ts)
  = do tokens <- resolveCommands ts
       return (TnCommand n str : tokens)
resolveCommands' _ (TnNumber n str : _)
  = throw (ExceptLineMessage n ("Cannot escape number: " ++ str ++ "."))
resolveCommands' _ (TnSymbol n c : ts)
  = do tokens <- resolveCommands ts
       return (TnCommand n [c] : tokens)
resolveCommands' _ (TnSpace n : _)
  = throw (ExceptLineMessage n "Cannot escape white space.")
resolveCommands' _ (TnCommand n str : ts)
  = do tokens <- resolveCommands ts
       return (TnCommand n str : tokens)
resolveCommands' n []
  = throw (ExceptLineMessage n "Encountered unescaped \\ as last token.")



{- *** White Space Functions *** -}

isSpaceTok :: Token -> Bool
isSpaceTok (TnSpace _)
  = True
isSpaceTok _
  = False

dropSpaceToks :: [Token] -> [Token]
dropSpaceToks
  = dropWhile isSpaceTok

trimSpaceToks :: [Token] -> [Token]
trimSpaceToks
  = dropSpaceToks .> reverse .> dropSpaceToks .> reverse



{- *** Create an Exception based on a Token *** -}

tokenException :: Token -> String -> Exception
tokenException t str
  = ExceptLineMessage (tokenLine t) message
  where
    message :: String
    message = (str ++ " " ++ "Found token: " ++ tokenName t ++ ".")


tokenName :: Token -> String
tokenName (TnWord _ str)
  = "(word " ++ "\"" ++ str ++ "\"" ++ ")"
tokenName (TnNumber _ str)
  = "(number " ++ str ++ ")"
tokenName (TnSymbol _ c)
  = "(symbol " ++ "'" ++ [c] ++ "'" ++ ")"
tokenName (TnCommand _ str)
  = "(command " ++ "\"" ++ str ++ "\"" ++ ")"
tokenName (TnSpace _)
  = "(space)"

tokenLine :: Token -> Int
tokenLine (TnWord n _)
  = n
tokenLine (TnNumber n _)
  = n
tokenLine (TnSymbol n _)
  = n
tokenLine (TnCommand n _)
  = n
tokenLine (TnSpace n)
  = n

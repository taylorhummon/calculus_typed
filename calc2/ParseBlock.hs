
module ParseBlock (grabBlock, grabBlockNoHints)
       where

import Compose
import DataBlock
import DataLocation


import Except (Excepted, Exception(..), throw)
import Token (Token(..), trimSpaceToks)
import Parse (Grabbed, Grabbed'(..), BeginEndArgs,
              caseGrabbed, grabBeginEnd)
import Block (packBlock, updateHints)


grabBlock :: Location -> String -> [Token]
             -> (Location -> Int -> [Token] -> Excepted a)
             -> Grabbed (Block a)
grabBlock loc str tokens parseThings
  = caseGrabbed (grabBeginEnd str tokens)
    -- Missed
    (return Missed)
    -- Grabbed (n, beginEndArgs, inside) rest
    (\ (n, beginEndArgs, inside) rest ->
      do hints <- parseBeginEndHints beginEndArgs
         things <- parseThings loc n inside
         let rv = things $> packBlock $> updateHints hints
         return (Grabbed rv rest)
    )


grabBlockNoHints :: Location -> String -> [Token]
                    -> (Location -> Int -> [Token] -> Excepted a)
                    -> Grabbed (Block a)
grabBlockNoHints loc str tokens parseThings
  = caseGrabbed (grabBeginEnd str tokens)
    -- Missed
    (return Missed)
    -- Grabbed (n, beginEndArgs, inside) rest
    (\ (n, beginEndArgs, inside) rest ->
      do hints <- parseBeginEndHints beginEndArgs
         _ <- verifyNoHints n hints
         things <- parseThings loc n inside
         let rv = things $> packBlock
         return (Grabbed rv rest)
    )


verifyNoHints :: Int -> (Hint, Hint) -> Excepted ()
verifyNoHints _ (HiNone, HiNone)
  = return ()
verifyNoHints n _
  = throw (ExceptLineMessage n
           "vspace and pagebreak hints are not allowed.")


parseBeginEndHints :: BeginEndArgs -> Excepted (Hint, Hint)
parseBeginEndHints (maybeTokensBefore, maybeTokensAfter)
  = do hintBefore <- parseTheHint maybeTokensBefore
       hintAfter <- parseTheHint maybeTokensAfter
       return (hintBefore, hintAfter)


parseTheHint :: Maybe [Token] -> Excepted Hint
parseTheHint Nothing
  = return HiNone
parseTheHint (Just tokens)
  = do hints <- parseHints tokens
       return (decideTheHint hints)


decideTheHint :: [Hint] -> Hint
decideTheHint hints
  | HiPageBreak `elem` hints
    = HiPageBreak
  | otherwise
    = decideTheHint' hints

decideTheHint' :: [Hint] -> Hint
decideTheHint' []
  = HiNone
decideTheHint' (hint@(HiSpace  _) : _)
  = hint
decideTheHint' (_ : rest)
  = decideTheHint' rest


parseHints :: [Token] -> Excepted [Hint]
parseHints []
  = return []
parseHints tokens
  = do (toks, rest) <- breakArg [] tokens
       hint <- parseHint toks
       hints <- parseHints rest
       return (hint : hints)


breakArg :: [Token] -> [Token] -> Excepted ([Token], [Token])
breakArg acc []
  = return (reverse acc, [])
breakArg acc (TnSymbol _ ',' : toks)
  = return (reverse acc, toks)
breakArg acc (tok : toks)
  = breakArg (tok : acc) toks


parseHint :: [Token] -> Excepted Hint
parseHint
  = trimSpaceToks .> parseHint'

parseHint' :: [Token] -> Excepted Hint
parseHint' (TnWord _ "pagebreak" : [])
  = return HiPageBreak
parseHint' (TnWord n "vspace" : rest)
  = case rest
    of (TnSymbol _ '=' : TnNumber _ number : [])
         -> return (HiSpace number)
       (TnSymbol _ '=' : TnSymbol _ '-' : TnNumber _ number : [])
         -> return (HiSpace ('-' : number))
       _
         -> throw (ExceptLineMessage n "Could not parse vspace argument.")
parseHint' _
  = return HiNone

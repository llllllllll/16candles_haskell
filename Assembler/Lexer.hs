-- |
-- Module      : Assembler.Lexer
-- Copyright   : Joe Jevnik
--
-- License     : GPL-2
-- Maintainer  : joejev@gmail.org
-- Stability   : stable
-- Portability : GHC
--
-- Functions needed in the lexing process of 16candles source code.

module Assembler.Lexer
    ( buildExpressions  -- :: [Token] -> [Expression]
    , tokenize          -- :: String -> [Token]
    , resolveBraces     -- :: [Token] -> [Token]
    , resolveWhenUnless -- :: [Expression] -> [Expression]
    ) where

import Assembler.Core

import Control.Arrow (first)
import Data.Maybe    (fromMaybe)
import Text.Read     (readMaybe)

-- | Resolves the 'WhenToken' and 'UnlessToken' tokens.
resolveWhenUnless :: [Expression] -> [Expression]
resolveWhenUnless [] = []
resolveWhenUnless ([WhenToken,OpenBrace l]:es)
    = [InstrToken OpJmpf,InvalidToken l] : resolveWhenUnless es
resolveWhenUnless ([UnlessToken,OpenBrace l]:es)
    = [InstrToken OpJmpt,InvalidToken l] : resolveWhenUnless es
resolveWhenUnless ([CloseBrace l]:es)
    = [Label l] : resolveWhenUnless es
resolveWhenUnless (e:es) = e : resolveWhenUnless es

-- | Builds a list of 'Expression's from a list of 'Token's
buildExpressions :: [Token] -> [Expression]
buildExpressions [] = []
buildExpressions ts = let (es,ts') = span (/= NewLine) ts
                     in es : buildExpressions (drop 1 ts')

-- | Resolves braces into a string identifier.
resolveBraces :: [Token] -> [Token]
resolveBraces ts = resolveBraces' ts 0 []
resolveBraces' [] _ _ = []
resolveBraces' ((OpenBrace  ""):ts) n cs
    = OpenBrace (braceName n) : resolveBraces' ts (n + 1) (n:cs)
resolveBraces' ((CloseBrace ""):ts) n (c:cs)
    = CloseBrace (braceName c) : resolveBraces' ts n cs
resolveBraces' (t:ts) n cs = t : resolveBraces' ts n cs

-- | Gives the name of the brace based on an integer seed.
braceName :: Int -> String
braceName = (++) "C16_INTERNAL_BRACE_" . show

-- | Converts a 'String' into a list of 'Token's to be processed.
tokenize :: String -> [Token]
tokenize [] = []
tokenize cs = let (t,rs) = getToken cs
              in t : tokenize rs

-- | Pulls the next 'Token' out of a 'String' and returns whatever is left of
-- the 'String'.
getToken :: String -> (Token,String)
getToken = first parseToken . grabString . dropWhile (== ' ')
  where
      grabString ""             = ("\n","")
      grabString ('\n':cs)      = ("\n",cs)
      grabString ('{':cs)       = ("{",cs)
      grabString ('#':cs)       = grabString $ dropWhile (/= '\n') cs
      grabString cs             = takeNewSpace cs ""
      takeNewSpace "" rs        = (reverse rs,"")
      takeNewSpace (' ':cs) rs  = (reverse rs,cs)
      takeNewSpace ('\n':cs) rs = (reverse rs,'\n':cs)
      takeNewSpace ('{':cs)  rs = (reverse rs,'{':cs)
      takeNewSpace (c:cs) rs    = takeNewSpace cs (c:rs)

-- | Parses a 'Token' out of a 'String'.
parseToken :: String -> Token
parseToken "\n"     = NewLine
parseToken "{"      = OpenBrace  ""
parseToken "}"      = CloseBrace ""
parseToken ('@':cs) = Label cs
parseToken ('*':cs)
    | cs `elem` registerStrings = MemoryRegister
                                  $ fromMaybe (error "parseToken: bad register")
                                  $ parseRegister cs
    | otherwise                 = case readMaybe cs :: Maybe Word16 of
                                      Just n -> MemoryAddress n
                                      _      -> InvalidToken ('*':cs)
parseToken "when"   = WhenToken
parseToken "unless" = UnlessToken
parseToken cs = case ( parseRegister cs,parseInstruction cs
                     , readMaybe cs :: Maybe Word16) of
                    (Nothing,Just instr,Nothing) -> InstrToken instr
                    (Just reg,Nothing,Nothing)   -> RegToken reg
                    (Nothing,Nothing,Just n)     -> Literal n
                    (_,_,_)                      -> InvalidToken cs

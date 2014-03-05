-- |
-- Module      : Compiler.Lexer
-- Copyright   : Joe Jevnik
--
-- License     : GPL-2
-- Maintainer  : joejev@gmail.org
-- Stability   : stable
-- Portability : GHC
--
-- Functions needed in the lexing process of 16candles source code.

module Compiler.Lexer where

import Compiler.Data

import Control.Arrow (first)
import Data.Maybe    (fromMaybe)
import Text.Read     (readMaybe)

-- | Builds a list of 'Expression's from a list of 'Token's
buildExressions :: [Token] -> [Expression]
buildExressions [] = []
buildExressions ts = let (es,ts') = span (/= NewLine) ts
                     in es : buildExressions (drop 1 ts')

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
      grabString ('#':cs)       = grabString $ dropWhile (/= '\n') cs
      grabString cs             = takeNewSpace cs ""
      takeNewSpace "" rs        = (reverse rs,"")
      takeNewSpace (' ':cs) rs  = (reverse rs,cs)
      takeNewSpace ('\n':cs) rs = (reverse rs,'\n':cs)
      takeNewSpace (c:cs) rs    = takeNewSpace cs (c:rs)

-- | Parses a 'Token' out of a 'String'.
parseToken :: String -> Token
parseToken ['\n']   = NewLine
parseToken ('@':cs) = Label cs
parseToken ('*':cs)
    | cs `elem` registerStrings = MemoryRegister
                                  $ fromMaybe (error "parseToken: bad register")
                                  $ parseRegister cs
    | otherwise                 = case readMaybe cs :: Maybe Word16 of
                                      Just n -> MemoryAddress n
                                      _      -> InvalidToken ('*':cs)
parseToken cs = case ( parseRegister cs,parseInstruction cs
                     , readMaybe cs :: Maybe Word16) of
                    (Nothing,Just instr,Nothing) -> InstrToken instr
                    (Just reg,Nothing,Nothing)   -> RegToken reg
                    (Nothing,Nothing,Just n)     -> Literal n
                    (_,_,_)                      -> InvalidToken cs

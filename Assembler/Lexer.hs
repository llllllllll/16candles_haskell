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
    , resolveBraces     -- :: [Token] -> [Token]
    , resolveWhenUnless -- :: [Expression] -> [Expression]
    , lexC16            -- :: String -> Either String [Token]
    ) where

import Assembler.Core
import Assembler.LexerInternal

import Control.Arrow (first)
import Data.Maybe    (fromMaybe)

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

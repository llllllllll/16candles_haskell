{
-- |
-- Module      : Assembler.Lexer
-- Copyright   : Joe Jevnik
--
-- License     : GPL-2
-- Maintainer  : joejev@gmail.org
-- Stability   : stable
-- Portability : GHC
--
-- Data types used in the lexing and assembling process of 16candles code.
{-# LANGUAGE LambdaCase #-}
module Assembler.LexerInternal
    ( lexC16  -- String -> Either String [Token]
    ) where

import Assembler.Core

import Control.Monad (liftM)
import Data.Char (isSpace)
import Numeric (readHex)
import Text.Read (readMaybe)
}


%wrapper "monad"


$alpha   = [a-zA-Z]
$digit   = [0-9a-fA-F]

@id       = $alpha ($alpha | $digit | _)*
@register = ((r $digit | "ipt" | "spt" | "ac" (1 | 2) | "tst")(_ (f | b))?
             | "inp" (_ (r | w))?)
@number   = "0x"? $digit+
@instr    = ( "and"|"&&"|"or"|"||"|"xand"|"!&"|"xor"|"!|"|"inv"| "~"| "lshift"
              |"<<"|"rshift"|">>"|"add"|"+"|"sub"|"-"|"mul"|"*"|"div"|"/"|"mod"
              |"%"|"inc"|"++"|"dec"|"--"|"gt"|">"|"lt"|"<"|"gte"|">="|"lte"|"<="
              |"eq"|"=="|"neq"|"!="|"min"|"max"|"jmp"|"=>"|"jmpt"|"->"
              |"jmpf"|"<-"|"push"|":"|"pop"|"$"|"peek"|"@"|"flush"|"#"|"set"|"="
              |"mset"|":="|"swap"|"\\"|"halt"|"nop"|"read"|"write"|"term")

tokens :-
       <0, arguments>       $white # \n ;
       <0, arguments, star> \# .* \n    { (\_ _ -> return NewLine) `andBegin` 0 }
       <0>                  \@ @id      { label }
       <0>                  \{          { \_ _ -> return $ OpenBrace "" }
       <0>                  \}          { \_ _ -> return $ CloseBrace "" }
       <0>                  "when"      { \_ _ -> return WhenToken }
       <0>                  "unless"    { \_ _ -> return UnlessToken }
       <0>                  @instr      { instruction `andBegin` arguments }
       <0, arguments>       \n          { (\_ _ -> return NewLine) `andBegin` 0 }
       <arguments>          \*          { begin star }
       <star>               @register   { memReg `andBegin` arguments }
       <star>               @number     { memAddr `andBegin` arguments }
       <arguments>          @register   { register }
       <arguments>          @number     { literal }
       <arguments>          @id         { invalidToken }


{


alexEOF :: Alex Token
alexEOF = return EOF


parseNum :: String -> Maybe Word16
parseNum ('0':'x':cs) = parseHex cs
parseNum ('0':'X':cs) = parseHex cs
parseNum cs           = readMaybe cs


parseHex :: String -> Maybe Word16
parseHex cs = case readHex cs of
                  [(n,"")] -> Just n
                  _        -> Nothing



label :: AlexAction Token
label (_,_,_,cs) l = return . Label . takeWhile (not . isSpace) . drop 1
                     $ take l cs


invalidToken :: AlexAction Token
invalidToken (_,_,_,cs) l = return . InvalidToken $ take l cs


action :: (a -> Token) -> (String -> Maybe a) -> AlexAction Token
action t p (_,_,_,cs) l
    = let match = take l cs
      in case p match of
             Nothing -> alexError $ "Failed to parse '" ++ match ++ "'"
             Just r  -> return $ t r


instruction :: AlexAction Token
instruction = action InstrToken parseInstruction


memReg :: AlexAction Token
memReg = action MemoryRegister parseRegister


memAddr :: AlexAction Token
memAddr = action MemoryAddress parseNum


register :: AlexAction Token
register = action RegToken parseRegister


literal :: AlexAction Token
literal = action Literal parseNum


alexReadAll :: Alex [Token]
alexReadAll = alexMonadScan >>= \case
                                   EOF -> return []
                                   t   -> liftM ((:) t) alexReadAll


lexC16 :: String -> Either String [Token]
lexC16 cs = case runAlex cs alexReadAll of
                Left e   -> Left e
                Right ts -> Right ts
}

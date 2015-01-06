-- |
-- Module      : Assembler.Bytecode
-- Copyright   : Joe Jevnik
--
-- License     : GPL-2
-- Maintainer  : joejev@gmail.org
-- Stability   : stable
-- Portability : GHC
--
-- Functions that take validated expression sets and convert them to bytecode.

-- -----------------------------------------------------------------------------
-- Pre image of the operation bytes without their suffixes.

module Assembler.Bytecode
    ( toByteCode -- :: String -> IO ByteString
    ) where

import Assembler.Core
import Assembler.Lexer

import Data.ByteString                (ByteString)
import qualified Data.ByteString as B (pack,unpack,length,empty,concat)
import Data.List                      (find)
import System.Exit                    (exitFailure)


toByteCode :: FilePath -> String -> IO ByteString
toByteCode f src = case lexC16 src of
                       Left e -> putStrLn ("error: " ++ f ++ ':' : ' ' : e)
                                 >> return B.empty
                       Right ts -> let (ms,bs) = processTokens ts
                                   in if null ms
                                        then return $ B.concat $ reverse bs
                                        else mapM_ (putStrLn . errorJumpMiss) ms
                                                 >> return B.empty
  where
      processTokens = resolveJumps
                      . toUnresolvedJumps
                      . resolveWhenUnless
                      . buildExpressions
                      . resolveBraces
      errorJumpMiss j@(JumpMiss label line) = "error: " ++ f ++ ':' : show line
                                              ++ ":\n  " ++ showJumpMiss j

-- -----------------------------------------------------------------------------
-- Suffix handling.

-- | The bianry operators.
binOps :: [Instruction]
binOps = [ OpAnd,OpOr,OpXand,OpXor,OpLshift,OpRshift,OpAdd,OpSub,OpMul,OpDiv
         , OpMod,OpMod,OpMin,OpMax,OpGt,OpLt,OpEq,OpNeq,OpGte,OpLte ]

-- | Operators with exactly two paramaters.
unOps :: [Instruction]
unOps = [ OpInv,OpInc,OpDec,OpSet,OpSwap ]

-- | Operators with exactly 0 paramaters.
noParamOps :: [Instruction]
noParamOps = [ OpFlush,OpHalt,OpNop ]

-- | Operators with exactly one paramater.
onePOps :: [Instruction]
onePOps = [ OpPush,OpWrite,OpPop,OpPeek,OpRead ]

-- | The jump operators.
jmpOps :: [Instruction]
jmpOps = [ OpJmp,OpJmpt,OpJmpf ]

-- | Pre binary operator suffix.
preBinOpSuf ((Literal _):(Literal _):_)   = SuffixLitLit
preBinOpSuf ((Literal _):(RegToken _):_)  = SuffixLitReg
preBinOpSuf ((RegToken _):(Literal _):_)  = SuffixRegLit
preBinOpSuf ((RegToken _):(RegToken _):_) = SuffixRegReg
preBinOpSuf _ = error "preBinOpSuf: Invalid binary operator form"

-- | Pre unary operator suffix.
preUnOpSuf :: Expression -> Suffix
preUnOpSuf [Literal _,_]  = SuffixLit
preUnOpSuf [RegToken _,_] = SuffixReg
preUnOpSuf _ = error "preUnOpSuf: Invalid unary operator form"

-- | Gets the suffix for the given 'Expression' if there is one.
getSuffix :: Expression -> Suffix
getSuffix [InstrToken OpMset,Literal _,MemoryAddress _]   = SuffixLitMemaddr
getSuffix [InstrToken OpMset,RegToken _,MemoryAddress _]  = SuffixRegMemaddr
getSuffix [InstrToken OpMset,Literal _,MemoryRegister _]  = SuffixLitMemreg
getSuffix [InstrToken OpMset,RegToken _,MemoryRegister _] = SuffixRegMemreg
getSuffix [InstrToken OpMset,MemoryAddress _,RegToken _]  = SuffixMemaddr
getSuffix [InstrToken OpMset,MemoryRegister _,RegToken _] = SuffixMemreg
getSuffix [InstrToken OpWrite,Literal _]                  = SuffixLit
getSuffix [InstrToken OpWrite,RegToken _]                 = SuffixReg
getSuffix (InstrToken OpMset:_) = error "getSuffix: Invalid mset form"
getSuffix (InstrToken i:ps)
    | i `elem` binOps = preBinOpSuf ps
    | i `elem` unOps  = preUnOpSuf  ps
    | otherwise       = NoSuffix

-- -----------------------------------------------------------------------------
-- Jump resolution

-- | Generates a list of bytecode 'Expression's or an unresolved jump.
toUnresolvedJumps :: [Expression] -> [Either Expression ByteString]
toUnresolvedJumps = reverse . toUnresolvedJumps'

-- | Helper function to toUnresolvedJumps
toUnresolvedJumps' :: [Expression] -> [Either Expression ByteString]
toUnresolvedJumps' [] = []
toUnresolvedJumps' (j@(InstrToken OpJmp:_):es)  = Left j : toUnresolvedJumps' es
toUnresolvedJumps' (j@(InstrToken OpJmpt:_):es) = Left j : toUnresolvedJumps' es
toUnresolvedJumps' (j@(InstrToken OpJmpf:_):es) = Left j : toUnresolvedJumps' es
toUnresolvedJumps' (l@(Label _:_):es)           = Left l : toUnresolvedJumps' es
toUnresolvedJumps' (e:es) =  Right (expressionToBytes e) : toUnresolvedJumps' es

-- | Converts an expression to it's bytecode representation.
expressionToBytes :: Expression -> ByteString
expressionToBytes e@(InstrToken i:ps) =
    B.pack $ resolveOpcode i + (suffixToWord . getSuffix) e
         : concatMap resolveParam ps
expressionToBytes _ = B.empty

-- | Resolves the jumps commands and new labels.
resolveJumps :: [Either Expression ByteString] -> ([JumpMiss],[ByteString])
resolveJumps ees = let (ls,es) = populateJumpTable $ ees
                   in cleanUp ([],[]) $ map (resolveJump ls) es
  where
      resolveJump _ (_,Right cs) = Right cs
      resolveJump ls (n,Left [InstrToken i,InvalidToken name]) =
          case find ((==) name . fst) ls of
              Nothing    -> Left $ JumpMiss name n
              Just (_,b) -> Right $ B.pack $ resolveOpcode i : shortToCharList b
      resolveJump _ _ = error "resolveJumps: Non jump slipped through"
      cleanUp (ms,es) [] = (ms,reverse es)
      cleanUp (ms,es) (Right b:rs) = cleanUp (ms,b : es) rs
      cleanUp (ms,es) (Left e:rs)  = cleanUp (e : ms,es) rs

-- | Populates a table with pairs of label names and locations and returns the
-- list of expressions without the labels.
populateJumpTable :: [Either Expression ByteString]
                  -> ([(String,Word16)],[(Int,Either Expression ByteString)])
populateJumpTable es = populateJumpTable' 0 0 ([],[]) $ reverse es
  where
      populateJumpTable' _ _ (ls,rs) [] =
          (ls,rs)
      populateJumpTable' c n (ls,rs) (Left [Label l]:es) =
          populateJumpTable' c (n + 1) ((l,c):ls,rs) es
      populateJumpTable' c n (ls,rs) (r@(Right bs):es) =
          populateJumpTable' (c + fromIntegral (B.length bs)) (n + 1)
                                 (ls,(n,r) : rs) es
      populateJumpTable' c n (ls,rs) (l@(Left _):es) =
          populateJumpTable' (c + 3) (n + 1) (ls,(n,l) : rs) es

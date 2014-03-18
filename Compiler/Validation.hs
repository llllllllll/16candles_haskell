-- |
-- Module      : Compiler.Validation
-- Copyright   : Joe Jevnik
--
-- License     : GPL-2
-- Maintainer  : joejev@gmail.org
-- Stability   : stable
-- Portability : GHC
--
-- Functions needed in the source validation process of compiling.


module Compiler.Validation
    ( validateSource     -- :: String -> IO Bool
    , validateExpression -- :: Expression -> Maybe ExpressionError
    ) where

import Compiler.Data
import Compiler.Lexer

import Control.Arrow (second)
import Data.Maybe    (isJust,fromMaybe)

-- | Checks if the source string is valid.
validateSource :: String -> IO Bool
validateSource src = do
    let es = map (second (fromMaybe (error "validateSource: caught Nothing")))
             $ filter (isJust . snd) $ map (second validateExpression)
             $ zip [0..] $ buildExressions . tokenize $ src
    if null es
      then return True
      else mapM_ printErrMsg es >> return False
  where
      printErrMsg (l,ee) = putStrLn $ "Error: Line " ++ show (l + 1) ++ ":\n  "
                           ++ showExprError ee

-- | Validates a given expression return 'Nothing' if the 'Expression' is valid.
validateExpression :: Expression -> Maybe ExpressionError
validateExpression []                 = Nothing
validateExpression (InvalidToken t:_) = Just $ UnknownInstruction t
validateExpression (Label _:t:ts)     = Just $ TooManyParameters (t:ts)
validateExpression (Label _:_)        = Nothing
validateExpression (InstrToken n:cs)  = validateInstruction n cs
validateExpression _                  = Just $ MissingInstruction

validateInstruction :: Instruction -> Expression -> Maybe ExpressionError
validateInstruction n
    | n `elem` binOps     = validateBinOp
    | n `elem` unOps      = validateUnOp
    | n `elem` cmpOps     = validateCmpOp
    | n `elem` noParamOps = validateNoParamOp
    | n `elem` onePOps    = validateOnePOp n
    | n `elem` jmpOps     = validateJmpOp
    | otherwise           = validateMset
  where
      binOps     = [ OpAnd,OpOr,OpXand,OpXor,OpLshift,OpRshift,OpAdd,OpSub,OpMul
                   , OpDiv,OpMod,OpMod,OpMin,OpMax ]
      unOps      = [ OpInv,OpInc,OpDec,OpSet,OpSwap ]
      cmpOps     = [ OpGt,OpLt,OpEq,OpNeq,OpGte,OpLte ]
      noParamOps = [ OpFlush,OpHalt,OpNop ]
      onePOps    = [ OpPush,OpWrite,OpPop,OpPeek,OpRead ]
      jmpOps     = [ OpJmp,OpJmpt,OpJmpf ]

-- | Checks if a binary operation is formed as a valid 'Expression'.
validateBinOp :: Expression -> Maybe ExpressionError
validateBinOp [p1,p2,p3]
    | isRegOrLit p1 && isRegOrLit p2 && isReg p3 = Nothing
    | isRegOrLit p1 && isRegOrLit p2             = Just $ ExpectedRegister p3
    | isRegOrLit p1 && isReg p3                  = Just $ ExpectedRegOrLit p2
    | isRegOrLit p2 && isReg p3                  = Just $ ExpectedRegOrLit p1
    | otherwise                                  = Just   MismatchedParameters
validateBinOp (_:_:_:t:ts)                       = Just $ TooManyParameters
                                                   (t:ts)
validateBinOp []                                 = Just   MissingParameters
validateBinOp [_]                                = Just   MissingParameters
validateBinOp [_,_]                              = Just   MissingParameters


-- | Checks if a comparison operation is formed as a valid 'Expression'.
validateCmpOp :: Expression -> Maybe ExpressionError
validateCmpOp [p1,p2]
    | isRegOrLit p1 && isRegOrLit p2 = Nothing
    | isRegOrLit p1                  = Just $ ExpectedRegOrLit p2
    | isRegOrLit p2                  = Just $ ExpectedRegOrLit p1
    | otherwise                      = Just   MismatchedParameters
validateCmpOp []                     = Just   MissingParameters
validateCmpOp [_]                    = Just   MissingParameters
validateCmpOp (_:_:t:ts)             = Just $ TooManyParameters (t:ts)

-- | Checks if a unary operation is formed as a valid 'Expression'.
validateUnOp :: Expression -> Maybe ExpressionError
validateUnOp [p1,p2]
    | isRegOrLit p1 && isReg p2 = Nothing
    | isRegOrLit p1             = Just $ ExpectedRegister p2
    | isReg p2                  = Just $ ExpectedRegOrLit p1
    | otherwise                 = Just   MismatchedParameters
validateUnOp (_:_:t:ts)         = Just $ TooManyParameters (t:ts)
validateUnOp []                 = Just   MissingParameters
validateUnOp [_]                = Just   MissingParameters

-- | Checks if an operation with no paramaters is a valid 'Expression'.
validateNoParamOp [] = Nothing
validateNoParamOp ts = Just $ TooManyParameters ts

-- | Checks if an operation with one paramater is a valid 'Expression'.
validateOnePOp :: Instruction -> Expression -> Maybe ExpressionError
validateOnePOp OpPop [RegToken _]  = Nothing
validateOnePOp OpPop [t]           = Just $ ExpectedRegister t
validateOnePOp OpPeek [RegToken _] = Nothing
validateOnePOp OpPeek [t]          = Just $ ExpectedRegister t
validateOnePOp OpRead [RegToken _] = Nothing
validateOnePOp OpRead [t]          = Just $ ExpectedRegister t
validateOnePOp OpPush [p]
    | isRegOrLit p                 = Nothing
    | otherwise                    = Just $ ExpectedRegOrLit p
validateOnePOp OpWrite [p]
    | isRegOrLit p                 = Nothing
    | otherwise                    = Just $ ExpectedRegOrLit p
validateOnePOp _ []                = Just MissingParameters
validateOnePOp _ (_:t:ts)          = Just $ TooManyParameters (t:ts)
validateOnePOp _ _                 = Just MismatchedParameters

-- | Validates the jump commands.
validateJmpOp :: Expression -> Maybe ExpressionError
validateJmpOp [InvalidToken _] = Nothing
validateJmpOp (_:t:ts)  = Just $ TooManyParameters (t:ts)
validateJmpOp []        = Just   MissingParameters

-- | Validates the mset (:=) command.
validateMset :: Expression -> Maybe ExpressionError
validateMset [RegToken _,MemoryAddress _]  = Nothing
validateMset [RegToken _,MemoryRegister _] = Nothing
validateMset [RegToken _,t]                = Just $ ExpectedMemory t
validateMset [Literal _,MemoryAddress _]   = Nothing
validateMset [Literal _,MemoryRegister _]  = Nothing
validateMset [Literal _,t]                 = Just $ ExpectedMemory t
validateMset [MemoryAddress _,RegToken _]  = Nothing
validateMset [MemoryAddress _,t]           = Just $ ExpectedRegister t
validateMset [MemoryRegister _,RegToken _] = Nothing
validateMset [MemoryRegister _,t]          = Just $ ExpectedRegister t
validateMset []                            = Just   MissingParameters
validateMset [_]                           = Just   MissingParameters
validateMset (_:_:t:ts)                    = Just $ TooManyParameters (t:ts)
validateMset _                             = Just   MismatchedParameters

-- | Checks if a given 'Token' is a 'Register'
isReg :: Token -> Bool
isReg (RegToken _) = True
isReg _            = False

-- | Checks if a given 'Token' is a 'Register' or a 'Literal'
isRegOrLit :: Token -> Bool
isRegOrLit (RegToken _) = True
isRegOrLit (Literal _)  = True
isRegOrLit _            = False

{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-missing-signatures #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LINE 4 "LexLI.x" #-}
{-# OPTIONS -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -w #-}

{-# LANGUAGE PatternSynonyms #-}

module LexLI where

import Prelude

import qualified Data.Bits
import Data.Char     (ord)
import Data.Function (on)
import Data.Word     (Word8)
#if __GLASGOW_HASKELL__ >= 603
#include "ghcconfig.h"
#elif defined(__GLASGOW_HASKELL__)
#include "config.h"
#endif
#if __GLASGOW_HASKELL__ >= 503
import Data.Array
#else
import Array
#endif
#if __GLASGOW_HASKELL__ >= 503
import Data.Array.Base (unsafeAt)
import GHC.Exts
#else
import GlaExts
#endif
alex_tab_size :: Int
alex_tab_size = 8
alex_base :: AlexAddr
alex_base = AlexA#
  "\xf8\xff\xff\xff\xd6\xff\xff\xff\x5b\x00\x00\x00\x00\x00\x00\x00\x12\x00\x00\x00\x56\x00\x00\x00"#

alex_table :: AlexAddr
alex_table = AlexA#
  "\x00\x00\x04\x00\x04\x00\x04\x00\x04\x00\x04\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x04\x00\x04\x00\x04\x00\x04\x00\x04\x00\x03\x00\x03\x00\x03\x00\x03\x00\x00\x00\x03\x00\x00\x00\x03\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x04\x00\x03\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x03\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x05\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x00\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x00\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

alex_check :: AlexAddr
alex_check = AlexA#
  "\xff\xff\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x20\x00\xff\xff\xff\xff\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\xff\xff\x2d\x00\xff\xff\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x20\x00\x3b\x00\xff\xff\x3d\x00\xff\xff\xff\xff\xff\xff\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x56\x00\x57\x00\x58\x00\x59\x00\x5a\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x61\x00\x62\x00\x63\x00\x64\x00\x65\x00\x66\x00\x67\x00\x68\x00\x69\x00\x6a\x00\x6b\x00\x6c\x00\x6d\x00\x6e\x00\x6f\x00\x70\x00\x71\x00\x72\x00\x73\x00\x74\x00\x75\x00\x76\x00\x77\x00\x78\x00\x79\x00\x7a\x00\x7b\x00\xff\xff\x7d\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x27\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x56\x00\x57\x00\x58\x00\x59\x00\x5a\x00\xff\xff\xff\xff\xff\xff\xff\xff\x5f\x00\xc3\x00\x61\x00\x62\x00\x63\x00\x64\x00\x65\x00\x66\x00\x67\x00\x68\x00\x69\x00\x6a\x00\x6b\x00\x6c\x00\x6d\x00\x6e\x00\x6f\x00\x70\x00\x71\x00\x72\x00\x73\x00\x74\x00\x75\x00\x76\x00\x77\x00\x78\x00\x79\x00\x7a\x00\x80\x00\x81\x00\x82\x00\x83\x00\x84\x00\x85\x00\x86\x00\x87\x00\x88\x00\x89\x00\x8a\x00\x8b\x00\x8c\x00\x8d\x00\x8e\x00\x8f\x00\x90\x00\x91\x00\x92\x00\x93\x00\x94\x00\x95\x00\x96\x00\xff\xff\x98\x00\x99\x00\x9a\x00\x9b\x00\x9c\x00\x9d\x00\x9e\x00\x9f\x00\xa0\x00\xa1\x00\xa2\x00\xa3\x00\xa4\x00\xa5\x00\xa6\x00\xa7\x00\xa8\x00\xa9\x00\xaa\x00\xab\x00\xac\x00\xad\x00\xae\x00\xaf\x00\xb0\x00\xb1\x00\xb2\x00\xb3\x00\xb4\x00\xb5\x00\xb6\x00\xff\xff\xb8\x00\xb9\x00\xba\x00\xbb\x00\xbc\x00\xbd\x00\xbe\x00\xbf\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xc3\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

alex_deflt :: AlexAddr
alex_deflt = AlexA#
  "\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

alex_accept = listArray (0 :: Int, 5)
  [ AlexAccNone
  , AlexAcc 2
  , AlexAcc 1
  , AlexAcc 0
  , AlexAccSkip
  , AlexAccNone
  ]

alex_actions = array (0 :: Int, 3)
  [ (2,alex_action_3)
  , (1,alex_action_2)
  , (0,alex_action_1)
  ]

alex_action_1 = tok (eitherResIdent TV)
alex_action_2 = tok (eitherResIdent TV)
alex_action_3 = tok TI

#define ALEX_GHC 1
#define ALEX_NOPRED 1
-- -----------------------------------------------------------------------------
-- ALEX TEMPLATE
--
-- This code is in the PUBLIC DOMAIN; you may copy it freely and use
-- it for any purpose whatsoever.

-- -----------------------------------------------------------------------------
-- INTERNALS and main scanner engine

#ifdef ALEX_GHC
#  define ILIT(n) n#
#  define IBOX(n) (I# (n))
#  define FAST_INT Int#
-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#  if __GLASGOW_HASKELL__ > 706
#    define GTE(n,m) (tagToEnum# (n >=# m))
#    define EQ(n,m) (tagToEnum# (n ==# m))
#  else
#    define GTE(n,m) (n >=# m)
#    define EQ(n,m) (n ==# m)
#  endif
#  define PLUS(n,m) (n +# m)
#  define MINUS(n,m) (n -# m)
#  define TIMES(n,m) (n *# m)
#  define NEGATE(n) (negateInt# (n))
#  define IF_GHC(x) (x)
#else
#  define ILIT(n) (n)
#  define IBOX(n) (n)
#  define FAST_INT Int
#  define GTE(n,m) (n >= m)
#  define EQ(n,m) (n == m)
#  define PLUS(n,m) (n + m)
#  define MINUS(n,m) (n - m)
#  define TIMES(n,m) (n * m)
#  define NEGATE(n) (negate (n))
#  define IF_GHC(x)
#endif

#ifdef ALEX_GHC
data AlexAddr = AlexA# Addr#
-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#if __GLASGOW_HASKELL__ < 503
uncheckedShiftL# = shiftL#
#endif

{-# INLINE alexIndexInt16OffAddr #-}
alexIndexInt16OffAddr :: AlexAddr -> Int# -> Int#
alexIndexInt16OffAddr (AlexA# arr) off =
#ifdef WORDS_BIGENDIAN
  narrow16Int# i
  where
        i    = word2Int# ((high `uncheckedShiftL#` 8#) `or#` low)
        high = int2Word# (ord# (indexCharOffAddr# arr (off' +# 1#)))
        low  = int2Word# (ord# (indexCharOffAddr# arr off'))
        off' = off *# 2#
#else
#if __GLASGOW_HASKELL__ >= 901
  int16ToInt#
#endif
    (indexInt16OffAddr# arr off)
#endif
#else
alexIndexInt16OffAddr arr off = arr ! off
#endif

#ifdef ALEX_GHC
{-# INLINE alexIndexInt32OffAddr #-}
alexIndexInt32OffAddr :: AlexAddr -> Int# -> Int#
alexIndexInt32OffAddr (AlexA# arr) off =
#ifdef WORDS_BIGENDIAN
  narrow32Int# i
  where
   i    = word2Int# ((b3 `uncheckedShiftL#` 24#) `or#`
                     (b2 `uncheckedShiftL#` 16#) `or#`
                     (b1 `uncheckedShiftL#` 8#) `or#` b0)
   b3   = int2Word# (ord# (indexCharOffAddr# arr (off' +# 3#)))
   b2   = int2Word# (ord# (indexCharOffAddr# arr (off' +# 2#)))
   b1   = int2Word# (ord# (indexCharOffAddr# arr (off' +# 1#)))
   b0   = int2Word# (ord# (indexCharOffAddr# arr off'))
   off' = off *# 4#
#else
#if __GLASGOW_HASKELL__ >= 901
  int32ToInt#
#endif
    (indexInt32OffAddr# arr off)
#endif
#else
alexIndexInt32OffAddr arr off = arr ! off
#endif

#ifdef ALEX_GHC

#if __GLASGOW_HASKELL__ < 503
quickIndex arr i = arr ! i
#else
-- GHC >= 503, unsafeAt is available from Data.Array.Base.
quickIndex = unsafeAt
#endif
#else
quickIndex arr i = arr ! i
#endif

-- -----------------------------------------------------------------------------
-- Main lexing routines

data AlexReturn a
  = AlexEOF
  | AlexError  !AlexInput
  | AlexSkip   !AlexInput !Int
  | AlexToken  !AlexInput !Int a

-- alexScan :: AlexInput -> StartCode -> AlexReturn a
alexScan input__ IBOX(sc)
  = alexScanUser undefined input__ IBOX(sc)

alexScanUser user__ input__ IBOX(sc)
  = case alex_scan_tkn user__ input__ ILIT(0) input__ sc AlexNone of
  (AlexNone, input__') ->
    case alexGetByte input__ of
      Nothing ->
#ifdef ALEX_DEBUG
                                   trace ("End of input.") $
#endif
                                   AlexEOF
      Just _ ->
#ifdef ALEX_DEBUG
                                   trace ("Error.") $
#endif
                                   AlexError input__'

  (AlexLastSkip input__'' len, _) ->
#ifdef ALEX_DEBUG
    trace ("Skipping.") $
#endif
    AlexSkip input__'' len

  (AlexLastAcc k input__''' len, _) ->
#ifdef ALEX_DEBUG
    trace ("Accept.") $
#endif
    AlexToken input__''' len (alex_actions ! k)


-- Push the input through the DFA, remembering the most recent accepting
-- state it encountered.

alex_scan_tkn user__ orig_input len input__ s last_acc =
  input__ `seq` -- strict in the input
  let
  new_acc = (check_accs (alex_accept `quickIndex` IBOX(s)))
  in
  new_acc `seq`
  case alexGetByte input__ of
     Nothing -> (new_acc, input__)
     Just (c, new_input) ->
#ifdef ALEX_DEBUG
      trace ("State: " ++ show IBOX(s) ++ ", char: " ++ show c) $
#endif
      case fromIntegral c of { IBOX(ord_c) ->
        let
                base   = alexIndexInt32OffAddr alex_base s
                offset = PLUS(base,ord_c)

                new_s = if GTE(offset,ILIT(0))
                          && let check  = alexIndexInt16OffAddr alex_check offset
                             in  EQ(check,ord_c)
                          then alexIndexInt16OffAddr alex_table offset
                          else alexIndexInt16OffAddr alex_deflt s
        in
        case new_s of
            ILIT(-1) -> (new_acc, input__)
                -- on an error, we want to keep the input *before* the
                -- character that failed, not after.
            _ -> alex_scan_tkn user__ orig_input
#ifdef ALEX_LATIN1
                   PLUS(len,ILIT(1))
                   -- issue 119: in the latin1 encoding, *each* byte is one character
#else
                   (if c < 0x80 || c >= 0xC0 then PLUS(len,ILIT(1)) else len)
                   -- note that the length is increased ONLY if this is the 1st byte in a char encoding)
#endif
                   new_input new_s new_acc
      }
  where
        check_accs (AlexAccNone) = last_acc
        check_accs (AlexAcc a  ) = AlexLastAcc a input__ IBOX(len)
        check_accs (AlexAccSkip) = AlexLastSkip  input__ IBOX(len)
#ifndef ALEX_NOPRED
        check_accs (AlexAccPred a predx rest)
           | predx user__ orig_input IBOX(len) input__
           = AlexLastAcc a input__ IBOX(len)
           | otherwise
           = check_accs rest
        check_accs (AlexAccSkipPred predx rest)
           | predx user__ orig_input IBOX(len) input__
           = AlexLastSkip input__ IBOX(len)
           | otherwise
           = check_accs rest
#endif

data AlexLastAcc
  = AlexNone
  | AlexLastAcc !Int !AlexInput !Int
  | AlexLastSkip     !AlexInput !Int

data AlexAcc user
  = AlexAccNone
  | AlexAcc Int
  | AlexAccSkip
#ifndef ALEX_NOPRED
  | AlexAccPred Int (AlexAccPred user) (AlexAcc user)
  | AlexAccSkipPred (AlexAccPred user) (AlexAcc user)

type AlexAccPred user = user -> AlexInput -> Int -> AlexInput -> Bool

-- -----------------------------------------------------------------------------
-- Predicates on a rule

alexAndPred p1 p2 user__ in1 len in2
  = p1 user__ in1 len in2 && p2 user__ in1 len in2

--alexPrevCharIsPred :: Char -> AlexAccPred _
alexPrevCharIs c _ input__ _ _ = c == alexInputPrevChar input__

alexPrevCharMatches f _ input__ _ _ = f (alexInputPrevChar input__)

--alexPrevCharIsOneOfPred :: Array Char Bool -> AlexAccPred _
alexPrevCharIsOneOf arr _ input__ _ _ = arr ! alexInputPrevChar input__

--alexRightContext :: Int -> AlexAccPred _
alexRightContext IBOX(sc) user__ _ _ input__ =
     case alex_scan_tkn user__ input__ ILIT(0) input__ sc AlexNone of
          (AlexNone, _) -> False
          _ -> True
        -- TODO: there's no need to find the longest
        -- match when checking the right context, just
        -- the first match will do.
#endif
{-# LINE 50 "LexLI.x" #-}
-- | Create a token with position.
tok :: (String -> Tok) -> (Posn -> String -> Token)
tok f p = PT p . f

-- | Token without position.
data Tok
  = TK {-# UNPACK #-} !TokSymbol  -- ^ Reserved word or symbol.
  | TL !String                    -- ^ String literal.
  | TI !String                    -- ^ Integer literal.
  | TV !String                    -- ^ Identifier.
  | TD !String                    -- ^ Float literal.
  | TC !String                    -- ^ Character literal.
  deriving (Eq, Show, Ord)

-- | Smart constructor for 'Tok' for the sake of backwards compatibility.
pattern TS :: String -> Int -> Tok
pattern TS t i = TK (TokSymbol t i)

-- | Keyword or symbol tokens have a unique ID.
data TokSymbol = TokSymbol
  { tsText :: String
      -- ^ Keyword or symbol text.
  , tsID   :: !Int
      -- ^ Unique ID.
  } deriving (Show)

-- | Keyword/symbol equality is determined by the unique ID.
instance Eq  TokSymbol where (==)    = (==)    `on` tsID

-- | Keyword/symbol ordering is determined by the unique ID.
instance Ord TokSymbol where compare = compare `on` tsID

-- | Token with position.
data Token
  = PT  Posn Tok
  | Err Posn
  deriving (Eq, Show, Ord)

-- | Pretty print a position.
printPosn :: Posn -> String
printPosn (Pn _ l c) = "line " ++ show l ++ ", column " ++ show c

-- | Pretty print the position of the first token in the list.
tokenPos :: [Token] -> String
tokenPos (t:_) = printPosn (tokenPosn t)
tokenPos []    = "end of file"

-- | Get the position of a token.
tokenPosn :: Token -> Posn
tokenPosn (PT p _) = p
tokenPosn (Err p)  = p

-- | Get line and column of a token.
tokenLineCol :: Token -> (Int, Int)
tokenLineCol = posLineCol . tokenPosn

-- | Get line and column of a position.
posLineCol :: Posn -> (Int, Int)
posLineCol (Pn _ l c) = (l,c)

-- | Convert a token into "position token" form.
mkPosToken :: Token -> ((Int, Int), String)
mkPosToken t = (tokenLineCol t, tokenText t)

-- | Convert a token to its text.
tokenText :: Token -> String
tokenText t = case t of
  PT _ (TS s _) -> s
  PT _ (TL s)   -> show s
  PT _ (TI s)   -> s
  PT _ (TV s)   -> s
  PT _ (TD s)   -> s
  PT _ (TC s)   -> s
  Err _         -> "#error"

-- | Convert a token to a string.
prToken :: Token -> String
prToken t = tokenText t

-- | Finite map from text to token organized as binary search tree.
data BTree
  = N -- ^ Nil (leaf).
  | B String Tok BTree BTree
      -- ^ Binary node.
  deriving (Show)

-- | Convert potential keyword into token or use fallback conversion.
eitherResIdent :: (String -> Tok) -> String -> Tok
eitherResIdent tv s = treeFind resWords
  where
  treeFind N = tv s
  treeFind (B a t left right) =
    case compare s a of
      LT -> treeFind left
      GT -> treeFind right
      EQ -> t

-- | The keywords and symbols of the language organized as binary search tree.
resWords :: BTree
resWords =
  b "/" 6
    (b "*" 3 (b ")" 2 (b "(" 1 N N) N) (b "-" 5 (b "+" 4 N N) N))
    (b "while" 9 (b "=" 8 (b ";" 7 N N) N) (b "}" 11 (b "{" 10 N N) N))
  where
  b s n = B bs (TS bs n)
    where
    bs = s

-- | Unquote string literal.
unescapeInitTail :: String -> String
unescapeInitTail = id . unesc . tail . id
  where
  unesc s = case s of
    '\\':c:cs | elem c ['\"', '\\', '\''] -> c : unesc cs
    '\\':'n':cs  -> '\n' : unesc cs
    '\\':'t':cs  -> '\t' : unesc cs
    '\\':'r':cs  -> '\r' : unesc cs
    '\\':'f':cs  -> '\f' : unesc cs
    '"':[]       -> []
    c:cs         -> c : unesc cs
    _            -> []

-------------------------------------------------------------------
-- Alex wrapper code.
-- A modified "posn" wrapper.
-------------------------------------------------------------------

data Posn = Pn !Int !Int !Int
  deriving (Eq, Show, Ord)

alexStartPos :: Posn
alexStartPos = Pn 0 1 1

alexMove :: Posn -> Char -> Posn
alexMove (Pn a l c) '\t' = Pn (a+1)  l     (((c+7) `div` 8)*8+1)
alexMove (Pn a l c) '\n' = Pn (a+1) (l+1)   1
alexMove (Pn a l c) _    = Pn (a+1)  l     (c+1)

type Byte = Word8

type AlexInput = (Posn,     -- current position,
                  Char,     -- previous char
                  [Byte],   -- pending bytes on the current char
                  String)   -- current input string

tokens :: String -> [Token]
tokens str = go (alexStartPos, '\n', [], str)
    where
      go :: AlexInput -> [Token]
      go inp@(pos, _, _, str) =
               case alexScan inp 0 of
                AlexEOF                   -> []
                AlexError (pos, _, _, _)  -> [Err pos]
                AlexSkip  inp' len        -> go inp'
                AlexToken inp' len act    -> act pos (take len str) : (go inp')

alexGetByte :: AlexInput -> Maybe (Byte,AlexInput)
alexGetByte (p, c, (b:bs), s) = Just (b, (p, c, bs, s))
alexGetByte (p, _, [], s) =
  case s of
    []  -> Nothing
    (c:s) ->
             let p'     = alexMove p c
                 (b:bs) = utf8Encode c
              in p' `seq` Just (b, (p', c, bs, s))

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (p, c, bs, s) = c

-- | Encode a Haskell String to a list of Word8 values, in UTF8 format.
utf8Encode :: Char -> [Word8]
utf8Encode = map fromIntegral . go . ord
  where
  go oc
   | oc <= 0x7f       = [oc]

   | oc <= 0x7ff      = [ 0xc0 + (oc `Data.Bits.shiftR` 6)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]

   | oc <= 0xffff     = [ 0xe0 + (oc `Data.Bits.shiftR` 12)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]
   | otherwise        = [ 0xf0 + (oc `Data.Bits.shiftR` 18)
                        , 0x80 + ((oc `Data.Bits.shiftR` 12) Data.Bits..&. 0x3f)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]
